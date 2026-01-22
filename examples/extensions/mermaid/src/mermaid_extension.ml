(** Mermaid diagram extension for odoc.

    Renders [{@mermaid[...]}] code blocks as diagrams. By default uses client-side
    JavaScript, but can render server-side to PNG/SVG with format option
    (requires mermaid-cli/mmdc).

    Example:
    {[
      {@mermaid theme=forest[
        graph LR
          A[Start] --> B{Decision}
          B -->|Yes| C[OK]
          B -->|No| D[Cancel]
      ]}
    ]}
*)

module Api = Odoc_extension_api
module Block = Odoc_document.Types.Block
module Inline = Odoc_document.Types.Inline

(** Mermaid.js CDN URL *)
let mermaid_js_url = "https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"

(** Generate a unique ID for each diagram *)
let diagram_counter = ref 0

let fresh_id () =
  incr diagram_counter;
  Printf.sprintf "mermaid-diagram-%d" !diagram_counter

(** Extract option values *)
let get_theme tags =
  Api.get_binding "theme" tags
  |> Option.value ~default:"default"

let get_format tags =
  Api.get_binding "format" tags

let get_filename tags =
  Api.get_binding "filename" tags

let get_dimensions tags =
  let width = Api.get_binding "width" tags in
  let height = Api.get_binding "height" tags in
  (width, height)

(** Build inline style string from dimensions *)
let make_style width height =
  let parts = [] in
  let parts = match width with
    | Some w -> Printf.sprintf "width: %s" w :: parts
    | None -> parts
  in
  let parts = match height with
    | Some h -> Printf.sprintf "height: %s" h :: parts
    | None -> parts
  in
  match parts with
  | [] -> ""
  | ps -> String.concat "; " (List.rev ps)

(** HTML-escape content for safe embedding *)
let html_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&quot;"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Puppeteer config for environments that need --no-sandbox *)
let puppeteer_config = {|{"args": ["--no-sandbox", "--disable-setuid-sandbox"]}|}

(** Run mmdc (mermaid-cli) to render to a specific format *)
let run_mmdc ~theme ~format content =
  let tmp_in = Filename.temp_file "odoc_mermaid_" ".mmd" in
  let tmp_out = Filename.temp_file "odoc_mermaid_" ("." ^ format) in
  let tmp_config = Filename.temp_file "odoc_mermaid_" ".json" in
  Fun.protect ~finally:(fun () ->
    (try Sys.remove tmp_in with _ -> ());
    (try Sys.remove tmp_out with _ -> ());
    (try Sys.remove tmp_config with _ -> ())
  ) (fun () ->
    (* Write Mermaid content *)
    let oc = open_out tmp_in in
    output_string oc content;
    close_out oc;
    (* Write puppeteer config for --no-sandbox *)
    let oc = open_out tmp_config in
    output_string oc puppeteer_config;
    close_out oc;
    (* Run mmdc command with puppeteer config *)
    let cmd = Printf.sprintf "mmdc -i %s -o %s -t %s -b transparent -p %s 2>&1"
      (Filename.quote tmp_in) (Filename.quote tmp_out) theme (Filename.quote tmp_config) in
    let ic = Unix.open_process_in cmd in
    let error_output = Buffer.create 256 in
    (try
      while true do
        Buffer.add_string error_output (input_line ic);
        Buffer.add_char error_output '\n'
      done
    with End_of_file -> ());
    let status = Unix.close_process_in ic in
    match status with
    | Unix.WEXITED 0 ->
        (* Read the output file *)
        let ic = open_in_bin tmp_out in
        let len = in_channel_length ic in
        let data = Bytes.create len in
        really_input ic data 0 len;
        close_in ic;
        Ok data
    | _ ->
        Error (Buffer.contents error_output)
  )

module Mermaid_handler : Api.Code_Block_Extension = struct
  let prefix = "mermaid"

  let to_document meta content =
    let id = fresh_id () in
    let theme = get_theme meta.Api.tags in
    let format = get_format meta.Api.tags in
    let filename_opt = get_filename meta.Api.tags in
    let (width, height) = get_dimensions meta.Api.tags in
    let style = make_style width height in
    let style_attr = if style = "" then "" else Printf.sprintf " style=\"%s\"" style in

    match format with
    | Some "png" | Some "svg" ->
        (* Server-side rendering with mmdc *)
        let fmt = match format with Some f -> f | None -> "png" in
        let base_filename = match filename_opt with
          | Some f -> f
          | None -> Printf.sprintf "mermaid-%s.%s" id fmt
        in
        (match run_mmdc ~theme ~format:fmt content with
        | Ok data ->
            let html = Printf.sprintf
              {|<div id="%s" class="odoc-mermaid-diagram"%s><img src="%s" alt="Mermaid diagram" /></div>|}
              id style_attr base_filename
            in
            let block = Block.[{
              attr = ["odoc-mermaid"];
              desc = Raw_markup ("html", html)
            }] in
            Some {
              Api.content = block;
              overrides = [];
              resources = [];
              assets = [{ Api.asset_filename = base_filename; asset_content = data }];
            }
        | Error err ->
            (* Show error message *)
            let html = Printf.sprintf
              "<div id=\"%s\" class=\"odoc-mermaid-diagram odoc-mermaid-error\"><pre style=\"color: red;\">Error rendering Mermaid diagram (is mmdc installed?):\n%s</pre><pre>%s</pre></div>"
              id err (html_escape content)
            in
            let block = Block.[{
              attr = ["odoc-mermaid"; "odoc-mermaid-error"];
              desc = Raw_markup ("html", html)
            }] in
            Some {
              Api.content = block;
              overrides = [];
              resources = [];
              assets = [];
            })

    | Some unknown_format ->
        let html = Printf.sprintf
          {|<div class="odoc-mermaid-error"><pre style="color: red;">Unknown format: %s (supported: png, svg)</pre></div>|}
          unknown_format
        in
        let block = Block.[{
          attr = ["odoc-mermaid-error"];
          desc = Raw_markup ("html", html)
        }] in
        Some {
          Api.content = block;
          overrides = [];
          resources = [];
          assets = [];
        }

    | None ->
        (* Default: client-side JavaScript rendering *)
        let html = Printf.sprintf
          {|<div id="%s" class="odoc-mermaid-diagram"%s><pre class="mermaid">%s</pre></div>|}
          id style_attr (html_escape content)
        in
        let init_script = Printf.sprintf {|
if (typeof window.mermaidInitialized === 'undefined') {
  window.mermaidInitialized = true;
  mermaid.initialize({
    startOnLoad: true,
    theme: '%s',
    securityLevel: 'loose'
  });
}
|} theme
        in
        let block = Block.[{
          attr = ["odoc-mermaid"];
          desc = Raw_markup ("html", html)
        }] in
        Some {
          Api.content = block;
          overrides = [];
          resources = [
            Api.Js_url mermaid_js_url;
            Api.Js_inline init_script;
          ];
          assets = [];
        }
end

(** CSS for mermaid diagrams *)
let mermaid_css = {|
.odoc-mermaid-diagram {
  margin: 1em 0;
  overflow: auto;
}

.odoc-mermaid-diagram svg,
.odoc-mermaid-diagram img {
  max-width: 100%;
  height: auto;
}

.odoc-mermaid-diagram pre.mermaid {
  background: transparent;
}

.odoc-mermaid-error pre {
  color: #c00;
}
|}

(** Extension documentation *)
let extension_info : Api.extension_info = {
  info_kind = `Code_block;
  info_prefix = "mermaid";
  info_description = "Render Mermaid diagrams (flowcharts, sequence diagrams, etc.). Uses client-side JS by default, or server-side mmdc with format=png|svg.";
  info_options = [
    { opt_name = "format"; opt_description = "Output format: png, svg (requires mmdc), or omit for client-side JS"; opt_default = None };
    { opt_name = "theme"; opt_description = "Mermaid theme (default, forest, dark, neutral)"; opt_default = Some "default" };
    { opt_name = "width"; opt_description = "CSS width (e.g., 500px, 100%)"; opt_default = None };
    { opt_name = "height"; opt_description = "CSS height"; opt_default = None };
    { opt_name = "filename"; opt_description = "Output filename for server-side rendering"; opt_default = Some "auto-generated" };
  ];
  info_example = Some "{@mermaid format=png theme=forest[graph LR; A-->B]}";
}

let () =
  Api.Registry.register_code_block (module Mermaid_handler);
  Api.Registry.register_extension_info extension_info;
  Api.Registry.register_support_file ~prefix:"mermaid" {
    filename = "extensions/mermaid.css";
    content = mermaid_css;
  }
