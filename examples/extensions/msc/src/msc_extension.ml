(** Message Sequence Chart extension for odoc.

    Renders [{@msc[...]}] code blocks as sequence diagrams. By default uses
    client-side JavaScript (mscgen-inpage), but can render server-side to
    PNG/SVG with format option (requires mscgen).

    Example:
    {[
      {@msc format=png width=600px[
        msc {
          a, b, c;
          a -> b [label="request"];
          b -> c [label="forward"];
          c -> b [label="response"];
          b -> a [label="reply"];
        }
      ]}
    ]}
*)

module Api = Odoc_extension_api
module Block = Odoc_document.Types.Block
module Inline = Odoc_document.Types.Inline

(** MscGen.js CDN URL - the inpage version auto-renders on DOMContentLoaded *)
let mscgen_js_url = "https://unpkg.com/mscgenjs-inpage@4/dist/mscgen-inpage.js"

(** Script to load mscgenjs with defer-like behavior *)
let loader_script = Printf.sprintf {|
(function() {
  function loadMscgen() {
    var script = document.createElement('script');
    script.src = %S;
    script.async = false;
    document.head.appendChild(script);
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', loadMscgen);
  } else {
    loadMscgen();
  }
})();
|} mscgen_js_url


(** Generate a unique ID for each diagram *)
let diagram_counter = ref 0

let fresh_id () =
  incr diagram_counter;
  Printf.sprintf "msc-diagram-%d" !diagram_counter

(** Extract option values *)
let get_style tags =
  Api.get_binding "named-style" tags
  |> Option.value ~default:"basic"

let get_format tags =
  Api.get_binding "format" tags

let get_filename tags =
  Api.get_binding "filename" tags

(** Extract CSS dimensions *)
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

(** Run mscgen to render to a specific format *)
let run_mscgen ~format content =
  let tmp_in = Filename.temp_file "odoc_msc_" ".msc" in
  let tmp_out = Filename.temp_file "odoc_msc_" ("." ^ format) in
  Fun.protect ~finally:(fun () ->
    (try Sys.remove tmp_in with _ -> ());
    (try Sys.remove tmp_out with _ -> ())
  ) (fun () ->
    (* Write MSC content *)
    let oc = open_out tmp_in in
    output_string oc content;
    close_out oc;
    (* Run mscgen command *)
    let cmd = Printf.sprintf "mscgen -T %s -i %s -o %s 2>&1"
      format (Filename.quote tmp_in) (Filename.quote tmp_out) in
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

module Msc_handler : Api.Code_Block_Extension = struct
  let prefix = "msc"

  let to_document meta content =
    let id = fresh_id () in
    let style_name = get_style meta.Api.tags in
    let format = get_format meta.Api.tags in
    let filename_opt = get_filename meta.Api.tags in
    let (width, height) = get_dimensions meta.Api.tags in
    let style = make_style width height in
    let style_attr = if style = "" then "" else Printf.sprintf " style=\"%s\"" style in

    match format with
    | Some "png" | Some "svg" ->
        (* Server-side rendering with mscgen *)
        let fmt = match format with Some f -> f | None -> "png" in
        let base_filename = match filename_opt with
          | Some f -> f
          | None -> Printf.sprintf "msc-%s.%s" id fmt
        in
        (match run_mscgen ~format:fmt content with
        | Ok data ->
            let html = Printf.sprintf
              {|<div id="%s" class="odoc-msc-diagram"%s><img src="%s" alt="MSC diagram" /></div>|}
              id style_attr base_filename
            in
            let block = Block.[{
              attr = ["odoc-msc"];
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
              "<div id=\"%s\" class=\"odoc-msc-diagram odoc-msc-error\"><pre style=\"color: red;\">Error rendering MSC diagram (is mscgen installed?):\n%s</pre><pre>%s</pre></div>"
              id err (html_escape content)
            in
            let block = Block.[{
              attr = ["odoc-msc"; "odoc-msc-error"];
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
          {|<div class="odoc-msc-error"><pre style="color: red;">Unknown format: %s (supported: png, svg)</pre></div>|}
          unknown_format
        in
        let block = Block.[{
          attr = ["odoc-msc-error"];
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
        let data_style = if style_name = "basic" then "" else Printf.sprintf " data-named-style=\"%s\"" style_name in
        let html = Printf.sprintf
          {|<div id="%s" class="odoc-msc-diagram"%s><script type="text/x-mscgen"%s>%s</script><noscript><pre>%s</pre></noscript></div>|}
          id style_attr data_style content (html_escape content)
        in
        let block = Block.[{
          attr = ["odoc-msc"];
          desc = Raw_markup ("html", html)
        }] in
        Some {
          Api.content = block;
          overrides = [];
          resources = [
            Api.Js_inline loader_script;
          ];
          assets = [];
        }
end

(** CSS for MSC diagrams *)
let msc_css = {|
.odoc-msc-diagram {
  margin: 1em 0;
  overflow: auto;
}

.odoc-msc-diagram svg,
.odoc-msc-diagram img {
  max-width: 100%;
  height: auto;
}

/* Fallback for noscript */
.odoc-msc-diagram noscript pre {
  background: #f8f8f8;
  padding: 1em;
  border-radius: 4px;
  overflow-x: auto;
}

.odoc-msc-error pre {
  color: #c00;
}
|}

(** Extension documentation *)
let extension_info : Api.extension_info = {
  info_kind = `Code_block;
  info_prefix = "msc";
  info_description = "Render Message Sequence Charts. Uses client-side mscgen-inpage.js by default, or server-side mscgen with format=png|svg.";
  info_options = [
    { opt_name = "format"; opt_description = "Output format: png, svg (requires mscgen), or omit for client-side JS"; opt_default = None };
    { opt_name = "named-style"; opt_description = "MscGen style (basic, lazy, classic, etc.)"; opt_default = Some "basic" };
    { opt_name = "width"; opt_description = "CSS width (e.g., 500px, 100%)"; opt_default = None };
    { opt_name = "height"; opt_description = "CSS height"; opt_default = None };
    { opt_name = "filename"; opt_description = "Output filename for server-side rendering"; opt_default = Some "auto-generated" };
  ];
  info_example = Some "{@msc format=png[msc { a,b; a->b; }]}";
}

let () =
  Api.Registry.register_code_block (module Msc_handler);
  Api.Registry.register_extension_info extension_info;
  Api.Registry.register_support_file ~prefix:"msc" {
    filename = "extensions/msc.css";
    content = msc_css;
  }
