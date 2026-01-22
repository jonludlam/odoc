(** Graphviz/DOT diagram extension for odoc.

    Renders [{@dot[...]}] code blocks as diagrams. By default uses client-side
    JavaScript (Viz.js), but can render server-side to PNG/SVG with format option.

    Example:
    {[
      {@dot layout=neato[
        digraph G {
          a -> b -> c;
          b -> d;
        }
      ]}
    ]}
*)

module Api = Odoc_extension_api
module Block = Odoc_document.Types.Block
module Inline = Odoc_document.Types.Inline

(** The Viz.js library URL for client-side rendering *)
let viz_js_url = "https://unpkg.com/viz.js@2.1.2/viz.js"
let viz_full_js_url = "https://unpkg.com/viz.js@2.1.2/full.render.js"

(** Generate a unique ID for each diagram *)
let diagram_counter = ref 0

let fresh_id () =
  incr diagram_counter;
  Printf.sprintf "dot-diagram-%d" !diagram_counter

(** Extract option values *)
let get_layout tags =
  Api.get_binding "layout" tags
  |> Option.value ~default:"dot"

let get_format tags =
  Api.get_binding "format" tags

let get_filename tags =
  Api.get_binding "filename" tags

let get_dimensions tags =
  let width = Api.get_binding "width" tags in
  let height = Api.get_binding "height" tags in
  (width, height)

(** Check if content looks like a complete DOT graph *)
let has_graph_wrapper content =
  let trimmed = String.trim content in
  String.length trimmed > 0 &&
  (let starts_with prefix s =
     String.length s >= String.length prefix &&
     String.sub s 0 (String.length prefix) = prefix
   in
   starts_with "digraph" trimmed ||
   starts_with "graph" trimmed ||
   starts_with "strict" trimmed)

(** Wrap content in a digraph if needed *)
let ensure_graph_wrapper content =
  if has_graph_wrapper content then content
  else Printf.sprintf "digraph G {\n%s\n}" content

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

(** Run the dot command to render to a specific format *)
let run_dot ~layout ~format content =
  (* Create temp file for input *)
  let tmp_in = Filename.temp_file "odoc_dot_" ".dot" in
  let tmp_out = Filename.temp_file "odoc_dot_" ("." ^ format) in
  Fun.protect ~finally:(fun () ->
    (try Sys.remove tmp_in with _ -> ());
    (try Sys.remove tmp_out with _ -> ())
  ) (fun () ->
    (* Write DOT content *)
    let oc = open_out tmp_in in
    output_string oc content;
    close_out oc;
    (* Run dot command *)
    let cmd = Printf.sprintf "dot -K%s -T%s -o %s %s 2>&1"
      layout format (Filename.quote tmp_out) (Filename.quote tmp_in) in
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

(** JavaScript code to render a single diagram (for client-side rendering) *)
let render_script id layout content =
  Printf.sprintf {|
(function() {
  function renderDot() {
    var container = document.getElementById('%s');
    if (!container) return;

    if (typeof Viz === 'undefined') {
      container.innerHTML = '<pre style="color: red;">Viz.js not loaded</pre>';
      return;
    }

    var viz = new Viz();
    viz.renderSVGElement(%S, { engine: %S })
      .then(function(svg) {
        container.innerHTML = '';
        container.appendChild(svg);
      })
      .catch(function(error) {
        container.innerHTML = '<pre style="color: red;">' + error + '</pre>';
      });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', renderDot);
  } else {
    renderDot();
  }
})();
|} id content layout

module Dot_handler : Api.Code_Block_Extension = struct
  let prefix = "dot"

  let to_document meta content =
    let id = fresh_id () in
    let layout = get_layout meta.Api.tags in
    let format = get_format meta.Api.tags in
    let filename_opt = get_filename meta.Api.tags in
    let (width, height) = get_dimensions meta.Api.tags in
    let style = make_style width height in
    let style_attr = if style = "" then "" else Printf.sprintf " style=\"%s\"" style in

    (* Auto-wrap in digraph if needed *)
    let dot_content = ensure_graph_wrapper content in

    match format with
    | Some "png" | Some "svg" ->
        (* Server-side rendering *)
        let fmt = match format with Some f -> f | None -> "png" in
        let base_filename = match filename_opt with
          | Some f -> f
          | None -> Printf.sprintf "dot-%s.%s" id fmt
        in
        (match run_dot ~layout ~format:fmt dot_content with
        | Ok data ->
            let html = Printf.sprintf
              {|<div id="%s" class="odoc-dot-diagram"%s><img src="%s" alt="DOT diagram" /></div>|}
              id style_attr base_filename
            in
            let block = Block.[{
              attr = ["odoc-dot"];
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
              "<div id=\"%s\" class=\"odoc-dot-diagram odoc-dot-error\"><pre style=\"color: red;\">Error rendering DOT diagram (is graphviz installed?):\n%s</pre><pre>%s</pre></div>"
              id err content
            in
            let block = Block.[{
              attr = ["odoc-dot"; "odoc-dot-error"];
              desc = Raw_markup ("html", html)
            }] in
            Some {
              Api.content = block;
              overrides = [];
              resources = [];
              assets = [];
            })

    | Some unknown_format ->
        (* Unknown format - show error *)
        let html = Printf.sprintf
          {|<div class="odoc-dot-error"><pre style="color: red;">Unknown format: %s (supported: png, svg)</pre></div>|}
          unknown_format
        in
        let block = Block.[{
          attr = ["odoc-dot-error"];
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
          {|<div id="%s" class="odoc-dot-diagram"%s><pre>%s</pre></div>|}
          id style_attr content
        in
        let script = render_script id layout dot_content in
        let block = Block.[{
          attr = ["odoc-dot"];
          desc = Raw_markup ("html", html)
        }] in
        Some {
          Api.content = block;
          overrides = [];
          resources = [
            Api.Js_url viz_js_url;
            Api.Js_url viz_full_js_url;
            Api.Js_inline script;
          ];
          assets = [];
        }
end

(** CSS for dot diagrams *)
let dot_css = {|
.odoc-dot-diagram {
  margin: 1em 0;
  overflow: auto;
}

.odoc-dot-diagram svg,
.odoc-dot-diagram img {
  max-width: 100%;
  height: auto;
}

.odoc-dot-diagram pre {
  background: #f5f5f5;
  padding: 1em;
  border-radius: 4px;
  overflow-x: auto;
}

.odoc-dot-error pre {
  color: #c00;
}
|}

(** Extension documentation *)
let extension_info : Api.extension_info = {
  info_kind = `Code_block;
  info_prefix = "dot";
  info_description = "Render Graphviz/DOT diagrams. Uses client-side Viz.js by default, or server-side graphviz with format=png|svg.";
  info_options = [
    { opt_name = "format"; opt_description = "Output format: png, svg (server-side), or omit for client-side JS"; opt_default = None };
    { opt_name = "layout"; opt_description = "Graphviz layout engine"; opt_default = Some "dot" };
    { opt_name = "width"; opt_description = "CSS width (e.g., 500px, 100%)"; opt_default = None };
    { opt_name = "height"; opt_description = "CSS height"; opt_default = None };
    { opt_name = "filename"; opt_description = "Output filename for server-side rendering"; opt_default = Some "auto-generated" };
  ];
  info_example = Some "{@dot format=png layout=neato[a -> b -> c]}";
}

let () =
  Api.Registry.register_code_block (module Dot_handler);
  Api.Registry.register_extension_info extension_info;
  Api.Registry.register_support_file ~prefix:"dot" {
    filename = "extensions/dot.css";
    content = dot_css;
  }
