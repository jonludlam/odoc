(** Graphviz/DOT diagram extension for odoc.

    Renders [{@dot[...]}] code blocks as SVG diagrams.

    Example:
    {[
      {@dot layout=neato[
        digraph G {
          a -> b -> c;
          b -> d;
        }
      ]}
    ]}

    Supported options:
    - [width]: CSS width (e.g., "500px", "100%")
    - [height]: CSS height
    - [layout]: Graphviz layout engine (dot, neato, fdp, sfdp, twopi, circo)
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

(** Extract layout engine option *)
let get_layout tags =
  Api.get_binding "layout" tags
  |> Option.value ~default:"dot"

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

(** JavaScript code to render a single diagram *)
let render_script id layout content =
  (* Use %S for proper escaping - it handles newlines, quotes, backslashes *)
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
    let (width, height) = get_dimensions meta.Api.tags in
    let style = make_style width height in

    (* Create a container div with the diagram placeholder *)
    let style_attr = if style = "" then "" else Printf.sprintf " style=\"%s\"" style in
    let html = Printf.sprintf
      {|<div id="%s" class="odoc-dot-diagram"%s><pre>%s</pre></div>|}
      id style_attr content
    in

    (* JavaScript to render the diagram *)
    let script = render_script id layout content in

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
    }
end

(** CSS for dot diagrams *)
let dot_css = {|
.odoc-dot-diagram {
  margin: 1em 0;
  overflow: auto;
}

.odoc-dot-diagram svg {
  max-width: 100%;
  height: auto;
}

.odoc-dot-diagram pre {
  background: #f5f5f5;
  padding: 1em;
  border-radius: 4px;
  overflow-x: auto;
}
|}

let () =
  Api.Registry.register_code_block (module Dot_handler);
  Api.Registry.register_support_file ~prefix:"dot" {
    filename = "extensions/dot.css";
    content = dot_css;
  }
