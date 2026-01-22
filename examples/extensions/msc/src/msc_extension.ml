(** Message Sequence Chart extension for odoc.

    Renders [{@msc[...]}] code blocks as sequence diagrams.

    Example:
    {[
      {@msc width=600px[
        msc {
          a, b, c;
          a -> b [label="request"];
          b -> c [label="forward"];
          c -> b [label="response"];
          b -> a [label="reply"];
        }
      ]}
    ]}

    Supported options:
    - [width]: CSS width (e.g., "600px", "100%")
    - [height]: CSS height
    - [named-style]: MscGen style (basic, lazy, classic, etc.)
*)

module Api = Odoc_extension_api
module Block = Odoc_document.Types.Block
module Inline = Odoc_document.Types.Inline

(** MscGen.js CDN URL - the inpage version auto-renders on DOMContentLoaded *)
let mscgen_js_url = "https://cdn.jsdelivr.net/npm/mscgenjs@6/dist/mscgen-inpage.js"

(** Initialization script - ensures mscgen renders after library loads *)
let init_script = {|
(function() {
  function initMscgen() {
    if (typeof window.mscgen_js_inpage !== 'undefined' &&
        typeof window.mscgen_js_inpage.renderAll === 'function') {
      window.mscgen_js_inpage.renderAll();
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function() {
      setTimeout(initMscgen, 100);
    });
  } else {
    setTimeout(initMscgen, 100);
  }
})();
|}

(** Generate a unique ID for each diagram *)
let diagram_counter = ref 0

let fresh_id () =
  incr diagram_counter;
  Printf.sprintf "msc-diagram-%d" !diagram_counter

(** Extract style option *)
let get_style tags =
  Api.get_binding "named-style" tags
  |> Option.value ~default:"basic"

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

module Msc_handler : Api.Code_Block_Extension = struct
  let prefix = "msc"

  let to_document meta content =
    let id = fresh_id () in
    let style_name = get_style meta.Api.tags in
    let (width, height) = get_dimensions meta.Api.tags in
    let style = make_style width height in

    (* Create a container div with the MSC content *)
    (* mscgen_js looks for <pre class="mscgen"> or data-language="mscgen" *)
    let style_attr = if style = "" then "" else Printf.sprintf " style=\"%s\"" style in
    let html = Printf.sprintf
      {|<div id="%s" class="odoc-msc-diagram"%s><pre class="mscgen" data-named-style="%s">%s</pre></div>|}
      id style_attr style_name (html_escape content)
    in

    let block = Block.[{
      attr = ["odoc-msc"];
      desc = Raw_markup ("html", html)
    }] in

    Some {
      Api.content = block;
      overrides = [];
      resources = [
        Api.Js_url mscgen_js_url;
        Api.Js_inline init_script;
      ];
    }
end

(** CSS for MSC diagrams *)
let msc_css = {|
.odoc-msc-diagram {
  margin: 1em 0;
  overflow: auto;
}

.odoc-msc-diagram svg {
  max-width: 100%;
  height: auto;
}

.odoc-msc-diagram pre.mscgen {
  background: #f8f8f8;
  padding: 1em;
  border-radius: 4px;
  overflow-x: auto;
}

/* Hide the source once rendered */
.odoc-msc-diagram pre.mscgen[data-renderedby="mscgen_js"] {
  display: none;
}
|}

let () =
  Api.Registry.register_code_block (module Msc_handler);
  Api.Registry.register_support_file ~prefix:"msc" {
    filename = "extensions/msc.css";
    content = msc_css;
  }
