(** Mermaid diagram extension for odoc.

    Renders [{@mermaid[...]}] code blocks as interactive diagrams.

    Example:
    {[
      {@mermaid theme=dark[
        sequenceDiagram
          Alice->>Bob: Hello Bob
          Bob-->>Alice: Hi Alice
      ]}
    ]}

    Supported options:
    - [theme]: Mermaid theme (default, dark, forest, neutral)
    - [width]: CSS width
    - [height]: CSS height
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

(** Extract theme option *)
let get_theme tags =
  Api.get_binding "theme" tags
  |> Option.value ~default:"default"

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

module Mermaid_handler : Api.Code_Block_Extension = struct
  let prefix = "mermaid"

  let to_document meta content =
    let id = fresh_id () in
    let theme = get_theme meta.Api.tags in
    let (width, height) = get_dimensions meta.Api.tags in
    let style = make_style width height in

    (* Create a container div with the mermaid content *)
    let style_attr = if style = "" then "" else Printf.sprintf " style=\"%s\"" style in
    let html = Printf.sprintf
      {|<div id="%s" class="odoc-mermaid-diagram"%s><pre class="mermaid">%s</pre></div>|}
      id style_attr (html_escape content)
    in

    (* Initialization script - runs once per page *)
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

.odoc-mermaid-diagram svg {
  max-width: 100%;
  height: auto;
}

.odoc-mermaid-diagram .mermaid {
  background: transparent;
}
|}

let () =
  Api.Registry.register_code_block (module Mermaid_handler);
  Api.Registry.register_support_file ~prefix:"mermaid" {
    filename = "extensions/mermaid.css";
    content = mermaid_css;
  }
