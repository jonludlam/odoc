Odoc currently supports a number of "tags" like `@raise` `@param` `@since` and so on. I would like to add support for "custom tags" where a user-defined tag can be created with new behaviour at the linking step and the HTML generation step.

For example, we might have a new tag for referencing IETF RFCs - e.g. `@rfc 9110 Section 5.5`. Or we might have a new tag for an example block:

```
@example This is an example of foo bar.
It's a multi-line thing that ends up in an
outlined box in the HTML
```

Or we might have something where we can resolve references:

```
@handles This handles the {!Foo} exception.
```

Now the way to handle these is to write an odoc extension somehow
where we'd write code that uses the odoc APIs to handle the extensions. These pieces of code would be called during the link
and HTML generation phases of odoc.

We need to come up with some mechanisms to make this happen.

Firstly, how do we tell odoc about this? Do we use Dynlink to
load in the new handlers, or do we recompile a new odoc binary, linking in the new handlers?

Secondly, how do we tell `dune` that this needs to be done?

Thirdly, how do we tell the ocaml docs CI that this needs to be done? This would presumably require some new fields in the opam
file.

## Design Decisions

### Q1: Static linking (with future dynlink option)

Extensions are OCaml libraries implementing a defined interface. Initially we
support static linking only - the extension is compiled and linked into either:
- A custom odoc binary, or
- The documentation generation pipeline

This avoids Dynlink's cross-platform complexity and ABI compatibility issues.
The interface should be designed to allow dynlink as a future enhancement.

### Q2: Package-wide declaration in dune-project

Extensions are declared per-package in `dune-project`, within the existing
`(documentation ...)` stanza:

```lisp
(package
  (name mypkg)
  (documentation
    (depends odoc)
    (extensions my_rfc_extension my_example_extension)))
```

This maps 1:1 to opam packages, so extension dependencies flow naturally
into each package's generated `.opam` file.

### Q3: Opam file for CI solver

The CI solver only has access to opam files during dependency resolution, so
extension info must be in opam. Since dune generates opam files, we maintain
a single source of truth:

1. Declare in `dune-project`
2. Dune generates opam with extension metadata (e.g., `x-odoc-extensions`)
3. CI solver reads opam, installs extension packages
4. Build system uses extensions during doc generation

Non-dune builds can manually add the opam fields.

## Extension Interface

Extensions are OCaml modules implementing the `Odoc_tag_extension` signature.
Each extension claims a prefix and handles all tags starting with that prefix:

- `@rfc` → rfc extension
- `@rfc.section` → rfc extension
- `@callout` → callout extension
- `@callout.box` → callout extension

### Extension Output

Extensions return content that can be rendered by any backend, with optional
backend-specific overrides for cases where different output is needed:

```ocaml
(** Resources that can be injected into the page (HTML only) *)
type resource =
  | Js_url of string      (** External JavaScript: <script src="..."> *)
  | Css_url of string     (** External CSS: <link rel="stylesheet" href="..."> *)
  | Js_inline of string   (** Inline JavaScript: <script>...</script> *)
  | Css_inline of string  (** Inline CSS: <style>...</style> *)

(** Output from the document phase *)
type extension_output = {
  content : Odoc_document.Types.Block.t;
  (** Universal content - used by all backends unless overridden *)

  overrides : (string * string) list;
  (** Backend-specific raw content overrides.
      E.g., [("html", "<div>...</div>"); ("markdown", "```dot\n...\n```")]
      If present for a backend, used instead of [content]. *)

  resources : resource list;
  (** Page-level resources (JS/CSS). Only used by HTML backend. *)
}
```

**Rendering logic:**
1. Backend checks `overrides` for its name (e.g., "html", "markdown", "latex")
2. If found, use that raw string directly
3. Otherwise, render `content` using the standard Document → output pipeline
4. HTML backend also collects and deduplicates `resources` for page HEAD/BODY

### Module Signature

```ocaml
module type Odoc_tag_extension = sig
  (** The tag prefix this extension handles.
      E.g., "callout" handles @callout, @callout.box, @callout.bubble *)
  val prefix : string

  (** Link phase: process/validate content, resolve custom references.
      Called during odoc link with the linking environment. *)
  val link :
    tag:string ->
    Odoc_xref2.Env.t ->
    Odoc_model.Comment.nestable_block_element list ->
    Odoc_model.Comment.nestable_block_element list

  (** Document phase: convert tag to document elements for rendering.
      Called during document generation. Returns content plus any
      page-level resources needed (JS/CSS). *)
  val to_document :
    tag:string ->
    Odoc_model.Comment.nestable_block_element list ->
    extension_output
end

(** Raised when an extension receives a tag variant it doesn't support.
    E.g., callout extension receiving @callout.unknown *)
exception Unsupported_tag of string
```

### Example Extensions

#### Graphviz (with backend overrides)

This extension needs different output for HTML vs Markdown:

```ocaml
(* odoc_graphviz_extension.ml *)

let prefix = "dot"

let link ~tag _env content = content

let to_document ~tag content =
  let dot_source = extract_text content in
  {
    (* Fallback: just show the source as a code block *)
    content = Block.[Source [...]];

    (* Backend-specific rendering *)
    overrides = [
      ("html", Printf.sprintf {|<div class="graphviz">%s</div>|}
                 (escape_html dot_source));
      ("markdown", Printf.sprintf "```dot\n%s\n```" dot_source);
    ];

    (* HTML needs the renderer script *)
    resources = [
      Js_url "https://cdn.jsdelivr.net/npm/@viz-js/viz/lib/viz-standalone.js";
      Js_inline {|
        document.querySelectorAll('.graphviz').forEach(async el => {
          const viz = await Viz.instance();
          el.innerHTML = viz.renderSVGElement(el.textContent).outerHTML;
        });
      |};
    ];
  }
```

#### Callout (universal content)

Simple extensions can use Document types that work everywhere:

```ocaml
(* odoc_callout_extension.ml *)

let prefix = "callout"

let link ~tag _env content = content

let to_document ~tag content =
  let block_content = render_content content in
  let content = match tag with
    | "callout" | "callout.box" ->
        (* Returns Block.t with a styled div - works for all backends *)
        make_callout_block ~style:`Box block_content
    | "callout.bubble" ->
        make_callout_block ~style:`Bubble block_content
    | _ ->
        raise (Unsupported_tag tag)
  in
  (* No overrides needed - Document types render well everywhere *)
  { content; overrides = []; resources = [] }
```

### Error Handling

When odoc encounters a custom tag:

1. Look up extension by prefix (first component before `.`)
2. If no extension registered: warning "Unknown tag @foo"
3. If extension raises `Unsupported_tag`: error "Tag @foo.bar not supported by 'foo' extension"
4. Extension errors during link/render are reported with source location

