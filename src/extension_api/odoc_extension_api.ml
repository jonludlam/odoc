(** Odoc Extension API

    This module provides the interface for odoc tag extensions.
    Extensions are dynamically loaded plugins that handle custom tags
    like [@note], [@rfc], [@example], etc.
*)

(** {1 Re-exported Types}

    These are the odoc types that extensions need to work with.
*)

module Comment = Odoc_model.Comment
module Location_ = Odoc_model.Location_
module Block = Odoc_document.Types.Block
module Inline = Odoc_document.Types.Inline
module Description = Odoc_document.Types.Description
module Url = Odoc_document.Url
module Target = Odoc_document.Types.Target

(** {1 Extension Types} *)

(** Resources that can be injected into the page (HTML only) *)
type resource = Odoc_extension_registry.resource =
  | Js_url of string      (** External JavaScript: <script src="..."> *)
  | Css_url of string     (** External CSS: <link rel="stylesheet" href="..."> *)
  | Js_inline of string   (** Inline JavaScript: <script>...</script> *)
  | Css_inline of string  (** Inline CSS: <style>...</style> *)

(** Output from the document phase *)
type extension_output = {
  content : Block.t;
  (** Universal content - used by all backends unless overridden *)

  overrides : (string * string) list;
  (** Backend-specific raw content overrides.
      E.g., [("html", "<div>...</div>"); ("markdown", "...")] *)

  resources : resource list;
  (** Page-level resources (JS/CSS). Only used by HTML backend. *)
}

(** Raised when an extension receives a tag variant it doesn't support *)
exception Unsupported_tag of string

(** {1 Extension Interface} *)

(** The signature that all tag extensions must implement *)
module type Extension = sig
  val prefix : string
  (** The tag prefix this extension handles.
      E.g., "note" handles [@note], "admonition" handles [@admonition.note] *)

  val to_document :
    tag:string ->
    Comment.nestable_block_element Location_.with_location list ->
    extension_output
  (** Document phase: convert tag to renderable content.
      Called during document generation. Returns content plus any
      page-level resources needed (JS/CSS). *)
end

(** {1 Support Files}

    Extensions can register support files (CSS, JS, images, etc.) that
    will be output by [odoc support-files].
*)

type support_file = Odoc_extension_registry.support_file = {
  filename : string;  (** Relative path, e.g., "extensions/admonition.css" *)
  content : string;   (** File content *)
}

(** {1 Extension Registry}

    Extensions register themselves here when loaded.
    odoc queries the registry when processing custom tags.
*)

module Registry = struct
  let register (module E : Extension) =
    let handler tag content =
      try
        let result = E.to_document ~tag content in
        Some {
          Odoc_extension_registry.content = result.content;
          overrides = result.overrides;
          resources = result.resources;
        }
      with Unsupported_tag _ -> None
    in
    Odoc_extension_registry.register_handler ~prefix:E.prefix handler

  (** Register a support file for this extension.
      The file will be output when [odoc support-files] is run. *)
  let register_support_file ~prefix file =
    Odoc_extension_registry.register_support_file ~prefix file

  let find prefix =
    Odoc_extension_registry.find_handler ~prefix

  let list_prefixes () =
    Odoc_extension_registry.list_prefixes ()

  let list_support_files () =
    Odoc_extension_registry.list_support_files ()
end

(** {1 Helper Functions} *)

(** Extract plain text from nestable block elements (for simple parsing) *)
let rec text_of_inline (inline : Comment.inline_element Location_.with_location) =
  match inline.Location_.value with
  | `Space -> " "
  | `Word w -> w
  | `Code_span c -> c
  | `Math_span m -> m
  | `Raw_markup (_, r) -> r
  | `Styled (_, inlines) -> text_of_inlines inlines
  | `Reference (_, content) -> text_of_link_content content
  | `Link (_, content) -> text_of_link_content content

and text_of_inlines inlines =
  String.concat "" (List.map text_of_inline inlines)

and text_of_link_content (content : Comment.link_content) =
  String.concat "" (List.map text_of_non_link content)

and text_of_non_link (el : Comment.non_link_inline_element Location_.with_location) =
  match el.Location_.value with
  | `Space -> " "
  | `Word w -> w
  | `Code_span c -> c
  | `Math_span m -> m
  | `Raw_markup (_, r) -> r
  | `Styled (_, content) -> text_of_link_content content

let text_of_paragraph (p : Comment.paragraph) =
  text_of_inlines p

let rec text_of_nestable_block_elements elements =
  let buf = Buffer.create 256 in
  List.iter (fun (el : Comment.nestable_block_element Location_.with_location) ->
    match el.Location_.value with
    | `Paragraph p -> Buffer.add_string buf (text_of_paragraph p)
    | `Code_block (_, code, _) -> Buffer.add_string buf code.Location_.value
    | `Math_block m -> Buffer.add_string buf m
    | `Verbatim v -> Buffer.add_string buf v
    | `Modules _ -> ()
    | `Table _ -> ()
    | `List (_, items) ->
        List.iter (fun item ->
          Buffer.add_string buf (text_of_nestable_block_elements item)
        ) items
    | `Media _ -> ()
  ) elements;
  Buffer.contents buf

(** Create a simple paragraph block *)
let paragraph text =
  let inline = Inline.[ { attr = []; desc = Text text } ] in
  Block.[ { attr = []; desc = Paragraph inline } ]

(** Create an inline link *)
let link ~url ~text =
  Inline.[{
    attr = [];
    desc = Link {
      target = External url;
      content = [{ attr = []; desc = Text text }];
      tooltip = None
    }
  }]

(** Create an empty extension output with just content *)
let simple_output content =
  { content; overrides = []; resources = [] }
