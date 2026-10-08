(** Extensions to odoc.

    An extension is a value of type {!t}, which says what it renders and what it
    adds to the output. A program that passes extensions to [Odoc_cli.main] is
    an odoc with those extensions built in:

    {[
      let extension =
        {
          (Odoc_extension.v ~name:"mermaid") with
          code_blocks = [ ("mermaid", render) ];
          head = Some head;
        }

      let () = Odoc_cli.main ~extensions:[ extension ] ()
    ]}

    Build extensions from {!v} with [{ (v ~name) with ... }], as more fields may
    be added. *)

type t = {
  name : string;  (** Names the extension in error messages. *)
  code_blocks : (string * Odoc_document.Extension.code_block_handler) list;
      (** Handlers for the code blocks in a language: [("dot", handler)] for
          [{@dot[...]}]. *)
  raw_markup : (string * Odoc_document.Extension.raw_markup_handler) list;
      (** Handlers for the raw markup of a target: [("wodoc", handler)] for
          [{%wodoc:...%}]. *)
  tags : (string * Odoc_document.Extension.tag_handler) list;
      (** Handlers for custom tags, by prefix: [("admonition", handler)] for
          [@admonition] and [@admonition.note]. *)
  head : Odoc_html.Extension.head option;
      (** What to add to the [<head>] of an HTML page. *)
  support_files : (string * string) list;
      (** Files for [odoc support-files] to write, as [(path, content)]. *)
}

val v : name:string -> t
(** [v ~name] is the extension [name], which does nothing. *)

val register : t list -> (unit, string) result
(** [register extensions] makes odoc use [extensions]. It is an error for two of
    them to handle the same language, target, tag or support file, and for one
    to handle the raw markup of a target that odoc renders itself, [html],
    [latex] or [man], to replace one of odoc's own support files, or to handle
    tags with a prefix that cannot be a custom tag's (see
    [Odoc_model.Custom_tags.is_valid_prefix]). *)
