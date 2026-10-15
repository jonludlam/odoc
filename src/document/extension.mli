(** Extensions to the way comments are rendered: the handlers that an extension,
    [Odoc_extension.t], gives, and the registries that [Odoc_extension.register]
    puts them in. *)

(** What a handler knows about the element it renders, and what it can do
    besides returning the rendering. *)
module Context : sig
  type t

  val url : t -> Url.Path.t option
  (** The page the element is rendered in, or [None] when it is rendered outside
      a page, for example in the sidebar or the search index. *)

  val location : t -> Odoc_model.Location_.span
  (** Where the element is in the source. *)

  val backend : t -> string
  (** The output the element is rendered for: ["html"], ["latex"], ["man"] or
      ["markdown"], the names that raw markup uses, as in [{%html:...%}]. A
      handler that returns raw markup should return it only for its backend, and
      [None] for the others. *)

  val warn : t -> string -> unit
  (** [warn t message] reports a warning at the element, which is printed, or is
      an error with [--warn-error], like odoc's own warnings. Warnings are only
      reported when the element is rendered in a page. *)

  val fresh_id : t -> string -> string
  (** [fresh_id t prefix] is ["prefix-1"], then ["prefix-2"] and so on: an
      identifier that is different each time it is asked for on the page. *)

  val require : t -> string -> unit
  (** [require t feature] notes that the page needs [feature], for example the
      script that draws the element, so that the HTML head of the page can ask
      for it (see [Odoc_html.Extension.Context.requires]). Name features after
      the extension. *)
end

type code_block_handler =
  Context.t -> Odoc_model.Comment.code_block -> Types.Block.t option
(** A handler renders a code block, and its output if it has one, or returns
    [None] to leave the code block to the default rendering. *)

val register_code_block : language:string -> code_block_handler -> unit
(** [register_code_block ~language handler] makes [handler] render the code
    blocks written in [language]. A handler for ["ocaml"] also renders the code
    blocks that have no language. A later registration for the same language
    replaces an earlier one. *)

type raw_markup_handler = Context.t -> string -> Types.Inline.one option
(** A raw markup handler is given the content of [{%target:...%}], and returns
    what to render in its place, or [None] to leave it to the default rendering.
*)

val register_raw_markup : target:string -> raw_markup_handler -> unit
(** [register_raw_markup ~target handler] makes [handler] render the raw markup
    for [target], which is otherwise dropped by the backends that do not know
    the target. Targets are compared ignoring case. A later registration for the
    same target replaces an earlier one. *)

(** {1 Used by odoc's renderer} *)

val in_page : Url.Path.t -> (unit -> 'a) -> 'a
(** [in_page url f] runs [f], which generates the page [url]. *)

val for_backend : string -> (unit -> 'a) -> 'a
(** [for_backend backend f] runs [f], which generates documents for [backend].
    Documents are generated for ["html"] otherwise. *)

val required : Url.Path.t -> string list
(** The features that the handlers required on a page, in the order they were
    first required. *)

val render_code_block :
  language:string ->
  location:Odoc_model.Location_.span ->
  Odoc_model.Comment.code_block ->
  Types.Block.t option
(** [render_code_block ~language ~location code_block] is what the handler for
    [language] renders [code_block] to, or [None] if there is no handler or it
    leaves the code block to the default rendering. *)

val render_raw_markup :
  target:string ->
  location:Odoc_model.Location_.span ->
  string ->
  Types.Inline.one option
(** [render_raw_markup ~target ~location content] is what the handler for
    [target] renders [content] to, or [None] if there is no handler or it leaves
    the raw markup to the default rendering. *)
