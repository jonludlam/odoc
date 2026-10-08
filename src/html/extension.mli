(** Extensions to the HTML output.

    A program that links odoc as a library can register extensions here and then
    run odoc with [Odoc_cli.main]. *)

(** The page whose head is being made. *)
module Context : sig
  type t

  val config : t -> Config.t

  val url : t -> Odoc_document.Url.Path.t

  val page : t -> Odoc_document.Types.Page.t

  val requires : t -> string -> bool
  (** [requires t feature] is whether a handler rendering the page required
      [feature] (see [Odoc_document.Extension.Context.require]). *)
end

type head = Context.t -> Html_types.head_content_fun Tyxml.Html.elt list
(** A function that returns the elements to add to the [<head>] of a page, for
    example the scripts that draw the blocks an extension has rendered. It is
    called for every page, and returns [[]] for the pages that need nothing. *)

val register_head : head -> unit

val head :
  config:Config.t ->
  url:Odoc_document.Url.Path.t ->
  Odoc_document.Types.Page.t ->
  Html_types.head_content_fun Tyxml.Html.elt list
(** The elements that all registered extensions add to a page, in the order the
    extensions were registered. *)
