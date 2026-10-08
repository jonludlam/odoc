(** Extensions to the HTML output.

    A program that links odoc as a library can register extensions here and then
    run odoc with [Odoc_cli.main]. *)

type head =
  config:Config.t ->
  url:Odoc_document.Url.Path.t ->
  Odoc_document.Types.Page.t ->
  Html_types.head_content_fun Tyxml.Html.elt list
(** A function that returns the elements to add to the [<head>] of a page, for
    example the scripts that render the blocks an extension has produced. It is
    called for every page, and can return [[]] for the pages that need nothing:
    {!Odoc_document.Doctree.Exists} finds out whether a page contains a given
    block. *)

val register_head : head -> unit

val head :
  config:Config.t ->
  url:Odoc_document.Url.Path.t ->
  Odoc_document.Types.Page.t ->
  Html_types.head_content_fun Tyxml.Html.elt list
(** The elements that all registered extensions add to a page, in the order the
    extensions were registered. *)
