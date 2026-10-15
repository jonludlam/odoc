(** Extensions to the HTML output: the head elements and support files that an
    extension, [Odoc_extension.t], gives, and the registries that
    [Odoc_extension.register] puts them in. *)

(** The page whose head is being made. *)
module Context : sig
  type t

  val config : t -> Config.t

  val url : t -> Odoc_document.Url.Path.t

  val page : t -> Odoc_document.Types.Page.t

  val requires : t -> string -> bool
  (** [requires t feature] is whether a handler rendering the page required
      [feature] (see [Odoc_document.Extension.Context.require]). *)

  val support_file_uri : t -> string -> string
  (** [support_file_uri t path] is the URI of the support file [path] from the
      page. *)
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

(** {1 Support files} *)

val register_support_file : path:string -> string -> unit
(** [register_support_file ~path content] adds a file to the ones that
    [odoc support-files] writes, at [path] in the support directory, for example
    a script that the elements added to the [<head>] load. *)

val support_files : unit -> (string * string) list
(** The registered support files, as [(path, content)], in the order they were
    registered. *)
