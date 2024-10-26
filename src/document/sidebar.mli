type toc = (Url.t * Types.Inline.one) option Odoc_utils.Tree.t
type pages = { name : string; pages : toc }
type library = { name : string; units : toc list }

type t = { pages : pages list; libraries : library list }

val of_lang : Odoc_index.t -> t

val to_block : t -> Url.Path.t -> Types.Block.t
(** Generates the sidebar document given a global sidebar and the path at which
    it will be displayed *)
