type scope = { pages : (string * Fpath.t) list; libs : (string * Fpath.t) list }
(** The link-time reference scope of a package: the page trees ([-P]) and the
    module trees ([-L]) that its units may refer to. Every unit of a package is
    linked with the same scope. Paths are absolute. *)

type sidebar = { output_file : Fpath.t; json : bool; pkg_dir : Fpath.t }

type index = {
  output_file : Fpath.t;
  json : bool;
  search_dir : Fpath.t;
  sidebar : sidebar option;
}
(** An index is built from the [.odocl] files of the units of its package (see
    [Compile.html_generate]), so it is defined by its output, not by input
    directories. *)

type 'a t = {
  parent_id : Odoc.Id.t;
  input_file : Fpath.t;
  input_copy : Fpath.t option;
      (** Used to stash cmtis from virtual libraries into the odoc dir for
          voodoo mode. *)
  odoc_file : Fpath.t;
  odocl_file : Fpath.t;
  enable_warnings : bool;
  to_output : bool;
  kind : 'a;
}
(** A single artifact. What is shared by the units of a library or of a package
    lives in {!lib} and {!pkg} instead. *)

type intf_extra = {
  hidden : bool;
  hash : string;
  deps : (string * Digest.t) list;
}
and intf = [ `Intf of intf_extra ]

type impl_extra = { src_id : Odoc.Id.t; src_path : Fpath.t }
type impl = [ `Impl of impl_extra ]

type mld = [ `Mld ]
type md = [ `Md ]
type asset = [ `Asset ]

type any = [ impl | intf | mld | asset | md ] t

val pp : any Fmt.t

type lib = { lib_name : string; includes : Fpath.t list; units : any list }
(** A library: its modules and their implementations, and the [-I] search path
    they are compiled and linked with. *)

type pkg = {
  pkgname : string option;
  scope : scope;
  index : index option;
  libs : lib list;
  pages : any list;
}
(** A package, the unit of building: its libraries, its pages (including the
    generated landing pages), the reference scope they all link with, and the
    index they are gathered in. [pkgname] is [None] only for the top-level index
    page, which belongs to no package. *)

val all_units : pkg -> any list
val pp_pkg : pkg Fmt.t

val pkg_dir : Packages.t -> Fpath.t

val lib_dir : Packages.t -> Packages.libty -> Fpath.t
(** [lib_dir pkg lib] is the parent id of the library's units: it determines
    their identifiers and the URLs of their pages. *)

val lib_obj_dir : Packages.t -> Packages.libty -> Fpath.t
(** [lib_obj_dir pkg lib] is where the library's [.odoc] (and [.odocl]) files
    are written, relative to the odoc (resp. odocl) directory. It mirrors the
    library's object directory -- the compiled documentation is laid out like
    the compiled objects -- so libraries sharing an object directory share an
    odoc directory, and the [-I] set of a unit mirrors the compiler's. *)

val doc_dir : Packages.t -> Fpath.t
val src_dir : Packages.t -> Fpath.t
val src_lib_dir : Packages.t -> Packages.libty -> Fpath.t

val output_root : _ t -> Fpath.t
(** [output_root u] is the directory below which [odoc] places [u]'s output when
    given [--output-dir] and [--parent-id] rather than [-o]: the directory of
    [u.odoc_file] with the parent id stripped. Used for the commands that have
    no [-o] ([compile-asset], [odoc-md]). *)

type dirs = {
  odoc_dir : Fpath.t;
  odocl_dir : Fpath.t;
  index_dir : Fpath.t;
  mld_dir : Fpath.t;
}

val fix_virtual :
  precompiled_units:intf t list Util.StringMap.t ->
  units:intf t list Util.StringMap.t ->
  intf t list Util.StringMap.t
(** [fix_virtual ~precompiled_units ~units] replaces the input file in units
    representing implementations of virtual libraries. Implementation units have
    a [cmt] but no [cmti], even though the interface is actually constrained by
    a [mli]. The [cmi] file for the implementation is actually taken from the
    virtual library, so this function replaces the [cmt] used for the interface
    rendering of the implemenetation library units with the [cmti] taken from
    the virtual library. [units] should contain all the units that might be
    changed by this function. [precompiled_units] should be empty if this is
    being called before any compilation has taken place - ie. in monorepo or
    opam mode. In voodoo mode if the virtual library is in a different package
    it will have already been compiled. and thus should not be changed. The
    types of the inputs and outputs are hashtbls of units, where the hashtable
    key is the interface hash. *)
