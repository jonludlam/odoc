type scope = {
  page_roots : (string * Fpath.t) list;
  lib_roots : (string * Fpath.t) list;
}
(** A package's index, built from the [.odocl] files of its units (see
    [Compile.generate]), the sidebar derived from it, and the directory of the
    package's HTML, where the search database and the JSON sidebar go. *)
type index = {
  index_file : Fpath.t;
  sidebar_file : Fpath.t;
  html_dir : Fpath.t;
}
(** An index is built from the [.odocl] files of the units of its package (see
    [Compile.html_generate]), so it is defined by its output, not by input
    directories. *)

type +'a t = {
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

type module_unit = [ intf | impl ] t
(** An interface or an implementation: the units of a library. *)

type page = [ mld | md | asset ] t
(** A page or an asset: the units of a package's documentation. *)

type lib = {
  lib_name : string;
  requires : string list;
  includes : Fpath.t list;
  units : module_unit list;
}
(** A library: its modules and their implementations, the libraries of this
    build that must be compiled before it ([requires]: its direct META
    dependencies, restricted to the libraries being built and with alias
    libraries expanded), and the [-I] search path its units are compiled and
    linked with ([includes]: the directories of its dependency cone). *)

type pkg = {
  pkgname : string option;
  scope : scope;
  index : index option;
  libs : lib list;
  pages : page list;
}

val all_units : pkg -> any list

val pkg_dir : Packages.t -> Fpath.t

val lib_dir : Packages.t -> Packages.libty -> Fpath.t
(** [lib_dir pkg lib] is the parent id of the library's units: it determines
    their identifiers and the URLs of their pages. *)

(** {2 Where the files go}

    The odoc directory mirrors the switch: a library's [.odoc] files are
    written beside where its objects are installed ([lib/<findlib dir>], so
    libraries sharing an object directory share an odoc directory and the
    [-I] set mirrors the compiler's), and a package's pages below
    [doc/<pkg>], reproducing the layout of their identifiers. Every location
    is thus a function of a library or package name, and an earlier run's
    output is found without any record of it. The same paths hold below the
    odocl directory. *)

val lib_obj_dir : Packages.libty -> Fpath.t
val pages_dir : Packages.t -> Fpath.t

val page_obj_dir : Packages.t -> Fpath.t -> Fpath.t
(** [page_obj_dir pkg rel_dir] is where a page whose parent id is [rel_dir]
    goes: [rel_dir]'s position below the package's {!doc_dir}, reproduced below
    its {!pages_dir}. *)

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
