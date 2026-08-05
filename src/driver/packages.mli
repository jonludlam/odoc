(** {1 OCaml compilation unit} *)

(** {2 Interface part} *)

type dep = string * Digest.t

type intf = { mif_hash : string; mif_path : Fpath.t; mif_deps : dep list }

val pp_intf : Format.formatter -> intf -> unit

(** {2 Implementation part} *)

type src_info = { src_path : Fpath.t }

type impl = {
  mip_path : Fpath.t;
  mip_src_info : src_info option;
  mip_deps : dep list;
}

val pp_impl : Format.formatter -> impl -> unit

(** {2 OCaml Compilation unit} *)

type modulety = {
  m_name : string;
  m_intf : intf;
  m_impl : impl option;
  m_hidden : bool;
}

(** {1 Standalone pages units} *)

type mld = { mld_path : Fpath.t; mld_rel_path : Fpath.t }

type md = { md_path : Fpath.t; md_rel_path : Fpath.t }

val pp_mld : Format.formatter -> mld -> unit

val pp_md : Format.formatter -> md -> unit

(** {1 Asset units} *)

type asset = { asset_path : Fpath.t; asset_rel_path : Fpath.t }

val pp_asset : Format.formatter -> asset -> unit

(** {1 Packages} *)

(** Compilation units are associated to libraries, while documentation are
    associated to package *)

type libty = {
  lib_name : string;
  dir : Fpath.t;
  archive_name : string option;
  lib_deps : Util.StringSet.t;
  modules : modulety list;
  id_override : string option;
}

module Lib : sig
  val v :
    libname_of_archive:string Fpath.Map.t ->
    pkg_name:string ->
    dir:Fpath.t ->
    cmtidir:Fpath.t option ->
    all_lib_deps:Util.StringSet.t Util.StringMap.t ->
    cmi_only_libs:(Fpath.t * string) list ->
    id_override:string option ->
    libty list

  val pp : Format.formatter -> libty -> unit
end

type t = {
  name : string;
  version : string;
  libraries : libty list;
  mlds : mld list;
  assets : asset list;
  selected : bool;
  remaps : (string * string) list;
  other_docs : md list;
  pkg_dir : Fpath.t;
  doc_dir : Fpath.t;
  config : Global_config.t;
}

val pp : Format.formatter -> t -> unit

type lib_graph = Util.StringSet.t Util.StringMap.t
(** Every library the driver has seen a declaration for, mapped to the libraries
    its [META] [requires] names — not transitively closed.

    This is deliberately not built from the libraries that were compiled. A
    library with no archive of its own never becomes a {!libty}, so a graph
    assembled from those would dead-end at every ocamlfind wrapper package —
    [num], whose modules live in [num.core], or [compiler-libs] — and nothing
    depending on one could reach what it actually provides. *)

val mk_mlds : Opam.doc_file list -> mld list * asset list * md list

val of_libs :
  packages_dir:Fpath.t option -> Util.StringSet.t -> t list * lib_graph
(** Turns a set of libraries into a map from package name to package *)

val of_packages :
  packages_dir:Fpath.t option -> string list -> t list * lib_graph

val remap_virtual :
  precompiled:(string * Fpath.t) list Util.StringMap.t -> t list -> t list
(** [remap_virtual ~precompiled pkgs] points each module of an implementation of
    a virtual library at the [.cmti] that constrains it — the one belonging to
    the virtual library, which is what carries the interface and its
    documentation. Implementations ship only a [.cmt].

    The [.cmti] is found by interface digest, either among [pkgs] or, when the
    virtual library was compiled by an earlier run and its build tree is gone,
    among [precompiled]: a map from interface digest to the
    [(module name, path)] pairs of [.cmti] files stashed beside earlier output
    and recorded in its {!Marker.Lib}. Paths must be usable as they stand. *)
