(** The {e reference scope} passed to the link step: the page trees ([-P]) and
    module trees ([-L]) against which references in this unit resolve. The
    search path ([-I]) is a separate, deeper thing and lives in the [includes]
    field of {!t}. *)
module Pkg_args : sig
  type t

  val compiled_pages : t -> (string * Fpath.t) list
  val compiled_libs : t -> (string * Fpath.t) list

  val v :
    odoc_dir:Fpath.t ->
    pages:(string * Fpath.t) list ->
    libs:(string * Fpath.t) list ->
    t

  val pp : t Fmt.t
end

type sidebar = { output_file : Fpath.t; json : bool; pkg_dir : Fpath.t }
type index = {
  roots : Fpath.t list;
  output_file : Fpath.t;
  json : bool;
  search_dir : Fpath.t;
  sidebar : sidebar option;
}

type 'a t = {
  parent_id : Odoc.Id.t;
  input_file : Fpath.t;
  input_copy : Fpath.t option;
      (* Used to stash cmtis from virtual libraries into the odoc dir for voodoo mode *)
  output_dir : Fpath.t;
  odoc_file : Fpath.t;
  odocl_file : Fpath.t;
  pkg_args : Pkg_args.t;  (** Consumed only at link time (see {!Pkg_args}). *)
  pkgname : string option;
  lib_name : string;
      (** The library this unit belongs to (empty for pages and assets). *)
  deps : (string * Digest.t) list;
      (** The unit's per-module dependencies (interface deps for [`Intf], the
          implementation's for [`Impl]; empty otherwise). *)
  includes : Fpath.Set.t;
      (** The search path ([-I]) used to compile and to link this unit: the
          directories holding the odoc files of its library and of that
          library's dependency closure. Empty for pages and assets. Every unit
          of a library shares one set. *)
  index : index option;
  enable_warnings : bool;
  to_output : bool;
  kind : 'a;
}

type intf_extra = { hidden : bool; hash : string }
and intf = [ `Intf of intf_extra ]

type impl_extra = { src_id : Odoc.Id.t; src_path : Fpath.t }
type impl = [ `Impl of impl_extra ]

type mld = [ `Mld ]
type md = [ `Md ]
type asset = [ `Asset ]

type any = [ impl | intf | mld | asset | md ] t

val pp : any Fmt.t

val intf_unit_name : Packages.intf -> string
(** The name odoc gives a unit compiled from this interface. *)

val stash_basename : Packages.intf -> string
(** Basename of the [.cmti] copy the compile step stashes beside a virtual
    library's [.odoc], so that implementations compiled by later runs — after
    the virtual library's build tree is gone — can still be compiled against its
    interface. *)

val pkg_dir : Packages.t -> Fpath.t
val lib_dir : Packages.t -> Packages.libty -> Fpath.t
val doc_dir : Packages.t -> Fpath.t
val src_dir : Packages.t -> Fpath.t
val src_lib_dir : Packages.t -> Packages.libty -> Fpath.t

type dirs = {
  odoc_dir : Fpath.t;
  odocl_dir : Fpath.t;
  index_dir : Fpath.t;
  mld_dir : Fpath.t;
}
