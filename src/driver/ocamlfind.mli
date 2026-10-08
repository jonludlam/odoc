(** What findlib knows about the libraries installed in the switch.

    A library here is a findlib package, for example [fmt] or [tyxml.functor].
    Findlib is initialised from the switch's [findlib.conf], so these answers
    hold for the switch the driver documents. *)

val get_dir : string -> (Fpath.t, [> `Msg of string ]) result
(** The directory a library's objects are installed in. An error for a library
    that is not installed. *)

val archives : string -> string list
(** The archive files of a library, for example [["fmt.cma"; "fmt.cmxa"]]. A
    library with no archive is either virtual or an alias for the libraries it
    requires. *)

val direct_deps : string -> (Util.StringSet.t, [> `Msg of string ]) result
(** The libraries a library requires directly: its [META] [requires] field under
    the [ppx_driver] predicate, plus [stdlib]. The field is read as written, so
    a requirement on a library that is not installed is kept. An error for a
    library that is not installed. *)

(** Everything the driver needs to know about the libraries findlib knows,
    gathered in one pass. *)
module Db : sig
  type t = {
    all_lib_deps : Util.StringSet.t Util.StringMap.t;
        (** Each library's direct requirements. *)
    libname_of_archive : string Fpath.map;
        (** The library each archive, given by its full path without extension,
            belongs to. *)
    cmi_only_libs : (Fpath.t * string) list;
        (** Libraries with no archive, with their directory. *)
  }

  val create : unit -> t
end
