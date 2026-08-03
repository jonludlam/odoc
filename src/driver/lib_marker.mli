(** The record dropped into each library's [.odoc] directory.

    Its original purpose was to mark the directory as holding a library's
    compiled output, which is what {!Voodoo.extra_paths} uses it for. It also
    carries what a {e later} driver run needs to know about the library without
    access to the build tree it was compiled from:

    - the library's declared dependencies, which is what lets a run holding only
      its own package's [META] files reconstruct the library graph beyond its
      own package;
    - its modules' interface digests, and where the [.cmti] of a virtual library
      was stashed, so that an implementation compiled by a later run can be
      pointed at the interface it implements.

    The format is versioned. Anything else — a marker written by a driver with a
    different format version, or the prose marker written before this record
    existed — reads back as [None], and the library is treated as carrying no
    information beyond the existence of its directory. Markers are a cache of
    facts that recompiling the package would recover, so they are never
    migrated. *)

val filename : string
(** Basename of the marker file. *)

type module_info = {
  m_name : string;
  digest : string;  (** Interface digest. *)
  stashed_cmti : string option;
      (** Basename of the stashed [.cmti], in the marker's own directory. *)
}

type t = {
  lib_name : string;
  pkg_name : string;
  requires : string list;
      (** The library's [META] [requires], {e not} transitively closed. *)
  modules : module_info list;
}

val of_lib : pkg_name:string -> Packages.libty -> t

val to_string : t -> string

val of_string : string -> t option
(** [None] if the contents are not a marker of the current format version. *)
