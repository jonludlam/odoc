(** The records the driver leaves beside its output, so that a later run — which
    in voodoo mode has only one package's [META] files — can learn what it needs
    about the packages and libraries compiled before it.

    Both records are versioned. Anything else, including the prose markers
    written before these records existed, reads back as [None]: the marker then
    conveys only what it originally did, that the directory it sits in holds a
    package's or a library's compiled output. Markers cache facts that
    recompiling the package recovers, so they are never migrated. *)

module Pkg : sig
  val filename : string

  type t = {
    pkg_name : string;
    libraries : (string * string list) list;
        (** Every library this package's [META] files declare, paired with the
            libraries its [requires] names — not transitively closed.

            Libraries with no archive of their own are included, and are the
            reason this is recorded per package rather than per library: an
            ocamlfind wrapper such as [num], whose modules all live in its
            [core] subpackage, never becomes a library in its own right and so
            gets no library marker. Without its entry, a walk from anything
            declaring a dependency on [num] stops there instead of reaching
            [num.core]. *)
  }

  val to_string : t -> string
  val of_string : string -> t option
end

module Lib : sig
  val filename : string

  type module_info = {
    m_name : string;
    digest : string;  (** Interface digest. *)
    stashed_cmti : string option;
        (** Basename of the stashed [.cmti], in the marker's own directory. *)
  }

  type t = { lib_name : string; pkg_name : string; modules : module_info list }

  val of_lib : pkg_name:string -> Packages.libty -> t
  val to_string : t -> string
  val of_string : string -> t option
end
