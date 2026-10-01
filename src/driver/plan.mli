(** The packages of one build, with the names they use resolved.

    A package's {!Odoc_unit.scope} names the packages it may refer to, and a
    library's {!Odoc_unit.lib.requires} names the libraries it must be compiled
    after. Both hold names, because a name is what goes on the command line.
    This module resolves them once, against the packages of this build, so that
    {!Build} works with packages and libraries.

    A name this build has nothing for is dropped. It belongs to a package an
    earlier run compiled, or to no package at all. *)

type t

val of_packages : Odoc_unit.pkg list -> t
(** The plan for building these packages, in this order. *)

val packages : t -> Odoc_unit.pkg list
(** The packages to build, in the order given. *)

val requires : t -> Odoc_unit.lib -> (Odoc_unit.pkg * Odoc_unit.lib) list
(** The libraries of this build that must be compiled before this one, each with
    the package that provides it. *)

val scope_packages : t -> Odoc_unit.pkg -> Odoc_unit.pkg list
(** The packages of this build that the package's scope names. All of them must
    be compiled before it is linked. *)
