(** The packages of one build, with the names they use resolved.

    A package's {!Odoc_unit.scope} names the packages it may refer to. It holds
    names, because a name is what goes on the command line. This module resolves
    them once, against the packages of this build, so that {!Build} works with
    packages.

    A name this build has nothing for is dropped. It belongs to a package an
    earlier run compiled, or to no package at all. *)

type t

val of_packages : Odoc_unit.pkg list -> t

val scope_packages : t -> Odoc_unit.pkg -> Odoc_unit.pkg list
(** The packages of this build that the package's scope names. All of them must
    be compiled before it is linked. *)
