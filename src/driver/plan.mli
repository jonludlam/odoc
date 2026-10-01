(** The order in which a build does its work.

    Compiling a library needs the libraries it requires compiled first, and
    linking a package needs every package its {!Odoc_unit.scope} names compiled
    first. Nothing needs a link: [odoc link] reads the output of [odoc compile]
    and writes somewhere else. Both relations are therefore acyclic, and the
    whole build is one sequence of steps, worked out once here rather than
    discovered as it runs.

    A package's scope and a library's requires hold names. A name this build has
    nothing for is dropped: it belongs to a package an earlier run compiled, or
    to no package at all. *)

type step =
  | Compile_lib of Odoc_unit.pkg * Odoc_unit.lib
      (** Compile a library, after the libraries it requires. *)
  | Compile_pages of Odoc_unit.pkg  (** Compile a package's pages. *)
  | Link of Odoc_unit.pkg
      (** Link and render a package, after everything its scope names is
          compiled. *)

val of_packages : Odoc_unit.pkg list -> step list
(** The steps for building these packages, in the order given. Each library is
    compiled once, and each package's pages once, however many packages name
    them. *)

val compile_only : Odoc_unit.pkg -> step list
(** The compile steps of one package, for voodoo mode: ocaml-docs-ci gives it
    one package per job, and earlier jobs compiled the dependencies. *)
