(** Building a set of packages, one package at a time. *)

val compile_package : Odoc_unit.pkg -> unit
(** Compile one package: its libraries, each after the libraries it requires,
    and then its pages. This is what {!all} does for each package, and what
    voodoo mode does on its own, since ocaml-docs-ci gives it one package per
    job and the dependencies were compiled by earlier jobs. *)

val all :
  html_dir:Fpath.t ->
  remaps:(string * string) list ->
  generate_json:bool ->
  warnings_tags:string list ->
  Odoc_unit.pkg list ->
  unit
(** Build the packages in the order given.

    Libraries are compiled in dependency order: a library after the libraries it
    {!Odoc_unit.lib.requires}, each once. A package is linked and rendered once
    every package its scope names has been compiled. That is usually at once;
    when the scope names a package that comes later, that package is compiled
    first. This is the same split ocaml.org makes between compiling a package
    and linking it. *)
