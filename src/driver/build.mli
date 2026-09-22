(** Building a set of packages, one package at a time. *)

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
