(** The odoc commands for each unit, at each step.

    The driver either runs these commands ({!Compile}) or writes them as rules
    for [make] ({!Makefile}). Both take them from here, so the two build the
    same output. What waits for what is left to each of them. *)

val compile_module :
  Odoc_unit.lib -> Odoc_unit.module_unit -> Cmd_outputs.action list
(** Compile an interface, and copy its input where {!Odoc_unit.t.input_copy}
    says; or compile an implementation. *)

val compile_page : Odoc_unit.page -> Cmd_outputs.action
(** Compile a page, a Markdown file or an asset. *)

val link :
  warnings_tags:string list ->
  libs:(string * Fpath.t) list ->
  Odoc_unit.pkg ->
  Odoc_unit.any ->
  Cmd_outputs.action
(** Link a unit of the package with the libraries [libs] (see
    {!Odoc_unit.link_units}) and the package's page roots. *)

val index :
  html_dir:Fpath.t ->
  file_list:Fpath.t ->
  Odoc_unit.index ->
  Cmd_outputs.action * Cmd_outputs.action list
(** Build a package's index from the [.odocl] files listed in [file_list]. The
    first command writes the index. The others read it, and write the sidebar,
    the JSON sidebar and the search database. *)

val generate :
  html_dir:Fpath.t ->
  ?remap_file:Fpath.t ->
  generate_json:bool ->
  Odoc_unit.pkg ->
  Odoc_unit.any ->
  Cmd_outputs.action list
(** Render a unit as HTML and, with [generate_json], as JSON as well. A unit
    that is not {!Odoc_unit.is_output} needs no command. *)
