type compiled = Odoc_unit.pkg

val init_stats : Odoc_unit.pkg list -> unit

val compile : Odoc_unit.pkg list -> compiled list
(** Compile the units of the given packages. Modules are compiled with their
    library's [-I] search path, in dependency order; a dependency that is not
    among these packages is expected to have been compiled already and to be
    reachable through that search path. *)

type linked

val link :
  warnings_tags:string list ->
  custom_layout:bool ->
  compiled list ->
  linked list
(** Link the units of the given packages. Every unit of a package links with the
    package's reference scope ([-P]/[-L]), modules with their library's [-I] as
    well. *)

val html_generate :
  remaps:(string * string) list ->
  generate_json:bool ->
  Fpath.t ->
  linked list ->
  unit
(** Build each package's index, sidebar and search database, then generate the
    HTML of its units. *)

val json_index : occurrence_file:Fpath.t -> Fpath.t -> linked list -> unit
(** [json_index ~occurrence_file html_dir pkgs] writes each package's JSON
    search index ([index.js]), with occurrence counts, into the HTML directory.
    This is the only step that uses the occurrence counts. *)
