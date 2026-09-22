(** The steps of building one package. *)

val init_stats : Odoc_unit.pkg list -> unit

val compile_lib : Odoc_unit.pkg -> Odoc_unit.lib -> unit
(** Compile the modules of a library, in dependency order, with the library's
    [-I] search path. Modules of other libraries are expected to have been
    compiled already and to be reachable through that search path. *)

val compile_pages : Odoc_unit.pkg -> unit
(** Compile the pages and assets of a package. *)

val link : warnings_tags:string list -> Odoc_unit.pkg -> unit
(** Link the units of a package: every unit with the package's reference scope
    ([-P]/[-L]), modules with their library's [-I] as well. Everything the scope
    names must have been compiled. *)

val html_support : Fpath.t -> unit
(** Write the files every page relies on into the HTML directory. *)

val with_remaps : (string * string) list -> (Fpath.t option -> 'a) -> 'a
(** [with_remaps remaps f] calls [f] with a file describing [remaps], for
    {!generate}, or with [None] when there are none. *)

val generate :
  ?remap_file:Fpath.t -> generate_json:bool -> Fpath.t -> Odoc_unit.pkg -> unit
(** Build a package's index, sidebar and search database, then render the HTML
    of its units into the given directory. *)

val json_index : occurrence_file:Fpath.t -> Fpath.t -> Odoc_unit.pkg -> unit
(** [json_index ~occurrence_file html_dir pkg] writes the package's JSON search
    index ([index.js]), with occurrence counts, into the HTML directory. This is
    the only step that uses the occurrence counts. *)
