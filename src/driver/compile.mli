type compiled = Odoc_unit.pkg

val init_stats : Odoc_unit.pkg list -> unit

val compile :
  ?partial:Fpath.t -> partial_dir:Fpath.t -> Odoc_unit.pkg list -> compiled list
(** Compile the units of the given packages. Modules are compiled with their
    library's [-I] search path, in dependency order.

    Use [partial] to reuse the output of a previous call to [compile]. Useful in
    the voodoo context. *)

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
  occurrence_file:Fpath.t ->
  remaps:(string * string) list ->
  generate_json:bool ->
  simplified_search_output:bool ->
  Fpath.t ->
  linked list ->
  unit
(** Build each package's index and sidebar, then generate the HTML of its units.
*)
