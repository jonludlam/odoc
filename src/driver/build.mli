val all :
  html_dir:Fpath.t ->
  remaps:(string * string) list ->
  generate_json:bool ->
  warnings_tags:string list ->
  Odoc_unit.pkg list ->
  unit
(** Build the packages one at a time: each is compiled (its libraries after the
    libraries they require), then, once every package its reference scope names
    has been compiled, linked and rendered. *)
