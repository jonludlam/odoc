type path =
  [ `Root of string
  | `LibraryRoot of string * string
    (** [`LibraryRoot (lib, name)]: the root [name] in library [lib], written
        [/lib/name]. *)
  | `Dot of path * string ]

val parse :
  Location_.span -> string -> Paths.Reference.t Error.with_errors_and_warnings

val parse_asset :
  Location_.span ->
  string ->
  Paths.Reference.Asset.t Error.with_errors_and_warnings

val own_library : (string * string) option ref
(** TEMPORARY, until dune writes [@canonical /lib/Name.X]. [Some (unit, lib)]
    while [odoc compile] reads the unit [unit] of the library [lib]. A
    canonical tag whose root [N] is how dune names that library's main module
    ([unit] is [N], or starts with [N__]) is then read as [/lib/N]. *)

val read_path_longident :
  Location_.span -> string -> path Error.with_errors_and_warnings

val read_mod_longident :
  Location_.span ->
  string ->
  Paths.Reference.Module.t Error.with_errors_and_warnings
