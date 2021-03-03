val parse :
  Error.warning_accumulator ->
  Location_.span ->
  string Location_.with_location option ->
  Odoc_parser.Ast.reference ->
  (Paths.Reference.t, Error.t) Result.result

type path = [ `Root of string | `Dot of Paths.Path.Module.t * string ]

val read_path_longident :
  Location_.span -> string -> (path, Error.t) Result.result

val read_mod_longident :
  Error.warning_accumulator ->
  Location_.span ->
  Odoc_parser.Ast.reference ->
  (Paths.Reference.Module.t, Error.t) Result.result
