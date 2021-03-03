val parse_reference :
  Error.warning_accumulator ->
  Location.span ->
  string ->
  string Location.with_location option * Ast.reference

val parse :
  Error.warning_accumulator ->
  Token.t Location.with_location Stream.t ->
  Ast.docs
