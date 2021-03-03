module Ast = Ast
module Location = Location

module Error : sig
  type t = Error.t = { location : Location.span; message : string }

  type 'a with_warnings = { value : 'a; warnings : t list }
end

val parse_comment :
  location:Lexing.position -> text:string -> Ast.docs Error.with_warnings

val parse_reference :
  text:string -> (string Location.with_location option * Ast.reference) Error.with_warnings