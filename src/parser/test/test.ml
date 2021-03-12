open Odoc_parser

type sexp = Sexplib0.Sexp.t = Atom of string | List of sexp list

module Location_to_sexp = struct
  let point : Location.point -> sexp =
   fun { line; column } ->
    List [ Atom (string_of_int line); Atom (string_of_int column) ]

  let span : Location.span -> sexp =
   fun { file; start; end_ } -> List [ Atom file; point start; point end_ ]

  let at : ('a -> sexp) -> 'a Location.with_location -> sexp =
   fun f { location; value } -> List [ span location; f value ]
end

module Ast_to_sexp = struct
  let at = Location_to_sexp.at

  let str s = Atom s

  let opt f s = match s with Some s -> List [ f s ] | None -> List []

  let style : Ast.style -> sexp = function
    | `Bold -> Atom "bold"
    | `Italic -> Atom "italic"
    | `Emphasis -> Atom "emphasis"
    | `Superscript -> Atom "superscript"
    | `Subscript -> Atom "subscript"

  let reference_kind : Ast.reference_kind -> sexp = function
    | `Simple -> Atom "simple"
    | `With_text -> Atom "with_text"

  let rec inline_element : Ast.inline_element -> sexp = function
    | `Space _ -> Atom "space"
    | `Word w -> List [ Atom "word"; Atom w ]
    | `Code_span c -> List [ Atom "code_span"; Atom c ]
    | `Raw_markup (target, s) ->
        List [ Atom "raw_markup"; opt str target; Atom s ]
    | `Styled (s, es) ->
        List [ style s; List (List.map (at inline_element) es) ]
    | `Reference (kind, r, es) ->
        List
          [
            reference_kind kind;
            at str r;
            List (List.map (at inline_element) es);
          ]
    | `Link (u, es) -> List [ str u; List (List.map (at inline_element) es) ]

  let rec nestable_block_element : Ast.nestable_block_element -> sexp = function
    | `Paragraph es ->
        List [ Atom "paragraph"; List (List.map (at inline_element) es) ]
    | `Code_block c -> List [ Atom "code_block"; Atom c ]
    | `Verbatim t -> List [ Atom "verbatim"; Atom t ]
    | `Modules ps -> List [ Atom "modules"; List (List.map (at str) ps) ]
    | `List (kind, weight, items) ->
        let kind =
          match kind with `Unordered -> "unordered" | `Ordered -> "ordered"
        in
        let weight =
          match weight with `Light -> "light" | `Heavy -> "heavy"
        in
        let items =
          items
          |> List.map (fun item ->
                 List (List.map (at nestable_block_element) item))
          |> fun items -> List items
        in
        List [ Atom kind; Atom weight; items ]

  let tag : Ast.tag -> sexp = function
    | `Author s -> List [ Atom "@author"; Atom s ]
    | `Deprecated es ->
        List (Atom "@deprecated" :: List.map (at nestable_block_element) es)
    | `Param (s, es) ->
        List
          ([ Atom "@param"; Atom s ] @ List.map (at nestable_block_element) es)
    | `Raise (s, es) ->
        List
          ([ Atom "@raise"; Atom s ] @ List.map (at nestable_block_element) es)
    | `Return es ->
        List (Atom "@return" :: List.map (at nestable_block_element) es)
    | `See (kind, s, es) ->
        let kind =
          match kind with
          | `Url -> "url"
          | `File -> "file"
          | `Document -> "document"
        in
        List
          ([ Atom "@see"; Atom kind; Atom s ]
          @ List.map (at nestable_block_element) es)
    | `Since s -> List [ Atom "@since"; Atom s ]
    | `Before (s, es) ->
        List
          ([ Atom "@before"; Atom s ] @ List.map (at nestable_block_element) es)
    | `Version s -> List [ Atom "@version"; Atom s ]
    | `Canonical p -> List [ Atom "@canonical"; at str p ]
    | `Inline -> Atom "@inline"
    | `Open -> Atom "@open"
    | `Closed -> Atom "@closed"

  let block_element : Ast.block_element -> sexp = function
    | #Ast.nestable_block_element as e -> nestable_block_element e
    | `Heading (level, label, es) ->
        let label = List [ Atom "label"; opt str label ] in
        let level = string_of_int level in
        List [ Atom level; label; List (List.map (at inline_element) es) ]
    | `Tag t -> tag t

  let docs : Ast.docs -> sexp = fun f -> List (List.map (at block_element) f)
end

let error err = Atom (Odoc_parser.Error.to_string err)

let parser_output formatter { Odoc_parser.Error.value; warnings } =
  let value = Ast_to_sexp.docs value in
  let warnings = List (List.map error warnings) in
  let output =
    List [ List [ Atom "output"; value ]; List [ Atom "warnings"; warnings ] ]
  in
  Sexplib0.Sexp.pp_hum formatter output;
  Format.pp_print_flush formatter ()

let test ?(location = { Location.line = 1; column = 0 }) str =
  let location =
    {
      Lexing.pos_fname = "none";
      pos_lnum = location.line;
      pos_bol = 0;
      pos_cnum = location.column;
    }
  in
  let ast = Odoc_parser.parse_comment ~location ~text:str in
  Format.printf "%a" parser_output ast

let%expect_test _ =
  let module T = struct
    let empty = test ""; [%expect "((output ()) (warnings ()))"]
    let space = test " "; [%expect "((output ()) (warnings ()))"]
    let two_spaces = test "  "; [%expect "((output ()) (warnings ()))"]
    let tab = test "\t"; [%expect "((output ()) (warnings ()))"]
    let mixed_space = test " \t \t"; [%expect "((output ()) (warnings ()))"]
    let newline = test "\n"; [%expect "((output ()) (warnings ()))"]
    let blank_line = test "\n\n"; [%expect "((output ()) (warnings ()))"]
    let cf_lf = test "\r\n"; [%expect "((output ()) (warnings ()))"] 
  end [@ocaml.warning "-32"] in ()
