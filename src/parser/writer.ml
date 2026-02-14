(** An implementation of the Writer monad for parser error reporting *)

type +'a t = Writer of ('a * warning list)

(** A warning can either be totally self-contained, or a function requiring
    input. This is so that we can pass the input string in later in order
    to extract spans where errors occur.

    EscapedBlock carries a block-level element that was consumed during
    error recovery in an inline context (e.g., a code block inside {b ...}).
    These are extracted after parsing and spliced into the AST at the
    appropriate position. *)
and warning =
  | InputNeeded of (string -> Warning.t)
  | Warning of Warning.t
  | EscapedBlock of Ast.nestable_block_element Loc.with_location

let return : 'a -> 'a t = fun x -> Writer (x, [])
let bind : 'a t -> f:('a -> 'b t) -> 'b t =
 fun (Writer (node, warnings)) ~f ->
  let (Writer (next, next_warnings)) = f node in
  Writer (next, warnings @ next_warnings)

let map : f:('a -> 'b) -> 'a t -> 'b t =
 fun ~f (Writer (x, ws)) -> Writer (f x, ws)

let map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t =
 fun ~f (Writer (a, aws)) (Writer (b, bws)) -> Writer (f a b, aws @ bws)

let ( <$> ) f w = map ~f w

let seq : 'a t -> 'b t -> 'b t =
 fun (Writer (_, ws)) (Writer (x, ws2)) -> Writer (x, ws @ ws2)
let ( *> ) = seq

(** Useful functions for working with Writer.t *)
module Prelude = struct
  let return = return
  let ( let* ) w f = bind w ~f
  let ( <$> ) = ( <$> )
  let ( *> ) = ( *> )
end

let sequence : 'a t list -> 'a list t =
 fun xs ->
  let xs, ws = List.map (fun (Writer (x, ws)) -> (x, ws)) xs |> List.split in
  Writer (xs, List.flatten ws)

let sequence_loc : 'a t Loc.with_location -> 'a Loc.with_location t =
 fun { value; location } -> Loc.at location <$> value

(** [warning Warning.t Writer.t] is equivalent to Haskell's [tell] *)
let warning warning (Writer (n, ws)) = Writer (n, warning :: ws)

let return_warning node warning = Writer (node, [ warning ])

(** [ensure pred warning Writer] Logs a warning if the predicate returns true 
    for the contained value *)
let ensure : ('a -> bool) -> warning -> 'a t -> 'a t =
 fun pred warning (Writer (x, ws) as self) ->
  if pred x then self else Writer (x, warning :: ws)

type 'a run_result = {
  value : 'a;
  warnings : Warning.t list;
  escaped_blocks : Ast.nestable_block_element Loc.with_location list;
}

let run : input:string -> 'a t -> 'a run_result =
 fun ~input (Writer (tree, warnings)) ->
  let ws = ref [] and blocks = ref [] in
  List.iter (fun w ->
    match w with
    | InputNeeded f -> ws := f input :: !ws
    | Warning w -> ws := w :: !ws
    | EscapedBlock b -> blocks := b :: !blocks
  ) warnings;
  { value = tree; warnings = List.rev !ws; escaped_blocks = List.rev !blocks }

let get : 'a t -> 'a = fun (Writer (x, _)) -> x
