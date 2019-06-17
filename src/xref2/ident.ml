
(* For simplicity keep a global counter *)
let counter = ref 0

(* Again, for simplicity right now all idents are just strings.
    Later we'll likely convert these to be typed Names *)
type t = string * int

let fresh_int () =
    let n = !counter in
    incr counter;
    n

let of_identifier : Odoc_model.Paths.Identifier.t -> t =
    let open Odoc_model.Names in
    fun i ->
        let n = fresh_int () in 
        match i with
        | `Module (_,s) -> (ModuleName.to_string s, n)
        | `Type (_,s) -> (TypeName.to_string s, n)
        | `ModuleType (_,s) -> (ModuleTypeName.to_string s, n)
        | _ -> failwith "Unhandled"

let name : t -> string = fst

let reset () = counter := 0

let counter : t -> int = snd

let fmt ppf (id : t) = Format.fprintf ppf "%s/%d" (fst id) (snd id)

let rename (s, _) = (s, fresh_int ())

