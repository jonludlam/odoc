
(* For simplicity keep a global counter *)
let counter = ref 0

(* Again, for simplicity right now all idents are just strings.
    Later we'll likely convert these to be typed Names *)
type t = [
    | `Local of string * int
    | `Identifier of Odoc_model.Paths.Identifier.t
]

let fresh_int () =
    let n = !counter in
    incr counter;
    n

let rec local_of_identifier : Odoc_model.Paths.Identifier.t -> t =
    let open Odoc_model.Names in
    fun i ->
        let n = fresh_int () in 
            match i with
            | `Module (_,s) -> `Local (ModuleName.to_string s, n)
            | `Type (_,s) -> `Local (TypeName.to_string s, n)
            | `ModuleType (_,s) -> `Local (ModuleTypeName.to_string s, n)
            | `Parameter (_, s) -> `Local (ParameterName.to_string s, n)
            | `CoreType s -> `Local (TypeName.to_string s, n)
            | `Value (_,s) -> `Local (ValueName.to_string s, n)
            | `Result s -> local_of_identifier (s :> Odoc_model.Paths.Identifier.t)
            | `Class (_, s) -> `Local (ClassName.to_string s, n)
            | `ClassType (_, s) -> `Local (ClassTypeName.to_string s, n)
            | `Constructor (_, s) -> `Local (ConstructorName.to_string s, n)
            | `Field (_, s) -> `Local (FieldName.to_string s, n)
            | `Extension (_, s) -> `Local (ExtensionName.to_string s, n)
            | `Exception (_, s) -> `Local (ExceptionName.to_string s, n)
            | `Method (_, s) -> `Local (MethodName.to_string s, n)
            | `InstanceVariable (_, s) -> `Local (InstanceVariableName.to_string s, n)
            | `Root (_, s) -> `Local (UnitName.to_string s, n)
            | _ -> failwith "Unhandled in of_identifier"

let name : t -> string =
    function
    | `Local f -> fst f
    | `Identifier i -> Odoc_model.Paths.Identifier.name i

let reset () = counter := 0

let counter : t -> int =
    function
    | `Local f -> snd f
    | `Identifier _ -> 0

let fmt ppf (id : t) = Format.fprintf ppf "%s/%d" (name id) (counter id)

let rename =
    function
    | `Local (s, _) -> `Local (s, fresh_int ())
    | `Identifier _ -> failwith "Can't rename identifier"

