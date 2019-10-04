
(* For simplicity keep a global counter *)
let counter = ref 0

(* Again, for simplicity right now all idents are just strings.
    Later we'll likely convert these to be typed Names *)
type t = string * int

let fresh_int () =
    let n = !counter in
    incr counter;
    n

let rec of_identifier : Odoc_model.Paths.Identifier.t -> t =
    let open Odoc_model.Names in
    fun i ->
        let n = fresh_int () in 
        match i with
        | `Module (_,s) -> (ModuleName.to_string s, n)
        | `Type (_,s) -> (TypeName.to_string s, n)
        | `ModuleType (_,s) -> (ModuleTypeName.to_string s, n)
        | `Parameter (_, s) -> (ParameterName.to_string s, n)
        | `CoreType s -> (TypeName.to_string s, n)
        | `Value (_,s) -> (ValueName.to_string s, n)
        | `Result s -> of_identifier (s :> Odoc_model.Paths.Identifier.t)
        | `Class (_, s) -> (ClassName.to_string s, n)
        | `ClassType (_, s) -> (ClassTypeName.to_string s, n)
        | `Constructor (_, s) -> (ConstructorName.to_string s, n)
        | `Field (_, s) -> (FieldName.to_string s, n)
        | `Extension (_, s) -> (ExtensionName.to_string s, n)
        | `Exception (_, s) -> (ExceptionName.to_string s, n)
        | `Method (_, s) -> (MethodName.to_string s, n)
        | `InstanceVariable (_, s) -> (InstanceVariableName.to_string s, n)
        | `Root (_, s) -> (UnitName.to_string s, n)
        | `CoreException e -> (ExceptionName.to_string e, n)
        | `Label (_, s) -> (LabelName.to_string s, n)
        | `Page (_, s) -> (PageName.to_string s, n)

let name : t -> string = fst

let reset () = counter := 0

let counter : t -> int = snd

let fmt ppf (id : t) = Format.fprintf ppf "%s/%d" (fst id) (snd id)

let rename (s, _) = (s, fresh_int ())

