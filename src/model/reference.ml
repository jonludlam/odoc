open Result

let expected_err : string -> Location_.span -> Error.t =
  Error.make "Expected %s."

let unknown_reference_qualifier : string -> Location_.span -> Error.t =
  Error.make "Unknown reference qualifier '%s'."

let deprecated_reference_kind : string -> string -> Location_.span -> Error.t =
  Error.make "'%s' is deprecated, use '%s' instead."

let reference_kinds_do_not_match : string -> string -> Location_.span -> Error.t
    =
  Error.make "Old-style reference kind ('%s:') does not match new ('%s-')."

let not_allowed :
    ?suggestion:string ->
    what:string ->
    in_what:string ->
    Location_.span ->
    Error.t =
 fun ?suggestion ~what ~in_what ->
  Error.make ?suggestion "%s is not allowed in %s."
    (Astring.String.Ascii.capitalize what)
    in_what

let deprecated_reference_kind warnings location kind replacement =
  deprecated_reference_kind kind replacement location |> Error.warning warnings

(* http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sec359. *)
let match_ocamldoc_reference_kind (_warnings as w) (_location as loc) s :
    Paths.Reference.tag_any option =
  let d = deprecated_reference_kind in
  match s with
  | Some "module" -> Some `TModule
  | Some "modtype" ->
      d w loc "modtype" "module-type";
      Some `TModuleType
  | Some "class" -> Some `TClass
  | Some "classtype" ->
      d w loc "classtype" "class-type";
      Some `TClassType
  | Some "val" -> Some `TValue
  | Some "type" -> Some `TType
  | Some "exception" -> Some `TException
  | Some "attribute" -> None
  | Some "method" -> Some `TMethod
  | Some "section" -> Some `TLabel
  | Some "const" ->
      d w loc "const" "constructor";
      Some `TConstructor
  | Some "recfield" ->
      d w loc "recfield" "field";
      Some `TField
  | Some "childpage" -> Some `TChildPage
  | Some "childmodule" -> Some `TChildModule
  | _ -> None

let match_extra_odoc_reference_kind (_warnings as w) (_location as loc) s :
    Paths.Reference.tag_any option =
  let d = deprecated_reference_kind in
  match s with
  | Some "class-type" -> Some `TClassType
  | Some "constructor" -> Some `TConstructor
  | Some "exn" ->
      d w loc "exn" "exception";
      Some `TException
  | Some "extension" -> Some `TExtension
  | Some "field" -> Some `TField
  | Some "instance-variable" -> Some `TInstanceVariable
  | Some "label" ->
      d w loc "label" "section";
      Some `TLabel
  | Some "module-type" -> Some `TModuleType
  | Some "page" -> Some `TPage
  | Some "value" ->
      d w loc "value" "val";
      Some `TValue
  | _ -> None

(* Ideally, [tokenize] would call this on every reference kind annotation during
   tokenization, when generating the token list. However, that constrains the
   phantom tag type to be the same for all tokens in the list (because lists are
   homogeneous). So, the parser stores kinds as strings in the token list
   instead, and this function is called on each string at the latest possible
   time to prevent typing issues.

   A secondary reason to delay parsing, and store strings in the token list, is
   that we need the strings for user-friendly error reporting. *)
let match_reference_kind warnings location s : Paths.Reference.tag_any =
  match s with
  | None -> `TUnknown
  | Some s as wrapped -> (
      let result =
        match match_ocamldoc_reference_kind warnings location wrapped with
        | Some kind -> Some kind
        | None -> match_extra_odoc_reference_kind warnings location wrapped
      in
      match result with
      | Some kind -> kind
      | None -> unknown_reference_qualifier s location |> Error.raise_exception
      )

let expected allowed location =
  let unqualified = "or an unqualified reference" in
  let allowed =
    match allowed with
    | [ one ] -> Printf.sprintf "'%s-' %s" one unqualified
    | _ ->
        String.concat ", "
          (List.map (Printf.sprintf "'%s-'") allowed @ [ unqualified ])
  in
  expected_err allowed location

let parse :
    Error.warning_accumulator ->
    Location_.span ->
    string Location_.with_location option ->
    Odoc_parser.Ast.reference ->
    (Paths.Reference.t, Error.t) Result.result =
 fun warnings whole_reference_location old_kind r ->
  let open Paths.Reference in
  let open Names in
  let open Location_ in
  let rec signature r : Signature.t =
    match r with
    | `Root { value = kind, identifier; location } -> (
        let kind = match_reference_kind warnings location kind in

        match kind with
        | (`TUnknown | `TModule | `TModuleType) as kind ->
            `Root (identifier, kind)
        | _ ->
            expected [ "module"; "module-type" ] location
            |> Error.raise_exception )
    | `Dot (p, { value = kind, identifier; location }) -> (
        let kind = match_reference_kind warnings location kind in

        match kind with
        | `TUnknown -> `Dot ((parent p :> LabelParent.t), identifier)
        | `TModule -> `Module (signature p, ModuleName.make_std identifier)
        | `TModuleType ->
            `ModuleType (signature p, ModuleTypeName.make_std identifier)
        | _ ->
            expected [ "module"; "module-type" ] location
            |> Error.raise_exception )
  and parent r : Parent.t =
    match r with
    | `Root { value = kind, identifier; location } -> (
        let kind = match_reference_kind warnings location kind in

        match kind with
        | (`TUnknown | `TModule | `TModuleType | `TType | `TClass | `TClassType)
          as kind ->
            `Root (identifier, kind)
        | _ ->
            expected
              [ "module"; "module-type"; "type"; "class"; "class-type" ]
              location
            |> Error.raise_exception )
    | `Dot (p, { value = kind, identifier; location }) -> (
        let kind = match_reference_kind warnings location kind in

        match kind with
        | `TUnknown -> `Dot ((parent p :> LabelParent.t), identifier)
        | `TModule -> `Module (signature p, ModuleName.make_std identifier)
        | `TModuleType ->
            `ModuleType (signature p, ModuleTypeName.make_std identifier)
        | `TType -> `Type (signature p, TypeName.make_std identifier)
        | `TClass -> `Class (signature p, ClassName.make_std identifier)
        | `TClassType ->
            `ClassType (signature p, ClassTypeName.make_std identifier)
        | _ ->
            expected
              [ "module"; "module-type"; "type"; "class"; "class-type" ]
              location
            |> Error.raise_exception )
  in

  let class_signature r : ClassSignature.t =
    match r with
    | `Root { value = kind, identifier; location } -> (
        let kind = match_reference_kind warnings location kind in

        match kind with
        | (`TUnknown | `TClass | `TClassType) as kind -> `Root (identifier, kind)
        | _ ->
            expected [ "class"; "class-type" ] location |> Error.raise_exception
        )
    | `Dot (p, { value = kind, identifier; location }) -> (
        let kind = match_reference_kind warnings location kind in

        match kind with
        | `TUnknown -> `Dot ((parent p :> LabelParent.t), identifier)
        | `TClass -> `Class (signature p, ClassName.make_std identifier)
        | `TClassType ->
            `ClassType (signature p, ClassTypeName.make_std identifier)
        | _ ->
            expected [ "class"; "class-type" ] location |> Error.raise_exception
        )
  in

  let datatype r : DataType.t =
    match r with
    | `Root { value = kind, identifier; location } -> (
        let kind = match_reference_kind warnings location kind in

        match kind with
        | (`TUnknown | `TType) as kind -> `Root (identifier, kind)
        | _ -> expected [ "type" ] location |> Error.raise_exception )
    | `Dot (p, { value = kind, identifier; location }) -> (
        let kind = match_reference_kind warnings location kind in

        match kind with
        | `TUnknown -> `Dot ((parent p :> LabelParent.t), identifier)
        | `TType -> `Type (signature p, TypeName.make_std identifier)
        | _ -> expected [ "type" ] location |> Error.raise_exception )
  in

  let rec label_parent r : LabelParent.t =
    match r with
    | `Root { value = kind, identifier; location } -> (
        let kind = match_reference_kind warnings location kind in
        match kind with
        | ( `TUnknown | `TModule | `TModuleType | `TType | `TClass | `TClassType
          | `TPage ) as kind ->
            `Root (identifier, kind)
        | _ ->
            expected
              [ "module"; "module-type"; "type"; "class"; "class-type"; "page" ]
              location
            |> Error.raise_exception )
    | `Dot (p, { value = kind, identifier; location }) -> (
        let kind = match_reference_kind warnings location kind in
        match kind with
        | `TUnknown -> `Dot (label_parent p, identifier)
        | `TModule -> `Module (signature p, ModuleName.make_std identifier)
        | `TModuleType ->
            `ModuleType (signature p, ModuleTypeName.make_std identifier)
        | `TType -> `Type (signature p, TypeName.make_std identifier)
        | `TClass -> `Class (signature p, ClassName.make_std identifier)
        | `TClassType ->
            `ClassType (signature p, ClassTypeName.make_std identifier)
        | _ ->
            expected
              [ "module"; "module-type"; "type"; "class"; "class-type" ]
              location
            |> Error.raise_exception )
  in

  let start_from_last_component old_kind reference =
    let kind new_kind new_kind_string =
      match old_kind with
      | None -> new_kind
      | Some
          {
            Odoc_parser.Location.value = old_kind_string;
            location = old_kind_location;
          } -> (
          let old_kind =
            match_reference_kind warnings old_kind_location
              (Some old_kind_string)
          in
          match new_kind with
          | `TUnknown -> old_kind
          | _ ->
              if old_kind <> new_kind then
                reference_kinds_do_not_match old_kind_string new_kind_string
                  whole_reference_location
                |> Error.warning warnings;
              new_kind )
    in

    match reference with
    | `Root { value = new_kind, identifier; location } ->
        let new_kind_string = match new_kind with Some s -> s | None -> "" in
        let new_kind = match_reference_kind warnings location new_kind in
        let kind = kind new_kind new_kind_string in
        `Root (identifier, kind)
    | `Dot (p, { value = new_kind, identifier; location }) -> (
        let new_kind_string = match new_kind with Some s -> s | None -> "" in
        let new_kind = match_reference_kind warnings location new_kind in

        let kind = kind new_kind new_kind_string in
        match kind with
        | `TUnknown -> `Dot (label_parent p, identifier)
        | `TModule -> `Module (signature p, ModuleName.make_std identifier)
        | `TModuleType ->
            `ModuleType (signature p, ModuleTypeName.make_std identifier)
        | `TType -> `Type (signature p, TypeName.make_std identifier)
        | `TConstructor ->
            `Constructor (datatype p, ConstructorName.make_std identifier)
        | `TField -> `Field (parent p, FieldName.make_std identifier)
        | `TExtension ->
            `Extension (signature p, ExtensionName.make_std identifier)
        | `TException ->
            `Exception (signature p, ExceptionName.make_std identifier)
        | `TValue -> `Value (signature p, ValueName.make_std identifier)
        | `TClass -> `Class (signature p, ClassName.make_std identifier)
        | `TClassType ->
            `ClassType (signature p, ClassTypeName.make_std identifier)
        | `TMethod -> `Method (class_signature p, MethodName.make_std identifier)
        | `TInstanceVariable ->
            `InstanceVariable
              (class_signature p, InstanceVariableName.make_std identifier)
        | `TLabel -> `Label (label_parent p, LabelName.make_std identifier)
        | `TChildPage | `TChildModule ->
            let suggestion =
              Printf.sprintf "'child-%s' should be first." identifier
            in
            not_allowed ~what:"Child label"
              ~in_what:"the last component of a reference path" ~suggestion
              location
            |> Error.raise_exception
        | `TPage ->
            let suggestion =
              Printf.sprintf "'page-%s' should be first." identifier
            in
            not_allowed ~what:"Page label"
              ~in_what:"the last component of a reference path" ~suggestion
              location
            |> Error.raise_exception )
  in

  Error.catch (fun () -> start_from_last_component old_kind r)

type path = [ `Root of string | `Dot of Paths.Path.Module.t * string ]

let read_path_longident location s =
  let open Paths.Path in
  let rec loop : string -> int -> path option =
   fun s pos ->
    try
      let idx = String.rindex_from s pos '.' in
      let name = String.sub s (idx + 1) (pos - idx) in
      if String.length name = 0 then None
      else
        match loop s (idx - 1) with
        | None -> None
        | Some parent -> Some (`Dot ((parent :> Module.t), name))
    with Not_found ->
      let name = String.sub s 0 (pos + 1) in
      if String.length name = 0 then None else Some (`Root name)
  in
  match loop s (String.length s - 1) with
  | Some r -> Result.Ok (r :> path)
  | None -> Result.Error (expected_err "a valid path" location)

let read_mod_longident warnings location lid :
    (Paths.Reference.Module.t, Error.t) Result.result =
  match parse warnings location None lid with
  | Error _ as e -> e
  | Ok p -> (
      match p with
      | (`Root (_, (`TUnknown | `TModule)) | `Dot (_, _) | `Module (_, _)) as r
        ->
          Result.Ok r
      | _ -> Result.Error (expected_err "a reference to a module" location) )
