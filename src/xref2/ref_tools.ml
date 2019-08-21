open Odoc_model.Paths.Reference

type module_lookup_result =
  Resolved.Module.t * Component.Element.module_

type module_type_lookup_result =
  Resolved.ModuleType.t * Component.Element.module_type

type signature_lookup_result =
  Resolved.Signature.t * Component.Element.signature

type type_lookup_result =
  Resolved.Type.t * Component.Element.type_

type value_lookup_result =
  Resolved.Value.t * Component.Element.value

let resolve_module_reference : Env.t -> Module.t -> module_lookup_result option =
  let open Tools.OptionMonad in
  fun env r ->
    match r with
    | `Resolved _r ->
      failwith "Erk"
    | `Dot (_parent, _name) ->
      failwith "Erk"
    | `Module (_parent, _name) ->
      failwith "Erk"
    | `Root (name, _) -> begin
      Env.lookup_module_by_name name env 
      >>= function | `Module (id, _) as m -> return (`Identifier id, m)
      end

and resolve_signature_reference : Env.t -> Signature.t -> signature_lookup_result option =
  fun _env _r ->
    failwith "foo"

and resolve_value_reference : Env.t -> Value.t -> value_lookup_result option =
  let open Tools.OptionMonad in
  fun env r ->
    match r with
    | `Root (name, _) -> begin
      Env.lookup_value_by_name name env >>= function
      | `Value (id, _) as v -> return (`Identifier id, v)
      end
    | _ -> failwith "erk"
  
and resolve_reference : Env.t -> Odoc_model.Paths.Reference.t -> Odoc_model.Paths.Reference.t =
  fun env r ->
    match r with
    | `Root (name, `TUnknown) -> begin
        match Env.lookup_any_by_name name env with
        | Some (`Module (id,_)) -> `Resolved (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | Some (`ModuleType (id, _)) -> `Resolved (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | Some (`Value (id, _)) -> `Resolved (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | Some (`Type (id, _)) -> `Resolved (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | Some (`Label id) -> `Resolved (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | None -> r
        end
    | `Root (_name, _ ) -> failwith "Unimplemented"
    | `Resolved _ -> r
    | `Module (_sg, _name) ->
        r
    | _ -> r


