open Model.Names


let prefix_signature path s =
    let open Component.Signature in
    let sub = List.fold_left (fun map item ->
        match item with
        | Type t -> Subst.add t.id (`Global (`Resolved (`Type (path, TypeName.of_string (Ident.name t.id)) :> Model.Paths.Path.Resolved.t))) map
        | Module m -> Subst.add m.id (`Global (`Resolved (`Module (path, ModuleName.of_string (Ident.name m.id)) :> Model.Paths.Path.Resolved.t))) map
        | ModuleType mt -> Subst.add mt.id (`Global (`Resolved (`ModuleType (path, ModuleTypeName.of_string (Ident.name mt.id)) :> Model.Paths.Path.Resolved.t))) map)
            Subst.identity s in
    List.map (function
            | Module m -> Module {(Subst.module_ sub m) with id=Ident.rename m.id}
            | ModuleType mt -> ModuleType {(Subst.module_type sub mt) with id=Ident.rename mt.id}
            | Type t -> Type {(Subst.type_ sub t) with id=Ident.rename t.id}) s


type module_lookup_result =
    Model.Paths.Path.Resolved.Module.t * Component.Module.t

type module_type_lookup_result =
    Model.Paths.Path.Resolved.ModuleType.t * Component.ModuleType.t

type type_lookup_result =
    Model.Paths.Path.Resolved.Type.t * Component.Type.t

exception Lookup_failure of Env.t * Cpath.t * string

let rec lookup_module_from_model_path : Env.t -> Model.Paths.Path.Module.t -> (module_lookup_result, Model.Paths.Path.Module.t) Result.result = fun env p ->
    match p with
    | `Resolved p -> Ok (lookup_module_from_resolved_model_path env p)
    | `Dot (m, x) -> begin
        match lookup_module_from_model_path env m with
        | Error p -> Error (`Dot (p, x))
        | Ok (p', m) -> 
            let s = signature_of_module env m |> prefix_signature p' in
            let m' = Component.Find.module_in_sig s x in
            Ok (`Module (p', Model.Names.ModuleName.of_string x), m')
        end
    | _ -> Error p

and lookup_module_from_resolved_model_path : Env.t -> Model.Paths.Path.Resolved.Module.t -> module_lookup_result = fun env p ->
    match p with
    | `Identifier i ->
        let m = Env.lookup_module i env in
        (p, m)
    | `Hidden m ->
        let (p, m) = lookup_module_from_resolved_model_path env m in
        (`Hidden p, m)
    | `Module (p, name) ->
        let (p', m) = lookup_module_from_resolved_model_path env p in
        let sg = signature_of_module env m |> prefix_signature p in
        let m' = Component.Find.module_in_sig sg (Model.Names.ModuleName.to_string name) in
        (`Module (p', name), m')
    | _ ->
        let b = Buffer.create 1024 in
        let fmt = Format.formatter_of_buffer b in
        Format.fprintf fmt "Failed to lookup path: %a@." Component.Fmt.model_resolved_path (p :> Model.Paths.Path.Resolved.t);
        failwith (Buffer.contents b)

and lookup_module_from_path : Env.t -> Cpath.t -> (module_lookup_result, Model.Paths.Path.Module.t) Result.result = fun env p ->
    match p with
    | `Local _id -> raise (Lookup_failure (env, p, "module")) 
    | `Ldot (parent, id) -> begin
        match lookup_module_from_path env parent with
        | Ok (p, m) ->
            let sg = signature_of_module env m |> prefix_signature p in
            let m' = Component.Find.module_in_sig sg id in
            Ok (`Module (p, Model.Names.ModuleName.of_string id), m')
        | Error p ->
            Error (`Dot (p, id))
        end
    | `Global ((
              `Root _
            | `Forward _
            | `Dot (#Model.Paths.Path.Module.t, _)
            | `Resolved (#Model.Paths_types.Resolved_path.module_no_id)
            | `Resolved (`Identifier (#Model.Paths_types.Identifier.module_))
            | `Apply (_,_)) as p) ->
        lookup_module_from_model_path env p
    | _ -> failwith "bad lookup"



and lookup_module_type_from_model_path : Env.t -> Model.Paths.Path.ModuleType.t -> (module_type_lookup_result, Model.Paths.Path.ModuleType.t) Result.result = fun env p ->
    match p with
    | `Resolved p -> Ok (lookup_module_type_from_resolved_model_path env p)
    | `Dot (m, x) -> begin
        match lookup_module_from_model_path env m with
        | Error p -> Error (`Dot (p, x))
        | Ok (p', m) ->
            let s = signature_of_module env m in
            let m' = Component.Find.module_type_in_sig s x in
            Ok (`ModuleType (p', Model.Names.ModuleTypeName.of_string x), m')
        end

and lookup_module_type_from_resolved_model_path : Env.t -> Model.Paths.Path.Resolved.ModuleType.t -> module_type_lookup_result = fun env p ->
    match p with
    | `Identifier i ->
        let m = Env.lookup_module_type i env in
        (p, m)
    | `ModuleType (parent, name) ->
        let (p', m) = lookup_module_from_resolved_model_path env parent in
        let sg = signature_of_module env m in
        let mt = Component.Find.module_type_in_sig sg name in
        (`ModuleType (p', name), mt)


and lookup_module_type_from_path : Env.t -> Cpath.t -> (module_type_lookup_result, Model.Paths.Path.ModuleType.t) Result.result = fun env p ->
    match p with
    | `Local _id -> raise (Lookup_failure (env, p, "module_type"))
    | `Ldot (parent, id) -> begin
        match lookup_module_from_path env parent with
        | Ok (p, m) ->
            let sg = signature_of_module env m in
            let m' = Component.Find.module_type_in_sig sg id in
            Ok (`ModuleType (p, Model.Names.ModuleName.of_string id), m')
        | Error p ->
            Error (`Dot (p, id))
        end
    | `Global ((
              `Resolved (#Model.Paths_types.Resolved_path.module_type_no_id)
            | `Resolved (`Identifier (#Model.Paths_types.Identifier.module_type))
            | `Dot (_,_)) as p) ->
        lookup_module_type_from_model_path env p
    | _ -> failwith "bad lookup"

and lookup_type_from_model_path : Env.t -> Model.Paths.Path.Type.t -> (type_lookup_result, Model.Paths.Path.Type.t) Result.result = fun env p ->
    match p with
    | `Resolved p -> Ok (lookup_type_from_resolved_model_path env p)
    | `Dot (m, x) -> begin
        match lookup_module_from_model_path env m with
        | Error p -> Error (`Dot (p, x))
        | Ok (p', m) ->
            let s = signature_of_module env m in
            let m' = Component.Find.type_in_sig s x in
            Ok (`Type (p', Model.Names.TypeName.of_string x), m')
        end

and lookup_type_from_resolved_model_path : Env.t -> Model.Paths.Path.Resolved.Type.t -> type_lookup_result = fun env p ->
    match p with
    | `Identifier (`Type _ as i) ->
        let m = Env.lookup_type i env in
        (p, m)
    | `Type (parent, name) ->
        let (p', m) = lookup_module_from_resolved_model_path env parent in
        let sg = signature_of_module env m in
        let t = Component.Find.type_in_sig sg name in
        (`Type (p', name), t)
    | _ -> failwith "Unhandled"

and lookup_type_from_path : Env.t -> Cpath.t -> (type_lookup_result, Model.Paths.Path.Type.t) Result.result = fun env p ->
    match p with
    | `Local _id -> raise (Lookup_failure (env, p, "type"))
    | `Ldot (parent, id) -> begin
        match lookup_module_from_path env parent with
        | Ok (p, m) ->
            let sg = signature_of_module env m in
            let t' = Component.Find.type_in_sig sg id in
            Ok (`Type (p, Model.Names.ModuleName.of_string id), t')
        | Error p ->
            Error (`Dot (p, id))
        end
    | `Global ((
              `Resolved (#Model.Paths_types.Resolved_path.type_no_id)
            | `Resolved (`Identifier (#Model.Paths_types.Identifier.type_))
            | `Dot (_,_)) as p) ->
        lookup_type_from_model_path env p
    | _ -> failwith "bad lookup"

and signature_of_module_type_expr : Env.t -> Component.ModuleType.expr -> Component.Signature.t = fun env m ->
    match m with
    | Component.ModuleType.Path p -> begin
        match lookup_module_type_from_path env p with
        | Ok (_p, mt) ->
            let sg = signature_of_module_type env mt in
(*            Component.Strengthen.signature _p sg *)
            sg
        | Error _p ->
            failwith "Couldn't find signature"
        end
    | Component.ModuleType.Signature s -> s
    | Component.ModuleType.With (s, subs) ->
        let sg = signature_of_module_type_expr env s in
        List.fold_left (fun sg sub ->
            match sub with
            | Component.ModuleType.ModuleEq (frag, Alias path) -> begin
                match lookup_module_from_path env path with
                | Ok (p, m) ->
                    let m' = Strengthen.module_ p m in
                    fragmap_unresolved_module env frag (fun _ -> m') sg
                | Error _ ->
                    failwith "Failed to lookup substitute module"
                end
            | ModuleEq (frag, ModuleType expr) ->
                fragmap_unresolved_module env frag (fun m -> Component.Module.{m with type_ = ModuleType expr}) sg
            ) sg subs

and signature_of_module_type : Env.t -> Component.ModuleType.t -> Component.Signature.t = fun env m ->
    match m.expr with
    | None -> failwith "oh no"
    | Some expr -> signature_of_module_type_expr env expr

and signature_of_module env m =
    match m.Component.Module.type_ with
    | Component.Module.Alias _ -> failwith "Unhandled"
    | Component.Module.ModuleType expr -> signature_of_module_type_expr env expr



and fragmap_signature : Env.t -> Model.Paths.Fragment.Resolved.Signature.t -> (Component.Signature.t -> Component.Signature.t) -> Component.Signature.t -> Component.Signature.t =
    fun env frag fn sg ->
        match frag with
        | `Root -> fn sg
        | `Module (parent, name) ->
            fragmap_signature env parent (fun s ->
                List.map (function
                    | Component.Signature.Module m when (Ident.name m.id = name) ->
                        let sg = signature_of_module env m in
                        let sg' = fn sg in
                        Component.Signature.Module {id=m.id; type_=ModuleType (Component.ModuleType.Signature sg')}
                    | x -> x) s) sg
        | _ -> failwith "foo"

and fragmap_module : Env.t -> Model.Paths.Fragment.Resolved.Module.t -> (Component.Module.t -> Component.Module.t) -> Component.Signature.t -> Component.Signature.t =
    fun env frag fn sg ->
        match frag with
        | `Module (parent, name) ->
            fragmap_signature env parent (fun s ->
                List.map (function 
                | Component.Signature.Module m when Ident.name m.id = name ->
                    let m' = fn m in
                    Component.Signature.Module {m' with id=m.id}
                | x -> x) s) sg
        | `Subst (_, x) -> fragmap_module env x fn sg
        | `SubstAlias (_, x) -> fragmap_module env x fn sg


(*        fragmap_signature env (frag : Model.Paths.Fragment.Resolved.Module.t :> Model.Paths.Fragment.Resolved.Signature.t) fn sg*)

and fragmap_unresolved_signature env frag fn sg =
    match frag with
    | `Dot (parent, name) ->
        fragmap_unresolved_signature env parent (fun s ->
            List.map (function
                | Component.Signature.Module m when Ident.name m.id = name ->
                    let sg = signature_of_module env m in
                    let sg' = fn sg in
                    Component.Signature.Module {id=m.id; type_=ModuleType (Component.ModuleType.Signature sg')}
                | x -> x) s) sg
    | `Resolved x ->
        fragmap_signature env x fn sg

and fragmap_unresolved_module env frag fn sg =
    match frag with
    | `Dot (parent, name) ->
        fragmap_unresolved_signature env parent (fun s ->
            List.map (function
                | Component.Signature.Module m when Ident.name m.id = name ->
                    let m' = fn m in
                    Component.Signature.Module {m' with id=m.id}
                | x -> x) s) sg
    | `Resolved x ->
        fragmap_module env x fn sg

