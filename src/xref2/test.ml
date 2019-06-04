(* Playground for new model *)


module XComponent = struct
    open Model.Names

    let prefix_signature path s =
        let open Component.Signature in
        let sub = List.fold_left (fun map item ->
            match item with
            | Type (ident,_) -> Subst.add ident (`Global (`Resolved (`Type (path, TypeName.of_string (Ident.name ident)) :> Model.Paths.Path.Resolved.t))) map
            | Module (ident, _) -> Subst.add ident (`Global (`Resolved (`Module (path, ModuleName.of_string (Ident.name ident)) :> Model.Paths.Path.Resolved.t))) map
            | ModuleType (ident, _) -> Subst.add ident (`Global (`Resolved (`ModuleType (path, ModuleTypeName.of_string (Ident.name ident)) :> Model.Paths.Path.Resolved.t))) map)
                Subst.identity s in
        List.map (function
                | Module (id, m) -> Module (Ident.rename id, Subst.module_ sub m)
                | ModuleType (id, m) -> ModuleType (Ident.rename id, Subst.module_type sub m)
                | Type (id, t) -> Type (Ident.rename id, Subst.type_ sub t)) s

end

type env = Env.t

let find_module_in_sig s name =
    let m =
        List.find_opt
            (function
            | Component.Signature.Module ((s,_), _) when s=name -> true
            | _ -> false) s
    in
    match m with
    | Some (Component.Signature.Module (_,x)) -> x
    | _ ->
        Printf.printf "Failed to find '%s'" name;
        failwith "Failed to find component"

let find_module_type_in_sig s name =
    let m =
        List.find_opt
            (function
            | Component.Signature.ModuleType ((s,_), _) when s=name -> true
            | _ -> false) s
    in
    match m with
    | Some (Component.Signature.ModuleType (_,x)) -> x
    | _ ->
        Printf.printf "Failed to find '%s'" name;
        failwith "Failed to find component"

let find_type_in_sig s name =
    let m =
        List.find_opt
            (function
            | Component.Signature.Type ((s,_), _) when s=name -> true
            | _ -> false) s
    in
    match m with
    | Some (Component.Signature.Type (_,x)) -> x
    | _ ->
        Printf.printf "Failed to find '%s'" name;
        failwith "Failed to find component"

type module_lookup_result =
    Model.Paths.Path.Resolved.Module.t * Component.Module.t

type module_type_lookup_result =
    Model.Paths.Path.Resolved.ModuleType.t * Component.ModuleType.t

type type_lookup_result =
    Model.Paths.Path.Resolved.Type.t * Component.Type.t

let rec lookup_module_from_model_path : env -> Model.Paths.Path.Module.t -> (module_lookup_result, Model.Paths.Path.Module.t) Result.result = fun env p ->
    match p with
    | `Resolved p -> Ok (lookup_module_from_resolved_model_path env p)
    | `Dot (m, x) -> begin
        match lookup_module_from_model_path env m with
        | Error p -> Error (`Dot (p, x))
        | Ok (p', m) -> 
            let s = signature_of_module env m |> XComponent.prefix_signature p' in
            let m' = find_module_in_sig s x in
            Ok (`Module (p', Model.Names.ModuleName.of_string x), m')
        end
    | _ -> Error p

and lookup_module_from_resolved_model_path : env -> Model.Paths.Path.Resolved.Module.t -> module_lookup_result = fun env p ->
    match p with
    | `Identifier i ->
        let m = Env.lookup_module i env in
        (p, m)
    | `Hidden m ->
        let (p, m) = lookup_module_from_resolved_model_path env m in
        (`Hidden p, m)
    | `Module (p, name) ->
        let (p', m) = lookup_module_from_resolved_model_path env p in
        let sg = signature_of_module env m |> XComponent.prefix_signature p in
        let m' = find_module_in_sig sg (Model.Names.ModuleName.to_string name) in
        (`Module (p', name), m')
    | _ ->
        let b = Buffer.create 1024 in
        let fmt = Format.formatter_of_buffer b in
        Format.fprintf fmt "Failed to lookup path: %a@." Component.Fmt.model_resolved_path (p :> Model.Paths.Path.Resolved.t);
        failwith (Buffer.contents b)

and lookup_module_from_path : env -> Cpath.t -> (module_lookup_result, Model.Paths.Path.Module.t) Result.result = fun env p ->
    match p with
    | `Local id -> failwith (Printf.sprintf "oh no %s" (Ident.name id))
    | `Ldot (parent, id) -> begin
        match lookup_module_from_path env parent with
        | Ok (p, m) ->
            let sg = signature_of_module env m |> XComponent.prefix_signature p in
            let m' = find_module_in_sig sg id in
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



and lookup_module_type_from_model_path : env -> Model.Paths.Path.ModuleType.t -> (module_type_lookup_result, Model.Paths.Path.ModuleType.t) Result.result = fun env p ->
    match p with
    | `Resolved p -> Ok (lookup_module_type_from_resolved_model_path env p)
    | `Dot (m, x) -> begin
        match lookup_module_from_model_path env m with
        | Error p -> Error (`Dot (p, x))
        | Ok (p', m) ->
            let s = signature_of_module env m in
            let m' = find_module_type_in_sig s x in
            Ok (`ModuleType (p', Model.Names.ModuleTypeName.of_string x), m')
        end

and lookup_module_type_from_resolved_model_path : env -> Model.Paths.Path.Resolved.ModuleType.t -> module_type_lookup_result = fun env p ->
    match p with
    | `Identifier i ->
        let m = Env.lookup_module_type i env in
        (p, m)
    | `ModuleType (parent, name) ->
        let (p', m) = lookup_module_from_resolved_model_path env parent in
        let sg = signature_of_module env m in
        let mt = find_module_type_in_sig sg name in
        (`ModuleType (p', name), mt)

and lookup_module_type_from_path : env -> Cpath.t -> (module_type_lookup_result, Model.Paths.Path.ModuleType.t) Result.result = fun env p ->
    match p with
    | `Local id -> failwith (Printf.sprintf "oh no %s" (Ident.name id))
    | `Ldot (parent, id) -> begin
        match lookup_module_from_path env parent with
        | Ok (p, m) ->
            let sg = signature_of_module env m in
            let m' = find_module_type_in_sig sg id in
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

and lookup_type_from_model_path : env -> Model.Paths.Path.Type.t -> (type_lookup_result, Model.Paths.Path.Type.t) Result.result = fun env p ->
    match p with
    | `Resolved p -> Ok (lookup_type_from_resolved_model_path env p)
    | `Dot (m, x) -> begin
        match lookup_module_from_model_path env m with
        | Error p -> Error (`Dot (p, x))
        | Ok (p', m) ->
            let s = signature_of_module env m in
            let m' = find_type_in_sig s x in
            Ok (`Type (p', Model.Names.TypeName.of_string x), m')
        end

and lookup_type_from_resolved_model_path : env -> Model.Paths.Path.Resolved.Type.t -> type_lookup_result = fun env p ->
    match p with
    | `Identifier (`Type _ as i) ->
        let m = Env.lookup_type i env in
        (p, m)
    | `Type (parent, name) ->
        let (p', m) = lookup_module_from_resolved_model_path env parent in
        let sg = signature_of_module env m in
        let t = find_type_in_sig sg name in
        (`Type (p', name), t)
    | _ -> failwith "Unhandled"

and lookup_type_from_path : env -> Cpath.t -> (type_lookup_result, Model.Paths.Path.Type.t) Result.result = fun env p ->
    match p with
    | `Local id -> failwith (Printf.sprintf "oh no %s" (Ident.name id))
    | `Ldot (parent, id) -> begin
        match lookup_module_from_path env parent with
        | Ok (p, m) ->
            let sg = signature_of_module env m in
            let t' = find_type_in_sig sg id in
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


and fragmap_signature : env -> Model.Paths.Fragment.Resolved.Signature.t -> (Component.Signature.t -> Component.Signature.t) -> Component.Signature.t -> Component.Signature.t =
    fun env frag fn sg ->
        match frag with
        | `Root -> fn sg
        | `Module (parent, name) ->
            fragmap_signature env parent (fun s ->
                List.map (function
                    | Component.Signature.Module ((id, _) as id', m) when id=name ->
                        let sg = signature_of_module env m in
                        let sg' = fn sg in
                        Component.Signature.Module (id',Component.Module.{type_=ModuleType (Component.ModuleType.Signature sg')})
                    | x -> x) s) sg
        | _ -> failwith "foo"

and fragmap_module : env -> Model.Paths.Fragment.Resolved.Module.t -> (Component.Module.t -> Component.Module.t) -> Component.Signature.t -> Component.Signature.t =
    fun env frag fn sg ->
        match frag with
        | `Module (parent, name) ->
            fragmap_signature env parent (fun s ->
                List.map (function 
                | Component.Signature.Module ((id, _) as id', m) when id=name ->
                    let m' = fn m in
                    Component.Signature.Module (id', m')
                | x -> x) s) sg
        | `Subst (_, x) -> fragmap_module env x fn sg
        | `SubstAlias (_, x) -> fragmap_module env x fn sg


(*        fragmap_signature env (frag : Model.Paths.Fragment.Resolved.Module.t :> Model.Paths.Fragment.Resolved.Signature.t) fn sg*)

and fragmap_unresolved_signature env frag fn sg =
    match frag with
    | `Dot (parent, name) ->
        fragmap_unresolved_signature env parent (fun s ->
            List.map (function
                | Component.Signature.Module ((id, _) as id', m) when id=name ->
                    let sg = signature_of_module env m in
                    let sg' = fn sg in
                    Component.Signature.Module (id', Component.Module.{type_=ModuleType (Component.ModuleType.Signature sg')})
                | x -> x) s) sg
    | `Resolved x ->
        fragmap_signature env x fn sg

and fragmap_unresolved_module env frag fn sg =
    match frag with
    | `Dot (parent, name) ->
        fragmap_unresolved_signature env parent (fun s ->
            List.map (function
                | Component.Signature.Module ((id, _) as id', m) when id=name ->
                    let m' = fn m in
                    Component.Signature.Module (id', m')
                | x -> x) s) sg
    | `Resolved x ->
        fragmap_module env x fn sg

and signature_of_module_type_expr : env -> Component.ModuleType.expr -> Component.Signature.t = fun env m ->
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
                | Ok (_p, m) ->
                    fragmap_unresolved_module env frag (fun _ -> m) sg
                | Error _ ->
                    failwith "Failed to lookup substitute module"
                end
            | ModuleEq (frag, ModuleType expr) ->

                fragmap_unresolved_module env frag (fun _ -> Component.Module.{type_ = ModuleType expr}) sg
            ) sg subs

and signature_of_module_type : env -> Component.ModuleType.t -> Component.Signature.t = fun env m ->
    match m with
    | None -> failwith "oh no"
    | Some expr -> signature_of_module_type_expr env expr

and signature_of_module env m =
    match m.Component.Module.type_ with
    | Component.Module.Alias _ -> failwith "Unhandled"
    | Component.Module.ModuleType expr -> signature_of_module_type_expr env expr




(* When resolving paths, we go down the path until we find an Identifier. Once we've
   got that far we look up the component via the Identifier in the environment and return the
   resolved path for that Identifier and the Module component for it. As we then go up the Path.t
   we look up the Ident.t in the module component, finding a new module and passing
   that up the chain, building the resolved path as we go, until we pop out at the
   top to find the final thing - so far the only thing that can be handled is a
   type. *)

and resolve_type_path : env -> Model.Paths.Path.Type.t -> Model.Paths.Path.Type.t = fun env p ->
    match lookup_type_from_model_path env p with
    | Ok (p', _) -> `Resolved p'
    | Error p -> p

and resolve_module_type_path : env -> Model.Paths.Path.ModuleType.t -> Model.Paths.Path.ModuleType.t = fun env p ->
    match lookup_module_type_from_model_path env p with
    | Ok (p', _) -> `Resolved p'
    | Error p -> p
    
let rec resolve_unit env t =
    let open Model.Lang.Compilation_unit in
    {t with content = resolve_content env t.content}

and resolve_content env =
    let open Model.Lang.Compilation_unit in
    function
    | Module m -> Module (resolve_signature env m)
    | Pack _ -> failwith "Unhandled content"

and resolve_signature : env -> Model.Lang.Signature.t -> _ = fun env s ->
    let open Model.Lang.Signature in
    let env = Env.open_signature s env in
    let (_, items') = 
        List.fold_right (fun item (env, items) ->
            match item with
            | Module (r, m) ->
                let m' = resolve_module env m in
(*                let env' = update_env env (`Module m') in *)
                (env, (Module (r, m'))::items)
            | Type (r, t) ->
                let t' = resolve_type env t in
(*                let env' = update_env env (`Type t') in*)
                (env, (Type (r, t'))::items)
            | ModuleType mt ->
                let mt' = resolve_module_type env mt in
                (env, (ModuleType mt')::items)
            | _ -> failwith "Unhandled signature element") s (env, [])
    in items'

and resolve_module : env -> Model.Lang.Module.t -> Model.Lang.Module.t = fun env m ->
    let open Model.Lang.Module in
    match m.type_ with
    | ModuleType expr ->
        {m with type_ = ModuleType (resolve_module_type_expr env expr)}
    | _ -> failwith "Unhandled module"

and resolve_module_type : env -> Model.Lang.ModuleType.t -> Model.Lang.ModuleType.t = fun env m ->
    let open Model.Lang.ModuleType in
    let expr' = match m.expr with | None -> None | Some expr -> Some (resolve_module_type_expr env expr) in
    {m with expr = expr'}

and resolve_module_type_expr : env -> Model.Lang.ModuleType.expr -> Model.Lang.ModuleType.expr = fun env expr ->
    let open Model.Lang.ModuleType in
    match expr with
    | Signature s -> Signature (resolve_signature env s)
    | Path p -> Path (resolve_module_type_path env p)
    | With (expr, subs) ->
        With (resolve_module_type_expr env expr,
            List.map (function
                | ModuleEq (frag, eqn) -> ModuleEq (frag, eqn)
                | TypeEq (frag, eqn) -> TypeEq (frag, eqn)
                | x -> x) subs)
    | _ -> failwith "Unhandled module type expression"

and resolve_type env t =
    let open Model.Lang.TypeDecl in
    match t.equation.manifest with
    | Some texpr ->
        let texpr' = resolve_type_expression env texpr in
        {t with equation = {t.equation with manifest = Some texpr'}}
    | None -> t

and resolve_type_expression : env -> _ -> _ = fun env texpr ->
    let open Model.Lang.TypeExpr in 
    match texpr with
    | Constr (path, ts) -> begin
        match lookup_type_from_model_path env path with
        | Ok (p, _) ->  Constr (`Resolved p, ts)
        | Error p -> Constr (p, ts)
        end
    | _ -> failwith "Unhandled type expression"

and resolve_compilation_unit : env -> Odoc.Compilation_unit.t -> Odoc.Compilation_unit.t = fun env c ->
    let open Model.Lang.Compilation_unit in
    let content' = 
        match c.content with
        | Module s -> Module (resolve_signature env s)
        | Pack _ -> failwith "Unhandled"
    in
    { c with
      content = content' }

let mkenv () =
  Odoc.Env.create ~important_digests:false ~directories:[]

let resolve unit =
  let env = mkenv () in
  let resolve_env = Odoc.Env.build env (`Unit unit) in
  let resolver = Odoc.Env.resolver resolve_env in
  let result = Xref.resolve resolver unit in
  let tbl = Xref.tbl resolver in
  (result,tbl)

