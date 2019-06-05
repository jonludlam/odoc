let type_path : Env.t -> Model.Paths.Path.Type.t -> Model.Paths.Path.Type.t = fun env p ->
    match Tools.lookup_type_from_model_path env p with
    | Ok (p', _) -> `Resolved p'
    | Error p -> p

and module_type_path : Env.t -> Model.Paths.Path.ModuleType.t -> Model.Paths.Path.ModuleType.t = fun env p ->
    match Tools.lookup_module_type_from_model_path env p with
    | Ok (p', _) -> `Resolved p'
    | Error p -> p
    
let rec unit env t =
    let open Model.Lang.Compilation_unit in
    {t with content = content env t.content}

and content env =
    let open Model.Lang.Compilation_unit in
    function
    | Module m -> Module (signature env m)
    | Pack _ -> failwith "Unhandled content"

and signature : Env.t -> Model.Lang.Signature.t -> _ = fun env s ->
    let open Model.Lang.Signature in
    let env = Env.open_signature s env in
    let (_, items') = 
        List.fold_right (fun item (env, items) ->
            match item with
            | Module (r, m) ->
                let m' = module_ env m in
(*                let env' = update_env env (`Module m') in *)
                (env, (Module (r, m'))::items)
            | Type (r, t) ->
                let t' = type_ env t in
(*                let env' = update_env env (`Type t') in*)
                (env, (Type (r, t'))::items)
            | ModuleType mt ->
                let mt' = module_type env mt in
                (env, (ModuleType mt')::items)
            | _ -> failwith "Unhandled signature element") s (env, [])
    in items'

and module_ : Env.t -> Model.Lang.Module.t -> Model.Lang.Module.t = fun env m ->
    let open Model.Lang.Module in
    match m.type_ with
    | ModuleType expr ->
        {m with type_ = ModuleType (module_type_expr env expr)}
    | _ -> failwith "Unhandled module"

and module_type : Env.t -> Model.Lang.ModuleType.t -> Model.Lang.ModuleType.t = fun env m ->
    let open Model.Lang.ModuleType in
    let expr' = match m.expr with | None -> None | Some expr -> Some (module_type_expr env expr) in
    {m with expr = expr'}

and module_type_expr : Env.t -> Model.Lang.ModuleType.expr -> Model.Lang.ModuleType.expr = fun env expr ->
    let open Model.Lang.ModuleType in
    match expr with
    | Signature s -> Signature (signature env s)
    | Path p -> Path (module_type_path env p)
    | With (expr, subs) ->
        With (module_type_expr env expr,
            List.map (function
                | ModuleEq (frag, eqn) -> ModuleEq (frag, eqn)
                | TypeEq (frag, eqn) -> TypeEq (frag, eqn)
                | x -> x) subs)
    | _ -> failwith "Unhandled module type expression"

and type_ env t =
    let open Model.Lang.TypeDecl in
    match t.equation.manifest with
    | Some texpr ->
        let texpr' = type_expression env texpr in
        {t with equation = {t.equation with manifest = Some texpr'}}
    | None -> t

and type_expression : Env.t -> _ -> _ = fun env texpr ->
    let open Model.Lang.TypeExpr in 
    match texpr with
    | Constr (path, ts) -> begin
        match Tools.lookup_type_from_model_path env path with
        | Ok (p, _) ->  Constr (`Resolved p, ts)
        | Error p -> Constr (p, ts)
        end
    | _ -> failwith "Unhandled type expression"

and compilation_unit : Env.t -> Odoc.Compilation_unit.t -> Odoc.Compilation_unit.t = fun env c ->
    let open Model.Lang.Compilation_unit in
    let content' = 
        match c.content with
        | Module s -> Module (signature env s)
        | Pack _ -> failwith "Unhandled"
    in
    { c with
      content = content' }
