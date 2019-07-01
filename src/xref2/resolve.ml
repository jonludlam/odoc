module Opt = struct
    let map f = function | Some x -> Some (f x) | None -> None
end

let type_path : Env.t -> Odoc_model.Paths.Path.Type.t -> Odoc_model.Paths.Path.Type.t = fun env p ->
    match Tools.lookup_type_from_model_path env p with
    | Ok (p', _) -> `Resolved p'
    | Error p -> p

and module_type_path : Env.t -> Odoc_model.Paths.Path.ModuleType.t -> Odoc_model.Paths.Path.ModuleType.t = fun env p ->
    match Tools.lookup_module_type_from_model_path env p with
    | Ok (_, p', _) -> `Resolved p'
    | Error p -> p

and module_path : Env.t -> Odoc_model.Paths.Path.Module.t -> Odoc_model.Paths.Path.Module.t = fun env p ->
    match Tools.lookup_module_from_model_path env p with
    | Ok (_, p', _) -> `Resolved p'
    | Error p -> p

let rec unit env t =
    let open Odoc_model.Lang.Compilation_unit in
    {t with content = content env t.content}

and content env =
    let open Odoc_model.Lang.Compilation_unit in
    function
    | Module m -> Module (signature env m)
    | Pack _ -> failwith "Unhandled content"

and signature : Env.t -> Odoc_model.Lang.Signature.t -> _ = fun env s ->
    let open Odoc_model.Lang.Signature in
    let env = Env.open_signature s env in
    let (_, items') = 
        List.fold_right (fun item (env, items) ->
            match item with
            | Module (r, m) ->
                let m' = module_ env m in
                (env, (Module (r, m'))::items)
            | Type (r, t) ->
                let t' = type_ env t in
                (env, (Type (r, t'))::items)
            | ModuleType mt ->
                let mt' = module_type env mt in
                (env, (ModuleType mt')::items)
            | _ -> failwith "Unhandled signature element") s (env, [])
    in items'

and module_ : Env.t -> Odoc_model.Lang.Module.t -> Odoc_model.Lang.Module.t = fun env m ->
    let open Odoc_model.Lang.Module in
    let env' = Env.add_functor_args (m.id :> Odoc_model.Paths.Identifier.Signature.t) env in
    {m with type_ = module_decl env' m.type_}

and module_decl : Env.t -> Odoc_model.Lang.Module.decl -> Odoc_model.Lang.Module.decl = fun env decl ->
    let open Odoc_model.Lang.Module in
    match decl with
    | ModuleType expr -> ModuleType (module_type_expr env expr)
    | Alias p ->
        match Tools.lookup_module_from_model_path env p with
        | Ok (_, p', _) -> Alias (`Resolved p')
        | _ -> decl

and module_type : Env.t -> Odoc_model.Lang.ModuleType.t -> Odoc_model.Lang.ModuleType.t = fun env m ->
    let open Odoc_model.Lang.ModuleType in
    let env' = Env.add_functor_args (m.id :> Odoc_model.Paths.Identifier.Signature.t) env in
    let expr' = match m.expr with | None -> None | Some expr -> Some (module_type_expr env' expr) in
    {m with expr = expr'}

and functor_argument : Env.t -> Odoc_model.Lang.FunctorArgument.t -> Odoc_model.Lang.FunctorArgument.t = fun env a ->
    { a with expr = module_type_expr env a.expr }

and module_type_expr : Env.t -> Odoc_model.Lang.ModuleType.expr -> Odoc_model.Lang.ModuleType.expr = fun env expr ->
    let open Odoc_model.Lang.ModuleType in
    match expr with
    | Signature s -> Signature (signature env s)
    | Path p -> Path (module_type_path env p)
    | With (expr, subs) ->
        With (module_type_expr env expr,
            List.map (function
                | ModuleEq (frag, decl) -> ModuleEq (frag, module_decl env decl)
                | TypeEq (frag, eqn) -> TypeEq (frag, type_decl_equation env eqn)
                | ModuleSubst (frag, mpath) -> ModuleSubst (frag, module_path env mpath)
                | TypeSubst (frag, eqn) -> TypeSubst (frag, type_decl_equation env eqn)
                ) subs)
    | Functor (arg, res) ->
        let arg' = Opt.map (functor_argument env) arg in
        let res' = module_type_expr env res in
        Functor (arg', res')
    | _ -> failwith "boo"

and type_ env t =
    let open Odoc_model.Lang.TypeDecl in
    match t.equation.manifest with
    | Some texpr ->
        let texpr' = type_expression env texpr in
        {t with equation = {t.equation with manifest = Some texpr'}}
    | None -> t

and type_decl_equation env t =
    let open Odoc_model.Lang.TypeDecl.Equation in
    let manifest = Opt.map (type_expression env) t.manifest in
    let constraints = List.map (fun (tex1, tex2) ->
        (type_expression env tex1, type_expression env tex2)) t.constraints in
    {t with manifest; constraints}

and type_expression : Env.t -> _ -> _ = fun env texpr ->
    let open Odoc_model.Lang.TypeExpr in 
    match texpr with
    | Constr (path, ts) -> begin
        match Tools.lookup_type_from_model_path env path with
        | Ok (p, _) ->  Constr (`Resolved p, ts)
        | Error p -> Constr (p, ts)
        end
    | _ -> failwith "Unhandled type expression"

and compilation_unit : Env.t -> Odoc_odoc.Compilation_unit.t -> Odoc_odoc.Compilation_unit.t = fun env c ->
    let open Odoc_model.Lang.Compilation_unit in
    let content' = 
        match c.content with
        | Module s -> Module (signature env s)
        | Pack _ -> failwith "Unhandled"
    in
    { c with
      content = content' }
