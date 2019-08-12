type resolver =
    { lookup_unit: string -> Odoc_xref.lookup_result
    ; resolve_unit: Odoc_model.Root.t -> Odoc_model.Lang.Compilation_unit.t
    ; lookup_page: string -> Odoc_model.Root.t option
    ; resolve_page: Odoc_model.Root.t -> Odoc_model.Lang.Page.t }

module Opt = struct
    let map f = function | Some x -> Some (f x) | None -> None
end

let type_path : Env.t -> Odoc_model.Paths.Path.Type.t -> Odoc_model.Paths.Path.Type.t = fun env p ->
    let cp = Component.Of_Lang.local_path_of_path [] (p :> Odoc_model.Paths.Path.t) in
    match Tools.lookup_type_from_path env cp with
    | Ok (p', _) -> `Resolved (Cpath.resolved_type_path_of_cpath p')
    | Error _ -> p

and module_type_path : Env.t -> Odoc_model.Paths.Path.ModuleType.t -> Odoc_model.Paths.Path.ModuleType.t = fun env p ->
    let cp = Component.Of_Lang.local_path_of_path [] (p :> Odoc_model.Paths.Path.t) in
    match Tools.lookup_and_resolve_module_type_from_path true env cp with
    | Ok (p', _) -> `Resolved (Cpath.resolved_module_type_path_of_cpath p')
    | Error _ -> p

and module_path : Env.t -> Odoc_model.Paths.Path.Module.t -> Odoc_model.Paths.Path.Module.t = fun env p ->
    let cp = Component.Of_Lang.local_path_of_path [] (p :> Odoc_model.Paths.Path.t) in
    match Tools.lookup_and_resolve_module_from_path true env cp with
    | Ok (p', _) -> `Resolved (Cpath.resolved_module_path_of_cpath p')
    | Error _ -> p

let rec unit resolver t =
    let open Odoc_model.Lang.Compilation_unit in
    let (imports, env) = List.fold_left (fun (imports,env) import ->
        match import with
        | Import.Resolved root ->
            let unit = resolver.resolve_unit root in
            let env = Env.open_unit unit env in
            (import::imports, env)
        | Import.Unresolved (str, _) ->
            match resolver.lookup_unit str with
            | Odoc_xref.Forward_reference -> (import::imports, env)
            | Found f ->
                let unit = resolver.resolve_unit f.root in
                let env = Env.open_unit unit env in
                ((Resolved f.root)::imports, env)
            | Not_found ->
                Printf.fprintf stderr "Failed to lookup import %s in Resolve.unit\n%!" str;
                (import::imports,env)
    ) ([],Env.empty) t.imports in
    {t with content = content env t.content; imports}

and content env =
    let open Odoc_model.Lang.Compilation_unit in
    function
    | Module m -> Module (signature env m)
    | Pack _ -> failwith "Unhandled content"

and value_ env t =
    let open Odoc_model.Lang.Value in
    { t with type_ = type_expression env t.type_}

and comment _env x = x

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
            | Value v ->
                let v' = value_ env v in
                (env, (Value v')::items)
            | Comment c ->
                let c' = comment env c in
                (env, (Comment c')::items)
            | _ -> failwith "Unhandled signature element") s (env, [])
    in items'

and module_ : Env.t -> Odoc_model.Lang.Module.t -> Odoc_model.Lang.Module.t = fun env m ->
    let open Odoc_model.Lang.Module in
    let env' = Env.add_functor_args (m.id :> Odoc_model.Paths.Identifier.Signature.t) env in
    {m with type_ = module_decl env' (m.id :> Odoc_model.Paths.Identifier.Signature.t) m.type_}

and module_decl : Env.t -> Odoc_model.Paths.Identifier.Signature.t -> Odoc_model.Lang.Module.decl -> Odoc_model.Lang.Module.decl = fun env id decl ->
    let open Odoc_model.Lang.Module in
    match decl with
    | ModuleType expr -> ModuleType (module_type_expr env id expr)
    | Alias p ->
        let cp = Component.Of_Lang.local_path_of_path [] (p :> Odoc_model.Paths.Path.t) in
        match Tools.lookup_and_resolve_module_from_path true env cp with
        | Ok (p', _) -> Alias (`Resolved (Cpath.resolved_module_path_of_cpath p'))
        | _ -> decl

and module_type : Env.t -> Odoc_model.Lang.ModuleType.t -> Odoc_model.Lang.ModuleType.t = fun env m ->
    let open Odoc_model.Lang.ModuleType in
    let env' = Env.add_functor_args (m.id :> Odoc_model.Paths.Identifier.Signature.t) env in
    let expr' = match m.expr with | None -> None | Some expr -> Some (module_type_expr env' (m.id :> Odoc_model.Paths.Identifier.Signature.t) expr) in
    {m with expr = expr'}

and functor_argument : Env.t -> Odoc_model.Lang.FunctorArgument.t -> Odoc_model.Lang.FunctorArgument.t = fun env a ->
    { a with expr = module_type_expr env (a.id :> Odoc_model.Paths.Identifier.Signature.t) a.expr }

and module_type_expr : Env.t -> Odoc_model.Paths.Identifier.Signature.t -> Odoc_model.Lang.ModuleType.expr -> Odoc_model.Lang.ModuleType.expr = fun env id expr ->
    let open Odoc_model.Lang.ModuleType in
    match expr with
    | Signature s -> Signature (signature env s)
    | Path p -> Path (module_type_path env p)
    | With (expr, subs) ->
        With (module_type_expr env id expr,
            List.map (function
                | ModuleEq (frag, decl) ->
                    let frag' = Tools.resolve_module_fragment env id frag in
                    ModuleEq (`Resolved frag', module_decl env id decl)
                | TypeEq (frag, eqn) ->
                    let frag' = Tools.resolve_type_fragment env id frag in
                    TypeEq (`Resolved frag', type_decl_equation env eqn)
                | ModuleSubst (frag, mpath) ->
                    let frag' = Tools.resolve_module_fragment env id frag in
                    ModuleSubst (`Resolved frag', module_path env mpath)
                | TypeSubst (frag, eqn) ->
                    let frag' = Tools.resolve_type_fragment env id frag in
                    TypeSubst (`Resolved frag', type_decl_equation env eqn)
                ) subs)
    | Functor (arg, res) ->
        let arg' = Opt.map (functor_argument env) arg in
        let res' = module_type_expr env id res in
        Functor (arg', res')
    | TypeOf decl ->
        TypeOf (module_decl env id decl)

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
        let cp = Component.Of_Lang.local_path_of_path [] (path :> Odoc_model.Paths.Path.t) in
        match Tools.lookup_type_from_path env cp with
        | Ok (p, _) ->
            Constr (`Resolved (Cpath.resolved_type_path_of_cpath p), ts)
        | Error _ -> Constr (path, ts)
        end
    | _ -> failwith "Unhandled type expression"

let build_resolver :
    ?equal:(Odoc_model.Root.t -> Odoc_model.Root.t -> bool) -> ?hash:(Odoc_model.Root.t -> int)
    -> (string -> Odoc_xref.lookup_result)
    -> (Odoc_model.Root.t -> Odoc_model.Lang.Compilation_unit.t)
    -> (string -> Odoc_model.Root.t option) -> (Odoc_model.Root.t -> Odoc_model.Lang.Page.t)
    -> resolver =
    fun ?equal:_ ?hash:_ lookup_unit resolve_unit lookup_page resolve_page ->
    {lookup_unit; resolve_unit; lookup_page; resolve_page; }

let resolve x y =
    let before = y in
    let after = unit x before in
    after

let resolve_page _ x = x