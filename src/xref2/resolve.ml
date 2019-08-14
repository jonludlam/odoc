open Odoc_model
open Lang

type resolver =
    { lookup_unit: string -> Odoc_xref.lookup_result
    ; resolve_unit: Root.t -> Compilation_unit.t
    ; lookup_page: string -> Root.t option
    ; resolve_page: Root.t -> Page.t }

module Opt = struct
    let map f = function | Some x -> Some (f x) | None -> None
end

let type_path : Env.t -> Paths.Path.Type.t -> Paths.Path.Type.t = fun env p ->
    let cp = Component.Of_Lang.local_path_of_path [] (p :> Paths.Path.t) in
    match Tools.lookup_type_from_path env cp with
    | Ok (p', _) -> `Resolved (Cpath.resolved_type_path_of_cpath p')
    | Error _ -> p

and module_type_path : Env.t -> Paths.Path.ModuleType.t -> Paths.Path.ModuleType.t = fun env p ->
    let cp = Component.Of_Lang.local_path_of_path [] (p :> Paths.Path.t) in
    match Tools.lookup_and_resolve_module_type_from_path true env cp with
    | Ok (p', _) -> `Resolved (Cpath.resolved_module_type_path_of_cpath p')
    | Error _ -> p

and module_path : Env.t -> Paths.Path.Module.t -> Paths.Path.Module.t = fun env p ->
    let cp = Component.Of_Lang.local_path_of_path [] (p :> Paths.Path.t) in
    match Tools.lookup_and_resolve_module_from_path true env cp with
    | Ok (p', _) -> `Resolved (Cpath.resolved_module_path_of_cpath p')
    | Error _ -> p

let rec unit resolver t =
    let open Compilation_unit in
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
    let open Compilation_unit in
    function
    | Module m -> Module (signature env m)
    | Pack _ -> failwith "Unhandled content"

and value_ env t =
    let open Value in
    { t with type_ = type_expression env t.type_}

and comment _env x = x

and exception_ env e =
    let open Exception in
    let res = Opt.map (type_expression env) e.res in
    let args = type_decl_constructor_argument env e.args in
    { e with res; args }

and extension env t =
    let open Extension in
    let constructor c =
        let open Constructor in
        { c with args = type_decl_constructor_argument env c.args
        ; res = Opt.map (type_expression env) c.res }
    in
    let type_path = type_path env t.type_path in
    let constructors = List.map constructor t.constructors in
    { t with type_path; constructors }

and external_ env e =
    let open External in
    {e with type_ = type_expression env e.type_}

and signature : Env.t -> Signature.t -> _ = fun env s ->
    let open Signature in
    let env = Env.open_signature s env in
    let (_, items') = 
        List.fold_right (fun item (env, items) ->
            match item with
            | Module (r, m) ->
                let m' = module_ env m in
                (env, (Module (r, m'))::items)
            | Type (r, t) ->
                let t' = type_decl env t in
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
            | TypExt t ->
                let t' = extension env t in
                (env, (TypExt t')::items) 
            | Exception e ->
                let e' = exception_ env e in
                (env, (Exception e')::items)
            | External e ->
                let e' = external_ env e in
                (env, (External e')::items)
            | Class _ -> failwith "Unhandled signature element class"
            | ClassType _ -> failwith "Unhandled signature element classtype"
            | Include i ->
                let i' = include_ env i in
                (env, (Include i')::items)
            ) s (env, [])
    in items'

and module_ : Env.t -> Module.t -> Module.t = fun env m ->
    let open Module in
    let env' = Env.add_functor_args (m.id :> Paths.Identifier.Signature.t) env in
    {m with type_ = module_decl env' (m.id :> Paths.Identifier.Signature.t) m.type_}

and module_decl : Env.t -> Paths.Identifier.Signature.t -> Module.decl -> Module.decl = fun env id decl ->
    let open Module in
    match decl with
    | ModuleType expr -> ModuleType (module_type_expr env id expr)
    | Alias p ->
        let cp = Component.Of_Lang.local_path_of_path [] (p :> Paths.Path.t) in
        match Tools.lookup_and_resolve_module_from_path true env cp with
        | Ok (p', _) -> Alias (`Resolved (Cpath.resolved_module_path_of_cpath p'))
        | _ -> decl

and module_type : Env.t -> ModuleType.t -> ModuleType.t = fun env m ->
    let open ModuleType in
    let env' = Env.add_functor_args (m.id :> Paths.Identifier.Signature.t) env in
    let expr' = match m.expr with | None -> None | Some expr -> Some (module_type_expr env' (m.id :> Paths.Identifier.Signature.t) expr) in
    {m with expr = expr'}

and include_ : Env.t -> Include.t -> Include.t = fun env i ->
    let open Include in
    {i with decl = module_decl env i.parent i.decl}

and functor_argument : Env.t -> FunctorArgument.t -> FunctorArgument.t = fun env a ->
    { a with expr = module_type_expr env (a.id :> Paths.Identifier.Signature.t) a.expr }

and module_type_expr : Env.t -> Paths.Identifier.Signature.t -> ModuleType.expr -> ModuleType.expr = fun env id expr ->
    let open ModuleType in
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

and type_decl : Env.t -> TypeDecl.t -> TypeDecl.t = fun env t ->
    let open TypeDecl in
    match t.equation.Equation.manifest with
    | Some texpr ->
        let texpr' = type_expression env texpr in
        {t with equation = {t.equation with manifest = Some texpr'}}
    | None -> t

and type_decl_equation env t =
    let open TypeDecl.Equation in
    let manifest = Opt.map (type_expression env) t.manifest in
    let constraints = List.map (fun (tex1, tex2) ->
        (type_expression env tex1, type_expression env tex2)) t.constraints in
    {t with manifest; constraints}

and type_decl_field env f =
    let open TypeDecl.Field in
    { f with type_ = type_expression env f.type_ }

and type_decl_constructor_argument env c =
    let open TypeDecl.Constructor in
    match c with
    | Tuple ts -> Tuple (List.map (type_expression env) ts)
    | Record fs -> Record (List.map (type_decl_field env) fs)

and type_expression_polyvar env v =
    let open TypeExpr.Polymorphic_variant in
    let constructor c =
        let open Constructor in
        {c with arguments = List.map (type_expression env) c.arguments}
    in
    let element = function
    | Type t -> Type (type_expression env t)
    | Constructor c -> Constructor (constructor c)
    in
    { v with elements = List.map element v.elements }

and type_expression_object env o =
    let open TypeExpr.Object in
    let method_ m =
        {m with type_ = type_expression env m.type_}
    in
    let field = function
    | Method m -> Method (method_ m)
    | Inherit t -> Inherit (type_expression env t)
    in
    { o with fields = List.map field o.fields }

and type_expression_package env p =
    let open TypeExpr.Package in
    let cp = Component.Of_Lang.local_path_of_path [] (p.path :> Paths.Path.t) in
    match Tools.lookup_and_resolve_module_type_from_path true env cp with
    | Ok (path, _) ->
        let path = Cpath.resolved_module_type_path_of_cpath path in
        let identifier = Paths.Path.Resolved.ModuleType.identifier path in
        let substitution (frag, t) =
            let frag' = Tools.resolve_type_fragment env (identifier :> Paths.Identifier.Signature.t) frag in
            (`Resolved frag', type_expression env t) in
        { path = module_type_path env p.path
        ; substitutions = List.map substitution p.substitutions }
    | Error _ -> p

and type_expression : Env.t -> _ -> _ = fun env texpr ->
    let open TypeExpr in 
    match texpr with
    | Var _
    | Any -> texpr
    | Alias (t, str) -> Alias (type_expression env t, str)
    | Arrow (lbl, t1, t2) -> Arrow (lbl, type_expression env t1, type_expression env t2)
    | Tuple ts -> Tuple (List.map (type_expression env) ts)
    | Constr (path, ts) -> begin
        let cp = Component.Of_Lang.local_path_of_path [] (path :> Paths.Path.t) in
        match Tools.lookup_type_from_path env cp with
        | Ok (p, _) ->
            Constr (`Resolved (Cpath.resolved_type_path_of_cpath p), ts)
        | Error _ -> Constr (path, ts)
        end
    | Polymorphic_variant v ->
        Polymorphic_variant (type_expression_polyvar env v)
    | Object o ->
        Object (type_expression_object env o)
    | Class (path, ts) ->
        Class (path, List.map (type_expression env) ts)
    | Poly (strs, t) -> Poly (strs, type_expression env t)
    | Package p ->
        Package (type_expression_package env p)

let build_resolver :
    ?equal:(Root.t -> Root.t -> bool) -> ?hash:(Root.t -> int)
    -> (string -> Odoc_xref.lookup_result)
    -> (Root.t -> Compilation_unit.t)
    -> (string -> Root.t option) -> (Root.t -> Page.t)
    -> resolver =
    fun ?equal:_ ?hash:_ lookup_unit resolve_unit lookup_page resolve_page ->
    {lookup_unit; resolve_unit; lookup_page; resolve_page; }

let resolve x y =
    let before = y in
    let after = unit x before in
    after

let resolve_page _ x = x