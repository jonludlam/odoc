open Odoc_model
open Lang

type resolver =
    { lookup_unit: string -> Tools.lookup_unit_result
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
    | exception _ -> p

and module_type_path : Env.t -> Paths.Path.ModuleType.t -> Paths.Path.ModuleType.t = fun env p ->
    let cp = Component.Of_Lang.local_path_of_path [] (p :> Paths.Path.t) in
    match Tools.lookup_and_resolve_module_type_from_path true env cp with
    | Ok (p', _) -> `Resolved (Cpath.resolved_module_type_path_of_cpath p')
    | Error _ -> p
    | exception _ -> p

and module_path : Env.t -> Paths.Path.Module.t -> Paths.Path.Module.t = fun env p ->
    let cp = Component.Of_Lang.local_path_of_path [] (p :> Paths.Path.t) in
    match Tools.lookup_and_resolve_module_from_path true true env cp with
    | Ok (p', _) -> `Resolved (Cpath.resolved_module_path_of_cpath p')
    | Error _ -> p
    | exception _ -> p

let rec unit resolver t =
    let open Tools in
    let open Compilation_unit in
    let (imports, env) = List.fold_left (fun (imports,env) import ->
        match import with
        | Import.Resolved root ->
            let unit = resolver.resolve_unit root in
            Printf.fprintf stderr "Import: found module %s\n%!" (Root.to_string root);
            let env = Env.add_unit unit env in
            let env = Env.add_root (Odoc_model.Root.Odoc_file.name root.Odoc_model.Root.file) unit.id env in
            (import::imports, env)
        | Import.Unresolved (str, _) ->
            match resolver.lookup_unit str with
            | Forward_reference ->
(*                Printf.fprintf stderr "Import: forward reference %s\n%!" str;*)
                (import::imports, env)
            | Found f ->
                let unit = resolver.resolve_unit f.root in
(*                Printf.fprintf stderr "Import: found module %s\n%!" (Root.to_string f.root);*)
                let env = Env.add_unit unit env in
                ((Resolved f.root)::imports, env)
            | Not_found ->
(*                Printf.fprintf stderr "Not found: %s\n%!" str;*)
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

and comment_inline_element : Env.t -> Comment.inline_element -> Comment.inline_element = fun env x ->
    match x with
    | `Styled (s, ls) -> `Styled (s, List.map (with_location comment_inline_element env) ls)
    | `Reference (r, []) -> begin
        match Ref_tools.resolve_reference env r with
        | `Resolved (`Identifier (#Odoc_model.Paths.Identifier.Label.t as i)) as r ->
            let content = match Env.lookup_section_title i env with
                | Some x -> x | None -> []
            in `Reference (r, content)
        | x -> `Reference (x, [])
        end 
    | `Reference (r,content) -> `Reference (Ref_tools.resolve_reference env r, content)
    | y -> y

and comment_nestable_block_element env (x : Comment.nestable_block_element) =
    match x with
    | `Paragraph elts -> `Paragraph (List.map (with_location comment_inline_element env) elts)
    | `Code_block _
    | `Verbatim _ as x -> x
    | `List (x, ys) -> `List (x, List.map (List.map (with_location comment_nestable_block_element env)) ys)
    | x -> x

and comment_block_element env (x: Comment.block_element) = 
    match x with
    | #Comment.nestable_block_element as x ->
        (comment_nestable_block_element env x :> Comment.block_element)
    | `Heading _ as x -> x
    | `Tag _ as x -> x

and with_location : type a. (Env.t -> a -> a) -> Env.t -> a Location_.with_location -> a Location_.with_location = fun fn env x ->
    { x with Location_.value = fn env x.Location_.value }

and comment env = function
    | `Stop -> `Stop
    | `Docs d -> `Docs (List.map (with_location comment_block_element env) d)

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

and class_type_expr env =
    let open ClassType in
    function
    | Constr (path, texps) -> Constr (path, List.map (type_expression env) texps)
    | Signature s -> Signature (class_signature env s)

and class_type env c =
    let open ClassType in
    { c with expr = class_type_expr env c.expr }

and class_signature env c =
    let open ClassSignature in
    let map_item = function
    | Method m -> Method (method_ env m)
    | InstanceVariable i -> InstanceVariable (instance_variable env i)
    | Constraint (t1, t2) -> Constraint (type_expression env t1, type_expression env t2)
    | Inherit c -> Inherit (class_type_expr env c)
    | Comment c -> Comment c
    in
    { self = Opt.map (type_expression env) c.self
    ; items = List.map map_item c.items}

and method_ env m =
    let open Method in
    { m with type_ = type_expression env m.type_}

and instance_variable env i =
    let open InstanceVariable in
    { i with type_ = type_expression env i.type_}

and class_ env c =
    let open Class in
    let rec map_decl = function
    | ClassType expr -> ClassType (class_type_expr env expr)
    | Arrow (lbl, expr, decl) -> Arrow (lbl, type_expression env expr, map_decl decl)
    in
    {c with type_ = map_decl c.type_}

and signature : Env.t -> Signature.t -> _ = fun env s ->
    let open Signature in
    let env = Env.open_signature s env in
    List.map (fun item ->
        match item with
        | Module (r, m) -> Module (r, module_ env m)
        | Type (r, t) -> Type (r, type_decl env t)
        | ModuleType mt -> ModuleType (module_type env mt)
        | Value v -> Value (value_ env v)
        | Comment c -> Comment (comment env c)
        | TypExt t -> TypExt (extension env t) 
        | Exception e -> Exception (exception_ env e)
        | External e -> External (external_ env e)
        | Class (r,c) -> Class (r, class_ env c)
        | ClassType (r,c) -> ClassType (r, class_type env c)
        | Include i -> Include (include_ env i)
        ) s

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
        match Tools.lookup_and_resolve_module_from_path true true env cp with
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
            List.map (fun x ->
                try
                    match x with 
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
                with _ -> x
                ) subs)
    | Functor (arg, res) ->
        let arg' = Opt.map (functor_argument env) arg in
        let res' = module_type_expr env id res in
        Functor (arg', res')
    | TypeOf decl ->
        TypeOf (module_decl env id decl)

and type_decl : Env.t -> TypeDecl.t -> TypeDecl.t = fun env t ->
    let open TypeDecl in
    let equation = type_decl_equation env t.equation in
    {t with equation}

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
    try 
    match texpr with
    | Var _
    | Any -> texpr
    | Alias (t, str) -> Alias (type_expression env t, str)
    | Arrow (lbl, t1, t2) -> Arrow (lbl, type_expression env t1, type_expression env t2)
    | Tuple ts -> Tuple (List.map (type_expression env) ts)
    | Constr (path, ts) -> begin
        Printf.printf "In here!!\n%!";
        let cp = Component.Of_Lang.local_path_of_path [] (path :> Paths.Path.t) in
        match Tools.lookup_type_from_path env cp with
        | Ok (p, _) ->
            Printf.printf "Ok!\n%!";
            Format.fprintf Format.std_formatter "%a\n%!" Component.Fmt.resolved_path p;
            Constr (`Resolved (Cpath.resolved_type_path_of_cpath p), ts)
        | Error _e ->
            Constr (path, ts)
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
    with e ->
        Format.fprintf Format.std_formatter "Gah! exception %s backtrace %s\n%!" (Printexc.to_string e) (Printexc.get_backtrace ());
        texpr

let build_resolver :
    ?equal:(Root.t -> Root.t -> bool) -> ?hash:(Root.t -> int)
    -> (string -> Tools.lookup_unit_result)
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