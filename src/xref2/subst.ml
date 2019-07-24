 type t = {
    map : (Ident.t * Cpath.resolved) list
}

let identity = {
    map = []
}

let add id subst t =
    { map = (id, subst) :: t.map }

let rec resolved_path : t -> Cpath.resolved -> Cpath.resolved = fun s p ->
    match p with
    | `Local id -> begin
        match List.assoc_opt id s.map with
        | Some x ->
            x
        | None -> `Local id
        end
    | `Identifier _ ->
        p
    | `Apply (p1, p2) -> `Apply (resolved_path s p1, path s p2)
    | `Substituted p -> `Substituted (resolved_path s p)
    | `Module (p, n) -> `Module (resolved_path s p, n)
    | `ModuleType (p, n) -> `ModuleType (resolved_path s p, n)
    | `Type (p, n) -> `Type (resolved_path s p, n)
    | `Alias (p1, p2) -> `Alias (resolved_path s p1, resolved_path s p2)
    | `Subst (p1, p2) -> `Subst (resolved_path s p1, resolved_path s p2)
    | `SubstAlias (p1, p2) -> `SubstAlias (resolved_path s p1, resolved_path s p2)
    | `Hidden (p1) -> `Hidden (resolved_path s p1)
    | `Canonical (p1, p2) -> `Canonical (resolved_path s p1, path s p2)

and path : t -> Cpath.t -> Cpath.t = fun s p ->
    match p with
    | `Resolved p' -> `Resolved (resolved_path s p')
    | `Dot (p', str) -> `Dot (path s p', str)
    | `Apply (p1, p2) -> `Apply (path s p1, path s p2)
    | `Substituted p -> `Substituted (path s p)
    | `Forward s -> `Forward s

let rec type_ s t =
    let open Component.Type in
    let manifest = match t.manifest with
        | Some t' -> Some (type_expr s t')
        | None -> None
    in
    { t with manifest }

and type_expr s t =
    let open Component.TypeExpr in
    match t with
    | Var s -> Var s
    | Constr (p, ts) -> Constr (path s p, List.map (type_expr s) ts)

and module_type s t =
    let open Component.ModuleType in
    let expr =
        match t.expr with
        | Some m -> Some (module_type_expr s m)
        | None -> None
    in
    {t with expr}

and functor_argument_opt s t =
    let open Component.FunctorArgument in
    match t with
    | Some arg ->
        Some {arg with expr = module_type_expr s arg.expr}
    | None ->
        None

and module_type_expr s t =
    let open Component.ModuleType in
    match t with
    | Path p ->
        Path (path s p)
    | Signature sg ->
        Signature (signature s sg)
    | Functor (arg, expr) ->
        Functor (functor_argument_opt s arg, module_type_expr s expr)
    | With (e,args) ->
        With (module_type_expr s e,args)
    | TypeOf decl ->
        TypeOf (module_decl s decl)

and module_decl s t =
    match t with
    | Alias p ->
        Alias (path s p)
    | ModuleType t ->
        ModuleType (module_type_expr s t)

and module_ s t =
    let open Component.Module in
    let type_ = module_decl s t.type_ in
    { t with type_ }

and rename_bound_idents s sg =
    let open Component.Signature in
    function
    | [] -> s, sg
    | Module m :: rest ->
        let id' = Ident.rename m.id in
        rename_bound_idents
            (add m.id (`Local id') s)
            (Module {m with id=id'} :: sg)
            rest
    | ModuleType mt :: rest ->
        let id' = Ident.rename mt.id in
        rename_bound_idents
            (add mt.id (`Local id') s)
            (ModuleType {mt with id=id'} :: sg)
            rest
    | Type t :: rest ->
        let id' = Ident.rename t.id in
        rename_bound_idents
            (add t.id (`Local id') s)
            (Type {t with id=id'} :: sg)
            rest

and signature s sg =
    let open Component.Signature in
    let s, sg = rename_bound_idents s [] sg in
    List.rev_map (function
        | Module m -> Module (module_ s m)
        | ModuleType mt -> ModuleType (module_type s mt)
        | Type t -> Type (type_ s t)) sg
