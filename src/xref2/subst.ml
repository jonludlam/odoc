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
    | `Class(p,n) -> `Class(resolved_path s p, n)
    | `ClassType(p, n) -> `ClassType(resolved_path s p, n)

and path : t -> Cpath.t -> Cpath.t = fun s p ->
    match p with
    | `Resolved p' -> `Resolved (resolved_path s p')
    | `Dot (p', str) -> `Dot (path s p', str)
    | `Apply (p1, p2) -> `Apply (path s p1, path s p2)
    | `Substituted p -> `Substituted (path s p)
    | `Forward _ -> p
    | `Root _ -> p

let option_ conv s x =
    match x with
    | Some x -> Some (conv s x)
    | None -> None

let list conv s xs =
    List.map (conv s) xs

let rec type_ s t =
    let open Component.TypeDecl in
    let manifest = match t.equation.Equation.manifest with
        | Some t' -> Some (type_expr s t')
        | None -> None
    in
    { t with equation = {t.equation with manifest} }

and type_poly_var s v =
    let open Component.TypeExpr.Polymorphic_variant in
    let map_constr c =
        let open Constructor in
        { name = c.name
        ; constant = c.constant
        ; arguments = List.map (type_expr s) c.arguments
        ; doc = c.doc }
    in
    let map_element = function
    | Type t -> Type (type_expr s t)
    | Constructor c -> Constructor (map_constr c)
    in 
    { kind = v.kind
    ; elements = List.map map_element v.elements}

and type_object s o =
    let open Component.TypeExpr.Object in
    let map_field = function
    | Method m -> Method {m with type_ = type_expr s m.type_}
    | Inherit t -> Inherit (type_expr s t)
    in
    { fields = List.map map_field o.fields
    ; open_ = o.open_ }

and type_package s p =
    let open Component.TypeExpr.Package in
    let sub (x,y) = (x, type_expr s y) in
    { path = path s p.path
    ; substitutions = List.map sub p.substitutions}

and type_expr s t =
    let open Component.TypeExpr in
    match t with
    | Var s -> Var s
    | Any -> Any
    | Alias (t,str) -> Alias (type_expr s t, str)
    | Arrow (lbl, t1, t2) -> Arrow (lbl, type_expr s t1, type_expr s t2)
    | Tuple ts -> Tuple (List.map (type_expr s) ts)
    | Constr (p, ts) -> Constr (path s p, List.map (type_expr s) ts)
    | Polymorphic_variant v -> Polymorphic_variant (type_poly_var s v)
    | Object o -> Object (type_object s o)
    | Class (p,ts) -> Class (p, List.map (type_expr s) ts)
    | Poly (strs, ts) -> Poly (strs, type_expr s ts)
    | Package p -> Package (type_package s p)

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

and type_decl_field s f =
    let open Component.TypeDecl.Field in
    { f with type_ = type_expr s f.type_}

and type_decl_constructor_arg s a =
    let open Component.TypeDecl.Constructor in
    match a with
    | Tuple ts -> Tuple (list type_expr s ts)
    | Record fs -> Record (list type_decl_field s fs)

and exception_ s e =
    let open Component.Exception in
    let res = option_ type_expr s e.res in
    let args = type_decl_constructor_arg s e.args in
    { e with args; res }

and extension_constructor s c =
    let open Component.Extension.Constructor in
    { c with args = type_decl_constructor_arg s c.args
    ; res = option_ type_expr s c.res }

and extension s e =
    let open Component.Extension in
    { e with
    type_path = path s e.type_path;
    constructors = List.map (extension_constructor s) e.constructors
    }

and external_ s e =
    let open Component.External in
    { e with
    type_ = type_expr s e.type_}

and include_ s i =
    let open Component.Include in
    { i with
    decl = module_decl s i.decl }

and value s v =
    let open Component.Value in
    { v with
    type_ = type_expr s v.type_ }

and rename_bound_idents s sg =
    let open Component.Signature in
    function
    | [] -> s, sg
    | Module (id,r,m) :: rest ->
        let id' = Ident.rename id in
        rename_bound_idents
            (add id (`Local id') s)
            (Module (id',r,m) :: sg)
            rest
    | ModuleType (id,mt) :: rest ->
        let id' = Ident.rename id in
        rename_bound_idents
            (add id (`Local id') s)
            (ModuleType (id',mt) :: sg)
            rest
    | Type (id,r,t) :: rest ->
        let id' = Ident.rename id in
        rename_bound_idents
            (add id (`Local id') s)
            (Type (id', r, t) :: sg)
            rest
    | Exception (id,e) :: rest ->
        let id' = Ident.rename id in
        rename_bound_idents
            (add id (`Local id') s)
            (Exception (id', e) :: sg)
            rest
    | Value (id,v) :: rest ->
        let id' = Ident.rename id in
        rename_bound_idents
            (add id (`Local id') s)
            (Value (id',v) :: sg)
            rest
    | External (id, e) :: rest ->
        let id' = Ident.rename id in
        rename_bound_idents
            (add id (`Local id') s)
            (External (id',e) :: sg)
            rest
    | item :: rest ->
        rename_bound_idents
            s (item :: sg) rest

and removed_items s items =
    let open Component.Signature in
    List.map (function
        | RModule (id, _) when List.mem_assoc id s.map ->
            RModule (id, Some (List.assoc id s.map))
        | RType (id, _) when List.mem_assoc id s.map ->
            RType (id, Some (List.assoc id s.map))
        | x -> x) items

and signature s sg =
    let open Component.Signature in
    let s, items = rename_bound_idents s [] sg.items in
    let items = List.rev_map (function
        | Module (id, r, m) -> Module (id, r, Component.Delayed.put (fun () -> module_ s (Component.Delayed.get m)))
        | ModuleType (id,mt) -> ModuleType (id, module_type s mt)
        | Type (id, r, t) -> Type (id, r, type_ s t)
        | Exception (id,e) -> Exception (id, exception_ s e)
        | TypExt e -> TypExt (extension s e)
        | Value (id,v) -> Value (id, value s v)
        | External (id, e) -> External (id, external_ s e)
        | Class (id, r, c) -> Class (id, r, c)
        | ClassType (id, r, c) -> ClassType (id, r, c)
        | Include i -> Include (include_ s i)
        | Comment c -> Comment c
        ) items in
    {items; removed = removed_items s sg.removed}
