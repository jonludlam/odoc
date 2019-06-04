type t = {
    map : (Ident.t * [ `Local of Ident.t | `Global of Model.Paths.Path.t ]) list
}

let identity = {
    map = []
}

let add id subst t =
    { map = (id, subst) :: t.map }

let rec path : t -> Cpath.t -> Cpath.t = fun s p ->
    match p with
    | `Local id -> begin
        match List.assoc_opt id s.map with
        | Some (`Local id') ->
            `Local id'
        | Some (`Global path) ->
            `Global path
        | None ->
            `Local id
        end
    | `Global _ ->
        p
    | `Ldot (parent,x) -> `Ldot (path s parent, x)

let rec type_ s t = 
    match t with
    | Some t' -> Some (type_expr s t')
    | None -> None

and type_expr s t =
    let open Component.TypeExpr in
    match t with
    | Var s -> Var s
    | Constr (p, ts) -> Constr (path s p, List.map (type_expr s) ts)

and module_type s t =
    match t with
    | Some m -> Some (module_type_expr s m)
    | None -> None

and module_type_expr s t =
    let open Component.ModuleType in
    match t with
    | Path p ->
        Path (path s p)
    | Signature sg ->
        Signature (signature s sg)
    | With (_,_) -> failwith "Unahdlalell"

and module_ s t =
    let open Component.Module in
    let type_ = match t.type_ with
        | Alias p ->
            Alias (path s p)
        | ModuleType t ->
            ModuleType (module_type_expr s t)
    in
    { type_ }

and rename_bound_idents s sg =
    let open Component.Signature in
    function
    | [] -> s, sg
    | Module (id, m) :: rest ->
        let id' = Ident.rename id in
        rename_bound_idents
            (add id (`Local id') s)
            (Module (id', m) :: sg)
            rest
    | ModuleType (id, t) :: rest ->
        let id' = Ident.rename id in
        rename_bound_idents
            (add id (`Local id') s)
            (ModuleType (id', t) :: sg)
            rest
    | Type (id, t) :: rest ->
        let id' = Ident.rename id in
        rename_bound_idents
            (add id (`Local id') s)
            (Type (id', t) :: sg)
            rest

and signature s sg =
    let open Component.Signature in
    let s, sg = rename_bound_idents s [] sg in
    List.rev_map (function
        | Module (id, m) -> Module (id, module_ s m)
        | ModuleType (id, m) -> ModuleType (id, module_type s m)
        | Type (id, t) -> Type (id, type_ s t)) sg
