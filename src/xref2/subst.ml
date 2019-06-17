type t = {
    map : (Ident.t * [ `Local of Ident.t | `Global of Odoc_model.Paths.Path.t ]) list
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
