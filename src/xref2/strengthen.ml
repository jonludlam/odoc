let rec signature (prefix : Model.Paths.Path.Resolved.Module.t) sg =
    let open Component.Signature in
    let open Model.Names in
    let sg' = List.map (fun item ->
        match item with
        | Module (id, m) -> Module (id, module_ (`Module (prefix, ModuleName.of_string (Ident.name id))) m)
        | ModuleType (id, m) -> ModuleType (id, module_type (`ModuleType (prefix, ModuleTypeName.of_string (Ident.name id))) m)
        | Type (id, m) -> Type (id, type_ (`Type (prefix, TypeName.of_string (Ident.name id))) m))
        sg in
    (* The identity substitution used here is to rename all of the bound idents in the signature *)
    Subst.signature Subst.identity sg' 

and module_ (prefix : Model.Paths.Path.Resolved.Module.t) m =
    match m.Component.Module.type_ with
    | Alias _ -> m
    | ModuleType (Signature sg) -> { type_ = ModuleType (Signature (signature prefix sg)) }
    | ModuleType _ -> m

and module_type (prefix: Model.Paths.Path.Resolved.ModuleType.t) m =
    match m with
    | None -> Some (Component.ModuleType.Path (`Global ((`Resolved (prefix :> Model.Paths.Path.Resolved.t)))))
    | _ -> m

and type_ (path: Model.Paths.Path.Resolved.Type.t) t =
    match t with
    | None -> Some (Component.TypeExpr.Constr (`Global (`Resolved (path :> Model.Paths.Path.Resolved.t)), []))
    | _ -> t
