(* Note for future improvement (suggested by @lpw25):

You can actually do something even more interesting with that case for strengthening.

If you have:
[module type S = sig type t end]

and you want to strengthen [S] with the module path [M] then you can produce:
[S with type t = M.t]

The compiler doesn't do this because it doesn't actually have a representation for `with` in its type algebra
(`with` is always just expanded away right after paprsing). But since we do have a representation for it, 
this is probably the best thing to produce in this case.

*)


let rec signature (prefix : Odoc_model.Paths.Path.Resolved.Module.t) sg =
    let open Component.Signature in
    let open Odoc_model.Names in
    let sg' = List.map (fun item ->
        match item with
        | Module m -> Module (module_ (`Module (prefix, ModuleName.of_string (Ident.name m.id))) m)
        | ModuleType mt -> ModuleType (module_type (`ModuleType (prefix, ModuleTypeName.of_string (Ident.name mt.id))) mt)
        | Type t -> Type (type_ (`Type (prefix, TypeName.of_string (Ident.name t.id))) t))
        sg in
    (* The identity substitution used here is to rename all of the bound idents in the signature *)
    Subst.signature Subst.identity sg' 

and module_ (prefix : Odoc_model.Paths.Path.Resolved.Module.t) m =
    match m.Component.Module.type_ with
    | Alias _ -> m
    | ModuleType (Signature sg) -> {m with  type_ = ModuleType (Signature (signature prefix sg)) }
    | ModuleType _ -> m

and module_type (prefix: Odoc_model.Paths.Path.Resolved.ModuleType.t) m =
    let open Component.ModuleType in
    let expr =
        match m.expr with
        | None -> Some (Component.ModuleType.Path (`Global ((`Resolved (prefix :> Odoc_model.Paths.Path.Resolved.t)))))
        | Some (Component.ModuleType.Path _p) ->
            (* TODO *)
            m.expr
        | Some (Component.ModuleType.With (_,_)) ->
            (* TODO *)
            m.expr
        | Some (Component.ModuleType.Signature _) ->
            m.expr
    in {m with expr}

and type_ (path: Odoc_model.Paths.Path.Resolved.Type.t) t =
    let manifest = 
        match t.manifest with
        | None -> Some (Component.TypeExpr.Constr (`Global (`Resolved (path :> Odoc_model.Paths.Path.Resolved.t)), []))
        | _ -> t.manifest
    in
    {t with manifest}
