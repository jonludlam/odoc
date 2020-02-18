type expansion =
  | Signature of Component.Signature.t
  | Functor of Component.FunctorArgument.t option * Component.ModuleType.expr

let rec aux_expansion_of_module : Env.t -> Component.Module.t -> expansion =
  let open Component.Module in
  fun env m -> aux_expansion_of_module_decl env m.type_

and aux_expansion_of_module_decl env ty =
  let open Component.Module in
  match ty with
  | Alias path -> aux_expansion_of_module_alias env path
  | ModuleType expr -> aux_expansion_of_module_type_expr env expr

and aux_expansion_of_module_alias env path =
  match Tools.lookup_and_resolve_module_from_path false false env path with
  | Resolved (p, m) -> (
      match aux_expansion_of_module env m, m.doc with
      | Signature sg, [] -> Signature (Strengthen.signature p (aux_expand_includes env sg))
      | Signature sg, docs ->
        let sg = Strengthen.signature p (aux_expand_includes env sg) in
        Signature {sg with items = Comment (`Docs docs) :: sg.items}
      | Functor _ as x, _ -> x )
  | Unresolved p ->
      let err =
        Format.asprintf "Failed to lookup alias module (path=%a) (res=%a)"
          Component.Fmt.module_path path Component.Fmt.module_path p
      in
      failwith err

and aux_expansion_of_module_type_expr env expr : expansion =
  let open Component.ModuleType in
  match expr with
  | Path p -> (
      match Tools.lookup_and_resolve_module_type_from_path false env p with
      | Resolved (_, mt) -> aux_expansion_of_module_type env mt
      | Unresolved p ->
          let p = Component.Fmt.(string_of module_type_path p) in
          failwith (Printf.sprintf "Couldn't find signature: %s" p)
      | exception e ->
        Format.fprintf Format.err_formatter "Failure while looking up path: %a\n%!" Component.Fmt.module_type_path p;
        raise e)
  | Signature s -> Signature (aux_expand_includes env s)
  | With (s, subs) -> (
      let expn = aux_expansion_of_module_type_expr env s in
      match expn with
      | Functor _ -> failwith "This shouldn't be possible!"
      | Signature sg ->
          let sg = Tools.handle_signature_with_subs env sg subs in
          Signature (aux_expand_includes env sg))
  | Functor (arg, expr) -> Functor (arg, expr)
  | TypeOf decl -> aux_expansion_of_module_decl env decl

and aux_expansion_of_module_type env mt =
  let open Component.ModuleType in
  match mt.expr with
  | None -> raise Tools.OpaqueModule
  | Some expr -> aux_expansion_of_module_type_expr env expr

and aux_mk_ident_map sg =
  let open Component.Signature in
  let rec inner (ms, mts, ts) items =
    List.fold_right (fun item (ms, mts, ts) ->
      match item with
      | Module (id, _, _) -> ((Ident.Name.module_ id, id) :: ms, mts, ts)
      | ModuleType (id,_) -> (ms, (Ident.Name.module_type id, id) :: mts, ts)
      | Type (id, _, _) -> (ms, mts, (Ident.Name.type_ id, id) :: ts)
      | Include i -> inner (ms, mts, ts) i.expansion_.items
      | _ -> (ms, mts, ts)) items (ms, mts, ts)
  in
  inner ([],[],[]) sg.items

and aux_expand_includes env sg =
  let open Component.Signature in
  let (ms,mts,ts) = aux_mk_ident_map sg in
  let (items', subst) = List.fold_right (fun item (items, subst) ->
    match item with
    | Include i ->
      let aux_expansion = aux_expansion_of_module_decl env i.decl in
      begin match aux_expansion with
      | Functor _ -> failwith "This can't happen"
      | Signature sg' ->
        let sg'' = aux_expand_includes env sg' in
        (* Now we have to map the identifiers back to the originals *)
        let (items',subst) = List.fold_right (fun item (items,subst) ->
          match item with
          | Module (id, r, m) ->
            let id' = List.assoc (Ident.Name.module_ id) ms in
            let subst = Subst.add_module id (`Local id') subst in
            (Module (id', r, m)::items, subst)
          | ModuleType (id, mt) ->
            let id' = List.assoc (Ident.Name.module_type id) mts in
            let subst = Subst.add_module_type id (`Local id') subst in
            (ModuleType (id', mt)::items, subst)
          | Type (id, r, t) ->
            let id' = List.assoc (Ident.Name.type_ id) ts in
            Format.fprintf Format.err_formatter "subst: adding type %a -> %a\n%!" (Ident.fmt) id Ident.fmt (List.assoc (Ident.Name.type_ id) ts);
            let subst = Subst.add_type id (`Local (id' :> Ident.path_type)) subst in
            (Type (id', r, t) :: items, subst)
          | _ -> (item::items,subst)) sg'.items ([],subst) in
        (Include {i with expansion_ = {sg'' with items=items'}} :: items, subst)
      end
    | x -> (x::items, subst)) sg.items ([],Subst.identity) in
  Format.fprintf Format.err_formatter "Original:\n%!%a\n%!" Component.Fmt.signature sg;
  let result = Subst.apply_sig_map subst items' sg.removed in
  Format.fprintf Format.err_formatter "Final output: \n%!%a\n%!" Component.Fmt.signature result;
  result


and handle_expansion env id expansion =
  let handle_argument parent arg_opt expr env =
    (* If there's an argument, extend the environment with the argument, then
       do the substitution on the signature to replace the local identifier with
       the global one *)
    match arg_opt with
    | None -> (env, expr)
    | Some arg ->
        let identifier =
          `Parameter
            ( parent,
              Odoc_model.Names.ParameterName.of_string
                (Ident.Name.module_ arg.Component.FunctorArgument.id) )
        in
        let env' =
          Env.add_module identifier
            (Component.module_of_functor_argument arg)
            env
        in
        let subst =
          Subst.add_module arg.id (`Identifier identifier) Subst.identity
        in
        (env', Subst.module_type_expr subst expr)
  in
  let rec expand id env args expansion =
    match expansion with
    | Signature sg -> (
        match args with
        | [] -> (env, Component.Module.Signature sg)
        | args -> (env, Component.Module.Functor (args, sg)) )
    | Functor (arg, expr) ->
        let env', expr' = handle_argument id arg expr env in
        expand (`Result id) env' (arg :: args)
          (aux_expansion_of_module_type_expr env expr')
  in
  let env, e = expand id env [] expansion in
  begin match e with
  | Component.Module.Signature sg ->
    Format.fprintf Format.err_formatter "handle_expansion: module is\n%!%a\n%!"
    Component.Fmt.signature sg
  | _ -> Format.fprintf Format.err_formatter "handl_expansion: module is not a signature\n%!"
  end;
  (env, Lang_of.(module_expansion empty id e))

let expansion_of_module_type env id m =
  let open Odoc_model.Paths.Identifier in
  aux_expansion_of_module_type env m
  |> handle_expansion env (id : ModuleType.t :> Signature.t)

let expansion_of_module_type_expr env id expr =
  aux_expansion_of_module_type_expr env expr |> handle_expansion env id

let expansion_of_module env id m =
  let open Odoc_model.Paths.Identifier in
  aux_expansion_of_module env m
  |> handle_expansion env (id : Module.t :> Signature.t)
