open Odoc_model
open Lang

let rec unit (resolver : Env.resolver) forward_references t =
  let open Compilation_unit in
  let initial_env =
    let m = Env.module_of_unit t in
    Env.empty |> Env.add_module t.id m
    |> Env.add_root (Paths.Identifier.name t.id) (Env.Resolved (t.id, m))
  in
  let initial_env =
    if forward_references 
    then Env.set_resolver initial_env resolver
    else initial_env
  in
  let rec handle_import (imports, env) import =
    if List.exists (fun i -> i = import) imports then (imports, env)
    else
      match import with
      | Import.Resolved root ->
          let unit = resolver.resolve_unit root in
          let m = Env.module_of_unit unit in
          let env = Env.add_module unit.id m env in
          let env =
            Env.add_root
              (Odoc_model.Root.Odoc_file.name root.Odoc_model.Root.file)
              (Env.Resolved (unit.id, m))
              env
          in
          List.fold_left handle_import (import :: imports, env) unit.imports
      | Import.Unresolved (str, _) -> (
          match resolver.lookup_unit str with
          | Forward_reference ->
              let env = Env.add_root str Env.Forward env in
              (import :: imports, env)
          | Found f ->
              let unit = resolver.resolve_unit f.root in
              let m = Env.module_of_unit unit in
              let env = Env.add_module unit.id m env in
              let env =
                Env.add_root
                  (Odoc_model.Root.Odoc_file.name f.root.Odoc_model.Root.file)
                  (Env.Resolved (unit.id, m))
                  env
              in
              List.fold_left handle_import (import :: imports, env) unit.imports
          | Not_found ->
              (* Format.fprintf Format.err_formatter "Can't find: %s\n%!" str ;*)
              (import :: imports, env) )
  in
  let imports, env = List.fold_left handle_import ([], initial_env) t.imports in
  { t with content = content env t.content; imports }

and content env =
  let open Compilation_unit in
  function
  | Module m -> Module (signature env m)
  | Pack _ -> failwith "Unhandled content"

and include_ : Env.t -> Include.t -> Include.t =
 fun env i ->
  let open Include in
  let expansion =
    { i.expansion with content = signature env i.expansion.content }
  in
  { i with expansion }

and signature : Env.t -> Signature.t -> _ =
 fun env s ->
  let open Signature in
  let env = Env.open_signature s env in
  let _, items' =
    List.fold_left
      (fun (env, items) item ->
        match item with
        | Module (r, m) ->
            let env' =
              Env.add_functor_args (m.id :> Paths.Identifier.Signature.t) env
            in
            let m' = module_ env' m in
            (env, Module (r, m') :: items)
        | ModuleType mt ->
            (*Format.fprintf Format.err_formatter "Expanding module_type %a\n%!" (Component.Fmt.model_identifier) (mt.id :> Paths.Identifier.t);*)
            let env' =
              Env.add_functor_args (mt.id :> Paths.Identifier.Signature.t) env
            in
            let mt' = module_type env' mt in
            (env, ModuleType mt' :: items)
        | Class (r, c) ->
            let c' = class_ env c in
            (env, Class (r, c') :: items)
        | ClassType (r, c) ->
            let c' = class_type env c in
            (env, ClassType (r, c') :: items)
        | Include i ->
            let i' = include_ env i in
            (env, Include i' :: items)
        | x -> (env, x :: items))
      (env, []) s
  in
  List.rev items'

and expansion_of_module_type_expr (id : Paths_types.Identifier.signature) env
    expr =
  let rec get_env lenv parent :
      Component.ModuleType.expr -> Lang_of.maps * FunctorArgument.t option list
      = function
    | Functor (Some arg, expr) ->
        let identifier =
          `Parameter
            ( parent,
              Odoc_model.Names.ParameterName.of_string
                (Ident.Name.module_ arg.id) )
        in
        let lenv' =
          {
            lenv with
            Lang_of.module_ = (arg.id, identifier) :: lenv.Lang_of.module_;
          }
        in
        let subst = Subst.add_module arg.id (`Identifier identifier) Subst.identity in
        let lenv, args = get_env lenv' (`Result parent) (Subst.module_type_expr subst expr) in
        let arg_sg = Tools.signature_of_module_type_expr_nopath env arg.expr in
        let arg_sg = Lang_of.signature identifier lenv arg_sg in
        let arg_sg = signature env arg_sg in
        let arg_sg =
          if Env.has_resolver env
          then Resolve2.signature env arg_sg 
          else Resolve.signature env arg_sg in
        let lang_arg = Lang_of.functor_argument lenv arg in
        let lang_arg' = { lang_arg with expansion = Some (Signature arg_sg) } in
        (lenv, Some lang_arg' :: args)
    | Functor (None, expr) ->
        let lenv, args = get_env lenv (`Result parent) expr in
        (lenv, None :: args)
    | _ -> (lenv, [])
  in
  match expr with
  | Component.ModuleType.Functor _ ->
      let expansion_env, args =
        get_env Lang_of.empty (id :> Paths_types.Identifier.signature) expr
      in
      let sg = Tools.signature_of_module_type_expr_nopath env expr in
      let sg =
        Lang_of.signature
          (id :> Paths_types.Identifier.signature)
          expansion_env sg
      in
      let sg =
        if Env.has_resolver env
        then Resolve2.signature env sg 
        else Resolve.signature env sg in
      Odoc_model.Lang.Module.Functor (args, signature env sg)
  | Component.ModuleType.With (_expr, _subs) ->
      let sg = Tools.signature_of_module_type_expr_nopath env expr in
      let sg =
        Lang_of.signature
          (id :> Paths_types.Identifier.signature)
          Lang_of.empty sg
      in
      let sg =
        if Env.has_resolver env
        then Resolve2.signature env sg 
        else Resolve.signature env sg in
      Odoc_model.Lang.Module.Signature (signature env sg)
  | _ ->
      let sg = Tools.signature_of_module_type_expr_nopath env expr in
      let sg =
        try 
          Lang_of.signature
            (id :> Paths_types.Identifier.signature)
            Lang_of.empty sg
        with e ->
          let str = Format.asprintf "Failed to translate signature:\n%a\n" Component.Fmt.signature sg in
          Printf.fprintf stderr "%s\n%!" str;
          raise e
      in
      let sg =
        if Env.has_resolver env
        then Resolve2.signature env sg 
        else Resolve.signature env sg in
      Odoc_model.Lang.Module.Signature (signature env sg)

and module_decl env (id : Paths_types.Identifier.module_) decl =
  let open Module in
  match decl with
  | Alias path -> Alias path
  | ModuleType mty ->
      ModuleType
        (module_type_expr env (id :> Paths_types.Identifier.signature) mty)

and module_type_expr env (id : Paths_types.Identifier.signature) expr =
  let open ModuleType in
  match expr with
  | Path _ | Signature _ -> expr
  | With (expr, subs) -> With (module_type_expr env id expr, subs)
  | TypeOf decl -> TypeOf decl
  | Functor (arg, expr) ->
      
      Functor
        ( Component.Opt.map (functor_argument env id) arg,
          module_type_expr env id expr )

and functor_argument env id arg =
  let functor_arg = Env.lookup_module arg.id env in
  try
    let expansion =
      match functor_arg.type_ with
      | ModuleType expr ->
          (try Some (expansion_of_module_type_expr
            (arg.id :> Paths_types.Identifier.signature)
            env expr) with _ -> None)
      | _ -> failwith "error"
    in
    {
      arg with
      expansion = expansion;
      expr = module_type_expr env id arg.expr;
    }
  with e ->
    Format.fprintf Format.err_formatter
      "Error expanding functor argument: %s\nArgment: %a\n%!" (Printexc.to_string e) Component.Fmt.module_ functor_arg;
  raise e

and module_ env m =
  let open Module in
  let id = m.id in
  let expansion_needed =
    match m.type_ with
    | Alias p when Paths.Path.is_hidden (p :> Paths.Path.t) -> true
    | Alias (`Resolved p) -> (
        match Paths.Path.Resolved.Module.canonical_ident p with
        | Some i -> i = m.id (* Self-canonical *)
        | None -> false )
    | ModuleType _ -> true
    | Alias _ -> false
  in
  if not expansion_needed then m
  else
    let type_ = module_decl env id m.type_ in
    let m' = Env.lookup_module m.id env in
    try
      match m'.type_ with
      | Alias _ ->
          let _, sg = Tools.signature_of_module env (`Identifier id, m') in
          let sg =
            try
              Lang_of.signature
                (id :> Odoc_model.Paths.Identifier.Signature.t)
                Lang_of.empty sg
            with e ->
              Format.fprintf Format.err_formatter "Failed translating signature: %a %a" Component.Fmt.model_identifier (id :> Odoc_model.Paths.Identifier.t) Component.Fmt.signature sg;
              raise e
          in
          let sg = signature env sg in
          (* Now phase-2 resolve this signature as canonical paths might now make sense *)
          let sg' =
            if Env.has_resolver env
            then Resolve2.signature env sg 
            else Resolve.signature env sg in
          {
            m with
            type_;
            expansion = Some (Odoc_model.Lang.Module.Signature sg');
            display_type = None; (* Some (ModuleType (ModuleType.Signature sg')); *)
          }
      | ModuleType expr ->
          let expansion =
            expansion_of_module_type_expr
              (id :> Paths_types.Identifier.signature)
              env expr
          in
          { m with type_; expansion = Some expansion }
    with 
    | Tools.OpaqueModule -> m
    | Tools.UnresolvedForwardPath -> m
    | e ->
      let bt = Printexc.get_backtrace () in
      Format.fprintf Format.err_formatter
        "Failed during expansion: %s (of module %a)\n%!" (Printexc.to_string e)
        Component.Fmt.model_identifier
        (id :> Paths.Identifier.t);
      Printf.fprintf stderr "backtrace:\n%s\n%!" bt;
      m

and module_type env m =
  let id = (m.id :> Odoc_model.Paths.Identifier.Signature.t) in
  let expr = Component.Opt.map (module_type_expr env id) m.expr in
  match expr with
  | None -> { m with expr; expansion = Some (Signature []) }
  | _ -> (
      let m' = Env.lookup_module_type m.id env in
      try
        let sg = Tools.signature_of_module_type_nopath env m' in
        let sg = Lang_of.signature id Lang_of.empty sg in
        { m with expr; expansion = Some (Signature (signature env sg)) }
      with e ->
        Format.fprintf Format.err_formatter
          "Got exception %s expading module type %a" (Printexc.to_string e)
          Component.Fmt.model_identifier
          (id :> Paths.Identifier.t);
        { m with expr } )

and class_ : Env.t -> Odoc_model.Lang.Class.t -> Odoc_model.Lang.Class.t =
 fun env c ->
  try
    let c' = Env.lookup_class c.id env in
    let _p, sg =
      Tools.class_signature_of_class env
        (`Identifier (c.id :> Paths_types.Identifier.path_class_type), c')
    in
    let expansion =
      Lang_of.class_signature Lang_of.empty
        (c.id :> Paths_types.Identifier.path_class_type)
        sg
    in
    { c with expansion = Some expansion }
  with e ->
    let bt = Printexc.get_backtrace () in
    Format.fprintf Format.err_formatter "Failed to expand class: %s\n%s\n%!"
      (Printexc.to_string e) bt;
    c

and class_type :
    Env.t -> Odoc_model.Lang.ClassType.t -> Odoc_model.Lang.ClassType.t =
 fun env c ->
  let c' = Env.lookup_class_type c.id env in
  let _p, sg =
    Tools.class_signature_of_class_type env
      (`Identifier (c.id :> Paths_types.Identifier.path_class_type), c')
  in
  let expansion =
    Lang_of.class_signature Lang_of.empty
      (c.id :> Paths_types.Identifier.path_class_type)
      sg
  in
  { c with expansion = Some expansion }

let expand resolver y =
  let before = y in
  let after = unit resolver false before in
  after

let expand2 resolver y =
  let before = y in
  let after = unit resolver true before in
  after

let resolve_page _ x = x
