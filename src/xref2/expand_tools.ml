
type expansion =
  | Signature of Component.Signature.t
  | Functor of Component.FunctorArgument.t option * Component.ModuleType.expr

let rec aux_expansion_of_module : Env.t -> Component.Module.t -> expansion =
  let open Component.Module in
  fun env m ->
    aux_expansion_of_module_decl env m.type_

and aux_expansion_of_module_decl env ty =
  let open Component.Module in
  match ty with
  | Alias path -> aux_expansion_of_module_alias env path
  | ModuleType expr -> aux_expansion_of_module_type_expr env expr

and aux_expansion_of_module_alias env path =
  match Tools.lookup_and_resolve_module_from_path false false env path with
  | Resolved (p, m) -> begin
    match aux_expansion_of_module env m with
    | Signature sg ->
      Signature (Strengthen.signature p sg)
    | Functor _ as x -> x
  end
  | Unresolved p ->
    let err = Format.asprintf "Failed to lookup alias module (path=%a) (res=%a)"
    Component.Fmt.module_path path
    Component.Fmt.module_path p
    in
    failwith err

and aux_expansion_of_module_type_expr env expr : expansion =
  let open Component.ModuleType in
  match expr with
  | Path p -> begin
    match Tools.lookup_and_resolve_module_type_from_path false env p with
    | Resolved (_, mt) -> aux_expansion_of_module_type env mt
    | Unresolved p ->
      let p = Component.Fmt.(string_of module_type_path p) in
      failwith (Printf.sprintf "Couldn't find signature: %s" p)
    end
  | Signature s -> Signature s
  | With (s, subs) -> begin
    let expn = aux_expansion_of_module_type_expr env s in
    match expn with
    | Functor _ -> failwith "This shouldn't be possible!"
    | Signature sg ->
    let sg = Tools.handle_signature_with_subs env sg subs in
    Signature sg
    end
  | Functor (arg, expr) ->
    Functor (arg, expr)
  | TypeOf decl -> aux_expansion_of_module_decl env decl

and aux_expansion_of_module_type env mt =
    let open Component.ModuleType in
    match mt.expr with
    | None -> raise Tools.OpaqueModule
    | Some expr -> aux_expansion_of_module_type_expr env expr

let handle_expansion env id expansion =
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
        let env' = Env.add_module identifier (Component.module_of_functor_argument arg) env in
        let subst = Subst.add_module arg.id (`Identifier identifier) Subst.identity in
        (env', Subst.module_type_expr subst expr)
  in
  let rec expand id env args expansion =
    match expansion with
    | Signature sg -> begin
      match args with
      | [] -> env, Component.Module.Signature sg
      | args -> env, Component.Module.Functor (args, sg)
    end 
    | Functor (arg, expr) -> begin
      let (env', expr') = handle_argument id arg expr env in
      expand (`Result id) env' (arg :: args) (aux_expansion_of_module_type_expr env expr')
    end
  in
  let env, e = expand id env [] expansion in
  env, Lang_of.(module_expansion empty id e)

let expansion_of_module_type env id m =
  let open Odoc_model.Paths.Identifier in
  aux_expansion_of_module_type env m |>
  handle_expansion env (id : ModuleType.t :> Signature.t)

let expansion_of_module_type_expr env id expr =
  aux_expansion_of_module_type_expr env expr |>
  handle_expansion env id

let expansion_of_module env id m =
  let open Odoc_model.Paths.Identifier in
  aux_expansion_of_module env m |>
  handle_expansion env (id : Module.t :> Signature.t)
