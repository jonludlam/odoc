(* Type_of.ml *)

(* Deals with expanding `module type of` expressions *)

open Odoc_model
open Lang
module Id = Odoc_model.Paths.Identifier

let again = ref false

let rec signature : Env.t -> Signature.t -> Signature.t =
 fun env sg ->
  let sg', _ = signature_items env sg in
  sg'

and signature_items : Env.t -> Signature.t -> (Signature.t * Env.t) =
 fun env s ->
  let open Signature in
  let items, env' =
    List.fold_left
      (fun (items, env) item ->
        match item with
        | Module (r, m) ->
          let m' = module_ env m in
          let item' = Module (r, m') in
          let ty =
            Component.Delayed.(
              OfLang (Module, m', Component.Of_Lang.empty ()))
          in
          let env' = Env.add_module (m.id :> Odoc_model.Paths.Identifier.Path.Module.t) ty [] env in
          (item' :: items, env')
        | ModuleType mt ->
          (ModuleType (module_type env mt) :: items, env)
        | Include i ->
          let (item', env) = include_ env i in
          (Include item' :: items, env)
        | item -> (item :: items, env))
      ([], env) s.items
  in
  { s with items = List.rev items }, env'

and module_ env m =
  match m.type_ with
  | Alias _ -> m
  | ModuleType expr ->
      {
        m with
        type_ = ModuleType (module_type_expr env (m.id :> Id.Signature.t) expr);
      }

and module_type env m =
  match m.expr with
  | None -> m
  | Some expr ->
      {
        m with
        expr = Some (module_type_expr env (m.id :> Id.Signature.t) expr);
      }

and module_type_expr_typeof env (id : Id.Signature.t) t =
  let open Odoc_model.Lang.ModuleType in
  let p, strengthen =
    match t.t_desc with ModPath p -> (p, false) | StructInclude p -> (p, true)
  in
  let cp = Component.Of_Lang.(module_path (empty ()) p) in
  let open Expand_tools in
  let open Utils.ResultMonad in
  Tools.expansion_of_module_path env ~strengthen cp >>= handle_expansion env id
  >>= fun (_env, e) -> Ok e

and module_type_expr env (id : Id.Signature.t) expr =
  match expr with
  | Path _ -> expr
  | Functor (Unit, expr) -> Functor (Unit, module_type_expr env id expr)
  | Functor (Named p, expr) ->
      let env = Env.add_functor_parameter (Named p) env in
      Functor (Named (functor_parameter env p), module_type_expr env id expr)
  | Signature sg -> Signature (signature env sg)
  | With w -> With { w with w_expr = u_module_type_expr env id w.w_expr }
  | TypeOf t -> (
      match module_type_expr_typeof env id t with
      | Ok e ->
          let shadow = Env.find_shadow env id in
          let map = Lang_of.with_shadowed shadow in
          let se = Lang_of.(simple_expansion map id e) in
          TypeOf { t with t_expansion = Some (simple_expansion env se) }
      | Error e
        when Errors.is_unexpanded_module_type_of (e :> Errors.Tools_error.any)
        ->
          again := true;
          expr
      | Error _e -> expr)

and u_module_type_expr env id expr =
  match expr with
  | Path _ -> expr
  | Signature sg -> Signature (signature env sg)
  | With (subs, w) -> 
    With (subs, u_module_type_expr env id w)
  | TypeOf t -> (
      let fake_parent = (Paths.Identifier.Mk.module_ (id, Names.ModuleName.internal_of_string "__FAKE__")) in
      match module_type_expr_typeof env fake_parent t with
      | Ok e ->
        let shadow = Env.find_shadow env id in
        let map = Lang_of.with_shadowed shadow in
        let se = Lang_of.(simple_expansion map fake_parent e) in
        TypeOf { t with t_expansion = Some (simple_expansion env se) }
      | Error e
        when Errors.is_unexpanded_module_type_of (e :> Errors.Tools_error.any)
        ->
          again := true;
          expr
      | Error _e -> expr)

and functor_parameter env p =
  { p with expr = module_type_expr env (p.id :> Id.Signature.t) p.expr }

and simple_expansion :
    Env.t -> ModuleType.simple_expansion -> ModuleType.simple_expansion =
 fun env -> function
  | Signature sg -> Signature (signature env sg)
  | Functor (Named n, sg) ->
      Functor (Named (functor_parameter env n), simple_expansion env sg)
  | Functor (Unit, sg) -> Functor (Unit, simple_expansion env sg)

and include_ : Env.t -> Odoc_model.Lang.Include.t -> Odoc_model.Lang.Include.t * Env.t = fun env i ->
  let shadow_env = Env.add_shadow env i.parent i.expansion.shadowed in
  let decl =
    (* try  *)
      match i.decl with
      | Alias _ -> i.decl
      | ModuleType t -> ModuleType (u_module_type_expr shadow_env i.parent t)
    (* with e ->
      Format.eprintf "Failed to handle include: u_module_type_expr=\n%!";
      begin
        match i.decl with
        | Alias _ -> ()
        | ModuleType t ->
        Format.eprintf "%a\n%!" Component.Fmt.u_module_type_expr (Component.Of_Lang.(u_module_type_expr (empty ()) t));
        end;
      raise e *)
  in
  let content, env' =
    let { Include.content; _ } = i.expansion in
    signature_items env content
  in
  { i with expansion = { i.expansion with content }; decl }, env'

let signature env =
  let rec loop sg =
    again := false;
    let sg' = signature env sg in
    Tools.reset_caches ();
    if !again then if sg' = sg then sg else loop sg' else sg'
  in
  loop
