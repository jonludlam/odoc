open Odoc_model.Names

(* Add [result] and a bind operator over it in scope *)
open Utils
open ResultMonad
module RP = Odoc_model.Paths.Path.Resolved

let rec dget_impl : type a. a Component.Delayed.t -> a = function
  | Val x -> x
  | General { v = Some x; _ } -> x
  | General ({ v = None; get = Some f } as g) ->
      let res = f () in
      g.v <- Some res;
      res
  | General _ -> failwith "Bad general"
  | OfLang (Module, m, map) -> Component.Of_Lang.module_ map m
  | OfLang (ModuleType, m, map) -> Component.Of_Lang.module_type map m
  | OfLang (Type, m, map) -> Component.Of_Lang.type_decl map m
  | OfLang (Value, v, map) -> Component.Of_Lang.value map v
  | Strengthen (Module, m, p, canonical) ->
      Strengthen.module_ ?canonical p (dget_impl m)
  | Strengthen (ModuleType, m, p, _) -> Strengthen.module_type p (dget_impl m)
  | Strengthen (Type, t, p, _) -> Strengthen.type_decl p (dget_impl t)
  | Strengthen (_, v, _, _) -> dget_impl v
  | Subst (Module, m, sub) -> Subst.module_ sub (dget_impl m)
  | Subst (ModuleType, mt, sub) -> Subst.module_type sub (dget_impl mt)
  | Subst (Type, t, sub) -> Subst.type_ sub (dget_impl t)
  | Subst (Value, v, sub) -> Subst.value sub (dget_impl v)

let _ = Component.dget_impl := Some { Component.dget = dget_impl }

type ('a, 'b) either = Left of 'a | Right of 'b

let filter_map f x =
  List.rev
  @@ List.fold_left
       (fun acc x -> match f x with Some x -> x :: acc | None -> acc)
       [] x

type module_modifiers =
  [ `Aliased of Cpath.Resolved.module_ | `SubstMT of Cpath.Resolved.module_type ]

type module_type_modifiers = [ `AliasModuleType of Cpath.Resolved.module_type ]

(* These three functions take a fully-qualified canonical path and return
   a list of shorter possibilities to test *)
let c_mod_poss env p : Cpath.module_ list =
  (* canonical module paths *)
  let rec inner x =
    match x.Hc.v with
    | `Dot (p, n) -> (
        let rest = List.map (fun p -> Cpath.Mk.Module.dot (p, n)) (inner p) in
        match Env.lookup_by_name Env.s_module n env with
        | Ok (`Module (id, m)) ->
            let m = Component.dget m in
            Cpath.Mk.Module.identifier (id, m.hidden) :: rest
        | Error _ -> rest)
    | _ -> [ x ]
  in
  inner p

let c_modty_poss env (p : Cpath.module_type) : Cpath.module_type list =
  (* canonical module type paths *)
  match p.v with
  | `Dot (p, n) -> (
      let rest =
        List.map (fun p -> Cpath.Mk.ModuleType.dot (p, n)) (c_mod_poss env p)
      in
      match Env.lookup_by_name Env.s_module_type n env with
      | Ok (`ModuleType (id, _)) ->
          Cpath.Mk.ModuleType.identifier (id, false) :: rest
      | Error _ -> rest)
  | _ -> [ p ]

let c_ty_poss env (p : Cpath.type_) : Cpath.type_ list =
  (* canonical type paths *)
  match p.v with
  | `Dot (p, n) -> (
      let rest =
        List.map (fun p -> Cpath.Mk.Type.dot (p, n)) (c_mod_poss env p)
      in
      match Env.lookup_by_name Env.s_type n env with
      | Ok (`Type (id, _)) ->
          Cpath.Mk.Type.identifier
            ((id :> Odoc_model.Paths.Identifier.Path.Type.t), false)
          :: rest
      | Error _ -> rest)
  | _ -> [ p ]

(* Small helper function for resolving canonical paths.
   [canonical_helper env resolve lang_of possibilities p2] takes the
   fully-qualified path [p2] and returns the shortest resolved path
   whose identifier is the same as the resolved fully qualified path.
   [resolve] is a function that resolves an arbitrary unresolved path,
   [lang_of] turns a resolved path into a generic resolved Lang path
   and [possibilities] is a function that, given the fully qualified
   unresolved path, returns an ordered list of all possible unresolved
   paths starting with the shortest and including the longest one. *)
let canonical_helper :
      'unresolved 'resolved.
      Env.t ->
      (Env.t -> 'unresolved -> ('resolved * 'result, _) result) ->
      ('resolved -> RP.t) ->
      (Env.t -> 'unresolved -> 'unresolved list) ->
      'unresolved ->
      ('resolved * 'result) option =
 fun env resolve lang_of possibilities p2 ->
  let resolve p =
    match resolve env p with Ok rp -> Some rp | Error _ -> None
  in
  let get_identifier cpath = RP.identifier (lang_of cpath) in
  match resolve p2 with
  | None -> None
  | Some (rp2, _) -> (
      let fallback_id = get_identifier rp2 in
      let resolved = filter_map resolve (possibilities env p2) in
      let find_fn (r, _) = get_identifier r = fallback_id in
      try Some (List.find find_fn resolved) with _ -> None)

let core_types =
  let open Odoc_model.Lang.TypeDecl in
  let open Odoc_model.Paths in
  List.map
    (fun decl ->
      (Identifier.name decl.id, Component.Of_Lang.(type_decl (empty ()) decl)))
    Odoc_model.Predefined.core_types

let prefix_substitution path sg =
  let open Component.Signature in
  let module M = Cpath.Mk in
  let module RM = Cpath.Mk.Resolved in
  let rec get_sub sub' is =
    match is with
    | [] -> sub'
    | Type (id, _, _) :: rest ->
        let name = Ident.Name.typed_type id in
        get_sub
          (Subst.add_type id
             (M.Type.type_ (path, name))
             (RM.Type.type_ (path, name))
             sub')
          rest
    | Module (id, _, _) :: rest ->
        let name = Ident.Name.typed_module id in
        get_sub
          (Subst.add_module
             (id :> Ident.path_module)
             (M.Module.module_ (path, name))
             (RM.Module.module_ (path, name))
             sub')
          rest
    | ModuleType (id, _) :: rest ->
        let name = Ident.Name.typed_module_type id in
        get_sub
          (Subst.add_module_type id
             (M.ModuleType.module_type (path, name))
             (RM.ModuleType.module_type (path, name))
             sub')
          rest
    | ModuleTypeSubstitution (id, _) :: rest ->
        let name = Ident.Name.typed_module_type id in
        get_sub
          (Subst.add_module_type id
             (M.ModuleType.module_type (path, name))
             (RM.ModuleType.module_type (path, name))
             sub')
          rest
    | ModuleSubstitution (id, _) :: rest ->
        let name = Ident.Name.typed_module id in
        get_sub
          (Subst.add_module
             (id :> Ident.path_module)
             (M.Module.module_ (path, name))
             (RM.Module.module_ (path, name))
             sub')
          rest
    | TypeSubstitution (id, _) :: rest ->
        let name = Ident.Name.typed_type id in
        get_sub
          (Subst.add_type id
             (M.Type.type_ (path, name))
             (RM.Type.type_ (path, name))
             sub')
          rest
    | Exception _ :: rest
    | TypExt _ :: rest
    | Value (_, _) :: rest
    | Comment _ :: rest ->
        get_sub sub' rest
    | Class (id, _, _) :: rest ->
        let name = Ident.Name.typed_class id in
        get_sub
          (Subst.add_class id
             (M.ClassType.class_ (path, name))
             (RM.ClassType.class_ (path, name))
             sub')
          rest
    | ClassType (id, _, _) :: rest ->
        let name = Ident.Name.typed_class_type id in
        get_sub
          (Subst.add_class_type id
             (M.ClassType.class_type (path, name))
             (RM.ClassType.class_type (path, name))
             sub')
          rest
    | Include i :: rest -> get_sub (get_sub sub' i.expansion_.items) rest
    | Open o :: rest -> get_sub (get_sub sub' o.expansion.items) rest
  in
  let extend_sub_removed removed sub =
    List.fold_right
      (fun item map ->
        match item with
        | Component.Signature.RModule (id, _) ->
            let name = Ident.Name.typed_module id in
            Subst.add_module
              (id :> Ident.path_module)
              (M.Module.module_ (path, name))
              (RM.Module.module_ (path, name))
              map
        | Component.Signature.RModuleType (id, _) ->
            let name = Ident.Name.typed_module_type id in
            Subst.add_module_type
              (id :> Ident.module_type)
              (M.ModuleType.module_type (path, name))
              (RM.ModuleType.module_type (path, name))
              map
        | Component.Signature.RType (id, _, _) ->
            let name = Ident.Name.typed_type id in
            Subst.add_type id
              (M.Type.type_ (path, name))
              (RM.Type.type_ (path, name))
              map)
      removed sub
  in
  get_sub Subst.identity sg.items |> extend_sub_removed sg.removed

let prefix_signature (path, sg) =
  let open Component.Signature in
  let sub = prefix_substitution path sg in
  let items =
    List.map
      (function
        | Module (id, r, m) ->
            Module
              ( Ident.Rename.module_ id,
                r,
                Component.Delayed.(Subst (Module, m, sub)) )
        | ModuleType (id, mt) ->
            ModuleType
              ( Ident.Rename.module_type id,
                Component.Delayed.(Subst (ModuleType, mt, sub)) )
        | Type (id, r, t) ->
            Type
              ( Ident.Rename.type_ id,
                r,
                Component.Delayed.(Subst (Type, t, sub)) )
        | TypeSubstitution (id, t) ->
            TypeSubstitution (Ident.Rename.type_ id, Subst.type_ sub t)
        | ModuleSubstitution (id, m) ->
            ModuleSubstitution
              (Ident.Rename.module_ id, Subst.module_substitution sub m)
        | ModuleTypeSubstitution (id, m) ->
            ModuleTypeSubstitution
              (Ident.Rename.module_type id, Subst.module_type_substitution sub m)
        | Exception (id, e) -> Exception (id, Subst.exception_ sub e)
        | TypExt t -> TypExt (Subst.extension sub t)
        | Value (id, v) -> Value (id, Component.Delayed.(Subst (Value, v, sub)))
        | Class (id, r, c) ->
            Class (Ident.Rename.class_ id, r, Subst.class_ sub c)
        | ClassType (id, r, c) ->
            ClassType (Ident.Rename.class_type id, r, Subst.class_type sub c)
        | Include i -> Include (Subst.include_ sub i)
        | Open o -> Open (Subst.open_ sub o)
        | Comment c -> Comment c)
      sg.items
  in
  { sg with items }

open Errors.Tools_error

type resolve_module_result =
  ( Cpath.Resolved.module_ * Component.Module.t Component.Delayed.t,
    simple_module_lookup_error )
  Result.result

type resolve_module_type_result =
  ( Cpath.Resolved.module_type * Component.ModuleType.t,
    simple_module_type_lookup_error )
  Result.result

type resolve_type_result =
  ( Cpath.Resolved.type_ * Find.careful_type,
    simple_type_lookup_error )
  Result.result

type resolve_class_type_result =
  ( Cpath.Resolved.class_type * Find.careful_class,
    simple_type_lookup_error )
  Result.result

type ('a, 'b, 'c) sig_map = { type_ : 'a; module_ : 'b; module_type : 'c }

let id_map = { type_ = None; module_ = None; module_type = None }

module type MEMO = sig
  type result

  include Hashtbl.HashedType
end

module MakeMemo (X : MEMO) = struct
  module M = Hashtbl.Make (X)

  let cache : (X.result * int * Env.LookupTypeSet.t) M.t = M.create 10000

  let cache_hits : int M.t = M.create 10000

  let enabled = ref true

  let bump_counter arg =
    try
      let new_val = M.find cache_hits arg + 1 in
      M.replace cache_hits arg new_val;
      new_val
    with _ ->
      M.add cache_hits arg 1;
      1

  let memoize f env arg =
    if not !enabled then f env arg
    else
      let env_id = Env.id env in
      let n = bump_counter arg in
      let no_memo () =
        let lookups, result =
          Env.with_recorded_lookups env (fun env' -> f env' arg)
        in
        if n > 1 then M.add cache arg (result, env_id, lookups);
        result
      in
      match M.find_all cache arg with
      | [] -> no_memo ()
      | xs ->
          let rec find_fast = function
            | (result, env_id', _) :: _ when env_id' = env_id ->
                M.replace cache_hits arg (M.find cache_hits arg + 1);
                result
            | _ :: ys -> find_fast ys
            | [] -> find xs
          and find = function
            | (m, _, lookups) :: xs ->
                (* let b = Env.verify_lookups env lookups in *)
                if Env.verify_lookups env lookups then m else find xs
            | [] -> no_memo ()
          in
          find_fast xs

  let clear () =
    M.clear cache;
    M.clear cache_hits
end

module LookupModuleMemo = MakeMemo (struct
  type t = bool * Cpath.Resolved.module_

  type result =
    ( Component.Module.t Component.Delayed.t,
      simple_module_lookup_error )
    Result.result

  let equal (x1, x2) (y1, y2) = x1 = y1 && x2 = y2

  let hash (x, y) = Hashtbl.hash (x, y)
end)

module LookupParentMemo = MakeMemo (struct
  type t = bool * Cpath.Resolved.parent

  type result =
    ( Component.Signature.t * Component.Substitution.t,
      [ `Parent of parent_lookup_error ] )
    Result.result

  let equal (x1, x2) (y1, y2) = x1 = y1 && x2 = y2

  let hash (x, y) = Hashtbl.hash (x, y)
end)

module LookupAndResolveMemo = MakeMemo (struct
  type t = bool * bool * Cpath.module_

  type result = resolve_module_result

  let equal (x1, x2, x3) (y1, y2, y3) = x1 = y1 && x2 = y2 && x3 = y3

  let hash (x, x1, y) = Hashtbl.hash (x, x1, y)
end)

module SignatureOfModuleMemo = MakeMemo (struct
  type t = Cpath.Resolved.module_

  type result = (Component.Signature.t, signature_of_module_error) Result.result

  let equal x y = x = y

  let hash x = Hashtbl.hash x
end)

let disable_all_caches () =
  LookupModuleMemo.enabled := false;
  LookupAndResolveMemo.enabled := false;
  SignatureOfModuleMemo.enabled := false;
  LookupParentMemo.enabled := false

let reset_caches () =
  LookupModuleMemo.clear ();
  LookupAndResolveMemo.clear ();
  SignatureOfModuleMemo.clear ();
  LookupParentMemo.clear ()

let simplify_module : Env.t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_
    =
 fun env m ->
  let open Odoc_model.Paths.Identifier in
  match m.v with
  | `Module ({ v = `Module { v = `Gpath (`Identifier p); _ }; _ }, name) -> (
      let ident = (Mk.module_ ((p :> Signature.t), name) : Path.Module.t) in
      match Env.(lookup_by_id s_module (ident :> Signature.t) env) with
      | Some _ -> Cpath.Mk.Resolved.Module.gpath (`Identifier ident)
      | None -> m)
  | _ -> m

let simplify_module_type :
    Env.t -> Cpath.Resolved.module_type -> Cpath.Resolved.module_type =
 fun env m ->
  let open Odoc_model.Paths.Identifier in
  match m.v with
  | `ModuleType ({ v = `Module { v = `Gpath (`Identifier p); _ }; _ }, name)
    -> (
      let ident =
        (Mk.module_type ((p :> Signature.t), name) : Path.ModuleType.t)
      in
      match Env.(lookup_by_id s_module_type (ident :> Signature.t) env) with
      | Some _ -> Cpath.Mk.Resolved.ModuleType.gpath (`Identifier ident)
      | None -> m)
  | _ -> m

let simplify_type : Env.t -> Cpath.Resolved.type_ -> Cpath.Resolved.type_ =
 fun env m ->
  let open Odoc_model.Paths.Identifier in
  match m.v with
  | `Type ({ v = `Module { v = `Gpath (`Identifier p); _ }; _ }, name) -> (
      let ident = (Mk.type_ ((p :> Signature.t), name) : Path.Type.t) in
      match Env.(lookup_by_id s_type (ident :> Path.Type.t) env) with
      | Some _ -> Cpath.Mk.Resolved.Type.gpath (`Identifier ident)
      | None -> m)
  | _ -> m

let rec handle_apply ~mark_substituted env func_path arg_path m =
  let rec find_functor mty =
    match mty with
    | Component.ModuleType.Functor (Named arg, expr) ->
        Ok (arg.Component.FunctorParameter.id, expr)
    | Component.ModuleType.Path { p_path; _ } -> (
        match
          resolve_module_type ~mark_substituted:false ~add_canonical:true env
            p_path
        with
        | Ok (_, { Component.ModuleType.expr = Some mty'; _ }) ->
            find_functor mty'
        | _ -> Error `OpaqueModule)
    | _ -> Error `ApplyNotFunctor
  in
  module_type_expr_of_module env m >>= fun mty' ->
  find_functor mty' >>= fun (arg_id, result) ->
  let new_module = { m with Component.Module.type_ = ModuleType result } in
  let substitution =
    if mark_substituted then Cpath.Mk.Resolved.Module.substituted arg_path
    else arg_path
  in

  let path = Cpath.Mk.Resolved.Module.apply (func_path, arg_path) in
  let subst =
    Subst.add_module
      (arg_id :> Ident.path_module)
      (Cpath.Mk.Module.resolved substitution)
      substitution Subst.identity
  in
  let subst = Subst.unresolve_opaque_paths subst in
  Ok (path, Subst.module_ subst new_module)

and add_canonical_path :
    Component.Module.t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_ =
 fun m p ->
  match p.v with
  | `Canonical _ -> p
  | _ -> (
      match m.Component.Module.canonical with
      | Some cp -> Cpath.Mk.Resolved.Module.canonical (p, cp)
      | None -> p)

and add_canonical_path_mt :
    Component.ModuleType.t ->
    Cpath.Resolved.module_type ->
    Cpath.Resolved.module_type =
 fun m p ->
  match p.v with
  | `CanonicalModuleType _ -> p
  | _ -> (
      match m.canonical with
      | Some cp -> Cpath.Mk.Resolved.ModuleType.canonicalmoduletype (p, cp)
      | None -> p)

and get_substituted_module_type :
    Env.t -> Component.ModuleType.expr -> Cpath.Resolved.module_type option =
 fun env expr ->
  match expr with
  | Component.ModuleType.Path { p_path; _ } ->
      if Cpath.is_module_type_substituted p_path then
        match
          resolve_module_type ~mark_substituted:true ~add_canonical:true env
            p_path
        with
        | Ok (resolved_path, _) -> Some resolved_path
        | Error _ -> None
      else None
  | _ -> None

and get_module_type_path_modifiers :
    Env.t ->
    add_canonical:bool ->
    Component.ModuleType.t ->
    module_type_modifiers option =
 fun env ~add_canonical m ->
  let alias_of expr =
    match expr with
    | Component.ModuleType.Path alias_path -> (
        match
          resolve_module_type ~mark_substituted:true ~add_canonical env
            alias_path.p_path
        with
        | Ok (resolved_alias_path, _) -> Some resolved_alias_path
        | Error _ -> None)
    (* | Functor (_arg, res) -> alias_of res *)
    | _ -> None
  in
  match m.expr with
  | Some e -> (
      match alias_of e with Some e -> Some (`AliasModuleType e) | None -> None)
  | None -> None

and process_module_type env ~add_canonical m p' =
  let open Component.ModuleType in
  let open OptionMonad in
  (* Loop through potential chains of module_type equalities, looking for substitutions *)
  let substpath =
    m.expr >>= get_substituted_module_type env >>= fun p ->
    Some (Cpath.Mk.Resolved.ModuleType.substt (p, p'))
  in

  let p' = match substpath with Some p -> p | None -> p' in
  let p'' =
    match get_module_type_path_modifiers env ~add_canonical m with
    | Some (`AliasModuleType e) ->
        Cpath.Mk.Resolved.ModuleType.aliasmoduletype (e, p')
    | None -> p'
  in
  let p''' = if add_canonical then add_canonical_path_mt m p'' else p'' in
  p'''

and get_module_path_modifiers :
    Env.t -> add_canonical:bool -> Component.Module.t -> _ option =
 fun env ~add_canonical m ->
  match m.type_ with
  | Alias (alias_path, _) -> (
      match
        resolve_module ~mark_substituted:true ~add_canonical env alias_path
      with
      | Ok (resolved_alias_path, _) -> Some (`Aliased resolved_alias_path)
      | Error _ -> None)
  | ModuleType t -> (
      match get_substituted_module_type env t with
      | Some s -> Some (`SubstMT s)
      | None -> None)

and process_module_path env ~add_canonical md rp =
  let m = Component.dget md in
  let rp =
    if m.Component.Module.hidden then Cpath.Mk.Resolved.Module.hidden rp else rp
  in
  let rp' =
    match get_module_path_modifiers env ~add_canonical m with
    | None -> rp
    | Some (`Aliased rp') ->
        let dest_hidden =
          Cpath.is_resolved_module_hidden ~weak_canonical_test:true rp'
        in
        if dest_hidden then rp
        else
          let unresolved_rp =
            try Cpath.unresolve_resolved_module_path rp
            with _ -> Cpath.Mk.Module.resolved rp
          in
          (* Keep the resolved path for the canonical processing below in handle_canonical_module.strip_alias *)
          Cpath.Mk.Resolved.Module.alias (rp', unresolved_rp, Some rp)
    | Some (`SubstMT p') -> Cpath.Mk.Resolved.Module.subst (p', rp)
  in
  let p'' = if add_canonical then add_canonical_path m rp' else rp' in
  p''

and handle_module_lookup env ~add_canonical id rparent sg sub =
  match Find.careful_module_in_sig sg id with
  | Some (`FModule (name, m)) ->
      let rp' =
        simplify_module env (Cpath.Mk.Resolved.Module.module_ (rparent, name))
      in
      let md' = Component.Delayed.(Subst (Module, Val m, sub)) in
      Ok (process_module_path env ~add_canonical md' rp', md')
  | Some (`FModule_removed p) ->
      lookup_module ~mark_substituted:false env p >>= fun m -> Ok (p, m)
  | None -> Error `Find_failure

and handle_module_type_lookup env ~add_canonical id p sg sub =
  let open OptionMonad in
  Find.module_type_in_sig sg id >>= fun (`FModuleType (name, mt)) ->
  let mt = Subst.module_type sub mt in
  let p' =
    simplify_module_type env
      (Cpath.Mk.Resolved.ModuleType.module_type (p, name))
  in
  let p'' = process_module_type env ~add_canonical mt p' in
  Some (p'', mt)

and handle_type_lookup env id p sg =
  match Find.careful_type_in_sig sg id with
  | Some (`FClass (name, _) as t) ->
      Ok (Cpath.Mk.Resolved.Type.class_ (p, name), t)
  | Some (`FClassType (name, _) as t) ->
      Ok (Cpath.Mk.Resolved.Type.class_type (p, name), t)
  | Some (`FType (name, _) as t) ->
      Ok (simplify_type env (Cpath.Mk.Resolved.Type.type_ (p, name)), t)
  | Some (`FType_removed (name, _, _) as t) ->
      Ok (Cpath.Mk.Resolved.Type.type_ (p, name), t)
  | None -> Error `Find_failure

and handle_class_type_lookup id p sg =
  match Find.careful_class_in_sig sg id with
  | Some (`FClass (name, _) as t) ->
      Ok (Cpath.Mk.Resolved.ClassType.class_ (p, name), t)
  | Some (`FClassType (name, _) as t) ->
      Ok (Cpath.Mk.Resolved.ClassType.class_type (p, name), t)
  | Some (`FType_removed (_name, _, _) as _t) -> Error `Class_replaced
  | None -> Error `Find_failure

and lookup_module_gpath :
    mark_substituted:bool ->
    Env.t ->
    Odoc_model.Paths.Path.Resolved.Module.t ->
    ( Component.Module.t Component.Delayed.t,
      simple_module_lookup_error )
    Result.result =
 fun ~mark_substituted env path ->
  match path with
  | `Identifier i ->
      of_option ~error:(`Lookup_failure i) (Env.(lookup_by_id s_module) i env)
      >>= fun (`Module (_, m)) -> Ok m
  | `Apply (functor_path, argument_path) ->
      lookup_module_gpath ~mark_substituted env functor_path
      >>= fun functor_module ->
      let functor_module = Component.dget functor_module in
      handle_apply ~mark_substituted env
        (Cpath.Mk.Resolved.Module.gpath functor_path)
        (Cpath.Mk.Resolved.Module.gpath argument_path)
        functor_module
      |> map_error (fun e -> `Parent (`Parent_expr e))
      >>= fun (_, m) -> Ok (Component.Delayed.Val m)
  | `Module (parent, name) ->
      let find_in_sg sg sub =
        match Find.careful_module_in_sig sg (ModuleName.to_string name) with
        | None -> Error `Find_failure
        | Some (`FModule (_, m)) ->
            Ok Component.Delayed.(Subst (Module, Val m, sub))
        | Some (`FModule_removed p) -> lookup_module ~mark_substituted env p
      in
      lookup_parent_gpath ~mark_substituted env parent
      |> map_error (fun e -> (e :> simple_module_lookup_error))
      >>= fun (sg, sub) -> find_in_sg sg sub
  | `Alias (p, _) -> lookup_module_gpath ~mark_substituted env p
  | `Subst (_, p) -> lookup_module_gpath ~mark_substituted env p
  | `Hidden p -> lookup_module_gpath ~mark_substituted env p
  | `Canonical (p, _) -> lookup_module_gpath ~mark_substituted env p
  | `OpaqueModule m -> lookup_module_gpath ~mark_substituted env m

and lookup_module :
    mark_substituted:bool ->
    Env.t ->
    Cpath.Resolved.module_ ->
    ( Component.Module.t Component.Delayed.t,
      simple_module_lookup_error )
    Result.result =
 fun ~mark_substituted:m env' path' ->
  let lookup env (mark_substituted, (path : SignatureOfModuleMemo.M.key)) =
    match path.v with
    | `Local lpath -> Error (`Local (env, lpath))
    | `Gpath p -> lookup_module_gpath ~mark_substituted env p
    | `Substituted x -> lookup_module ~mark_substituted env x
    | `Apply (functor_path, argument_path) ->
        lookup_module ~mark_substituted env functor_path
        >>= fun functor_module ->
        let functor_module = Component.dget functor_module in
        handle_apply ~mark_substituted env functor_path argument_path
          functor_module
        |> map_error (fun e -> `Parent (`Parent_expr e))
        >>= fun (_, m) -> Ok (Component.Delayed.Val m)
    | `Module (parent, name) ->
        let find_in_sg sg sub =
          match Find.careful_module_in_sig sg (ModuleName.to_string name) with
          | None -> Error `Find_failure
          | Some (`FModule (_, m)) ->
              Ok (Component.Delayed.Subst (Module, Val m, sub))
          | Some (`FModule_removed p) -> lookup_module ~mark_substituted env p
        in
        lookup_parent ~mark_substituted env parent
        |> map_error (fun e -> (e :> simple_module_lookup_error))
        >>= fun (sg, sub) -> find_in_sg sg sub
    | `Alias (_, cs, _) -> (
        match resolve_module ~mark_substituted ~add_canonical:false env cs with
        | Ok (_, r) -> Ok r
        | Error e -> Error e)
    | `Subst (_, p) -> lookup_module ~mark_substituted env p
    | `Hidden p -> lookup_module ~mark_substituted env p
    | `Canonical (p, _) -> lookup_module ~mark_substituted env p
    | `OpaqueModule m -> lookup_module ~mark_substituted env m
  in
  LookupModuleMemo.memoize lookup env' (m, path')

and lookup_module_type_gpath :
    mark_substituted:bool ->
    Env.t ->
    Odoc_model.Paths.Path.Resolved.ModuleType.t ->
    (Component.ModuleType.t, simple_module_type_lookup_error) Result.result =
 fun ~mark_substituted env path ->
  match path with
  | `Identifier i ->
      of_option ~error:(`Lookup_failureMT i)
        (Env.(lookup_by_id s_module_type) i env)
      >>= fun (`ModuleType (_, mt)) -> Ok mt
  | `CanonicalModuleType (s, _) | `SubstT (_, s) ->
      lookup_module_type_gpath ~mark_substituted env s
  | `ModuleType (parent, name) ->
      let find_in_sg sg sub =
        match Find.module_type_in_sig sg (ModuleTypeName.to_string name) with
        | None -> Error `Find_failure
        | Some (`FModuleType (_, mt)) -> Ok (Subst.module_type sub mt)
      in
      lookup_parent_gpath ~mark_substituted env parent
      |> map_error (fun e -> (e :> simple_module_type_lookup_error))
      >>= fun (sg, sub) -> find_in_sg sg sub
  | `AliasModuleType (_, mt) ->
      lookup_module_type_gpath ~mark_substituted env mt
  | `OpaqueModuleType m -> lookup_module_type_gpath ~mark_substituted env m

and lookup_module_type :
    mark_substituted:bool ->
    Env.t ->
    Cpath.Resolved.module_type ->
    (Component.ModuleType.t, simple_module_type_lookup_error) Result.result =
 fun ~mark_substituted env path ->
  let lookup env =
    match path.v with
    | `Local l -> Error (`LocalMT (env, l))
    | `Gpath p -> lookup_module_type_gpath ~mark_substituted env p
    | `Substituted s | `CanonicalModuleType (s, _) | `SubstT (_, s) ->
        lookup_module_type ~mark_substituted env s
    | `ModuleType (parent, name) ->
        let find_in_sg sg sub =
          match Find.module_type_in_sig sg (ModuleTypeName.to_string name) with
          | None -> Error `Find_failure
          | Some (`FModuleType (_, mt)) -> Ok (Subst.module_type sub mt)
        in
        lookup_parent ~mark_substituted:true env parent
        |> map_error (fun e -> (e :> simple_module_type_lookup_error))
        >>= fun (sg, sub) -> find_in_sg sg sub
    | `AliasModuleType (_, mt) -> lookup_module_type ~mark_substituted env mt
    | `OpaqueModuleType m -> lookup_module_type ~mark_substituted env m
  in
  lookup env

and lookup_parent :
    mark_substituted:bool ->
    Env.t ->
    Cpath.Resolved.parent ->
    ( Component.Signature.t * Component.Substitution.t,
      [ `Parent of parent_lookup_error ] )
    Result.result =
 fun ~mark_substituted:m env' parent' ->
  let lookup env (mark_substituted, parent) =
    match parent.Hc.v with
    | `Module p ->
        lookup_module ~mark_substituted env p
        |> map_error (fun e -> `Parent (`Parent_module e))
        >>= fun m ->
        let m = Component.dget m in
        signature_of_module env m
        |> map_error (fun e -> `Parent (`Parent_sig e))
        >>= fun sg -> Ok (sg, prefix_substitution parent sg)
    | `ModuleType p ->
        lookup_module_type ~mark_substituted env p
        |> map_error (fun e -> `Parent (`Parent_module_type e))
        >>= fun mt ->
        signature_of_module_type env mt
        |> map_error (fun e -> `Parent (`Parent_sig e))
        >>= fun sg -> Ok (sg, prefix_substitution parent sg)
    | `FragmentRoot ->
        Env.lookup_fragment_root env
        |> of_option ~error:(`Parent `Fragment_root)
        >>= fun (_, sg) -> Ok (sg, prefix_substitution parent sg)
  in
  LookupParentMemo.memoize lookup env' (m, parent')

and lookup_parent_gpath :
    mark_substituted:bool ->
    Env.t ->
    Odoc_model.Paths.Path.Resolved.Module.t ->
    ( Component.Signature.t * Component.Substitution.t,
      [ `Parent of parent_lookup_error ] )
    Result.result =
 fun ~mark_substituted env parent ->
  lookup_module_gpath ~mark_substituted env parent
  |> map_error (fun e -> `Parent (`Parent_module e))
  >>= fun m ->
  let m = Component.dget m in
  signature_of_module env m |> map_error (fun e -> `Parent (`Parent_sig e))
  >>= fun sg ->
  Ok
    ( sg,
      prefix_substitution
        (Cpath.Mk.Resolved.Parent.module_
           (Cpath.Mk.Resolved.Module.gpath parent))
        sg )

and lookup_type_gpath :
    Env.t ->
    Odoc_model.Paths.Path.Resolved.Type.t ->
    (Find.careful_type, simple_type_lookup_error) Result.result =
 fun env p ->
  let do_type p name =
    lookup_parent_gpath ~mark_substituted:true env p
    |> map_error (fun e -> (e :> simple_type_lookup_error))
    >>= fun (sg, sub) ->
    match Find.careful_type_in_sig sg name with
    | Some (`FClass (name, c)) -> Ok (`FClass (name, Subst.class_ sub c))
    | Some (`FClassType (name, ct)) ->
        Ok (`FClassType (name, Subst.class_type sub ct))
    | Some (`FType (name, t)) -> Ok (`FType (name, Subst.type_ sub t))
    | Some (`FType_removed (name, texpr, eq)) ->
        Ok (`FType_removed (name, Subst.type_expr sub texpr, eq))
    | None -> Error `Find_failure
  in
  let res =
    match p with
    | `Identifier { iv = `CoreType name; _ } ->
        (* CoreTypes aren't put into the environment, so they can't be handled by the
              next clause. We just look them up here in the list of core types *)
        Ok (`FType (name, List.assoc (TypeName.to_string name) core_types))
    | `Identifier ({ iv = `Type _; _ } as i) ->
        of_option ~error:(`Lookup_failureT i) (Env.(lookup_by_id s_type) i env)
        >>= fun (`Type ({ iv = `CoreType name | `Type (_, name); _ }, t)) ->
        Ok (`FType (name, t))
    | `Identifier ({ iv = `Class _; _ } as i) ->
        of_option ~error:(`Lookup_failureT i) (Env.(lookup_by_id s_class) i env)
        >>= fun (`Class ({ iv = `Class (_, name); _ }, t)) ->
        Ok (`FClass (name, t))
    | `Identifier ({ iv = `ClassType _; _ } as i) ->
        of_option ~error:(`Lookup_failureT i)
          (Env.(lookup_by_id s_class_type) i env)
        >>= fun (`ClassType ({ iv = `ClassType (_, name); _ }, t)) ->
        Ok (`FClassType (name, t))
    | `CanonicalType (t1, _) -> lookup_type_gpath env t1
    | `Type (p, id) -> do_type p (TypeName.to_string id)
    | `Class (p, id) -> do_type p (ClassName.to_string id)
    | `ClassType (p, id) -> do_type p (ClassTypeName.to_string id)
  in
  res

and lookup_class_type_gpath :
    Env.t ->
    Odoc_model.Paths.Path.Resolved.ClassType.t ->
    (Find.careful_class, simple_type_lookup_error) Result.result =
 fun env p ->
  let do_type p name =
    lookup_parent_gpath ~mark_substituted:true env p
    |> map_error (fun e -> (e :> simple_type_lookup_error))
    >>= fun (sg, sub) ->
    match Find.careful_class_in_sig sg name with
    | Some (`FClass (name, c)) -> Ok (`FClass (name, Subst.class_ sub c))
    | Some (`FClassType (name, ct)) ->
        Ok (`FClassType (name, Subst.class_type sub ct))
    | Some (`FType_removed (name, texpr, eq)) ->
        Ok (`FType_removed (name, Subst.type_expr sub texpr, eq))
    | None -> Error `Find_failure
  in
  let res =
    match p with
    | `Identifier ({ iv = `Class _; _ } as i) ->
        of_option ~error:(`Lookup_failureT i) (Env.(lookup_by_id s_class) i env)
        >>= fun (`Class ({ iv = `Class (_, name); _ }, t)) ->
        Ok (`FClass (name, t))
    | `Identifier ({ iv = `ClassType _; _ } as i) ->
        of_option ~error:(`Lookup_failureT i)
          (Env.(lookup_by_id s_class_type) i env)
        >>= fun (`ClassType ({ iv = `ClassType (_, name); _ }, t)) ->
        Ok (`FClassType (name, t))
    | `Class (p, id) -> do_type p (ClassName.to_string id)
    | `ClassType (p, id) -> do_type p (ClassTypeName.to_string id)
  in
  res

and lookup_type :
    Env.t ->
    Cpath.Resolved.type_ ->
    (Find.careful_type, simple_type_lookup_error) Result.result =
 fun env p ->
  let do_type p name =
    lookup_parent ~mark_substituted:true env p
    |> map_error (fun e -> (e :> simple_type_lookup_error))
    >>= fun (sg, sub) ->
    handle_type_lookup env name p sg >>= fun (_, t') ->
    let t =
      match t' with
      | `FClass (name, c) -> `FClass (name, Subst.class_ sub c)
      | `FClassType (name, ct) -> `FClassType (name, Subst.class_type sub ct)
      | `FType (name, t) -> `FType (name, Subst.type_ sub t)
      | `FType_removed (name, texpr, eq) ->
          `FType_removed (name, Subst.type_expr sub texpr, eq)
    in
    Ok t
  in
  let res =
    match p.v with
    | `Local id -> Error (`LocalType (env, id))
    | `Gpath p -> lookup_type_gpath env p
    | `CanonicalType (t1, _) -> lookup_type env t1
    | `Substituted s -> lookup_type env s
    | `Type (p, id) -> do_type p (TypeName.to_string id)
    | `Class (p, id) -> do_type p (ClassName.to_string id)
    | `ClassType (p, id) -> do_type p (ClassTypeName.to_string id)
  in
  res

and lookup_class_type :
    Env.t ->
    Cpath.Resolved.class_type ->
    (Find.careful_class, simple_type_lookup_error) Result.result =
 fun env p ->
  let do_type p name =
    lookup_parent ~mark_substituted:true env p
    |> map_error (fun e -> (e :> simple_type_lookup_error))
    >>= fun (sg, sub) ->
    handle_class_type_lookup name p sg >>= fun (_, t') ->
    let t =
      match t' with
      | `FClass (name, c) -> `FClass (name, Subst.class_ sub c)
      | `FClassType (name, ct) -> `FClassType (name, Subst.class_type sub ct)
      | `FType_removed (name, texpr, eq) ->
          `FType_removed (name, Subst.type_expr sub texpr, eq)
    in
    Ok t
  in
  let res =
    match p.v with
    | `Local id -> Error (`LocalType (env, (id :> Ident.path_type)))
    | `Gpath p -> lookup_class_type_gpath env p
    | `Substituted s -> lookup_class_type env s
    | `Class (p, id) -> do_type p (ClassName.to_string id)
    | `ClassType (p, id) -> do_type p (ClassTypeName.to_string id)
  in
  res

and resolve_module :
    mark_substituted:bool ->
    add_canonical:bool ->
    Env.t ->
    Cpath.module_ ->
    resolve_module_result =
 fun ~mark_substituted ~add_canonical env' path ->
  let id = (mark_substituted, add_canonical, path) in
  let resolve : Env.t -> bool * bool * Cpath.module_ -> _ =
   fun env (mark_substituted, add_canonical, p) ->
    match p.v with
    | `Dot (parent, id) ->
        resolve_module ~mark_substituted ~add_canonical env parent
        |> map_error (fun e' -> `Parent (`Parent_module e'))
        >>= fun (p, m) ->
        let m = Component.dget m in
        signature_of_module_cached env p m
        |> map_error (fun e -> `Parent (`Parent_sig e))
        >>= fun parent_sig ->
        let sub =
          prefix_substitution (Cpath.Mk.Resolved.Parent.module_ p) parent_sig
        in
        handle_module_lookup env ~add_canonical id
          (Cpath.Mk.Resolved.Parent.module_ p)
          parent_sig sub
    | `Module (rparent, id) ->
        lookup_parent ~mark_substituted env rparent
        |> map_error (fun e -> (e :> simple_module_lookup_error))
        >>= fun (parent_sig, sub) ->
        handle_module_lookup env ~add_canonical (ModuleName.to_string id)
          rparent parent_sig sub
    | `Apply (m1, m2) -> (
        let func = resolve_module ~mark_substituted ~add_canonical env m1 in
        let arg = resolve_module ~mark_substituted ~add_canonical env m2 in
        match (func, arg) with
        | Ok (func_path', m), Ok (arg_path', _) -> (
            let m = Component.dget m in
            match handle_apply ~mark_substituted env func_path' arg_path' m with
            | Ok (p, m) -> Ok (p, Component.Delayed.Val m)
            | Error e -> Error (`Parent (`Parent_expr e)))
        | _ -> Error `Unresolved_apply)
    | `Identifier (i, hidden) ->
        of_option ~error:(`Lookup_failure i) (Env.(lookup_by_id s_module) i env)
        >>= fun (`Module (_, m)) ->
        let rp =
          if hidden then
            Cpath.Mk.Resolved.Module.(hidden (gpath (`Identifier i)))
          else Cpath.Mk.Resolved.Module.gpath (`Identifier i)
        in
        Ok (process_module_path env ~add_canonical m rp, m)
    | `Local (p, _) -> Error (`Local (env, p))
    | `Resolved r -> lookup_module ~mark_substituted env r >>= fun m -> Ok (r, m)
    | `Substituted s ->
        resolve_module ~mark_substituted ~add_canonical env s
        |> map_error (fun e -> `Parent (`Parent_module e))
        >>= fun (p, m) -> Ok (Cpath.Mk.Resolved.Module.substituted p, m)
    | `Root r -> (
        match Env.lookup_root_module r env with
        | Some (Env.Resolved (_, i, m)) ->
            let rp =
              Cpath.Mk.Resolved.Module.gpath
                Odoc_model.Paths.(`Identifier (i :> Identifier.Path.Module.t))
            in
            let p =
              process_module_path env ~add_canonical (Component.Delayed.Val m)
                rp
            in
            Ok (p, Component.Delayed.Val m)
        | Some Env.Forward ->
            Error (`Parent (`Parent_sig `UnresolvedForwardPath))
        | None -> Error (`Lookup_failure_root r))
    | `Forward f ->
        resolve_module ~mark_substituted ~add_canonical env
          (Cpath.Mk.Module.root f)
        |> map_error (fun e -> `Parent (`Parent_module e))
  in
  LookupAndResolveMemo.memoize resolve env' id

and resolve_module_type :
    mark_substituted:bool ->
    add_canonical:bool ->
    Env.t ->
    Cpath.module_type ->
    resolve_module_type_result =
 fun ~mark_substituted ~add_canonical env p ->
  match p.v with
  | `Dot (parent, id) ->
      resolve_module ~mark_substituted ~add_canonical:true env parent
      |> map_error (fun e -> `Parent (`Parent_module e))
      >>= fun (p, m) ->
      let m = Component.dget m in
      signature_of_module_cached env p m
      |> map_error (fun e -> `Parent (`Parent_sig e))
      >>= fun parent_sg ->
      let sub =
        prefix_substitution (Cpath.Mk.Resolved.Parent.module_ p) parent_sg
      in
      of_option ~error:`Find_failure
        (handle_module_type_lookup env ~add_canonical id
           (Cpath.Mk.Resolved.Parent.module_ p)
           parent_sg sub)
      >>= fun (p', mt) -> Ok (p', mt)
  | `ModuleType (parent, id) ->
      lookup_parent ~mark_substituted env parent
      |> map_error (fun e -> (e :> simple_module_type_lookup_error))
      >>= fun (parent_sig, sub) ->
      handle_module_type_lookup env ~add_canonical
        (ModuleTypeName.to_string id)
        parent parent_sig sub
      |> of_option ~error:`Find_failure
  | `Identifier (i, _) ->
      of_option ~error:(`Lookup_failureMT i)
        (Env.(lookup_by_id s_module_type) i env)
      >>= fun (`ModuleType (_, mt)) ->
      let p = Cpath.Mk.Resolved.ModuleType.gpath (`Identifier i) in
      let p' = process_module_type env ~add_canonical mt p in
      Ok (p', mt)
  | `Local (l, _) -> Error (`LocalMT (env, l))
  | `Resolved r ->
      lookup_module_type ~mark_substituted env r >>= fun m -> Ok (r, m)
  | `Substituted s ->
      resolve_module_type ~mark_substituted ~add_canonical env s
      |> map_error (fun e -> `Parent (`Parent_module_type e))
      >>= fun (p, m) -> Ok (Cpath.Mk.Resolved.ModuleType.substituted p, m)

and resolve_type :
    Env.t -> add_canonical:bool -> Cpath.type_ -> resolve_type_result =
 fun env ~add_canonical p ->
  let result =
    match p.v with
    | `Dot (parent, id) ->
        resolve_module ~mark_substituted:true ~add_canonical:true env parent
        |> map_error (fun e -> `Parent (`Parent_module e))
        >>= fun (p, m) ->
        let m = Component.dget m in
        signature_of_module_cached env p m
        |> map_error (fun e -> `Parent (`Parent_sig e))
        >>= fun sg ->
        let sub = prefix_substitution (Cpath.Mk.Resolved.Parent.module_ p) sg in
        handle_type_lookup env id (Cpath.Mk.Resolved.Parent.module_ p) sg
        >>= fun (p', t') ->
        let t =
          match t' with
          | `FClass (name, c) -> `FClass (name, Subst.class_ sub c)
          | `FClassType (name, ct) -> `FClassType (name, Subst.class_type sub ct)
          | `FType (name, t) -> `FType (name, Subst.type_ sub t)
          | `FType_removed (name, texpr, eq) ->
              `FType_removed (name, Subst.type_expr sub texpr, eq)
        in
        Ok (p', t)
    | `Type (parent, id) ->
        lookup_parent ~mark_substituted:true env parent
        |> map_error (fun e -> (e :> simple_type_lookup_error))
        >>= fun (parent_sig, sub) ->
        let result =
          match Find.datatype_in_sig parent_sig (TypeName.to_string id) with
          | Some (`FType (name, t)) ->
              Some
                ( Cpath.Mk.Resolved.Type.type_ (parent, name),
                  `FType (name, Subst.type_ sub t) )
          | None -> None
        in
        of_option ~error:`Find_failure result
    | `Class (parent, id) ->
        lookup_parent ~mark_substituted:true env parent
        |> map_error (fun e -> (e :> simple_type_lookup_error))
        >>= fun (parent_sig, sub) ->
        let t =
          match Find.type_in_sig parent_sig (ClassName.to_string id) with
          | Some (`FClass (name, t)) ->
              Some
                ( Cpath.Mk.Resolved.Type.class_ (parent, name),
                  `FClass (name, Subst.class_ sub t) )
          | Some _ -> None
          | None -> None
        in
        of_option ~error:`Find_failure t
    | `ClassType (parent, id) ->
        lookup_parent ~mark_substituted:true env parent
        |> map_error (fun e -> (e :> simple_type_lookup_error))
        >>= fun (parent_sg, sub) ->
        handle_type_lookup env (ClassTypeName.to_string id) parent parent_sg
        >>= fun (p', t') ->
        let t =
          match t' with
          | `FClass (name, c) -> `FClass (name, Subst.class_ sub c)
          | `FClassType (name, ct) -> `FClassType (name, Subst.class_type sub ct)
          | `FType (name, t) -> `FType (name, Subst.type_ sub t)
          | `FType_removed (name, texpr, eq) ->
              `FType_removed (name, Subst.type_expr sub texpr, eq)
        in
        Ok (p', t)
    | `Identifier (i, _) ->
        let i' = `Identifier i in
        lookup_type env (Cpath.Mk.Resolved.Type.gpath i') >>= fun t ->
        Ok (Cpath.Mk.Resolved.Type.gpath i', t)
    | `Resolved r -> lookup_type env r >>= fun t -> Ok (r, t)
    | `Local (l, _) -> Error (`LocalType (env, l))
    | `Substituted s ->
        resolve_type env ~add_canonical s >>= fun (p, m) ->
        Ok (Cpath.Mk.Resolved.Type.substituted p, m)
  in
  result >>= fun (p, t) ->
  match t with
  | `FType (_, { canonical = Some c; _ }) ->
      if add_canonical then Ok (Cpath.Mk.Resolved.Type.canonicaltype (p, c), t)
      else result
  | _ -> result

and resolve_class_type : Env.t -> Cpath.class_type -> resolve_class_type_result
    =
 fun env p ->
  match p.v with
  | `Dot (parent, id) ->
      resolve_module ~mark_substituted:true ~add_canonical:true env parent
      |> map_error (fun e -> `Parent (`Parent_module e))
      >>= fun (p, m) ->
      let m = Component.dget m in
      signature_of_module_cached env p m
      |> map_error (fun e -> `Parent (`Parent_sig e))
      >>= fun sg ->
      let sub = prefix_substitution (Cpath.Mk.Resolved.Parent.module_ p) sg in
      handle_class_type_lookup id (Cpath.Mk.Resolved.Parent.module_ p) sg
      >>= fun (p', t') ->
      let t =
        match t' with
        | `FClass (name, c) -> `FClass (name, Subst.class_ sub c)
        | `FClassType (name, ct) -> `FClassType (name, Subst.class_type sub ct)
        | `FType_removed (name, texpr, eq) ->
            `FType_removed (name, Subst.type_expr sub texpr, eq)
      in
      Ok (p', t)
  | `Identifier (i, _) ->
      let i' = `Identifier i in

      let id = Cpath.Mk.Resolved.ClassType.gpath i' in
      lookup_class_type env id >>= fun t -> Ok (id, t)
  | `Resolved r -> lookup_class_type env r >>= fun t -> Ok (r, t)
  | `Local (l, _) -> Error (`LocalType (env, (l :> Ident.path_type)))
  | `Substituted s ->
      resolve_class_type env s >>= fun (p, m) ->
      Ok (Cpath.Mk.Resolved.ClassType.substituted p, m)
  | `Class (parent, id) ->
      lookup_parent ~mark_substituted:true env parent
      |> map_error (fun e -> (e :> simple_type_lookup_error))
      >>= fun (parent_sig, sub) ->
      let t =
        match Find.type_in_sig parent_sig (ClassName.to_string id) with
        | Some (`FClass (name, t)) ->
            Some
              ( Cpath.Mk.Resolved.ClassType.class_ (parent, name),
                `FClass (name, Subst.class_ sub t) )
        | Some _ -> None
        | None -> None
      in
      of_option ~error:`Find_failure t
  | `ClassType (parent, id) ->
      lookup_parent ~mark_substituted:true env parent
      |> map_error (fun e -> (e :> simple_type_lookup_error))
      >>= fun (parent_sg, sub) ->
      handle_class_type_lookup (ClassTypeName.to_string id) parent parent_sg
      >>= fun (p', t') ->
      let t =
        match t' with
        | `FClass (name, c) -> `FClass (name, Subst.class_ sub c)
        | `FClassType (name, ct) -> `FClassType (name, Subst.class_type sub ct)
        | `FType_removed (name, texpr, eq) ->
            `FType_removed (name, Subst.type_expr sub texpr, eq)
      in
      Ok (p', t)

and reresolve_module_gpath :
    Env.t ->
    Odoc_model.Paths.Path.Resolved.Module.t ->
    Odoc_model.Paths.Path.Resolved.Module.t =
 fun env path ->
  match path with
  | `Identifier _ -> path
  | `Apply (functor_path, argument_path) ->
      `Apply
        ( reresolve_module_gpath env functor_path,
          reresolve_module_gpath env argument_path )
  | `Module (parent, name) -> `Module (reresolve_module_gpath env parent, name)
  | `Alias (p1, `Resolved p2) ->
      `Alias
        ( reresolve_module_gpath env p1,
          `Resolved (reresolve_module_gpath env p2) )
  | `Alias (p1, p2) ->
      let dest' = reresolve_module_gpath env p1 in
      let p2' =
        if
          Odoc_model.Paths.Path.Resolved.Module.is_hidden
            ~weak_canonical_test:false dest'
        then
          let cp2 = Component.Of_Lang.(module_path (empty ()) p2) in
          match
            resolve_module env ~mark_substituted:false ~add_canonical:true cp2
          with
          | Ok (p2', _) ->
              Lang_of.(
                Path.module_ (empty ())
                  (Cpath.Mk.Module.resolved (reresolve_module env p2')))
          | Error _ -> p2
        else p2
      in
      `Alias (dest', p2')
  | `Subst (p1, p2) ->
      `Subst (reresolve_module_type_gpath env p1, reresolve_module_gpath env p2)
  | `Hidden p ->
      let p' = reresolve_module_gpath env p in
      `Hidden p'
  | `Canonical (p, (`Resolved _ as p2)) ->
      `Canonical (reresolve_module_gpath env p, p2)
  | `Canonical (p, p2) ->
      `Canonical (reresolve_module_gpath env p, handle_canonical_module env p2)
  | `OpaqueModule m -> `OpaqueModule (reresolve_module_gpath env m)

and reresolve_module : Env.t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_
    =
 fun env path ->
  let open Cpath.Mk.Resolved.Module in
  match path.v with
  | `Local _ -> path
  | `Gpath g -> gpath (reresolve_module_gpath env g)
  | `Substituted x -> substituted (reresolve_module env x)
  | `Apply (functor_path, argument_path) ->
      apply
        (reresolve_module env functor_path, reresolve_module env argument_path)
  | `Module (parent, name) -> module_ (reresolve_parent env parent, name)
  | `Alias (p1, { v = `Resolved p2; _ }, p3) ->
      alias
        ( reresolve_module env p1,
          Cpath.Mk.Module.resolved (reresolve_module env p2),
          p3 )
  | `Alias (p1, p2, p3) ->
      let dest' = reresolve_module env p1 in
      if Cpath.is_resolved_module_hidden ~weak_canonical_test:false dest' then
        match
          resolve_module env ~mark_substituted:false ~add_canonical:true p2
        with
        | Ok ({ v = `Alias (_, _, Some p3); _ }, _) -> reresolve_module env p3
        | _ -> alias (dest', p2, p3)
      else alias (dest', p2, p3)
  | `Subst (p1, p2) ->
      subst (reresolve_module_type env p1, reresolve_module env p2)
  | `Hidden p ->
      let p' = reresolve_module env p in
      hidden p'
  | `Canonical (p, (`Resolved _ as p2')) ->
      canonical (reresolve_module env p, p2')
  | `Canonical (p, p2) -> (
      match handle_canonical_module env p2 with
      | `Resolved _ as r -> canonical (p, r)
      | r -> canonical (reresolve_module env p, r))
  | `OpaqueModule m -> opaquemodule (reresolve_module env m)

and handle_canonical_module env p2 =
  let strip_alias : Cpath.Resolved.module_ -> Cpath.Resolved.module_ =
   fun x -> match x.v with `Alias (_, _, Some p) -> p | _ -> x
  in
  let resolve env p =
    resolve_module env ~mark_substituted:false ~add_canonical:false p
    >>= fun (p, m) -> Ok (strip_alias p, m)
  in
  let lang_of cpath =
    (Lang_of.(Path.resolved_module (empty ()) cpath) :> RP.t)
  in
  let cp2 = Component.Of_Lang.(module_path (empty ()) p2) in
  match canonical_helper env resolve lang_of c_mod_poss cp2 with
  | None -> p2
  | Some (rp, dm) ->
      let m = Component.dget dm in
      (* Need to check if the module we're going to link to has been expanded.
         ModuleTypes are always expanded if possible, but Aliases are only expanded
         if they're an alias to a hidden module or if they're self canonical.

         Checking if a module is self canonical is a bit tricky, since this function
         is itself part of the process of resolving any canonical reference. Hence
         what we do here is to look through alias chains looking for one that's marked
         with the same _unresolved_ canonical path that we're currently trying to resolve.

         This is particularly important because some modules don't know they're canonical!
         For example the module Caml in base, which is marked as the canonical path for
         all references to the standard library in the file [import0.ml], but is itself just
         defined by including [Stdlib].

         If a module doesn't know it's canonical, it will fail the self-canonical check, and
         therefore not necessarily be expanded. If this happens, we call [process_module_path]
         to stick the [`Alias] constructor back on so we'll link to the correct place. *)
      let expanded =
        match m.type_ with
        | Component.Module.Alias (_, Some _) -> true
        | Alias ({ v = `Resolved p; _ }, None) ->
            (* we're an alias - check to see if we're marked as the canonical path.
               If not, check for an alias chain with us as canonical in it... *)
            let rec check m =
              match m.Component.Module.canonical with
              | Some p ->
                  p = p2
                  (* The canonical path is the same one we're trying to resolve *)
              | None -> (
                  match m.type_ with
                  | Component.Module.Alias ({ v = `Resolved p; _ }, _) -> (
                      match lookup_module ~mark_substituted:false env p with
                      | Error _ -> false
                      | Ok m ->
                          let m = Component.dget m in
                          check m)
                  | _ -> false)
            in
            let self_canonical () = check m in
            let hidden =
              Cpath.is_resolved_module_hidden ~weak_canonical_test:true p
            in
            hidden || self_canonical ()
        | Alias (_, _) -> false
        | ModuleType _ -> true
      in
      let cpath =
        if expanded then rp
        else process_module_path env ~add_canonical:false dm rp
      in
      Lang_of.(Path.module_ (empty ()) (Cpath.Mk.Module.resolved cpath))

and handle_canonical_module_type env p2 =
  let cp2 = Component.Of_Lang.(module_type_path (empty ()) p2) in
  let strip_alias : Cpath.Resolved.module_type -> Cpath.Resolved.module_type =
   fun x -> match x.v with `AliasModuleType (_, p) -> p | _ -> x
  in
  let resolve env p =
    resolve_module_type env ~mark_substituted:false ~add_canonical:false p
    >>= fun (p, m) -> Ok (strip_alias p, m)
  in
  let lang_of cpath =
    (Lang_of.(Path.resolved_module_type (empty ()) cpath) :> RP.t)
  in
  match canonical_helper env resolve lang_of c_modty_poss cp2 with
  | None -> p2
  | Some (rp, _) ->
      Lang_of.(Path.module_type (empty ()) (Cpath.Mk.ModuleType.resolved rp))

and handle_canonical_type env p2 =
  let cp2 = Component.Of_Lang.(type_path (empty ()) p2) in
  let lang_of cpath = (Lang_of.(Path.resolved_type (empty ()) cpath) :> RP.t) in
  let resolve env p =
    match resolve_type env ~add_canonical:false p with
    | Ok (_, `FType_removed _) -> Error `Find_failure
    | Ok (x, y) -> Ok (x, y)
    | Error y -> Error y
  in
  match canonical_helper env resolve lang_of c_ty_poss cp2 with
  | None -> p2
  | Some (rp, _) -> Lang_of.(Path.type_ (empty ()) (Cpath.Mk.Type.resolved rp))

and reresolve_module_type_gpath :
    Env.t ->
    Odoc_model.Paths.Path.Resolved.ModuleType.t ->
    Odoc_model.Paths.Path.Resolved.ModuleType.t =
 fun env path ->
  match path with
  | `Identifier _ -> path
  | `ModuleType (parent, name) ->
      `ModuleType (reresolve_module_gpath env parent, name)
  | `CanonicalModuleType (p1, (`Resolved _ as p2)) ->
      `CanonicalModuleType (reresolve_module_type_gpath env p1, p2)
  | `CanonicalModuleType (p1, p2) ->
      `CanonicalModuleType
        (reresolve_module_type_gpath env p1, handle_canonical_module_type env p2)
  | `SubstT (p1, p2) ->
      `SubstT
        (reresolve_module_type_gpath env p1, reresolve_module_type_gpath env p2)
  | `AliasModuleType (p1, p2) ->
      `AliasModuleType
        (reresolve_module_type_gpath env p1, reresolve_module_type_gpath env p2)
  | `OpaqueModuleType m -> `OpaqueModuleType (reresolve_module_type_gpath env m)

and reresolve_module_type :
    Env.t -> Cpath.Resolved.module_type -> Cpath.Resolved.module_type =
 fun env path ->
  let open Cpath.Mk.Resolved.ModuleType in
  match path.v with
  | `Local _ -> path
  | `Gpath g -> gpath (reresolve_module_type_gpath env g)
  | `Substituted x -> substituted (reresolve_module_type env x)
  | `ModuleType (parent, name) -> module_type (reresolve_parent env parent, name)
  | `CanonicalModuleType (p1, (`Resolved _ as p2')) ->
      canonicalmoduletype (reresolve_module_type env p1, p2')
  | `CanonicalModuleType (p1, p2) ->
      canonicalmoduletype
        (reresolve_module_type env p1, handle_canonical_module_type env p2)
  | `SubstT (p1, p2) ->
      substt (reresolve_module_type env p1, reresolve_module_type env p2)
  | `AliasModuleType (p1, p2) ->
      aliasmoduletype
        (reresolve_module_type env p1, reresolve_module_type env p2)
  | `OpaqueModuleType m -> opaquemoduletype (reresolve_module_type env m)

and reresolve_type : Env.t -> Cpath.Resolved.type_ -> Cpath.Resolved.type_ =
 fun env path ->
  let open Cpath.Mk.Resolved.Type in
  let result =
    match path.v with
    | `Gpath _ | `Local _ -> path
    | `Substituted s -> substituted (reresolve_type env s)
    | `CanonicalType (p1, p2) ->
        canonicaltype (reresolve_type env p1, handle_canonical_type env p2)
    | `Type (p, n) -> type_ (reresolve_parent env p, n)
    | `Class (p, n) -> class_ (reresolve_parent env p, n)
    | `ClassType (p, n) -> class_type (reresolve_parent env p, n)
  in
  result

and reresolve_class_type :
    Env.t -> Cpath.Resolved.class_type -> Cpath.Resolved.class_type =
 fun env path ->
  let open Cpath.Mk.Resolved.ClassType in
  let result =
    match path.v with
    | `Gpath _ | `Local _ -> path
    | `Substituted s -> substituted (reresolve_class_type env s)
    | `Class (p, n) -> class_ (reresolve_parent env p, n)
    | `ClassType (p, n) -> class_type (reresolve_parent env p, n)
  in
  result

and reresolve_parent : Env.t -> Cpath.Resolved.parent -> Cpath.Resolved.parent =
 fun env path ->
  let open Cpath.Mk.Resolved.Parent in
  match path.v with
  | `Module m -> module_ (reresolve_module env m)
  | `ModuleType mty -> module_type (reresolve_module_type env mty)
  | `FragmentRoot -> path

(* *)
and module_type_expr_of_module_decl :
    Env.t ->
    Component.Module.decl ->
    ( Component.ModuleType.expr,
      simple_module_type_expr_of_module_error )
    Result.result =
 fun env decl ->
  match decl with
  | Component.Module.Alias ({ v = `Resolved r; _ }, _) ->
      lookup_module ~mark_substituted:false env r
      |> map_error (fun e -> `Parent (`Parent_module e))
      >>= fun m ->
      let m = Component.dget m in
      module_type_expr_of_module_decl env m.type_
  | Component.Module.Alias (path, _) -> (
      match
        resolve_module ~mark_substituted:false ~add_canonical:true env path
      with
      | Ok (_, m) ->
          let m = Component.dget m in
          module_type_expr_of_module env m
      | Error _ when Cpath.is_module_forward path ->
          Error `UnresolvedForwardPath
      | Error e -> Error (`UnresolvedPath (`Module (path, e))))
  | Component.Module.ModuleType expr -> Ok expr

and module_type_expr_of_module :
    Env.t ->
    Component.Module.t ->
    ( Component.ModuleType.expr,
      simple_module_type_expr_of_module_error )
    Result.result =
 fun env m -> module_type_expr_of_module_decl env m.type_

and signature_of_module_path :
    Env.t ->
    strengthen:bool ->
    Cpath.module_ ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun env ~strengthen path ->
  match resolve_module ~mark_substituted:true ~add_canonical:true env path with
  | Ok (p', m) ->
      let m = Component.dget m in
      (* p' is the path to the aliased module *)
      let strengthen =
        strengthen
        && not (Cpath.is_resolved_module_hidden ~weak_canonical_test:true p')
      in
      signature_of_module_cached env p' m >>= fun sg ->
      if strengthen then
        Ok (Strengthen.signature (Cpath.Mk.Module.resolved p') sg)
      else Ok sg
  | Error _ when Cpath.is_module_forward path -> Error `UnresolvedForwardPath
  | Error e -> Error (`UnresolvedPath (`Module (path, e)))

and handle_signature_with_subs :
    mark_substituted:bool ->
    Env.t ->
    Component.Signature.t ->
    Component.ModuleType.substitution list ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun ~mark_substituted env sg subs ->
  let open ResultMonad in
  List.fold_left
    (fun sg_opt sub ->
      sg_opt >>= fun sg -> fragmap ~mark_substituted env sub sg)
    (Ok sg) subs

and signature_of_u_module_type_expr :
    mark_substituted:bool ->
    Env.t ->
    Component.ModuleType.U.expr ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun ~mark_substituted env m ->
  match m with
  | Component.ModuleType.U.Path p -> (
      match resolve_module_type ~mark_substituted ~add_canonical:true env p with
      | Ok (_, mt) -> signature_of_module_type env mt
      | Error e -> Error (`UnresolvedPath (`ModuleType (p, e))))
  | Signature s -> Ok s
  | With (subs, s) ->
      signature_of_u_module_type_expr ~mark_substituted env s >>= fun sg ->
      handle_signature_with_subs ~mark_substituted env sg subs
  | TypeOf { t_expansion = Some (Signature sg); _ } -> Ok sg
  | TypeOf { t_desc; _ } -> Error (`UnexpandedTypeOf t_desc)

and signature_of_simple_expansion :
    Component.ModuleType.simple_expansion -> Component.Signature.t = function
  | Signature sg -> sg
  | Functor (_, e) -> signature_of_simple_expansion e

and signature_of_module_type_expr :
    mark_substituted:bool ->
    Env.t ->
    Component.ModuleType.expr ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun ~mark_substituted env m ->
  match m with
  | Component.ModuleType.Path { p_expansion = Some e; _ } ->
      Ok (signature_of_simple_expansion e)
  | Component.ModuleType.Path { p_path; _ } -> (
      match
        resolve_module_type ~mark_substituted ~add_canonical:true env p_path
      with
      | Ok (_, mt) -> signature_of_module_type env mt
      | Error e -> Error (`UnresolvedPath (`ModuleType (p_path, e))))
  | Component.ModuleType.Signature s -> Ok s
  (* | Component.ModuleType.With { w_expansion = Some e; _ } ->
      Ok (signature_of_simple_expansion e)

      Recalculate 'With' expressions always, as we need to know which
      items have been removed
  *)
  | Component.ModuleType.With { w_substitutions; w_expr; _ } ->
      signature_of_u_module_type_expr ~mark_substituted env w_expr >>= fun sg ->
      handle_signature_with_subs ~mark_substituted env sg w_substitutions
  | Component.ModuleType.Functor (Unit, expr) ->
      signature_of_module_type_expr ~mark_substituted env expr
  | Component.ModuleType.Functor (Named arg, expr) ->
      ignore arg;
      signature_of_module_type_expr ~mark_substituted env expr
  | Component.ModuleType.TypeOf { t_expansion = Some e; _ } ->
      Ok (signature_of_simple_expansion e)
  | Component.ModuleType.TypeOf { t_desc; _ } ->
      Error (`UnexpandedTypeOf t_desc)

and signature_of_module_type :
    Env.t ->
    Component.ModuleType.t ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun env m ->
  match m.expr with
  | None -> Error `OpaqueModule
  | Some expr -> signature_of_module_type_expr ~mark_substituted:false env expr

and signature_of_module_decl :
    Env.t ->
    Component.Module.decl ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun env decl ->
  match decl with
  | Component.Module.Alias (_, Some e) -> Ok (signature_of_simple_expansion e)
  | Component.Module.Alias (p, _) ->
      signature_of_module_path env ~strengthen:true p
  | Component.Module.ModuleType expr ->
      signature_of_module_type_expr ~mark_substituted:false env expr

and signature_of_module :
    Env.t ->
    Component.Module.t ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun env m -> signature_of_module_decl env m.type_

and signature_of_module_cached :
    Env.t ->
    Cpath.Resolved.module_ ->
    Component.Module.t ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun env' path m ->
  let id = path in
  let run env _id = signature_of_module env m in
  SignatureOfModuleMemo.memoize run env' id

and umty_of_mty : Component.ModuleType.expr -> Component.ModuleType.U.expr =
  function
  | Signature sg -> Signature sg
  | Path { p_path; _ } -> Path p_path
  | TypeOf t -> TypeOf t
  | With { w_substitutions; w_expr; _ } -> With (w_substitutions, w_expr)
  | Functor _ -> assert false

and fragmap :
    mark_substituted:bool ->
    Env.t ->
    Component.ModuleType.substitution ->
    Component.Signature.t ->
    (Component.Signature.t, signature_of_module_error) Result.result =
 fun ~mark_substituted env sub sg ->
  (* Used when we haven't finished the substitution. For example, if the
     substitution is `M.t = u`, this function is used to map the declaration
     of `M` to be `M : ... with type t = u` *)
  let map_module_decl decl subst =
    let open Component.Module in
    match decl with
    | Alias (path, _) ->
        signature_of_module_path env ~strengthen:true path >>= fun sg ->
        Ok
          (ModuleType
             (With
                {
                  w_substitutions = [ subst ];
                  w_expansion = None;
                  w_expr =
                    TypeOf
                      {
                        t_desc = StructInclude path;
                        t_expansion = Some (Signature sg);
                      };
                }))
    | ModuleType mty' ->
        Ok
          (ModuleType
             (With
                {
                  w_substitutions = [ subst ];
                  w_expansion = None;
                  w_expr = umty_of_mty mty';
                }))
  in
  let map_include_decl decl subst =
    let open Component.Include in
    match decl with
    | Alias p ->
        signature_of_module_path env ~strengthen:true p >>= fun sg ->
        fragmap ~mark_substituted env subst sg >>= fun sg ->
        Ok (ModuleType (Signature sg))
    | ModuleType mty' -> Ok (ModuleType (With ([ subst ], mty')))
  in
  let map_module m new_subst =
    let open Component.Module in
    map_module_decl m.type_ new_subst >>= fun type_ ->
    Ok (Left { m with type_ })
  in
  let rec map_signature map items =
    List.fold_right
      (fun item acc ->
        acc >>= fun (items, handled, subbed_modules, removed) ->
        match (item, map) with
        | Component.Signature.Type (id, r, t), { type_ = Some (id', fn); _ }
          when Ident.Name.type_ id = id' -> (
            fn (Component.dget t) >>= function
            | Left x ->
                Ok
                  ( Component.Signature.Type (id, r, Component.Delayed.Val x)
                    :: items,
                    true,
                    subbed_modules,
                    removed )
            | Right (texpr, eq) ->
                Ok
                  ( items,
                    true,
                    subbed_modules,
                    Component.Signature.RType (id, texpr, eq) :: removed ))
        | Component.Signature.Module (id, r, m), { module_ = Some (id', fn); _ }
          when Ident.Name.module_ id = id' -> (
            fn (Component.dget m) >>= function
            | Left x ->
                Ok
                  ( Component.Signature.Module (id, r, Component.Delayed.Val x)
                    :: items,
                    true,
                    id :: subbed_modules,
                    removed )
            | Right y ->
                Ok
                  ( items,
                    true,
                    subbed_modules,
                    Component.Signature.RModule (id, y) :: removed ))
        | Component.Signature.Include ({ expansion_; _ } as i), _ ->
            map_signature map expansion_.items
            >>= fun (items', handled', subbed_modules', removed') ->
            let component =
              if handled' then
                map_include_decl i.decl sub >>= fun decl ->
                let expansion_ =
                  Component.Signature.
                    {
                      expansion_ with
                      items = items';
                      removed = removed';
                      compiled = false;
                    }
                in
                Ok
                  (Component.Signature.Include
                     { i with decl; expansion_; strengthened = None })
              else Ok item
            in
            component >>= fun c ->
            Ok
              ( c :: items,
                handled' || handled,
                subbed_modules' @ subbed_modules,
                removed' @ removed )
        | ( Component.Signature.ModuleType (id, mt),
            { module_type = Some (id', fn); _ } )
          when Ident.Name.module_type id = id' -> (
            fn (Component.dget mt) >>= function
            | Left x ->
                Ok
                  ( Component.Signature.ModuleType (id, Component.Delayed.Val x)
                    :: items,
                    true,
                    subbed_modules,
                    removed )
            | Right y ->
                Ok
                  ( items,
                    true,
                    subbed_modules,
                    Component.Signature.RModuleType (id, y) :: removed ))
        | x, _ -> Ok (x :: items, handled, subbed_modules, removed))
      items
      (Ok ([], false, [], []))
  in
  let handle_intermediate name new_subst =
    let modmaps = Some (name, fun m -> map_module m new_subst) in
    map_signature { id_map with module_ = modmaps } sg.items
  in
  let new_sg =
    match sub with
    | ModuleEq (frag, type_) -> (
        match Cfrag.module_split frag with
        | name, Some frag' ->
            let new_subst = Component.ModuleType.ModuleEq (frag', type_) in
            handle_intermediate name new_subst
        | name, None ->
            let mapfn m =
              let type_ =
                let open Component.Module in
                match type_ with
                | Alias ({ v = `Resolved p; _ }, _) ->
                    let new_p =
                      if mark_substituted then
                        Cpath.Mk.Resolved.Module.substituted p
                      else p
                    in
                    Alias (Cpath.Mk.Module.resolved new_p, None)
                | Alias _ | ModuleType _ -> type_
              in
              Ok (Left { m with Component.Module.type_ })
            in
            map_signature { id_map with module_ = Some (name, mapfn) } sg.items)
    | ModuleSubst (frag, p) -> (
        match Cfrag.module_split frag with
        | name, Some frag' ->
            let new_subst = Component.ModuleType.ModuleSubst (frag', p) in
            handle_intermediate name new_subst
        | name, None ->
            let mapfn _ =
              match
                resolve_module ~mark_substituted ~add_canonical:false env p
              with
              | Ok (p, _) -> Ok (Right p)
              | Error e ->
                  Format.fprintf Format.err_formatter
                    "failed to resolve path: %a\n%!" Component.Fmt.module_path p;
                  Error (`UnresolvedPath (`Module (p, e)))
            in
            map_signature { id_map with module_ = Some (name, mapfn) } sg.items)
    | ModuleTypeEq (frag, mtye) -> (
        match Cfrag.module_type_split frag with
        | name, Some frag' ->
            let new_subst = Component.ModuleType.ModuleTypeEq (frag', mtye) in
            handle_intermediate name new_subst
        | name, None ->
            let mapfn t =
              Ok (Left { t with Component.ModuleType.expr = Some mtye })
            in
            map_signature
              { id_map with module_type = Some (name, mapfn) }
              sg.items)
    | ModuleTypeSubst (frag, mtye) -> (
        match Cfrag.module_type_split frag with
        | name, Some frag' ->
            let new_subst =
              Component.ModuleType.ModuleTypeSubst (frag', mtye)
            in
            handle_intermediate name new_subst
        | name, None ->
            let mapfn _t = Ok (Right mtye) in
            map_signature
              { id_map with module_type = Some (name, mapfn) }
              sg.items)
    | TypeEq (frag, equation) -> (
        match Cfrag.type_split frag with
        | name, Some frag' ->
            let new_subst = Component.ModuleType.TypeEq (frag', equation) in
            handle_intermediate name new_subst
        | name, None ->
            let mapfn t = Ok (Left { t with Component.TypeDecl.equation }) in
            map_signature { id_map with type_ = Some (name, mapfn) } sg.items)
    | TypeSubst
        ( frag,
          ({ Component.TypeDecl.Equation.manifest = Some x; _ } as equation) )
      -> (
        match Cfrag.type_split frag with
        | name, Some frag' ->
            let new_subst = Component.ModuleType.TypeSubst (frag', equation) in
            handle_intermediate name new_subst
        | name, None ->
            let mapfn _t = Ok (Right (x, equation)) in
            map_signature { id_map with type_ = Some (name, mapfn) } sg.items)
    | TypeSubst (_, { Component.TypeDecl.Equation.manifest = None; _ }) ->
        failwith "Unhandled condition: TypeSubst with no manifest"
  in
  new_sg >>= fun (items, _handled, subbed_modules, removed) ->
  let sub_of_removed removed sub =
    match removed with
    | Component.Signature.RModule (id, p) ->
        Subst.add_module
          (id :> Ident.path_module)
          (Cpath.Mk.Module.resolved p)
          p sub
    | Component.Signature.RType (id, r_texpr, r_eq) ->
        Subst.add_type_replacement (id :> Ident.path_type) r_texpr r_eq sub
    | Component.Signature.RModuleType (id, e) ->
        Subst.add_module_type_replacement (id :> Ident.module_type) e sub
  in

  let sub = List.fold_right sub_of_removed removed Subst.identity in

  let map_items items =
    (* Invalidate resolved paths containing substituted idents - See the `With11`
       test for an example of why this is necessary *)
    let sub_of_substituted x sub =
      let x = (x :> Ident.path_module) in
      (if mark_substituted then Subst.add_module_substitution x sub else sub)
      |> Subst.path_invalidate_module x
      |> Subst.mto_invalidate_module x
    in

    let substituted_sub =
      List.fold_right sub_of_substituted subbed_modules Subst.identity
    in
    (* Need to call `apply_sig_map` directly as we're substituting for an item
       that's declared within the signature *)
    let items, _, _ = Subst.apply_sig_map substituted_sub items [] in
    (* Finished marking substituted stuff *)
    items
  in

  let items = map_items items in

  let res =
    Subst.signature sub
      {
        Component.Signature.items;
        removed = removed @ sg.removed;
        compiled = false;
        doc = sg.doc;
      }
  in
  Ok res

and find_external_module_path :
    Cpath.Resolved.module_ -> Cpath.Resolved.module_ option =
 fun p ->
  let open OptionMonad in
  let module M = Cpath.Mk.Resolved.Module in
  match p.v with
  | `Subst (x, y) ->
      find_external_module_type_path x >>= fun x ->
      find_external_module_path y >>= fun y -> Some (M.subst (x, y))
  | `Module (p, n) ->
      find_external_parent_path p >>= fun p -> Some (M.module_ (p, n))
  | `Local x -> Some (M.local x)
  | `Substituted x ->
      find_external_module_path x >>= fun x -> Some (M.substituted x)
  | `Canonical (x, y) ->
      find_external_module_path x >>= fun x -> Some (M.canonical (x, y))
  | `Hidden x -> find_external_module_path x >>= fun x -> Some (M.hidden x)
  | `Alias _ -> None
  | `Apply (x, y) ->
      find_external_module_path x >>= fun x ->
      find_external_module_path y >>= fun y -> Some (M.apply (x, y))
  | `Gpath x -> Some (M.gpath x)
  | `OpaqueModule m ->
      find_external_module_path m >>= fun x -> Some (M.opaquemodule x)

and find_external_module_type_path :
    Cpath.Resolved.module_type -> Cpath.Resolved.module_type option =
 fun p ->
  let open OptionMonad in
  let module M = Cpath.Mk.Resolved.ModuleType in
  match p.v with
  | `ModuleType (p, name) ->
      find_external_parent_path p >>= fun p -> Some (M.module_type (p, name))
  | `Local _ -> Some p
  | `SubstT (x, y) ->
      find_external_module_type_path x >>= fun x ->
      find_external_module_type_path y >>= fun y -> Some (M.substt (x, y))
  | `CanonicalModuleType (x, _) | `Substituted x ->
      find_external_module_type_path x >>= fun x -> Some (M.substituted x)
  | `Gpath _ -> Some p
  | `AliasModuleType (x, y) -> (
      match
        (find_external_module_type_path x, find_external_module_type_path y)
      with
      | Some x, Some y -> Some (M.aliasmoduletype (x, y))
      | Some x, None -> Some x
      | None, Some x -> Some x
      | None, None -> None)
  | `OpaqueModuleType m ->
      find_external_module_type_path m >>= fun x -> Some (M.opaquemoduletype x)

and find_external_parent_path :
    Cpath.Resolved.parent -> Cpath.Resolved.parent option =
 fun p ->
  let module M = Cpath.Mk.Resolved.Parent in
  let open OptionMonad in
  match p.v with
  | `Module m -> find_external_module_path m >>= fun m -> Some (M.module_ m)
  | `ModuleType m ->
      find_external_module_type_path m >>= fun m -> Some (M.module_type m)
  | `FragmentRoot -> None

and fixup_module_cfrag (f : Cfrag.resolved_module) : Cfrag.resolved_module =
  match f with
  | `Subst (path, frag) -> (
      match find_external_module_type_path path with
      | Some p -> `Subst (p, frag)
      | None -> frag)
  | `Alias (path, frag) -> (
      match find_external_module_path path with
      | Some p -> `Alias (p, frag)
      | None -> frag)
  | `Module (parent, name) -> `Module (fixup_signature_cfrag parent, name)
  | `OpaqueModule m -> `OpaqueModule (fixup_module_cfrag m)

and fixup_module_type_cfrag (f : Cfrag.resolved_module_type) :
    Cfrag.resolved_module_type =
  match f with
  | `ModuleType (parent, name) ->
      `ModuleType (fixup_signature_cfrag parent, name)

and fixup_signature_cfrag (f : Cfrag.resolved_signature) =
  match f with
  | `Root x -> `Root x
  | (`OpaqueModule _ | `Subst _ | `Alias _ | `Module _) as f ->
      (fixup_module_cfrag f :> Cfrag.resolved_signature)

and fixup_type_cfrag (f : Cfrag.resolved_type) : Cfrag.resolved_type =
  match f with
  | `Type (p, x) -> `Type (fixup_signature_cfrag p, x)
  | `Class (p, x) -> `Class (fixup_signature_cfrag p, x)
  | `ClassType (p, x) -> `ClassType (fixup_signature_cfrag p, x)

and find_module_with_replacement :
    Env.t ->
    Component.Signature.t ->
    string ->
    ( Component.Module.t Component.Delayed.t,
      simple_module_lookup_error )
    Result.result =
 fun env sg name ->
  match Find.careful_module_in_sig sg name with
  | Some (`FModule (_, m)) -> Ok (Component.Delayed.Val m)
  | Some (`FModule_removed path) ->
      lookup_module ~mark_substituted:false env path
  | None -> Error `Find_failure

and find_module_type_with_replacement :
    Env.t ->
    Component.Signature.t ->
    string ->
    ( Component.ModuleType.t Component.Delayed.t,
      simple_module_type_lookup_error )
    Result.result =
 fun _env sg name ->
  match Find.careful_module_type_in_sig sg name with
  | Some (`FModuleType (_, m)) -> Ok (Component.Delayed.Val m)
  | None -> Error `Find_failure
  | Some (`FModuleType_removed _mty) -> Error `Find_failure

and resolve_signature_fragment :
    Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.signature ->
    (Cfrag.resolved_signature * Cpath.Resolved.parent * Component.Signature.t)
    option =
 fun env (p, sg) frag ->
  match frag with
  | `Root ->
      let sg = prefix_signature (Cpath.Mk.Resolved.Parent.fragmentroot, sg) in
      Some (`Root p, Cpath.Mk.Resolved.Parent.fragmentroot, sg)
  | `Resolved _r -> None
  | `Dot (parent, name) ->
      let open OptionMonad in
      resolve_signature_fragment env (p, sg) parent
      >>= fun (pfrag, ppath, sg) ->
      of_result (find_module_with_replacement env sg name) >>= fun m' ->
      let mname = ModuleName.make_std name in
      let new_path = Cpath.Mk.Resolved.Module.module_ (ppath, mname) in
      let new_frag = `Module (pfrag, mname) in
      let m' = Component.dget m' in
      let modifier = get_module_path_modifiers env ~add_canonical:false m' in
      let cp', f' =
        match modifier with
        | None -> (new_path, new_frag)
        | Some (`Aliased p') ->
            ( Cpath.Mk.Resolved.Module.alias
                (p', Cpath.Mk.Module.resolved new_path, None),
              `Alias (p', new_frag) )
        | Some (`SubstMT p') ->
            ( Cpath.Mk.Resolved.Module.subst (p', new_path),
              `Subst (p', new_frag) )
      in
      (* Don't use the cached one - `FragmentRoot` is not unique *)
      of_result (signature_of_module env m') >>= fun parent_sg ->
      let sg =
        prefix_signature (Cpath.Mk.Resolved.Parent.module_ cp', parent_sg)
      in
      Some (f', Cpath.Mk.Resolved.Parent.module_ cp', sg)

and resolve_module_fragment :
    Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.module_ ->
    Cfrag.resolved_module option =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r -> Some r
  | `Dot (parent, name) ->
      let open OptionMonad in
      resolve_signature_fragment env (p, sg) parent
      >>= fun (pfrag, _ppath, sg) ->
      of_result (find_module_with_replacement env sg name) >>= fun m' ->
      let mname = ModuleName.make_std name in
      let new_frag = `Module (pfrag, mname) in
      let m' = Component.dget m' in
      let modifier = get_module_path_modifiers env ~add_canonical:false m' in
      let f' =
        match modifier with
        | None -> new_frag
        | Some (`Aliased p') -> `Alias (p', new_frag)
        | Some (`SubstMT p') -> `Subst (p', new_frag)
      in
      let f'' =
        match signature_of_module env m' with
        | Ok (_m : Component.Signature.t) -> f'
        | Error `OpaqueModule -> `OpaqueModule f'
        | Error (`UnresolvedForwardPath | `UnresolvedPath _) -> f'
        | Error (`UnexpandedTypeOf _) -> f'
      in
      Some (fixup_module_cfrag f'')

and resolve_module_type_fragment :
    Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.module_type ->
    Cfrag.resolved_module_type option =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r -> Some r
  | `Dot (parent, name) ->
      let open OptionMonad in
      resolve_signature_fragment env (p, sg) parent
      >>= fun (pfrag, _ppath, sg) ->
      of_result (find_module_type_with_replacement env sg name) >>= fun mt' ->
      let mtname = ModuleTypeName.make_std name in
      let f' = `ModuleType (pfrag, mtname) in
      let m' = Component.dget mt' in
      let f'' =
        match signature_of_module_type env m' with
        | Ok (_m : Component.Signature.t) -> f'
        | Error
            ( `UnresolvedForwardPath | `UnresolvedPath _ | `OpaqueModule
            | `UnexpandedTypeOf _ ) ->
            f'
      in
      Some (fixup_module_type_cfrag f'')

and resolve_type_fragment :
    Env.t ->
    Cfrag.root * Component.Signature.t ->
    Cfrag.type_ ->
    Cfrag.resolved_type option =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r -> Some r
  | `Dot (parent, name) ->
      let open OptionMonad in
      resolve_signature_fragment env (p, sg) parent
      >>= fun (pfrag, _ppath, _sg) ->
      let result = fixup_type_cfrag (`Type (pfrag, TypeName.make_std name)) in
      Some result

let rec reresolve_signature_fragment :
    Env.t -> Cfrag.resolved_signature -> Cfrag.resolved_signature =
 fun env m ->
  match m with
  | `Root (`ModuleType p) -> `Root (`ModuleType (reresolve_module_type env p))
  | `Root (`Module p) -> `Root (`Module (reresolve_module env p))
  | (`OpaqueModule _ | `Subst _ | `Alias _ | `Module _) as x ->
      (reresolve_module_fragment env x :> Cfrag.resolved_signature)

and reresolve_module_fragment :
    Env.t -> Cfrag.resolved_module -> Cfrag.resolved_module =
 fun env m ->
  match m with
  | `Subst (p, f) ->
      let p' = reresolve_module_type env p in
      `Subst (p', reresolve_module_fragment env f)
  | `Alias (p, f) ->
      let p' = reresolve_module env p in
      `Alias (p', reresolve_module_fragment env f)
  | `OpaqueModule m -> `OpaqueModule (reresolve_module_fragment env m)
  | `Module (sg, m) -> `Module (reresolve_signature_fragment env sg, m)

and reresolve_type_fragment :
    Env.t -> Cfrag.resolved_type -> Cfrag.resolved_type =
 fun env m ->
  match m with
  | `Type (p, n) -> `Type (reresolve_signature_fragment env p, n)
  | `ClassType (p, n) -> `ClassType (reresolve_signature_fragment env p, n)
  | `Class (p, n) -> `Class (reresolve_signature_fragment env p, n)

and reresolve_module_type_fragment :
    Env.t -> Cfrag.resolved_module_type -> Cfrag.resolved_module_type =
 fun env m ->
  match m with
  | `ModuleType (p, n) -> `ModuleType (reresolve_signature_fragment env p, n)

let rec class_signature_of_class :
    Env.t -> Component.Class.t -> Component.ClassSignature.t option =
 fun env c ->
  let rec inner decl =
    match decl with
    | Component.Class.ClassType e -> class_signature_of_class_type_expr env e
    | Arrow (_, _, d) -> inner d
  in
  inner c.type_

and class_signature_of_class_type_expr :
    Env.t -> Component.ClassType.expr -> Component.ClassSignature.t option =
 fun env e ->
  match e with
  | Signature s -> Some s
  | Constr (p, _) -> (
      match resolve_type env ~add_canonical:true (p :> Cpath.type_) with
      | Ok (_, `FClass (_, c)) -> class_signature_of_class env c
      | Ok (_, `FClassType (_, c)) -> class_signature_of_class_type env c
      | _ -> None)

and class_signature_of_class_type :
    Env.t -> Component.ClassType.t -> Component.ClassSignature.t option =
 fun env c -> class_signature_of_class_type_expr env c.expr

let resolve_module_path env p =
  resolve_module ~mark_substituted:true ~add_canonical:true env p
  >>= fun (p, m) ->
  match p.v with
  | `Gpath (`Identifier { iv = `Root _; _ })
  | `Hidden { v = `Gpath (`Identifier { iv = `Root _; _ }); _ } ->
      Ok p
  | _ -> (
      let m = Component.dget m in
      match signature_of_module_cached env p m with
      | Ok _ -> Ok p
      | Error `OpaqueModule -> Ok (Cpath.Mk.Resolved.Module.opaquemodule p)
      | Error (`UnresolvedForwardPath | `UnresolvedPath _) -> Ok p
      | Error (`UnexpandedTypeOf _) -> Ok p)

let resolve_module_type_path env p =
  resolve_module_type ~mark_substituted:true ~add_canonical:true env p
  >>= fun (p, mt) ->
  match signature_of_module_type env mt with
  | Ok _ -> Ok p
  | Error `OpaqueModule -> Ok (Cpath.Mk.Resolved.ModuleType.opaquemoduletype p)
  | Error (`UnresolvedForwardPath | `UnresolvedPath _)
  | Error (`UnexpandedTypeOf _) ->
      Ok p

let resolve_type_path env p =
  resolve_type env ~add_canonical:true p >>= fun (p, _) -> Ok p

let resolve_class_type_path env p =
  resolve_class_type env p >>= fun (p, _) -> Ok p
