open Odoc_model.Names
open Odoc_model.Paths

module OptionMonad = struct
  type 'a t = 'a option

  let return x = Some x

  let bind m f = match m with Some x -> f x | None -> None

  let ( >>= ) = bind
end

module ResultMonad = struct
  type ('a, 'b) t = Resolved of 'a | Unresolved of 'b

  let return x = Resolved x

  let bind m f =
    match m with Resolved x -> f x | Unresolved y -> Unresolved y

  let ( >>= ) = bind

  let map_unresolved f m =
    match m with Resolved x -> Resolved x | Unresolved y -> Unresolved (f y)

  let get_resolved = function
    | Resolved r ->
        r
    | Unresolved _ ->
        failwith "Unresolved"
end

let add_subst p = function
  | None, x ->
      (p, x)
  | Some p', x ->
      (`Subst (p', p), x)

let filter_map_record_removed f l =
  let rec inner (kept, removed) = function
    | x :: xs -> (
      match f x with
      | Some y ->
          inner (y :: kept, removed) xs
      | None ->
          inner (kept, x :: removed) xs )
    | [] ->
        (List.rev kept, removed)
  in
  inner ([], []) l

let core_types =
  let open Odoc_model.Lang.TypeDecl in
  let open Odoc_model.Paths in
  List.map
    (fun decl ->
      (Identifier.name decl.id, Component.Of_Lang.(type_decl empty decl)))
    Odoc_model.Predefined.core_types

let prefix_signature (path, s) =
  let open Component.Signature in
  let rec get_sub sub is =
    List.fold_left
      (fun map item ->
        match item with
        | Type (id, _, _) ->
            Subst.add_type id
              (`Type (path, TypeName.of_string (Ident.Name.type_ id)))
              map
        | Module (id, _, _) ->
            Subst.add_module id
              (`Module (path, ModuleName.of_string (Ident.Name.module_ id)))
              map
        | ModuleType (id, _) ->
            Subst.add_module_type id
              (`ModuleType
                (path, ModuleTypeName.of_string (Ident.Name.module_type id)))
              map
        | ModuleSubstitution (id, _) ->
            Subst.add_module id
              (`Module (path, ModuleName.of_string (Ident.Name.module_ id)))
              map
        | TypeSubstitution (id, _) ->
            Subst.add_type id
              (`Type (path, TypeName.of_string (Ident.Name.type_ id)))
              map
        | Exception _ ->
            map
        | TypExt _ ->
            map
        | Value (_, _) ->
            map
        | External (_, _) ->
            map
        | Comment _ ->
            map
        | Class (id, _, _) ->
            Subst.add_class id
              (`Class (path, ClassName.of_string (Ident.Name.class_ id)))
              map
        | ClassType (id, _, _) ->
            Subst.add_class_type id
              (`ClassType
                (path, ClassName.of_string (Ident.Name.class_type id)))
              map
        | Include i ->
            get_sub sub i.expansion.items)
      sub is
  in
  let sub = get_sub Subst.identity s.items in
  let items =
    List.map
      (function
        | Module (id, r, m) ->
            Module
              ( Ident.Rename.module_ id
              , r
              , Component.Delayed.put (fun () ->
                    Subst.module_ sub (Component.Delayed.get m)) )
        | ModuleType (id, mt) ->
            ModuleType (Ident.Rename.module_type id, Subst.module_type sub mt)
        | Type (id, r, t) ->
            Type (Ident.Rename.type_ id, r, Subst.type_ sub t)
        | TypeSubstitution (id, t) ->
            TypeSubstitution (Ident.Rename.type_ id, Subst.type_ sub t)
        | ModuleSubstitution (id, m) ->
            ModuleSubstitution
              (Ident.Rename.module_ id, Subst.module_substitution sub m)
        | Exception (id, e) ->
            Exception (id, Subst.exception_ sub e)
        | TypExt t ->
            TypExt (Subst.extension sub t)
        | Value (id, v) ->
            Value (id, Subst.value sub v)
        | External (id, e) ->
            External (id, Subst.external_ sub e)
        | Class (id, r, c) ->
            Class (Ident.Rename.class_ id, r, Subst.class_ sub c)
        | ClassType (id, r, c) ->
            ClassType (Ident.Rename.class_type id, r, Subst.class_type sub c)
        | Include i ->
            Include (Subst.include_ sub i)
        | Comment c ->
            Comment c)
      s.items
  in
  (path, {items; removed= s.removed})

let prefix_ident_signature
    ((ident, s) : Identifier.Signature.t * Component.Signature.t) =
  let open Component.Signature in
  let rec get_sub sub is =
    List.fold_left
      (fun map item ->
        match item with
        | Type (id, _, _) ->
            Subst.add_type id
              (`Identifier
                (`Type (ident, TypeName.of_string (Ident.Name.type_ id))))
              map
        | Module (id, _, _) ->
            Subst.add_module id
              (`Identifier
                (`Module (ident, ModuleName.of_string (Ident.Name.module_ id))))
              map
        | ModuleType (id, _) ->
            Subst.add_module_type id
              (`Identifier
                (`ModuleType
                  (ident, ModuleTypeName.of_string (Ident.Name.module_type id))))
              map
        | ModuleSubstitution (id, _) ->
            Subst.add_module id
              (`Identifier
                (`Module (ident, ModuleName.of_string (Ident.Name.module_ id))))
              map
        | TypeSubstitution (id, _) ->
            Subst.add_type id
              (`Identifier
                (`Type (ident, TypeName.of_string (Ident.Name.type_ id))))
              map
        | Exception _ ->
            map
        | TypExt _ ->
            map
        | Value (_, _) ->
            map
        | External (_, _) ->
            map
        | Comment _ ->
            map
        | Class (id, _, _) ->
            Subst.add_class id
              (`Identifier
                (`Class (ident, ClassName.of_string (Ident.Name.class_ id))))
              map
        | ClassType (id, _, _) ->
            Subst.add_class_type id
              (`Identifier
                (`ClassType
                  (ident, ClassName.of_string (Ident.Name.class_type id))))
              map
        | Include i ->
            get_sub sub i.expansion.items)
      sub is
  in
  let sub = get_sub Subst.identity s.items in
  let items =
    List.map
      (function
        | Module (id, r, m) ->
            Module
              ( Ident.Rename.module_ id
              , r
              , Component.Delayed.put (fun () ->
                    Subst.module_ sub (Component.Delayed.get m)) )
        | ModuleType (id, mt) ->
            ModuleType (Ident.Rename.module_type id, Subst.module_type sub mt)
        | Type (id, r, t) ->
            Type (Ident.Rename.type_ id, r, Subst.type_ sub t)
        | TypeSubstitution (id, t) ->
            TypeSubstitution (Ident.Rename.type_ id, Subst.type_ sub t)
        | ModuleSubstitution (id, m) ->
            ModuleSubstitution
              (Ident.Rename.module_ id, Subst.module_substitution sub m)
        | Exception (id, e) ->
            Exception (id, Subst.exception_ sub e)
        | TypExt t ->
            TypExt (Subst.extension sub t)
        | Value (id, v) ->
            Value (id, Subst.value sub v)
        | External (id, e) ->
            External (id, Subst.external_ sub e)
        | Class (id, r, c) ->
            Class (Ident.Rename.class_ id, r, Subst.class_ sub c)
        | ClassType (id, r, c) ->
            ClassType (Ident.Rename.class_type id, r, Subst.class_type sub c)
        | Include i ->
            Include (Subst.include_ sub i)
        | Comment c ->
            Comment c)
      s.items
  in
  (ident, {items; removed= s.removed})

let flatten_module_alias : Cpath.resolved_module -> Cpath.resolved_module =
  function
  | `Alias (`Alias (z, _), x) ->
      `Alias (z, x)
  | x ->
      x

type module_lookup_result = Cpath.resolved_module * Component.Module.t

type module_type_lookup_result =
  Cpath.resolved_module_type * Component.ModuleType.t

type type_lookup_result = Cpath.resolved_type * Component.Find.type_

type class_type_lookup_result =
  Cpath.resolved_class_type * Component.Find.class_type

exception Type_lookup_failure of Env.t * Cpath.resolved_type

exception Module_lookup_failure of Env.t * Cpath.resolved_module

exception ModuleType_lookup_failure of Env.t * Cpath.resolved_module_type

exception ClassType_lookup_failure of Env.t * Cpath.resolved_class_type

exception MyFailure of Component.Module.t * Component.Module.t

exception Couldnt_find_functor_argument

let rec handle_apply is_resolve env func_path arg_path m =
  let func_path', mty = module_type_expr_of_module env (func_path, m) in
  let arg_id, result =
    match mty with
    | Component.ModuleType.Functor (Some arg, expr) ->
        (arg.Component.FunctorArgument.id, expr)
    | _ ->
        failwith "Application must take a functor"
  in
  let new_module = {m with Component.Module.type_= ModuleType result} in
  let path = `Apply (func_path', `Substituted (`Resolved arg_path)) in
  let substitution = if is_resolve then `Substituted arg_path else arg_path in
  ( path
  , Subst.module_
      (Subst.add_module arg_id substitution Subst.identity)
      new_module )

and add_canonical_path env m p =
  match m.Component.Module.canonical with
  | Some (cp, _cr) -> (
    match lookup_and_resolve_module_from_path true false env cp with
    | Resolved (cp', _) ->
        `Canonical (p, `Resolved cp')
    | _ ->
        `Canonical (p, cp)
    | exception _e ->
        Format.fprintf Format.err_formatter
          "Warning: Failed to look up canonical path for module %a\n%!"
          Component.Fmt.resolved_module_path p ;
        `Canonical (p, cp) )
  | None ->
      p

and add_hidden m p = if m.Component.Module.hidden then `Hidden p else p

and handle_module_lookup env add_canonical id p m =
  let p', sg = signature_of_module env (p, m) |> prefix_signature in
  let m' = Component.Find.module_in_sig sg id in
  let p' =
    `Module (p', Odoc_model.Names.ModuleName.of_string id) |> add_hidden m'
  in
  let p'' = if add_canonical then add_canonical_path env m' p' else p' in
  (p'', m')

and handle_module_type_lookup env id p m =
  let p', sg = signature_of_module env (p, m) |> prefix_signature in
  let mt = Component.Find.module_type_in_sig sg id in
  (`ModuleType (p', Odoc_model.Names.ModuleTypeName.of_string id), mt)

and handle_type_lookup env id p m =
  let p', sg = signature_of_module env (p, m) |> prefix_signature in
  let mt = Component.Find.type_in_sig sg id in
  (`Type (p', Odoc_model.Names.TypeName.of_string id), mt)

and handle_class_type_lookup env id p m =
  let p', sg = signature_of_module env (p, m) |> prefix_signature in
  let c = Component.Find.class_type_in_sig sg id in
  (`ClassType (p', Odoc_model.Names.TypeName.of_string id), c)

and lookup_and_resolve_module_from_resolved_path :
    bool -> bool -> Env.t -> Cpath.resolved_module -> module_lookup_result =
 fun is_resolve add_canonical env p ->
  (* Format.fprintf Format.err_formatter "lookup_and_resolve_module_from_resolved_path: looking up %a\n%!" Component.Fmt.resolved_path p; *)
  match p with
  | `Local _id ->
      raise (Module_lookup_failure (env, p))
  | `Identifier i ->
      let m = Env.lookup_module i env in
      let p' = add_hidden m p in
      if add_canonical then (add_canonical_path env m p', m) else (p', m)
  | `Substituted x ->
      let p, m =
        lookup_and_resolve_module_from_resolved_path is_resolve add_canonical
          env x
      in
      (`Substituted p, m)
  | `Apply (func_path, arg_path) ->
      let func_path', m =
        lookup_and_resolve_module_from_resolved_path is_resolve add_canonical
          env func_path
      in
      let arg_path' =
        match
          lookup_and_resolve_module_from_path is_resolve add_canonical env
            arg_path
        with
        | Resolved (x, _) ->
            x
        | Unresolved _ ->
            failwith "erk2"
      in
      handle_apply is_resolve env func_path' arg_path' m
  | `Module (p, name) ->
      let p, m =
        lookup_and_resolve_module_from_resolved_path is_resolve add_canonical
          env p
      in
      let p', m' =
        handle_module_lookup env add_canonical
          (Odoc_model.Names.ModuleName.to_string name)
          p m
      in
      (p', m')
  | `Alias (p1, p2) ->
      let p2', m =
        lookup_and_resolve_module_from_resolved_path is_resolve add_canonical
          env p2
      in
      (`Alias (p1, p2'), m)
  | `Subst (p1, p2) ->
      let p2', m =
        lookup_and_resolve_module_from_resolved_path is_resolve add_canonical
          env p2
      in
      (`Subst (p1, p2'), m)
  | `SubstAlias (p1, p2) ->
      let p2', m =
        lookup_and_resolve_module_from_resolved_path is_resolve add_canonical
          env p2
      in
      (`SubstAlias (p1, p2'), m)
  | `Hidden p ->
      let p', m =
        lookup_and_resolve_module_from_resolved_path is_resolve add_canonical
          env p
      in
      (`Hidden p', m)
  | `Canonical (p1, p2) ->
      let p2' =
        match
          lookup_and_resolve_module_from_path is_resolve add_canonical env p2
        with
        | Resolved (p, _) ->
            `Resolved p
        | Unresolved p2 ->
            p2
        | exception e ->
            Format.fprintf Format.err_formatter
              "Warning: Failed to look up canonical path for module %a (got \
               exception %s)\n\
               %!"
              Component.Fmt.resolved_module_path p (Printexc.to_string e) ;
            p2
      in
      let p1', m =
        lookup_and_resolve_module_from_resolved_path is_resolve add_canonical
          env p1
      in
      (`Canonical (p1', p2'), m)

and lookup_module_from_path env cpath =
  lookup_and_resolve_module_from_path true true env cpath

and lookup_module_from_resolved_path env cpath =
  lookup_and_resolve_module_from_resolved_path true true env cpath

and lookup_and_resolve_module_from_path :
       bool
    -> bool
    -> Env.t
    -> Cpath.module_
    -> (module_lookup_result, Cpath.module_) ResultMonad.t =
 fun is_resolve add_canonical env p ->
  let open ResultMonad in
  (* Format.fprintf Format.err_formatter "lookup_and_resolve_module_from_path: looking up %a\n%!" Component.Fmt.path p; *)
  match p with
  | `Dot (parent, id) ->
      lookup_and_resolve_module_from_path is_resolve add_canonical env parent
      |> map_unresolved (fun p' -> `Dot (p', id))
      >>= fun (p, m) ->
      let p', m' = handle_module_lookup env add_canonical id p m in
      return (p', m')
  | `Apply (m1, m2) -> (
      let func =
        lookup_and_resolve_module_from_path is_resolve add_canonical env m1
      in
      let arg =
        lookup_and_resolve_module_from_path is_resolve add_canonical env m2
      in
      match (func, arg) with
      | Resolved (func_path', m), Resolved (arg_path', _) ->
          return (handle_apply is_resolve env func_path' arg_path' m)
      | Unresolved func_path', Resolved (arg_path', _) ->
          Unresolved (`Apply (func_path', `Resolved arg_path'))
      | Resolved (func_path', _), Unresolved arg_path' ->
          Unresolved (`Apply (`Resolved func_path', arg_path'))
      | Unresolved func_path', Unresolved arg_path' ->
          Unresolved (`Apply (func_path', arg_path')) )
  | `Resolved r ->
      return
        (lookup_and_resolve_module_from_resolved_path is_resolve add_canonical
           env r)
  | `Substituted s ->
      lookup_and_resolve_module_from_path is_resolve add_canonical env s
      |> map_unresolved (fun p -> `Substituted p)
      >>= fun (p, m) -> return (`Substituted p, m)
  | `Root r -> (
    match Env.lookup_root_module r env with
    | Some (Env.Resolved (p, m)) ->
        return (add_hidden m (`Identifier p), m)
    | Some Env.Forward ->
        Unresolved (`Forward r)
    | None ->
        Unresolved p )
  | `Forward f ->
      lookup_and_resolve_module_from_path is_resolve add_canonical env
        (`Root f)

and lookup_and_resolve_module_type_from_resolved_path :
    bool -> Env.t -> Cpath.resolved_module_type -> module_type_lookup_result =
 fun is_resolve env p ->
  (* Format.fprintf Format.err_formatter "lookup_and_resolve_module_type_from_resolved_path: looking up %a\n%!" Component.Fmt.resolved_path p; *)
  match p with
  | `Local _id ->
      raise (ModuleType_lookup_failure (env, p))
  | `Identifier (#Identifier.ModuleType.t as i) ->
      Format.(fprintf err_formatter "lookin' up %a\n%!" Component.Fmt.model_identifier i);
      let m = Env.lookup_module_type i env in
      (`Identifier i, m)
  | `Substituted s ->
      let p, m =
        lookup_and_resolve_module_type_from_resolved_path is_resolve env s
      in
      (`Substituted p, m)
  | `ModuleType (p, id) ->
      let p, m =
        lookup_and_resolve_module_from_resolved_path is_resolve true env p
      in
      let p', mt = handle_module_type_lookup env id p m in
      (p', mt)

and lookup_and_resolve_module_type_from_path :
       bool
    -> Env.t
    -> Cpath.module_type
    -> (module_type_lookup_result, Cpath.module_type) ResultMonad.t =
  let open ResultMonad in
  fun is_resolve env p ->
    (* Format.fprintf Format.err_formatter "lookup_and_resolve_module_type_from_path: looking up %a\n%!" Component.Fmt.path p; *)
    match p with
    | `Dot (parent, id) ->
        lookup_and_resolve_module_from_path is_resolve true env parent
        |> map_unresolved (fun p' -> `Dot (p', id))
        >>= fun (p, m) ->
        let p', mt = handle_module_type_lookup env id p m in
        return (p', mt)
    | `Resolved r ->
        return
          (lookup_and_resolve_module_type_from_resolved_path is_resolve env r)
    | `Substituted s ->
        lookup_and_resolve_module_type_from_path is_resolve env s
        |> map_unresolved (fun p' -> `Substituted p')
        >>= fun (p, m) -> return (`Substituted p, m)

and lookup_type_from_resolved_path :
    Env.t -> Cpath.resolved_type -> type_lookup_result =
 fun env p ->
  match p with
  | `Local _id ->
      raise (Type_lookup_failure (env, p))
  | `Identifier (`CoreType name) ->
      (* CoreTypes aren't put into the environment, so they can't be handled by the 
            next clause. We just look them up here in the list of core types *)
      ( `Identifier (`CoreType name)
      , `T (List.assoc (TypeName.to_string name) core_types) )
  | `Identifier (`Type _ as i) ->
      let t = Env.lookup_type i env in
      (`Identifier i, `T t)
  | `Substituted s ->
      let p, t = lookup_type_from_resolved_path env s in
      (`Substituted p, t)
  | `Type (p, id) ->
      let p, m =
        lookup_and_resolve_module_from_resolved_path true true env p
      in
      let p', t = handle_type_lookup env id p m in
      (p', t)
  | `Class (_p, _id) ->
      failwith "class"
  | `ClassType (_p, _id) ->
      failwith "class type"
  | _ ->
      failwith "class"

and lookup_type_from_path :
    Env.t -> Cpath.type_ -> (type_lookup_result, Cpath.type_) ResultMonad.t =
  let open ResultMonad in
  fun env p ->
    match p with
    | `Dot (parent, id) ->
        lookup_and_resolve_module_from_path true true env parent
        |> map_unresolved (fun p' -> `Dot (p', id))
        >>= fun (p, m) ->
        let p', t = handle_type_lookup env id p m in
        return (p', t)
    | `Resolved r ->
        return (lookup_type_from_resolved_path env r)
    | `Substituted s ->
        lookup_type_from_path env s
        |> map_unresolved (fun p' -> `Substituted p')
        >>= fun (p, m) -> return (`Substituted p, m)

and lookup_class_type_from_resolved_path :
    Env.t -> Cpath.resolved_class_type -> class_type_lookup_result =
 fun env p ->
  match p with
  | `Local _id ->
      raise (ClassType_lookup_failure (env, p))
  | `Identifier (`Class _ as c) ->
      let t = Env.lookup_class c env in
      (`Identifier c, `C t)
  | `Identifier (`ClassType _ as c) ->
      let t = Env.lookup_class_type c env in
      (`Identifier c, `CT t)
  | `Substituted s ->
      let p, t = lookup_class_type_from_resolved_path env s in
      (`Substituted p, t)
  | `Class (p, id) ->
      let p, m =
        lookup_and_resolve_module_from_resolved_path true true env p
      in
      let p', t = handle_class_type_lookup env id p m in
      (p', t)
  | `ClassType (p, id) ->
      let p, m =
        lookup_and_resolve_module_from_resolved_path true true env p
      in
      let p', t = handle_class_type_lookup env id p m in
      (p', t)

and lookup_class_type_from_path :
       Env.t
    -> Cpath.class_type
    -> (class_type_lookup_result, Cpath.class_type) ResultMonad.t =
  let open ResultMonad in
  fun env p ->
    match p with
    | `Dot (parent, id) ->
        lookup_and_resolve_module_from_path true true env parent
        |> map_unresolved (fun p' -> `Dot (p', id))
        >>= fun (p, m) ->
        let p', c = handle_class_type_lookup env id p m in
        return (p', c)
    | `Resolved r ->
        return (lookup_class_type_from_resolved_path env r)
    | `Substituted s ->
        lookup_class_type_from_path env s
        |> map_unresolved (fun p' -> `Substituted p')
        >>= fun (p, c) -> return (`Substituted p, c)

and lookup_signature_from_resolved_fragment :
       Env.t
    -> Cpath.resolved_module
    -> Fragment.Resolved.Signature.t
    -> Component.Signature.t
    -> Cpath.resolved_module * Component.Signature.t =
 fun env p f s ->
  match f with
  | `Root ->
      (p, s)
  | #Fragment.Resolved.Module.t as frag ->
      let _, p, m = lookup_module_from_resolved_fragment env p frag s in
      signature_of_module env (p, m)

and lookup_module_from_resolved_fragment :
       Env.t
    -> Cpath.resolved_module
    -> Fragment.Resolved.Module.t
    -> Component.Signature.t
    -> Ident.module_ * Cpath.resolved_module * Component.Module.t =
 fun env p f s ->
  match f with
  | `Subst (_, _) | `SubstAlias (_, _) ->
      failwith "What do we do with these?"
  | `Module (parent, name) ->
      let ppath, sg = lookup_signature_from_resolved_fragment env p parent s in
      let rec find = function
        | Component.Signature.Module (id, _, m') :: _
          when Ident.Name.module_ id
               = Odoc_model.Names.ModuleName.to_string name ->
            ( id
            , `Module (ppath, Ident.Name.module_ id)
            , Component.Delayed.get m' )
        | _ :: xs ->
            find xs
        | [] ->
            failwith "Can't find it"
      in
      find sg.items

and lookup_module_from_fragment :
       Env.t
    -> Cpath.resolved_module
    -> Fragment.Module.t
    -> Component.Signature.t
    -> Ident.module_ * Cpath.resolved_module * Component.Module.t =
 fun env p f s ->
  match f with
  | `Dot (parent, name) ->
      let ppath, sg = lookup_signature_from_fragment env p parent s in
      let rec find = function
        | Component.Signature.Module (id, _, m') :: _
          when Ident.Name.module_ id = name ->
            (id, `Module (ppath, name), Component.Delayed.get m')
        | _ :: xs ->
            find xs
        | [] ->
            failwith "Can't find it"
      in
      find sg.items
  | `Resolved r ->
      lookup_module_from_resolved_fragment env p r s

and lookup_signature_from_fragment :
       Env.t
    -> Cpath.resolved_module
    -> Fragment.Signature.t
    -> Component.Signature.t
    -> Cpath.resolved_module * Component.Signature.t =
 fun env p f s ->
  match f with
  | `Dot (_, _) as f' ->
      let _, p, m = lookup_module_from_fragment env p f' s in
      signature_of_module env (p, m)
  | `Resolved r ->
      lookup_signature_from_resolved_fragment env p r s

and module_type_expr_of_module_decl :
       Env.t
    -> Cpath.resolved_module * Component.Module.decl
    -> Cpath.resolved_module * Component.ModuleType.expr =
 fun env (p, decl) ->
  match decl with
  | Component.Module.Alias path -> (
    match lookup_and_resolve_module_from_path false true env path with
    | Resolved (x, y) ->
        let x', y' = module_type_expr_of_module env (x, y) in
        if Cpath.is_resolved_module_substituted x' then
          (`SubstAlias (p, x'), y')
        else (p, y')
    | Unresolved _ ->
        let err =
          Format.asprintf "Failed to lookup alias module (path=%a)"
            Component.Fmt.module_path path
        in
        failwith err )
  | Component.Module.ModuleType expr ->
      (p, expr)

and module_type_expr_of_module :
       Env.t
    -> Cpath.resolved_module * Component.Module.t
    -> Cpath.resolved_module * Component.ModuleType.expr =
 fun env (p, m) -> module_type_expr_of_module_decl env (p, m.type_)

and signature_of_module_alias_path :
       Env.t
    -> is_canonical:bool
    -> Cpath.resolved_module
    -> Cpath.module_
    -> Cpath.resolved_module * Component.Signature.t =
 fun env ~is_canonical incoming_path path ->
  match lookup_and_resolve_module_from_path false true env path with
  | Resolved (p', m) ->
      (* p' is the path to the aliased module *)
      let p'', m' =
        if is_canonical then signature_of_module env (incoming_path, m)
        else
          let p'', m' = signature_of_module env (p', m) in
          let m'' = Strengthen.signature p'' m' in
          let p''' = flatten_module_alias (`Alias (p'', incoming_path)) in
          (p''', m'')
      in
      (p'', m')
  | Unresolved _ ->
      let err =
        Format.asprintf "Failed to lookup alias module (path=%a)"
          Component.Fmt.module_path path
      in
      failwith err

and signature_of_module_alias_nopath :
    Env.t -> Cpath.module_ -> Component.Signature.t =
 fun env path ->
  match lookup_and_resolve_module_from_path false true env path with
  | Resolved (p', m) ->
      (* p' is the path to the aliased module *)
      let m' = signature_of_module_nopath env m in
      let m'' = Strengthen.signature p' m' in
      m''
  | Unresolved _ ->
      let err =
        Format.asprintf "Failed to lookup alias module (path=%a)"
          Component.Fmt.module_path path
      in
      failwith err

and handle_signature_with_subs :
       Component.Signature.t
    -> Component.ModuleType.substitution list
    -> Component.Signature.t =
 fun sg subs ->
  List.fold_left
    (fun sg sub ->
      match sub with
      | Component.ModuleType.ModuleEq (frag, _) ->
          fragmap_module frag sub sg
      | ModuleSubst (frag, _) ->
          fragmap_module frag sub sg
      | TypeEq (frag, _) ->
          fragmap_type frag sub sg
      | TypeSubst (frag, _) ->
          fragmap_type frag sub sg)
    sg subs

and signature_of_module_type_expr :
       Env.t
    -> Cpath.resolved_module * Component.ModuleType.expr
    -> Cpath.resolved_module * Component.Signature.t =
 fun env (incoming_path, m) ->
  match m with
  | Component.ModuleType.Path p -> (
    (*            Format.fprintf Format.std_formatter "Looking up path: %a\n%!" Component.Fmt.path p;*)
    match lookup_and_resolve_module_type_from_path false env p with
    | Resolved (p, mt) ->
        let p'', sg = signature_of_module_type env (incoming_path, mt) in
        if
          Cpath.is_resolved_module_type_substituted p
          || Cpath.is_resolved_module_substituted p''
        then (`Subst (p, incoming_path), sg)
        else (incoming_path, sg)
    | Unresolved _p ->
        let p = Component.Fmt.(string_of module_type_path p) in
        failwith (Printf.sprintf "Couldn't find signature: %s" p) )
  | Component.ModuleType.Signature s ->
      (incoming_path, s)
  | Component.ModuleType.With (s, subs) ->
      let p', sg = signature_of_module_type_expr env (incoming_path, s) in
      (p', handle_signature_with_subs sg subs)
  | Component.ModuleType.Functor (_, expr) ->
      signature_of_module_type_expr env (incoming_path, expr)
  | Component.ModuleType.TypeOf decl ->
      signature_of_module_decl env ~is_canonical:false (incoming_path, decl)

and signature_of_module_type_expr_nopath :
    Env.t -> Component.ModuleType.expr -> Component.Signature.t =
 fun env m ->
  match m with
  | Component.ModuleType.Path p -> (
    match lookup_and_resolve_module_type_from_path false env p with
    | Resolved (_, mt) ->
        signature_of_module_type_nopath env mt
    | Unresolved _p ->
        let p = Component.Fmt.(string_of module_type_path p) in
        failwith (Printf.sprintf "Couldn't find signature: %s" p) )
  | Component.ModuleType.Signature s ->
      s
  | Component.ModuleType.With (s, subs) ->
      let sg = signature_of_module_type_expr_nopath env s in
      handle_signature_with_subs sg subs
  | Component.ModuleType.Functor (_, expr) ->
      signature_of_module_type_expr_nopath env expr
  | Component.ModuleType.TypeOf decl ->
      signature_of_module_decl_nopath env decl

and signature_of_module_type :
       Env.t
    -> Cpath.resolved_module * Component.ModuleType.t
    -> Cpath.resolved_module * Component.Signature.t =
 fun env (p, m) ->
  match m.expr with
  | None ->
      failwith "oh no"
  | Some expr ->
      signature_of_module_type_expr env (p, expr)

and signature_of_module_type_nopath :
    Env.t -> Component.ModuleType.t -> Component.Signature.t =
 fun env m ->
  match m.expr with
  | None ->
      failwith "oh no"
  | Some expr ->
      signature_of_module_type_expr_nopath env expr

and signature_of_module_decl :
       Env.t
    -> is_canonical:bool
    -> Cpath.resolved_module * Component.Module.decl
    -> Cpath.resolved_module * Component.Signature.t =
 fun env ~is_canonical (incoming_path, decl) ->
  match decl with
  | Component.Module.Alias path ->
      signature_of_module_alias_path env ~is_canonical incoming_path path
  | Component.Module.ModuleType expr ->
      signature_of_module_type_expr env (incoming_path, expr)

and signature_of_module_decl_nopath :
    Env.t -> Component.Module.decl -> Component.Signature.t =
 fun env decl ->
  match decl with
  | Component.Module.Alias path ->
      signature_of_module_alias_nopath env path
  | Component.Module.ModuleType expr ->
      signature_of_module_type_expr_nopath env expr

and signature_of_module :
       Env.t
    -> Cpath.resolved_module * Component.Module.t
    -> Cpath.resolved_module * Component.Signature.t =
 fun env (path, m) ->
  match m.canonical with
  | Some _ ->
      signature_of_module_decl env ~is_canonical:true (path, m.type_)
  | None ->
      signature_of_module_decl env ~is_canonical:false (path, m.type_)

and signature_of_module_nopath :
    Env.t -> Component.Module.t -> Component.Signature.t =
 fun env m -> signature_of_module_decl_nopath env m.type_

and handle_removed :
       Component.Signature.item list
    -> Component.Signature.removed_item list
    -> Component.Signature.removed_item list =
 fun l acc ->
  let open Component.Signature in
  List.map
    (function
      | Module (id, _, _) ->
          RModule (id, None)
      | Type (id, _, _) ->
          RType (id, None)
      | _ ->
          failwith "Can't remove anything but modules or types")
    l
  @ acc

and opt_map f = function None -> None | Some x -> Some (f x)

and fragmap_module :
       Fragment.Module.t
    -> Component.ModuleType.substitution
    -> Component.Signature.t
    -> Component.Signature.t =
 fun frag sub sg ->
  let open OptionMonad in
  let name, frag' = Fragment.Module.split frag in
  Format.(
    fprintf err_formatter "split: name=%s frag=%a\n%!" name
      Component.Fmt.(option model_fragment)
      (opt_map (fun frag' -> (frag' :> Fragment.t)) frag')) ;
  let mapfn m =
    match (frag', sub) with
    | None, ModuleEq (_, type_) ->
        (* Finished the substitution *) Some {m with Component.Module.type_}
    | None, ModuleSubst (_, _) ->
        None
    | Some f, subst -> (
        let new_subst =
          match subst with
          | ModuleEq (_, type_) ->
              Component.ModuleType.ModuleEq (f, type_)
          | ModuleSubst (_, path) ->
              ModuleSubst (f, path)
          | TypeEq _ | TypeSubst _ ->
              failwith "Can't happen"
        in
        match m.type_ with
        | Alias path ->
            Some
              { m with
                type_=
                  ModuleType
                    Component.(
                      ModuleType.(
                        With (TypeOf (Module.Alias path), [new_subst]))) }
            (* Can this one happen? *)
        | ModuleType (With (mty', subs')) ->
            Some
              { m with
                type_=
                  ModuleType
                    (Component.ModuleType.With (mty', subs' @ [new_subst])) }
        | ModuleType mty ->
            Some
              { m with
                type_=
                  ModuleType (Component.ModuleType.With (mty, [new_subst])) } )
    | _, TypeEq _ | _, TypeSubst _ ->
        failwith "Can't happen"
  in
  let items, removed =
    filter_map_record_removed
      (function
        | Component.Signature.Module (id, r, m)
          when Ident.Name.module_ id = ModuleName.to_string name ->
            let m = Component.Delayed.get m in
            mapfn m
            >>= fun m' ->
            return
              (Component.Signature.Module
                 (id, r, Component.Delayed.put (fun () -> m')))
        | x ->
            return x)
      sg.items
  in
  let res = {Component.Signature.items; removed= handle_removed removed sg.removed} in
  Format.(
    fprintf err_formatter "after sig=%a\n%!" 
      Component.Fmt.(signature)
        res);
  res

and fragmap_type :
       Fragment.Type.t
    -> Component.ModuleType.substitution
    -> Component.Signature.t
    -> Component.Signature.t =
 fun frag sub sg ->
  let open OptionMonad in
  let name, frag' = Fragment.Type.split frag in
  match frag' with
  | None ->
      let mapfn t =
        match sub with
        | TypeEq (_, equation) ->
            (* Finished the substitution *)
            Some {t with Component.TypeDecl.equation}
        | TypeSubst (_, _) ->
            None
        | _ ->
            failwith "Can't happen"
      in
      let items, removed =
        filter_map_record_removed
          (function
            | Component.Signature.Type (id, r, t)
              when Ident.Name.type_ id = name ->
                mapfn t
                >>= fun m' -> return (Component.Signature.Type (id, r, m'))
            | x ->
                return x)
          sg.items
      in
      {items; removed= handle_removed removed sg.removed}
  | Some f ->
      let mapfn m =
        let new_subst =
          match sub with
          | ModuleEq _ | ModuleSubst _ ->
              failwith "Can't happen"
          | TypeEq (_, eqn) ->
              Component.ModuleType.TypeEq (f, eqn)
          | TypeSubst (_, eqn) ->
              Component.ModuleType.TypeSubst (f, eqn)
        in
        match m.Component.Module.type_ with
        | Alias path ->
            { m with
              type_=
                ModuleType
                  Component.(
                    ModuleType.(With (TypeOf (Module.Alias path), [new_subst])))
            }
            (* Can this one happen? *)
        | ModuleType (With (mty', subs')) ->
            { m with
              type_=
                ModuleType
                  (Component.ModuleType.With (mty', subs' @ [new_subst])) }
        | ModuleType mty ->
            { m with
              type_= ModuleType (Component.ModuleType.With (mty, [new_subst]))
            }
      in
      let items =
        List.map
          (function
            | Component.Signature.Module (id, r, m)
              when Ident.Name.module_ id = ModuleName.to_string name ->
                let m = Component.Delayed.get m in
                Component.Signature.Module
                  (id, r, Component.Delayed.put (fun () -> mapfn m))
            | x ->
                x)
          sg.items
      in
      {sg with items}

and find_module_with_replacement :
    Env.t -> Component.Signature.t -> string -> Component.Module.t =
 fun env sg name ->
  match Component.Find.careful_module_in_sig sg name with
  | Found m ->
      m
  | Replaced path ->
      let _, m = lookup_module_from_resolved_path env path in
      m

and find_type_with_replacement :
    Env.t -> Component.Signature.t -> string -> Component.Find.type_ =
 fun env sg name ->
  match Component.Find.careful_type_in_sig sg name with
  | Found m ->
      m
  | Replaced path ->
      let _, m = lookup_type_from_resolved_path env path in
      m

let rec resolve_resolved_signature_fragment :
       Env.t
    -> Cpath.resolved_module * Component.Signature.t
    -> Odoc_model.Paths.Fragment.Resolved.Signature.t
    -> Odoc_model.Paths.Fragment.Resolved.Signature.t
       * Cpath.resolved_module
       * Component.Signature.t =
 fun env (p, sg) frag ->
  match frag with
  | `Root ->
      let cp, sg = prefix_signature (p, sg) in
      (`Root, cp, sg)
  | `Module (parent, name) ->
      let parent, cp, sg =
        resolve_resolved_signature_fragment env (p, sg) parent
      in
      let m' = find_module_with_replacement env sg name in
      let cp, sg = signature_of_module env (cp, m') |> prefix_signature in
      (`Module (parent, Odoc_model.Names.ModuleName.of_string name), cp, sg)
  | _ ->
      failwith "foo"

and resolve_signature_fragment :
       Env.t
    -> Cpath.resolved_module * Component.Signature.t
    -> Odoc_model.Paths.Fragment.Signature.t
    -> Odoc_model.Paths.Fragment.Resolved.Signature.t
       * Cpath.resolved_module
       * Component.Signature.t =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r ->
      resolve_resolved_signature_fragment env (p, sg) r
  | `Dot (parent, name) ->
      let parent, cp, sg = resolve_signature_fragment env (p, sg) parent in
      let m' = find_module_with_replacement env sg name in
      let cp, sg = signature_of_module env (cp, m') |> prefix_signature in
      (`Module (parent, Odoc_model.Names.ModuleName.of_string name), cp, sg)

and resolve_resolved_module_fragment :
       Env.t
    -> Cpath.resolved_module * Component.Signature.t
    -> Odoc_model.Paths.Fragment.Resolved.Module.t
    -> Odoc_model.Paths.Fragment.Resolved.Module.t
       * Cpath.resolved_module
       * Component.Module.t =
 fun env (p, sg) frag ->
  match frag with
  | `Module (parent, name) ->
      let parent, cp, sg =
        resolve_resolved_signature_fragment env (p, sg) parent
      in
      let m' =
        find_module_with_replacement env sg
          (Odoc_model.Names.ModuleName.to_string name)
      in
      ( `Module (parent, Odoc_model.Names.ModuleName.of_string name)
      , `Module (cp, Odoc_model.Names.ModuleName.to_string name)
      , m' )
  | _ ->
      failwith ""

and resolve_module_fragment :
       Env.t
    -> Cpath.resolved_module * Component.Signature.t
    -> Odoc_model.Paths.Fragment.Module.t
    -> Odoc_model.Paths.Fragment.Resolved.Module.t =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r ->
      let result, _, _ = resolve_resolved_module_fragment env (p, sg) r in
      result
  | `Dot (parent, name) ->
      let parent, _cp, sg = resolve_signature_fragment env (p, sg) parent in
      let _ =
        find_module_with_replacement env sg
          (Odoc_model.Names.ModuleName.to_string name)
      in
      `Module (parent, Odoc_model.Names.ModuleName.of_string name)

and resolve_resolved_type_fragment :
       Env.t
    -> Cpath.resolved_module * Component.Signature.t
    -> Odoc_model.Paths.Fragment.Resolved.Type.t
    -> Odoc_model.Paths.Fragment.Resolved.Type.t
       * Cpath.resolved_type
       * Component.Find.type_ =
 fun env (p, sg) frag ->
  match frag with
  | `Type (parent, name) -> (
      let parent, cp, sg =
        resolve_resolved_signature_fragment env (p, sg) parent
      in
      let t' =
        Component.Find.careful_type_in_sig sg
          (Odoc_model.Names.TypeName.to_string name)
      in
      match t' with
      | Component.Find.Found t' ->
          ( `Type (parent, Odoc_model.Names.TypeName.of_string name)
          , `Type (cp, Odoc_model.Names.TypeName.of_string name)
          , t' )
      | Component.Find.Replaced p ->
          let _, t = lookup_type_from_resolved_path env p in
          ( `Type (parent, Odoc_model.Names.TypeName.of_string name)
          , `Type (cp, Odoc_model.Names.TypeName.to_string name)
          , t ) )
  | _ ->
      failwith ""

and resolve_type_fragment :
       Env.t
    -> Cpath.resolved_module * Component.Signature.t
    -> Odoc_model.Paths.Fragment.Type.t
    -> Odoc_model.Paths.Fragment.Resolved.Type.t =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r ->
      let result, _, _ = resolve_resolved_type_fragment env (p, sg) r in
      result
  | `Dot (parent, name) ->
      let parent, _, sg = resolve_signature_fragment env (p, sg) parent in
      let _ =
        Component.Find.careful_type_in_sig sg
          (Odoc_model.Names.TypeName.to_string name)
      in
      `Type (parent, Odoc_model.Names.TypeName.of_string name)

let rec resolve_mt_resolved_signature_fragment :
       Env.t
    -> Identifier.Signature.t * Component.Signature.t
    -> Odoc_model.Paths.Fragment.Resolved.Signature.t
    -> Odoc_model.Paths.Fragment.Resolved.Signature.t
       * Identifier.Signature.t
       * Component.Signature.t =
 fun env (p, sg) frag ->
  match frag with
  | `Root ->
      let _, sg = prefix_ident_signature (p, sg) in
      (`Root, p, sg)
  | `Module (parent, name) ->
      let parent, id, sg =
        resolve_mt_resolved_signature_fragment env (p, sg) parent
      in
      let env = Env.open_component_signature id sg env in
      let m' = find_module_with_replacement env sg name in
      let new_id = `Module (p, Odoc_model.Names.ModuleName.of_string name) in
      let _cp, sg =
        signature_of_module_nopath env m'
        |> fun m -> prefix_ident_signature (new_id, m)
      in
      ( `Module (parent, Odoc_model.Names.ModuleName.of_string name)
      , new_id
      , sg )
  | _ ->
      failwith "foo"

and resolve_mt_signature_fragment :
       Env.t
    -> Identifier.Signature.t * Component.Signature.t
    -> Odoc_model.Paths.Fragment.Signature.t
    -> Odoc_model.Paths.Fragment.Resolved.Signature.t
       * Identifier.Signature.t
       * Component.Signature.t =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r ->
      resolve_mt_resolved_signature_fragment env (p, sg) r
  | `Dot (parent, name) ->
      let parent, id, sg = resolve_mt_signature_fragment env (p, sg) parent in
      let m' = find_module_with_replacement env sg name in
      let env = Env.open_component_signature id sg env in
      let new_id = `Module (p, Odoc_model.Names.ModuleName.of_string name)in
      let _cp, sg =
        signature_of_module_nopath env m'
        |> fun m -> prefix_ident_signature (new_id, m)
      in
      ( `Module (parent, Odoc_model.Names.ModuleName.of_string name)
      , new_id
      , sg )

and resolve_mt_resolved_module_fragment :
       Env.t
    -> Identifier.Signature.t * Component.Signature.t
    -> Odoc_model.Paths.Fragment.Resolved.Module.t
    -> Odoc_model.Paths.Fragment.Resolved.Module.t
       * Identifier.Signature.t
       * Component.Module.t =
 fun env (p, sg) frag ->
  match frag with
  | `Module (parent, name) ->
      let parent, id, sg =
        resolve_mt_resolved_signature_fragment env (p, sg) parent
      in
      let m' =
        find_module_with_replacement env sg
          (Odoc_model.Names.ModuleName.to_string name)
      in
      ( `Module (parent, Odoc_model.Names.ModuleName.of_string name)
      , `Module (id, Odoc_model.Names.ModuleName.of_string name)
      , m' )
  | _ ->
      failwith ""

and resolve_mt_module_fragment :
       Env.t
    -> Identifier.Signature.t * Component.Signature.t
    -> Odoc_model.Paths.Fragment.Module.t
    -> Odoc_model.Paths.Fragment.Resolved.Module.t =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r ->
      let result, _, _ = resolve_mt_resolved_module_fragment env (p, sg) r in
      result
  | `Dot (parent, name) ->
      let parent, _cp, sg = resolve_mt_signature_fragment env (p, sg) parent in
      let _ =
        find_module_with_replacement env sg
          (Odoc_model.Names.ModuleName.to_string name)
      in
      `Module (parent, Odoc_model.Names.ModuleName.of_string name)

and resolve_mt_resolved_type_fragment :
       Env.t
    -> Identifier.Signature.t * Component.Signature.t
    -> Odoc_model.Paths.Fragment.Resolved.Type.t
    -> Odoc_model.Paths.Fragment.Resolved.Type.t =
 fun env (p, sg) frag ->
  match frag with
  | `Type (parent, name) -> (
      let parent, _id, sg =
        resolve_mt_resolved_signature_fragment env (p, sg) parent
      in
      let t' =
        Component.Find.careful_type_in_sig sg
          (Odoc_model.Names.TypeName.to_string name)
      in
      match t' with
      | Component.Find.Found _t' ->
          `Type (parent, Odoc_model.Names.TypeName.of_string name)
      | Component.Find.Replaced p ->
          let _, _t = lookup_type_from_resolved_path env p in
          `Type (parent, Odoc_model.Names.TypeName.of_string name) )
  | _ ->
      failwith ""

and resolve_mt_type_fragment :
       Env.t
    -> Identifier.Signature.t * Component.Signature.t
    -> Odoc_model.Paths.Fragment.Type.t
    -> Odoc_model.Paths.Fragment.Resolved.Type.t =
 fun env (p, sg) frag ->
  match frag with
  | `Resolved r ->
      resolve_mt_resolved_type_fragment env (p, sg) r
  | `Dot (parent, name) ->
      let parent, _, sg = resolve_mt_signature_fragment env (p, sg) parent in
      let _ =
        Component.Find.careful_type_in_sig sg
          (Odoc_model.Names.TypeName.to_string name)
      in
      `Type (parent, Odoc_model.Names.TypeName.of_string name)

let rec class_signature_of_class :
       Env.t
    -> Cpath.resolved_class_type * Component.Class.t
    -> Cpath.resolved_class_type * Component.ClassSignature.t =
 fun env (p, c) ->
  let rec inner decl =
    match decl with
    | Component.Class.ClassType e ->
        class_signature_of_class_type_expr env (p, e)
    | Arrow (_, _, d) ->
        inner d
  in
  inner c.type_

and class_signature_of_class_type_expr :
       Env.t
    -> Cpath.resolved_class_type * Component.ClassType.expr
    -> Cpath.resolved_class_type * Component.ClassSignature.t =
 fun env (p, e) ->
  match e with
  | Signature s ->
      (p, s)
  | Constr (p, _) -> (
      let p, c =
        match lookup_class_type_from_path env p with
        | Resolved (p, c) ->
            (p, c)
        | _ ->
            failwith "error"
      in
      match c with
      | `C c ->
          class_signature_of_class env (p, c)
      | `CT c ->
          class_signature_of_class_type env (p, c) )

and class_signature_of_class_type :
       Env.t
    -> Cpath.resolved_class_type * Component.ClassType.t
    -> Cpath.resolved_class_type * Component.ClassSignature.t =
 fun env (p, c) -> class_signature_of_class_type_expr env (p, c.expr)
