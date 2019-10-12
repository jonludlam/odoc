open Odoc_model.Names
open Odoc_model.Paths


module OptionMonad = struct
    type 'a t = 'a option
    let return x = Some x
    let bind m f = match m with | Some x -> f x | None -> None
    let (>>=) = bind
end

module ResultMonad = struct
    type ('a, 'b) t =
        | Resolved of 'a
        | Unresolved of 'b
    let return x = Resolved x
    let bind m f = match m with | Resolved x -> f x | Unresolved y -> Unresolved y 
    let (>>=) = bind
    let map_unresolved f m = match m with | Resolved x -> Resolved x | Unresolved y -> Unresolved (f y)
    let get_resolved = function | Resolved r -> r | Unresolved _ -> failwith "Unresolved"
end

let filter_map_record_removed f l =
    let rec inner (kept,removed) = function
      | x::xs -> begin match f x with | Some y -> inner (y::kept, removed) xs | None -> inner (kept, x::removed) xs end
      | [] -> (List.rev kept, removed)
    in inner ([],[]) l


let core_types = 
    let open Odoc_model.Lang.TypeDecl in
    let open Odoc_model.Paths in
    List.map
      (fun decl -> (Identifier.name decl.id, Component.Of_Lang.(type_decl empty decl)))
      Odoc_model.Predefined.core_types

let prefix_signature (path, s) =
    let open Component.Signature in
    let sub = List.fold_left (fun map item ->
        match item with
        | Type (id,_,_) -> Subst.add id (`Type (path, TypeName.of_string (Ident.name id))) map
        | Module (id,_,_) -> Subst.add id (`Module (path, ModuleName.of_string (Ident.name id))) map
        | ModuleType (id, _) -> Subst.add id (`ModuleType (path, ModuleTypeName.of_string (Ident.name id))) map
        | Exception _ -> map
        | TypExt _ -> map
        | Value (_,_) -> map
        | External (_, _) -> map
        | Comment _ -> map
        | _ -> map
        ) Subst.identity s.items in
    let items = List.map (function
            | Module (id,r, m) -> Module ((Ident.rename id), r, (Component.Delayed.put (fun () -> Subst.module_ sub (Component.Delayed.get m))))
            | ModuleType (id, mt) -> ModuleType ((Ident.rename id), Subst.module_type sub mt)
            | Type (id, r, t) -> Type ((Ident.rename id), r, Subst.type_ sub t)
            | Exception (id, e) -> Exception (id, Subst.exception_ sub e)
            | TypExt t -> TypExt (Subst.extension sub t)
            | Value (id, v) -> Value (id, Subst.value sub v)
            | External (id, e) -> External (id, Subst.external_ sub e)
            | Class (id,r,c) -> Class (id,r,c)
            | ClassType (id,r,c) -> ClassType (id,r,c)
            | Include i -> Include (Subst.include_ sub i)
            | Comment c -> Comment c
            ) s.items in
    (path, {items; removed=s.removed})

let flatten_module_alias : Cpath.resolved -> Cpath.resolved = function
  | `Alias (x, `Alias (_, z)) -> `Alias (x, z)
  | x -> x

type module_lookup_result =
    Cpath.resolved * Component.Module.t

type module_type_lookup_result =
    Cpath.resolved * Component.ModuleType.t

type type_lookup_result =
    Cpath.resolved * Component.TypeDecl.t

type class_type_lookup_result =
    Cpath.resolved * Component.Find.class_type

exception Lookup_failure of Env.t * Cpath.resolved * string
exception MyFailure of Component.Module.t * Component.Module.t
exception Couldnt_find_functor_argument


let rec handle_apply is_resolve env func_path arg_path m =
    let (func_path', mty) = module_type_expr_of_module env (func_path, m) in
    let arg_id, result =
        match mty with
        | Component.ModuleType.Functor (Some arg, expr) ->
            arg.Component.FunctorArgument.id, expr
        | _ -> failwith "Application must take a functor"
    in
    let new_module = {m with type_ = ModuleType result} in
    let path = `Apply (func_path', `Substituted (`Resolved arg_path)) in
    let substitution = if is_resolve then `Substituted arg_path else arg_path in
    (path, Subst.module_ (Subst.add arg_id substitution Subst.identity) new_module)

and add_canonical_path env m p =
    match m.Component.Module.canonical with
    | Some (cp,_cr) -> begin
        match lookup_and_resolve_module_from_path true false env cp with
        | Resolved (cp', _) ->
            `Canonical (p, `Resolved cp')
        | _ -> `Canonical (p, cp)
        end
    | None ->
        p

and add_hidden m p =
    if m.Component.Module.hidden then `Hidden p else p

and handle_module_lookup env add_canonical id p m =
    let (p', sg) = signature_of_module env (p, m) |> prefix_signature in
    let m' = Component.Find.module_in_sig sg id in
    let p' =
        `Module (p', Odoc_model.Names.ModuleName.of_string id)
        |> add_hidden m'
    in
    let p'' = if add_canonical then add_canonical_path env m' p' else p' in
    (p'', m')

and handle_module_type_lookup env id p m =
    let (p', sg) = signature_of_module env (p, m) |> prefix_signature in
    let mt = Component.Find.module_type_in_sig sg id in
    (`ModuleType (p', Odoc_model.Names.ModuleTypeName.of_string id), mt)

and handle_type_lookup env id p m =
    let (p', sg) = signature_of_module env (p, m) |> prefix_signature in
    let mt = Component.Find.type_in_sig sg id in
    (`Type (p', Odoc_model.Names.TypeName.of_string id), mt)

and handle_class_type_lookup env id p m =
    let (p', sg) = signature_of_module env (p, m) |> prefix_signature in
    let c = Component.Find.class_type_in_sig sg id in
    (`ClassType (p', Odoc_model.Names.TypeName.of_string id), c)

and lookup_and_resolve_module_from_resolved_path : bool -> bool -> Env.t -> Cpath.resolved -> module_lookup_result =
    fun is_resolve add_canonical env p ->
        match p with
        | `Local _id -> raise (Lookup_failure (env, p, "module")) 
        | `Identifier (#Identifier.Module.t as i) ->
            let m = Env.lookup_module i env in
            let p' = add_hidden m (`Identifier i) in
            if add_canonical
            then (add_canonical_path env m p', m)
            else (p', m)
        | `Identifier _ ->
            failwith "Invalid"
        | `Substituted x ->
            let (p, m) = lookup_and_resolve_module_from_resolved_path is_resolve add_canonical env x in
            (`Substituted p, m)
        | `Apply (func_path, arg_path) -> begin
            let (func_path', m) = lookup_and_resolve_module_from_resolved_path is_resolve add_canonical env func_path in
            let arg_path' = match lookup_and_resolve_module_from_path is_resolve add_canonical env arg_path with Resolved (x,_) -> x | Unresolved _ -> failwith "erk2" in
            handle_apply is_resolve env func_path' arg_path' m
            end
        | `Module (p, name) ->
            let (p, m) = lookup_and_resolve_module_from_resolved_path is_resolve add_canonical env p in
            let (p', m') = handle_module_lookup env add_canonical (Odoc_model.Names.ModuleName.to_string name) p m in
            (p', m')
        | `Alias (p1, p2) ->
            let p2',m = lookup_and_resolve_module_from_resolved_path is_resolve add_canonical env p2 in
            `Alias (p1, p2'),m
        | `Subst (p1, p2) ->
            let p2',m = lookup_and_resolve_module_from_resolved_path is_resolve add_canonical env p2 in
            `Subst(p1,p2'),m
        | `SubstAlias (p1, p2) ->
            let p2',m = lookup_and_resolve_module_from_resolved_path is_resolve add_canonical env p2 in
            `SubstAlias(p1,p2'),m
        | `Hidden p -> 
            let p',m = lookup_and_resolve_module_from_resolved_path is_resolve add_canonical env p in
            `Hidden(p'),m
        | `Canonical (p1, p2) ->
            let p2' =
                match lookup_and_resolve_module_from_path is_resolve add_canonical env p2 with
                | Resolved (p,_) -> `Resolved p
                | Unresolved p2 -> p2
            in
            let p1', m = lookup_and_resolve_module_from_resolved_path is_resolve add_canonical env p1 in
            `Canonical(p1',p2'),m
        | `ModuleType _
        | `Type _ ->
            let str = Component.Fmt.string_of Component.Fmt.resolved_path p in
            failwith (Printf.sprintf "Bad lookup: %s" str)
        | `Class (_p, _n) -> failwith "class"
        | `ClassType (_p, _n) -> failwith "classtype"

and lookup_module_from_path env cpath = lookup_and_resolve_module_from_path true true env cpath

and lookup_module_from_resolved_path env cpath = lookup_and_resolve_module_from_resolved_path true true env cpath

and lookup_and_resolve_module_from_path : bool -> bool -> Env.t -> Cpath.t -> (module_lookup_result, Cpath.t) ResultMonad.t = fun is_resolve add_canonical env p ->
    let open ResultMonad in
    match p with
    | `Dot (parent, id) ->
        lookup_and_resolve_module_from_path is_resolve add_canonical env parent
        |> map_unresolved (fun p' -> `Dot (p', id))
        >>= fun (p, m) ->
        let (p', m') = handle_module_lookup env add_canonical id p m in
        return (p', m')
    | `Apply (m1, m2) -> begin
        let func = lookup_and_resolve_module_from_path is_resolve add_canonical env m1 in 
        let arg = lookup_and_resolve_module_from_path is_resolve add_canonical env m2 in
        match (func, arg) with
        | Resolved (func_path', m), Resolved (arg_path', _) ->
            return (handle_apply is_resolve env func_path' arg_path' m)
        | Unresolved func_path', Resolved (arg_path', _) ->
            Unresolved (`Apply (func_path', `Resolved (arg_path')))
        | Resolved (func_path', _), Unresolved arg_path' ->
            Unresolved (`Apply (`Resolved func_path', arg_path'))
        | Unresolved func_path', Unresolved arg_path' ->
            Unresolved (`Apply (func_path', arg_path'))
        end
    | `Resolved r ->
        return (lookup_and_resolve_module_from_resolved_path is_resolve add_canonical env r)
    | `Substituted s ->
        lookup_and_resolve_module_from_path is_resolve add_canonical env s
        |> map_unresolved (fun p -> `Substituted p)
        >>= fun (p, m) ->
        return (`Substituted p, m)
    | `Root r -> begin
        match Env.lookup_root_module r env with
        | Some (Env.Resolved (p,m)) -> return (add_hidden m (`Identifier (p :> Odoc_model.Paths.Identifier.t)),m)
        | Some (Env.Forward) -> Unresolved (`Forward r)
        | None -> Unresolved p
        end
    | `Forward f ->
        lookup_and_resolve_module_from_path is_resolve add_canonical env (`Root f)

and lookup_and_resolve_module_type_from_resolved_path : bool -> Env.t -> Cpath.resolved -> module_type_lookup_result = fun is_resolve env p ->
    match p with
    | `Local _id -> raise (Lookup_failure (env, p, "module_type"))
    | `Identifier (#Identifier.ModuleType.t as i) ->
        let m = Env.lookup_module_type i env in
        (`Identifier i, m)
    | `Substituted s ->
        let (p, m) = lookup_and_resolve_module_type_from_resolved_path is_resolve env s in
        (`Substituted p, m)
    | `ModuleType (p, id) ->
        let (p, m) = lookup_and_resolve_module_from_resolved_path is_resolve true env p in
        let (p', mt) = handle_module_type_lookup env id p m in
        (p', mt)
    | `Subst _
    | `SubstAlias _
    | `Hidden _
    | `Canonical _
    | `Alias (_, _)
    | `Apply (_, _)
    | `Module _
    | `Identifier _
    | `Class _
    | `ClassType _
    | `Type _ -> failwith "erk"
        

and lookup_and_resolve_module_type_from_path : bool -> Env.t -> Cpath.t -> (module_type_lookup_result, Cpath.t) ResultMonad.t =
    let open ResultMonad in
    fun is_resolve env p ->
        match p with
        | `Dot (parent, id) ->
            lookup_and_resolve_module_from_path is_resolve true env parent
            |> map_unresolved (fun p' -> `Dot (p', id)) >>= fun (p, m) ->
            let (p', mt) = handle_module_type_lookup env id p m in
            return (p', mt)
        | `Resolved r ->
            return (lookup_and_resolve_module_type_from_resolved_path is_resolve env r)
        | `Substituted s ->
            lookup_and_resolve_module_type_from_path is_resolve env s
            |> map_unresolved (fun p' -> `Substituted p') >>= fun (p, m) ->
            return (`Substituted p, m)
        | `Forward _ 
        | `Root _
        | `Apply (_,_) -> failwith "erk"

and lookup_type_from_resolved_path : Env.t -> Cpath.resolved -> type_lookup_result =
    fun env p ->
        match p with
        | `Local _id -> raise (Lookup_failure (env, p, "module_type"))
        | `Identifier (`CoreType name) ->
            (* CoreTypes aren't put into the environment, so they can't be handled by the 
            next clause. We just look them up here in the list of core types *)
            (`Identifier (`CoreType name), List.assoc (TypeName.to_string name) core_types)
        | `Identifier (#Identifier.Type.t as i) ->
            let t = Env.lookup_type i env in
            (`Identifier i, t)
        | `Substituted s ->
            let (p, t) = lookup_type_from_resolved_path env s in
            (`Substituted p, t)
        | `Type (p, id) ->
            let (p, m) = lookup_and_resolve_module_from_resolved_path true true env p in
            let (p', t) = handle_type_lookup env id p m in
            (p', t)
        (* None of the following should still exist when we fix up the types *)
        | `Subst _
        | `SubstAlias _
        | `Hidden _
        | `Canonical _
        | `Alias _
        | `Apply (_, _)
        | `Module _
        | `Identifier _
        | `Class _
        | `ClassType _
        | `ModuleType _ -> failwith "erk"
        
and lookup_type_from_path : Env.t -> Cpath.t -> (type_lookup_result, Cpath.t) ResultMonad.t =
    let open ResultMonad in
    fun env p ->
        match p with
        | `Dot (parent, id) ->
            lookup_and_resolve_module_from_path true true env parent
            |> map_unresolved (fun p' -> `Dot (p', id)) >>= fun (p, m) ->
            let (p', t) = handle_type_lookup env id p m in
            return (p', t)
        | `Resolved r ->
            return (lookup_type_from_resolved_path env r)
        | `Substituted s ->
            lookup_type_from_path env s
            |> map_unresolved (fun p' -> `Substituted p') >>= fun (p, m) ->
            return (`Substituted p, m)
        | `Forward _
        | `Root _
        | `Apply (_,_) -> failwith "erk"

and lookup_class_type_from_resolved_path : Env.t -> Cpath.resolved -> class_type_lookup_result =
    fun env p ->
        match p with
        | `Local _id -> raise (Lookup_failure (env, p, "module_type"))
        | `Identifier (`Class _ as c) ->
            let t = Env.lookup_class c env in
            (`Identifier c, C t)
        | `Identifier (`ClassType _ as c) ->
            let t = Env.lookup_class_type c env in
            (`Identifier c, CT t)
        | `Substituted s ->
            let (p, t) = lookup_class_type_from_resolved_path env s in
            (`Substituted p, t)
        | `Class (p, id) ->
            let (p, m) = lookup_and_resolve_module_from_resolved_path true true env p in
            let (p', t) = handle_class_type_lookup env id p m in
            (p', t)
        | `ClassType (p, id) ->
            let (p, m) = lookup_and_resolve_module_from_resolved_path true true env p in
            let (p', t) = handle_class_type_lookup env id p m in
            (p', t)
        (* None of the following should still exist when we fix up the types *)
        | `Subst _
        | `SubstAlias _
        | `Hidden _
        | `Canonical _
        | `Alias _
        | `Apply (_, _)
        | `Module _
        | `Identifier _
        | `Type _
        | `ModuleType _ -> failwith "erk"

and lookup_class_type_from_path : Env.t -> Cpath.t -> (class_type_lookup_result, Cpath.t) ResultMonad.t =
    let open ResultMonad in
    fun env p ->
        match p with
        | `Dot (parent, id) ->
            lookup_and_resolve_module_from_path true true env parent
            |> map_unresolved (fun p' -> `Dot (p', id)) >>= fun (p,m) ->
            let (p', c) = handle_class_type_lookup env id p m in
            return (p', c)
        | `Resolved r ->
            return (lookup_class_type_from_resolved_path env r)
        | `Substituted s ->
            lookup_class_type_from_path env s
            |> map_unresolved (fun p' -> `Substituted p') >>= fun (p, c) ->
            return (`Substituted p, c)
        | `Forward _
        | `Root _
        | `Apply (_,_) -> failwith "erk"

    
and lookup_signature_from_resolved_fragment : Env.t -> Cpath.resolved -> Fragment.Resolved.Signature.t -> Component.Signature.t -> Cpath.resolved * Component.Signature.t =
    fun env p f s ->
        match f with
        | `Root -> (p,s)
        | #Fragment.Resolved.Module.t as frag ->
            let (_, p, m) = lookup_module_from_resolved_fragment env p frag s in
            signature_of_module env (p,m)

and lookup_module_from_resolved_fragment : Env.t -> Cpath.resolved -> Fragment.Resolved.Module.t -> Component.Signature.t -> Ident.t * Cpath.resolved * Component.Module.t =
    fun env p f s ->
        match f with
        | `Subst (_, _)
        | `SubstAlias (_, _) -> failwith "What do we do with these?"
        | `Module (parent, name) ->
            let (ppath, sg) = lookup_signature_from_resolved_fragment env p parent s in
            let rec find = function
            | (Component.Signature.Module (id, _, m'))::_ when Ident.name id = Odoc_model.Names.ModuleName.to_string name -> (id, `Module (ppath, Ident.name id), Component.Delayed.get m')
            | _::xs -> find xs
            | [] -> failwith "Can't find it"
            in
            find sg.items
    
and lookup_module_from_fragment : Env.t -> Cpath.resolved -> Fragment.Module.t -> Component.Signature.t -> Ident.t * Cpath.resolved * Component.Module.t =
    fun env p f s ->
        match f with
        | `Dot (parent, name) ->
            let (ppath, sg) = lookup_signature_from_fragment env p parent s in
            let rec find = function
            | (Component.Signature.Module (id, _, m'))::_ when Ident.name id = name -> (id, `Module (ppath, name), Component.Delayed.get m')
            | _::xs -> find xs
            | [] -> failwith "Can't find it"
            in
            find sg.items
        | `Resolved r -> lookup_module_from_resolved_fragment env p r s

and lookup_signature_from_fragment : Env.t -> Cpath.resolved -> Fragment.Signature.t -> Component.Signature.t -> Cpath.resolved * Component.Signature.t =
    fun env p f s ->
        match f with
        | (`Dot (_, _)) as f' -> let (_, p, m) = lookup_module_from_fragment env p f' s in signature_of_module env (p, m)
        | `Resolved r -> lookup_signature_from_resolved_fragment env p r s

and module_type_expr_of_module_decl : Env.t -> Cpath.resolved * Component.Module.decl -> Cpath.resolved * Component.ModuleType.expr =
    fun env (p,decl) ->
    match decl with
    | Component.Module.Alias path -> begin
        match lookup_and_resolve_module_from_path false true env path with
        | Resolved (x, y) ->
            let (x', y') = module_type_expr_of_module env (x, y) in
            if Cpath.is_resolved_substituted x'
            then `SubstAlias (p, x'), y'
            else p, y'
        | Unresolved _ ->
            let err = Format.asprintf "Failed to lookup alias module (path=%a)" Component.Fmt.path path in
            failwith err
        end
    | Component.Module.ModuleType expr -> (p, expr)

and module_type_expr_of_module : Env.t -> Cpath.resolved * Component.Module.t -> Cpath.resolved * Component.ModuleType.expr =
    fun env (p, m) -> module_type_expr_of_module_decl env (p, m.type_)

and signature_of_module_type_expr : Env.t -> Cpath.resolved * Component.ModuleType.expr -> Cpath.resolved * Component.Signature.t =
    fun env (incoming_path, m) ->
        match m with
        | Component.ModuleType.Path p -> begin
(*            Format.fprintf Format.std_formatter "Looking up path: %a\n%!" Component.Fmt.path p;*)
            match lookup_and_resolve_module_type_from_path false env p with
            | Resolved (p, mt) -> begin
                let (p'', sg) = signature_of_module_type env (incoming_path, mt) in
                match Cpath.is_resolved_substituted p || Cpath.is_resolved_substituted p'', incoming_path with
                | true, p' ->
                    (`Subst (p, p')), sg
                | _ -> (incoming_path, sg)
                end
            | Unresolved _p ->
                let p = Component.Fmt.(string_of path p) in 
                failwith (Printf.sprintf "Couldn't find signature: %s" p)
            end
        | Component.ModuleType.Signature s -> (incoming_path, s)
        | Component.ModuleType.With (s, subs) ->
            let (p', sg) = signature_of_module_type_expr env (incoming_path, s) in
            let sg' = List.fold_left (fun sg sub ->
                match sub with
                | Component.ModuleType.ModuleEq (frag, Alias path) ->
                    fragmap_unresolved_module env p' frag (fun _ m -> Some Component.Module.{m with type_ = Alias path}) sg
                | ModuleEq (frag, ModuleType expr) ->
                    fragmap_unresolved_module env p' frag (fun _ m -> Some Component.Module.{m with type_ = ModuleType expr}) sg
                | ModuleSubst (frag, cpath) -> begin
                    let (id, _, _) = lookup_module_from_fragment env incoming_path frag sg in
                    match lookup_and_resolve_module_from_path false true env cpath with
                    | Resolved (path', _) ->
                        let s = Subst.add id (`Substituted path') Subst.identity in
                        (* It's important to remove the module from the signature before doing the substitution,
                           since Subst doesn't work on bound idents *)
                        Subst.signature s (fragmap_unresolved_module env incoming_path frag (fun _ _ -> None) sg)
                    | Unresolved _ ->
                        failwith "erk"
                    end
                | TypeEq (frag, expr) ->
                    fragmap_unresolved_type env p' frag (fun _ t -> Some Component.TypeDecl.{t with equation = expr}) sg
                | TypeSubst (_, _) ->
                    sg
            ) sg subs in
            (p', sg')
        | Component.ModuleType.Functor (_,expr) ->
            signature_of_module_type_expr env (incoming_path, expr)
        | Component.ModuleType.TypeOf decl ->
            signature_of_module_decl env ~is_canonical:false (incoming_path, decl)

and signature_of_module_type : Env.t -> Cpath.resolved * Component.ModuleType.t -> Cpath.resolved * Component.Signature.t =
    fun env (p,m) ->
        match m.expr with
        | None -> failwith "oh no"
        | Some expr -> signature_of_module_type_expr env (p,expr)

and signature_of_module_decl : Env.t -> is_canonical:bool -> Cpath.resolved * Component.Module.decl -> Cpath.resolved * Component.Signature.t =
    fun env ~is_canonical (incoming_path, decl) ->
        match decl with
        | Component.Module.Alias path -> begin
            match lookup_and_resolve_module_from_path false true env path with
            | Resolved (p', m) -> (* p' is the path to the aliased module *)
                let (p'', m') =
                    if is_canonical
                    then
                        signature_of_module env (incoming_path, m)
                    else
                        let (p'', m') = signature_of_module env (p', m) in
                        let m'' = Strengthen.signature p'' m' in
                        (* p'' is the path to the real module *)
                        let p''' = flatten_module_alias (`Alias (incoming_path, p'')) in
                        (p''',m'')
                in
                (p'', m')
            | Unresolved _ ->
                let err = Format.asprintf "Failed to lookup alias module (path=%a)" Component.Fmt.path path in
                failwith err
            end
        | Component.Module.ModuleType expr -> signature_of_module_type_expr env (incoming_path, expr)

and signature_of_module : Env.t -> Cpath.resolved * Component.Module.t -> Cpath.resolved * Component.Signature.t =
    fun env (incoming_path, m) ->
        match m.canonical with
        | Some _ ->
            signature_of_module_decl env ~is_canonical:true (incoming_path, m.type_)
        | None ->
            signature_of_module_decl env ~is_canonical:false (incoming_path, m.type_)

and handle_removed : Component.Signature.item list -> Component.Signature.removed_item list -> Component.Signature.removed_item list = fun l acc ->
    let open Component.Signature in
    List.map (function
        | Module (id,_,_) -> RModule (id, None)
        | Type (id,_,_) -> RType (id, None)
        | _ -> failwith "Can't remove anything but modules or types") l
    @ acc

and fragmap_signature : Env.t -> Cpath.resolved -> Fragment.Resolved.Signature.t -> (Cpath.resolved -> Component.Signature.t -> Component.Signature.t) -> Component.Signature.t -> Component.Signature.t =
    fun env p frag fn sg ->
        match frag with
        | `Root -> fn p sg
        | `Module (parent, name) ->
            fragmap_signature env p parent (fun p s ->
                let items = List.map (function
                    | Component.Signature.Module (id,r,m) when (Ident.name id = ModuleName.to_string name) ->
                        let m = Component.Delayed.get m in
                        let (p, sg) = signature_of_module env (`Module (p, name), m) in
                        let sg' = fn (`Module (p, name)) sg in
                        Component.Signature.Module (id,r, Component.Delayed.put (fun () -> {m with type_=ModuleType (Component.ModuleType.Signature sg'); canonical=None; hidden=false}))
                    | x -> x) s.items in
                {items; removed=s.removed}) sg
        | _ -> failwith "foo"

and fragmap_module : Env.t -> Cpath.resolved -> Fragment.Resolved.Module.t -> (Cpath.resolved -> Component.Module.t -> Component.Module.t option) -> Component.Signature.t -> Component.Signature.t =
    fun env p frag fn sg ->
        let open OptionMonad in
        match frag with
        | `Module (parent, name) ->
            fragmap_signature env p parent (fun p s ->
                let items, removed = filter_map_record_removed (function
                | Component.Signature.Module (id,r,m) when Ident.name id = (ModuleName.to_string name) ->
                    let m = Component.Delayed.get m in
                    fn (`Module (p, name)) m >>= fun m' -> return (Component.Signature.Module (id, r, Component.Delayed.put (fun () -> m')))
                | x -> return x) s.items in
                {items; removed=handle_removed removed s.removed}) sg
        | `Subst (_, x) -> fragmap_module env p x fn sg
        | `SubstAlias (_, x) -> fragmap_module env p x fn sg

and fragmap_type : Env.t -> Cpath.resolved -> Fragment.Resolved.Type.t -> (Cpath.resolved -> Component.TypeDecl.t -> Component.TypeDecl.t option) -> Component.Signature.t -> Component.Signature.t =
    fun env p frag fn sg ->
        let open OptionMonad in
        match frag with
            | `Type (parent, name) ->
                fragmap_signature env p parent (fun p s ->
                    let items, removed = filter_map_record_removed (function
                    | Component.Signature.Type (id, r, t) when Ident.name id = TypeName.to_string name ->
                        fn (`Type (p, name)) t >>= fun t' -> return (Component.Signature.Type (id, r, t'))
                    | x -> Some x) s.items in
                {items; removed=handle_removed removed s.removed}) sg
            | `Class _
            | `ClassType _ -> failwith "Unhandled in fragmap_type"

and fragmap_unresolved_signature : Env.t -> Cpath.resolved -> Fragment.Signature.t -> (Cpath.resolved -> Component.Signature.t -> Component.Signature.t) -> Component.Signature.t -> Component.Signature.t =
    fun env p frag fn sg ->
        match frag with
        | `Dot (parent, name) ->
            fragmap_unresolved_signature env p parent (fun p s ->
                let items, removed = filter_map_record_removed (function
                    | Component.Signature.Module (id, r, m) when Ident.name id = name ->
                        let m = Component.Delayed.get m in
                        let p, sg = signature_of_module env (p, m) in
                        let sg' = fn (`Type (p, name)) sg in
                        Some (Component.Signature.Module (id,   r, Component.Delayed.put (fun () -> {m with type_=ModuleType (Component.ModuleType.Signature sg'); canonical=None; hidden=false})))
                    | x -> Some x) s.items in
                {items; removed=handle_removed removed s.removed}) sg 
        | `Resolved x ->
            fragmap_signature env p x fn sg

and fragmap_unresolved_module : Env.t -> Cpath.resolved -> Fragment.Module.t -> (Cpath.resolved -> Component.Module.t -> Component.Module.t option) -> Component.Signature.t -> Component.Signature.t =
    fun env p frag fn sg ->
        let open OptionMonad in
        match frag with
        | `Dot (parent, name) ->
            fragmap_unresolved_signature env p parent (fun p s ->
                let items,removed = filter_map_record_removed (function
                    | Component.Signature.Module (id, r, m) when Ident.name id = name ->
                        let m = Component.Delayed.get m in
                        fn (`Module (p, name)) m >>= fun m' -> return (Component.Signature.Module (id, r, Component.Delayed.put (fun () -> m')))
                    | x -> Some x) s.items in
                {items; removed=handle_removed removed s.removed}) sg
        | `Resolved x ->
            fragmap_module env p x fn sg

and fragmap_unresolved_type : Env.t -> Cpath.resolved -> Fragment.Type.t -> (Cpath.resolved -> Component.TypeDecl.t -> Component.TypeDecl.t option) -> Component.Signature.t -> Component.Signature.t =
    fun env p frag fn sg ->
        let open OptionMonad in
        match frag with
        | `Dot (parent, name) ->
            fragmap_unresolved_signature env p parent (fun p s ->
                let items, removed = filter_map_record_removed (function
                    | Component.Signature.Type (id, r, t) when Ident.name id = name ->
                        fn (`Type (p, name)) t >>= fun t' -> return (Component.Signature.Type (id, r, t'))
                    | x -> Some x) s.items in
                {items; removed=handle_removed removed s.removed}) sg
        | `Resolved x ->
            fragmap_type env p x fn sg

and find_module_with_replacement : Env.t -> Component.Signature.t -> string -> Component.Module.t = fun env sg name ->
    match Component.Find.careful_module_in_sig sg name with
    | Found m ->
        m
    | Replaced path ->
        let _, m = lookup_module_from_resolved_path env path in
        m

and find_type_with_replacement : Env.t -> Component.Signature.t -> string -> Component.TypeDecl.t = fun env sg name ->
    match Component.Find.careful_type_in_sig sg name with
    | Found m ->
        m
    | Replaced path ->
        let _, m = lookup_type_from_resolved_path env path in
        m

and resolve_resolved_signature_fragment : Env.t -> Odoc_model.Paths.Identifier.Signature.t -> Odoc_model.Paths.Fragment.Resolved.Signature.t -> (Odoc_model.Paths.Fragment.Resolved.Signature.t * Cpath.resolved * Component.Signature.t) =
fun env id frag ->
    match frag with
    | `Root -> begin
        let (cp, sg) =
            match id with
            | `ModuleType _ as id ->
                let mt = Env.lookup_module_type id env in
                signature_of_module_type env (`Identifier id, mt) |> prefix_signature
            | #Odoc_model.Paths.Identifier.Module.t as id ->
                let m = Env.lookup_module id env in
                signature_of_module env (`Identifier id, m) |> prefix_signature
        in
        (`Root, cp, sg)
        end
    | `Module (parent, name) -> begin
            let (parent,cp,sg) = resolve_resolved_signature_fragment env id parent in
            let m' = find_module_with_replacement env sg name in
            let (cp,sg) = signature_of_module env (`Module (cp, name), m') |> prefix_signature in
            (`Module (parent, Odoc_model.Names.ModuleName.of_string name), cp, sg)
        end
    | _ -> failwith "foo"


and resolve_signature_fragment : Env.t -> Odoc_model.Paths.Identifier.Signature.t -> Odoc_model.Paths.Fragment.Signature.t -> (Odoc_model.Paths.Fragment.Resolved.Signature.t * Cpath.resolved * Component.Signature.t) =
    fun env id frag ->
        match frag with
        | `Resolved r -> resolve_resolved_signature_fragment env id r
        | `Dot (parent, name) ->
            let (parent,cp,sg) = resolve_signature_fragment env id parent in
            let m' = find_module_with_replacement env sg name in
            let (cp,sg) = signature_of_module env (`Module (cp, name), m') |> prefix_signature in
            (`Module (parent, Odoc_model.Names.ModuleName.of_string name), cp, sg)

and resolve_resolved_module_fragment :
    Env.t ->
    Odoc_model.Paths.Identifier.Signature.t ->
    Odoc_model.Paths.Fragment.Resolved.Module.t ->
    (Odoc_model.Paths.Fragment.Resolved.Module.t * Cpath.resolved * Component.Module.t) =
    fun env id frag ->
        match frag with
        | `Module (parent, name) ->
            let (parent,cp,sg) = resolve_resolved_signature_fragment env id parent in
            let m' = find_module_with_replacement env sg (Odoc_model.Names.ModuleName.to_string name) in
            (`Module (parent, Odoc_model.Names.ModuleName.of_string name), `Module(cp, Odoc_model.Names.ModuleName.to_string name), m')
        | _ -> failwith ""

and resolve_module_fragment : Env.t -> Odoc_model.Paths.Identifier.Signature.t -> Odoc_model.Paths.Fragment.Module.t -> Odoc_model.Paths.Fragment.Resolved.Module.t =
    fun env id frag ->
        match frag with
        | `Resolved r ->
            let (result,_,_) = resolve_resolved_module_fragment env id r in
            result
        | `Dot(parent, name) ->
            let (parent,_cp,sg) = resolve_signature_fragment env id parent in
            let _ = find_module_with_replacement env sg (Odoc_model.Names.ModuleName.to_string name) in
            `Module (parent, Odoc_model.Names.ModuleName.of_string name)

and resolve_resolved_type_fragment :
Env.t ->
Odoc_model.Paths.Identifier.Signature.t ->
Odoc_model.Paths.Fragment.Resolved.Type.t ->
(Odoc_model.Paths.Fragment.Resolved.Type.t * Cpath.resolved * Component.TypeDecl.t) =
    fun env id frag ->
        match frag with
        | `Type (parent, name) ->
        let (parent,cp,sg) = resolve_resolved_signature_fragment env id parent in
        let t' = Component.Find.type_in_sig sg (Odoc_model.Names.TypeName.to_string name) in
        (`Type (parent, Odoc_model.Names.TypeName.of_string name), `Type(cp, Odoc_model.Names.TypeName.to_string name), t')
    | _ -> failwith ""

    and resolve_type_fragment : Env.t -> Odoc_model.Paths.Identifier.Signature.t -> Odoc_model.Paths.Fragment.Type.t -> Odoc_model.Paths.Fragment.Resolved.Type.t =
    fun env id frag ->
        match frag with
        | `Resolved r ->
            let (result,_,_) = resolve_resolved_type_fragment env id r in
            result
        | `Dot(parent, name) ->
            let (parent,_cp,sg) = resolve_signature_fragment env id parent in
            let _ = Component.Find.type_in_sig sg (Odoc_model.Names.TypeName.to_string name) in
            `Type (parent, Odoc_model.Names.TypeName.of_string name)

let rec class_signature_of_class : Env.t -> Cpath.resolved * Component.Class.t -> Cpath.resolved * Component.ClassSignature.t =
    fun env (p,c) ->
        let rec inner decl =
            match decl with
            | Component.Class.ClassType e -> class_signature_of_class_type_expr env (p,e)
            | Arrow (_,_,d) -> inner d
        in
        inner c.type_

and class_signature_of_class_type_expr : Env.t -> Cpath.resolved * Component.ClassType.expr -> Cpath.resolved * Component.ClassSignature.t =
    fun env (p,e) ->
        match e with
        | Signature s -> (p,s)
        | Constr (p,_) ->
            let (p,c) =
                match lookup_class_type_from_path env p with
                | Resolved (p,c) -> (p,c)
                | _ -> failwith "error"
            in
            match c with
            | C c -> class_signature_of_class env (p,c)
            | CT _c -> failwith "error"

