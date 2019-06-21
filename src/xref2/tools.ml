open Odoc_model.Names

let map_opt m f = match m with | Some x -> Some (f x) | None -> None
let filter_map f l =
    let rec inner = function
      | x::xs -> begin match f x with | Some y -> y::inner xs | None -> inner xs end
      | [] -> []
    in inner l
        
let prefix_signature (path, s) =
    let open Component.Signature in
    let sub = List.fold_left (fun map item ->
        match item with
        | Type t -> Subst.add t.id (`Global (`Resolved (`Type (path, TypeName.of_string (Ident.name t.id)) :> Odoc_model.Paths.Path.Resolved.t))) map
        | Module m -> Subst.add m.id (`Global (`Resolved (`Module (path, ModuleName.of_string (Ident.name m.id)) :> Odoc_model.Paths.Path.Resolved.t))) map
        | ModuleType mt -> Subst.add mt.id (`Global (`Resolved (`ModuleType (path, ModuleTypeName.of_string (Ident.name mt.id)) :> Odoc_model.Paths.Path.Resolved.t))) map)
            Subst.identity s in
    let new_sig = List.map (function
            | Module m -> Module {(Subst.module_ sub m) with id=Ident.rename m.id}
            | ModuleType mt -> ModuleType {(Subst.module_type sub mt) with id=Ident.rename mt.id}
            | Type t -> Type {(Subst.type_ sub t) with id=Ident.rename t.id}) s in
    (path, new_sig)

let flatten_module_alias : Odoc_model.Paths.Path.Resolved.Module.t -> Odoc_model.Paths.Path.Resolved.Module.t = function
  | `Alias (x, `Alias (_, z)) -> `Alias (x, z)
  | x -> x

type module_lookup_result =
    Odoc_model.Paths.Path.Resolved.Module.t * Component.Module.t

type module_type_lookup_result =
    Odoc_model.Paths.Path.Resolved.ModuleType.t * Component.ModuleType.t

type type_lookup_result =
    Odoc_model.Paths.Path.Resolved.Type.t * Component.Type.t

exception Lookup_failure of Env.t * Cpath.t * string

let rec lookup_module_from_model_path : Env.t -> Odoc_model.Paths.Path.Module.t -> (module_lookup_result, Odoc_model.Paths.Path.Module.t) Result.result = fun env p ->
    match p with
    | `Resolved p -> Ok (lookup_module_from_resolved_model_path env p)
    | `Dot (m, x) -> begin
        match lookup_module_from_model_path env m with
        | Error p -> Error (`Dot (p, x))
        | Ok (p', m) ->
            let p', s = signature_of_module env (p', m) |> prefix_signature in
            let m' = Component.Find.module_in_sig s x in
            Ok (`Module (p', Odoc_model.Names.ModuleName.of_string x), m')
            end
    | _ -> Error p

    
and lookup_module_from_resolved_model_path : Env.t -> Odoc_model.Paths.Path.Resolved.Module.t -> module_lookup_result = fun env p ->
    match p with
    | `Identifier i ->
        let m = Env.lookup_module i env in
        (p, m)
    | `Hidden m ->
        let (p, m) = lookup_module_from_resolved_model_path env m in
        (`Hidden p, m)
    | `Module (p, name) ->
        let (p', sg) =
            lookup_module_from_resolved_model_path env p
            |> signature_of_module env
            |> prefix_signature in
        let m' = Component.Find.module_in_sig sg (Odoc_model.Names.ModuleName.to_string name) in
        (`Module (p', name), m')
    | `Alias (p1, p2) ->
        let (p2', m) = lookup_module_from_resolved_model_path env p2 in
        (`Alias (p1, p2'), m)
    | `Apply (func_path, arg_path) ->
        let (func_path', m) = lookup_module_from_resolved_model_path env func_path in
        let arg_path' =
            match lookup_module_from_model_path env arg_path with
            | Ok (arg_path', _) -> `Resolved arg_path'
            | Error p -> p
        in
        let (_, mty) = module_type_of_module env (Some func_path', m) in
        let arg_id = match mty with
          | Component.ModuleType.Functor (Some arg, _expr) ->
            arg.Component.FunctorArgument.id
          | _ -> failwith "Invalid"
        in
        (func_path', Subst.module_ (Subst.add arg_id (`Global (arg_path' :> Odoc_model.Paths.Path.t)) Subst.identity) m)
    | _ ->
        let b = Buffer.create 1024 in
        let fmt = Format.formatter_of_buffer b in
        Format.fprintf fmt "Failed to lookup path: %a@." Component.Fmt.model_resolved_path (p :> Odoc_model.Paths.Path.Resolved.t);
        failwith (Buffer.contents b)

and lookup_module_from_path : Env.t -> Cpath.t -> (module_lookup_result, Odoc_model.Paths.Path.Module.t) Result.result = fun env p ->
    match p with
    | `Local _id -> raise (Lookup_failure (env, p, "module")) 
    | `Dot (parent, id) -> begin
        match lookup_module_from_path env parent with
        | Ok (p, m) ->
            let (p', sg) = signature_of_module env (p, m) |> prefix_signature in
            let m' = Component.Find.module_in_sig sg id in
            Ok (`Module (p', Odoc_model.Names.ModuleName.of_string id), m')
        | Error p ->
            Error (`Dot (p, id))
        end
    | `Global ((
            `Root _
            | `Forward _
            | `Dot (#Odoc_model.Paths.Path.Module.t, _)
            | `Resolved (#Odoc_model.Paths_types.Resolved_path.module_no_id)
            | `Resolved (`Identifier (#Odoc_model.Paths_types.Identifier.module_))
            | `Apply (_,_)) as p) ->
        lookup_module_from_model_path env p
    | _ -> failwith "bad lookup"

and lookup_module_type_from_model_path : Env.t -> Odoc_model.Paths.Path.ModuleType.t -> (module_type_lookup_result, Odoc_model.Paths.Path.ModuleType.t) Result.result = fun env p ->
    match p with
    | `Resolved p -> Ok (lookup_module_type_from_resolved_model_path env p)
    | `Dot (m, x) -> begin
        match lookup_module_from_model_path env m with
        | Error p -> Error (`Dot (p, x))
        | Ok (p', m) ->
            let (p', s) = signature_of_module env (p', m) in
            let m' = Component.Find.module_type_in_sig s x in
            Ok (`ModuleType (p', Odoc_model.Names.ModuleTypeName.of_string x), m')
        end

and lookup_module_type_from_resolved_model_path : Env.t -> Odoc_model.Paths.Path.Resolved.ModuleType.t -> module_type_lookup_result = fun env p ->
    match p with
    | `Identifier i ->
        let m = Env.lookup_module_type i env in
        (p, m)
    | `ModuleType (parent, name) ->
        let (p', sg) =
            lookup_module_from_resolved_model_path env parent
             |> signature_of_module env in
        let mt = Component.Find.module_type_in_sig sg (ModuleTypeName.to_string name) in
        (`ModuleType (p', name), mt)


and lookup_module_type_from_path : Env.t -> Cpath.t -> (module_type_lookup_result, Odoc_model.Paths.Path.ModuleType.t) Result.result = fun env p ->
    match p with
    | `Local _id -> raise (Lookup_failure (env, p, "module_type"))
    | `Dot (parent, id) -> begin
        match lookup_module_from_path env parent with
        | Ok (p, m) ->
            let (p', sg) = signature_of_module env (p, m) in
            let m' = Component.Find.module_type_in_sig sg id in
            Ok (`ModuleType (p', Odoc_model.Names.ModuleTypeName.of_string id), m')
        | Error p ->
            Error (`Dot (p, id))
        end
    | `Global ((
              `Resolved (#Odoc_model.Paths_types.Resolved_path.module_type_no_id)
            | `Resolved (`Identifier (#Odoc_model.Paths_types.Identifier.module_type))
            | `Dot (_,_)) as p) ->
        lookup_module_type_from_model_path env p
    | _ -> failwith "bad lookup"

and lookup_type_from_model_path : Env.t -> Odoc_model.Paths.Path.Type.t -> (type_lookup_result, Odoc_model.Paths.Path.Type.t) Result.result = fun env p ->
    match p with
    | `Resolved p -> Ok (lookup_type_from_resolved_model_path env p)
    | `Dot (m, x) -> begin
        match lookup_module_from_model_path env m with
        | Error p -> Error (`Dot (p, x))
        | Ok (p', m) ->
            let (p', s) = signature_of_module env (p', m) in
            let m' = Component.Find.type_in_sig s x in
            Ok (`Type (p', Odoc_model.Names.TypeName.of_string x), m')
        end

and lookup_type_from_resolved_model_path : Env.t -> Odoc_model.Paths.Path.Resolved.Type.t -> type_lookup_result = fun env p ->
    match p with
    | `Identifier (`Type _ as i) ->
        let m = Env.lookup_type i env in
        (p, m)
    | `Type (parent, name) ->
        let (p', sg) =
            lookup_module_from_resolved_model_path env parent
            |> signature_of_module env in
        let t = Component.Find.type_in_sig sg (TypeName.to_string name) in
        (`Type (p', name), t)
    | _ -> failwith "Unhandled"

and lookup_type_from_path : Env.t -> Cpath.t -> (type_lookup_result, Odoc_model.Paths.Path.Type.t) Result.result = fun env p ->
    match p with
    | `Local _id -> raise (Lookup_failure (env, p, "type"))
    | `Dot (parent, id) -> begin
        match lookup_module_from_path env parent with
        | Ok (p, m) ->
            let (p', sg) = signature_of_module env (p, m) in
            let t' = Component.Find.type_in_sig sg id in
            Ok (`Type (p', Odoc_model.Names.TypeName.of_string id), t')
        | Error p ->
            Error (`Dot (p, id))
        end
    | `Global ((
              `Resolved (#Odoc_model.Paths_types.Resolved_path.type_no_id)
            | `Resolved (`Identifier (#Odoc_model.Paths_types.Identifier.type_))
            | `Dot (_,_)) as p) ->
        lookup_type_from_model_path env p
    | _ -> failwith "bad lookup"

and lookup_signature_from_resolved_fragment : Env.t -> Odoc_model.Paths.Fragment.Resolved.Signature.t -> Component.Signature.t -> Component.Signature.t = fun env p s ->
    match p with
    | `Root -> s
    | #Odoc_model.Paths.Fragment.Resolved.Module.t as frag ->
        let m = lookup_module_from_resolved_fragment env frag s in
        signature_of_module_nopath env m

and lookup_module_from_resolved_fragment : Env.t -> Odoc_model.Paths.Fragment.Resolved.Module.t -> Component.Signature.t -> Component.Module.t = fun env p s ->
    match p with
    | `Subst (_, p) -> lookup_module_from_resolved_fragment env p s
    | `SubstAlias (_, p) -> lookup_module_from_resolved_fragment env p s
    | `Module (parent, name) ->
        let sg = lookup_signature_from_resolved_fragment env parent s in
        let rec find = function
          | (Component.Signature.Module m')::_ when Ident.name m'.Component.Module.id = Odoc_model.Names.ModuleName.to_string name -> m'
          | _::xs -> find xs
          | [] -> failwith "Can't find it"
        in
        find sg
    
and lookup_module_from_fragment : Env.t -> Odoc_model.Paths.Fragment.Module.t -> Component.Signature.t -> Component.Module.t = fun env p s ->
    match p with
    | `Dot (parent, name) ->
        let sg = lookup_signature_from_fragment env parent s in
        let rec find = function
          | (Component.Signature.Module m')::_ when Ident.name m'.Component.Module.id = name -> m'
          | _::xs -> find xs
          | [] -> failwith "Can't find it"
        in
        find sg
    | `Resolved r -> lookup_module_from_resolved_fragment env r s

and lookup_signature_from_fragment : Env.t -> Odoc_model.Paths.Fragment.Signature.t -> Component.Signature.t -> Component.Signature.t = fun env p s ->
    match p with
    | `Dot (parent, name) ->
        let sg = lookup_signature_from_fragment env parent s in
        let rec find = function
        | (Component.Signature.Module m')::_ when Ident.name m'.Component.Module.id = name -> m'
        | _::xs -> find xs
        | [] -> failwith "Can't find it"
        in
        let m = find sg in
        signature_of_module_nopath env m
    | `Resolved r ->
        lookup_signature_from_resolved_fragment env r s

and module_type_of_module : Env.t -> Odoc_model.Paths.Path.Resolved.Module.t option * Component.Module.t -> Odoc_model.Paths.Path.Resolved.Module.t option * Component.ModuleType.expr =
    fun env (p,m) ->
        match m.Component.Module.type_ with
        | Component.Module.Alias path -> begin
            match lookup_module_from_path env path with
            | Ok (x,y) -> module_type_of_module env (Some x, y)
            | Error _ -> failwith "Failed to lookup alias module"
            end
        | Component.Module.ModuleType expr -> (p,expr)

and signature_of_module_type_expr : Env.t -> Odoc_model.Paths.Path.Resolved.Module.t option * Component.ModuleType.expr -> Odoc_model.Paths.Path.Resolved.Module.t option * Component.Signature.t =
    fun env (incoming_path, m) ->
        match m with
        | Component.ModuleType.Path p -> begin
            match lookup_module_type_from_path env p with
            | Ok (_p, mt) ->
                let (_p, sg) = signature_of_module_type env (incoming_path, mt) in
                (incoming_path, sg)
            | Error _p ->
                failwith "Couldn't find signature"
            end
        | Component.ModuleType.Signature s -> (incoming_path, s)
        | Component.ModuleType.With (s, subs) ->
            let (p', sg) = signature_of_module_type_expr env (incoming_path, s) in
            let sg' = List.fold_left (fun sg sub ->
                match sub with
                | Component.ModuleType.ModuleEq (frag, Alias path) -> begin
                    match lookup_module_from_path env path with
                    | Ok (p, m) ->
                        let m' = Strengthen.module_ p m in
                        fragmap_unresolved_module env frag (fun _ -> Some m') sg
                    | Error _ ->
                        failwith "Failed to lookup substitute module"
                    end
                | ModuleEq (frag, ModuleType expr) ->
                    fragmap_unresolved_module env frag (fun m -> Some Component.Module.{m with type_ = ModuleType expr}) sg
                | ModuleSubst (frag, cpath) ->
                    let m = lookup_module_from_fragment env frag sg in
                    let id = m.Component.Module.id in
                    let subst = Subst.add id cpath Subst.identity in
                    fragmap_unresolved_module env frag (fun _ -> None) (Subst.signature subst sg)
                | TypeEq (_, _) ->
                    sg
                | TypeSubst (_, _) ->
                    sg
                ) sg subs in
            (p', sg')
        | Component.ModuleType.Functor (_,_) ->
            failwith "Unhandled me"

and signature_of_module_type : Env.t -> Odoc_model.Paths.Path.Resolved.Module.t option * Component.ModuleType.t -> Odoc_model.Paths.Path.Resolved.Module.t option * Component.Signature.t = fun env (p,m) ->
    match m.expr with
    | None -> failwith "oh no"
    | Some expr -> signature_of_module_type_expr env (p,expr)

and signature_of_module_nopath : Env.t -> Component.Module.t -> Component.Signature.t =
    fun env m ->
    match m.Component.Module.type_ with
    | Component.Module.Alias path -> begin
        match lookup_module_from_path env path with
        | Ok (p', m) -> (* p' is the path to the aliased module *)
            let (p'', m') = signature_of_module env (p', m) in
            let m'' = Strengthen.signature p'' m' in
            (* p'' is the path to the real module *)
            m''
        | Error _ ->
            failwith "Failed to lookup alias module"
        end
    | Component.Module.ModuleType expr ->
        snd @@ signature_of_module_type_expr env (None, expr)

and signature_of_module : Env.t -> Odoc_model.Paths.Path.Resolved.Module.t * Component.Module.t -> Odoc_model.Paths.Path.Resolved.Module.t * Component.Signature.t =
    fun env (incoming_path, m) ->
    match m.Component.Module.type_ with
    | Component.Module.Alias path -> begin
        match lookup_module_from_path env path with
        | Ok (p', m) -> (* p' is the path to the aliased module *)
            let (p'', m') = signature_of_module env (p', m) in
            let m'' = Strengthen.signature p'' m' in
            (* p'' is the path to the real module *)
            let p''' = flatten_module_alias (`Alias (incoming_path, p'')) in
            (p''', m'')
        | Error _ ->
            failwith "Failed to lookup alias module"
        end
    | Component.Module.ModuleType expr ->
        match signature_of_module_type_expr env (Some incoming_path, expr) with
        | Some p, sg -> (p, sg)
        | None, sg -> (incoming_path, sg)

and fragmap_signature : Env.t -> Odoc_model.Paths.Fragment.Resolved.Signature.t -> (Component.Signature.t -> Component.Signature.t option) -> Component.Signature.t -> Component.Signature.t =
    fun env frag fn sg ->
        match frag with
        | `Root -> (match fn sg with Some sg -> sg | None -> failwith "Ugh?")
        | `Module (parent, name) ->
            fragmap_signature env parent (fun s ->
                let r = filter_map (function
                    | Component.Signature.Module m when (Ident.name m.id = ModuleName.to_string name) ->
                        let sg = signature_of_module_nopath env m in
                        map_opt (fn sg) (fun sg' ->
                            Component.Signature.Module {id=m.id; type_=ModuleType (Component.ModuleType.Signature sg')})
                    | x -> Some x) s in
                Some r) sg
        | _ -> failwith "foo"

and fragmap_module : Env.t -> Odoc_model.Paths.Fragment.Resolved.Module.t -> (Component.Module.t -> Component.Module.t option) -> Component.Signature.t -> Component.Signature.t =
    fun env frag fn sg ->
        match frag with
        | `Module (parent, name) ->
            fragmap_signature env parent (fun s ->
                let r = filter_map (function
                | Component.Signature.Module m when Ident.name m.id = (ModuleName.to_string name) ->
                    map_opt (fn m) (fun m' ->
                        Component.Signature.Module {m' with id=m.id})
                | x -> Some x) s
                in Some r) sg
        | `Subst (_, x) -> fragmap_module env x fn sg
        | `SubstAlias (_, x) -> fragmap_module env x fn sg


(*        fragmap_signature env (frag : Odoc_model.Paths.Fragment.Resolved.Module.t :> Odoc_model.Paths.Fragment.Resolved.Signature.t) fn sg*)

and fragmap_unresolved_signature env frag fn sg =
    match frag with
    | `Dot (parent, name) ->
        fragmap_unresolved_signature env parent (fun s ->
            let r = filter_map (function
                | Component.Signature.Module m when Ident.name m.id = name ->
                    let sg = signature_of_module_nopath env m in
                    map_opt (fn sg) (fun sg' ->
                    Component.Signature.Module {id=m.id; type_=ModuleType (Component.ModuleType.Signature sg')})
                | x -> Some x) s
            in Some r) sg 
    | `Resolved x ->
        fragmap_signature env x fn sg

and fragmap_unresolved_module : Env.t -> Odoc_model.Paths.Fragment.Module.t -> (Component.Module.t -> Component.Module.t option) -> Component.Signature.t -> Component.Signature.t = fun env frag fn sg ->
    match frag with
    | `Dot (parent, name) ->
        fragmap_unresolved_signature env parent (fun s ->
            let r = filter_map (function
                | Component.Signature.Module m when Ident.name m.id = name ->
                    map_opt (fn m)
                     (fun m' -> Component.Signature.Module {m' with id=m.id})
                | x -> Some x) s in
            Some r) sg
    | `Resolved x ->
        fragmap_module env x fn sg

