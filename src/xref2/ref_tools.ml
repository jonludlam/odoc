open Odoc_model.Paths
open Reference

type module_lookup_result =
  Resolved.Module.t * Component.Module.t

type module_type_lookup_result =
  Resolved.ModuleType.t * Component.ModuleType.t

type signature_lookup_result =
  Resolved.Signature.t * Component.Signature.t

type type_lookup_result =
  Resolved.Type.t * [ `T of Component.TypeDecl.t | `C of Component.Class.t | `CT of Component.ClassType.t ]

type value_lookup_result =
  Resolved.Value.t * Component.Value.t

type label_parent_lookup_result =
  Resolved.LabelParent.t * [ `S of Component.Signature.t | `CS of Component.ClassSignature.t ]

let rec choose l =
  match l with
  | [] -> None
  | x :: rest ->
    match x () with
    | Some _ as x -> x
    | None -> choose rest

let signature_lookup_result_of_label_parent : label_parent_lookup_result -> signature_lookup_result option =
  fun (rr,c) ->
    match (rr, c) with
    | `Identifier (#Odoc_model.Paths.Identifier.Signature.t) as rr', `S s -> Some (rr', s)
    | `SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _  as rr', `S s -> Some (rr', s)
    | _ -> None

let rec resolve_type_reference : Env.t -> Type.t -> type_lookup_result option =
  let open Tools.OptionMonad in
  fun env r ->
  match r with
  | `Resolved _r -> failwith "unhandled"
  | `Root (name, _) -> begin
    Env.lookup_datatype_by_name name env >>= function
        | `Type (id, t) -> return (`Identifier (id :> Odoc_model.Paths_types.Identifier.reference_type), `T t)
        | `Class (id, t) -> return (`Identifier (id :> Odoc_model.Paths_types.Identifier.reference_type), `C t)
        | `ClassType (id, t) -> return (`Identifier (id :> Odoc_model.Paths_types.Identifier.reference_type), `CT t)
    end 
  | `Dot (parent, name) -> begin
    resolve_label_parent_reference env parent
    >>= signature_lookup_result_of_label_parent
    >>= fun (parent', sg) -> Component.Find.opt_type_in_sig sg name >>= fun t ->
      return (`Type (parent', name), t)
    end
  | `Class (parent, name) ->
    resolve_signature_reference env parent >>= fun (parent', sg) ->
    Component.Find.opt_type_in_sig sg name >>= begin function | `C _ as c -> return (`Class (parent', name), c) | _ -> None end
  | `ClassType (parent, name) ->
    resolve_signature_reference env parent >>= fun (parent', sg) ->
    Component.Find.opt_type_in_sig sg name >>= begin function | `CT _ as c -> return (`ClassType (parent', name), c) | _ -> None end
  | `Type (parent, name) ->
    resolve_signature_reference env parent >>= fun (parent', sg) ->
    Component.Find.opt_type_in_sig sg name >>= begin function | `T _ as c -> return (`Type (parent', name), c) | _ -> None end

and resolve_resolved_type_reference : Env.t -> Resolved.Type.t -> type_lookup_result =
  fun env r ->
  match r with
  | `Identifier i -> begin
    match i with
    | `Type _  as i' -> let t = Env.lookup_type i' env in (r, `T t)
    | `Class _ as i' -> let t = Env.lookup_class i' env in (r, `C t)
    | `ClassType _ as i' -> let t = Env.lookup_class_type i' env in (r, `CT t)
    | `CoreType _ -> failwith "core type"
    end
  | `Type (parent, name) ->
    resolve_resolved_signature_reference env parent |> fun (parent', sg) ->
    Component.Find.type_in_sig sg name |> begin function | `T _ as c -> `Type (parent', name), c | _ -> failwith "error" end
  | `ClassType (parent, name) ->
    resolve_resolved_signature_reference env parent |> fun (parent', sg) ->
    Component.Find.type_in_sig sg name |> begin function | `CT _ as c -> `ClassType (parent', name), c | _ -> failwith "error" end
  | `Class (parent,name) ->
    resolve_resolved_signature_reference env parent |> fun (parent', sg) ->
    Component.Find.type_in_sig sg name |> begin function | `CT _ as c -> `ClassType (parent', name), c | _ -> failwith "error" end

and resolve_resolved_signature_reference : Env.t -> Resolved.Signature.t -> signature_lookup_result =
fun env r ->
match r with
| `Identifier i ->
  let sg = 
    match i with
    | `Module _
    | `Parameter _
    | `Result _
    | `Root _ as i' -> Tools.signature_of_module_nopath env (Env.lookup_module i' env)
    | `ModuleType _ as i' -> Tools.signature_of_module_type_nopath env (Env.lookup_module_type i' env)
  in
  (r, sg)
| `Module (_, _) 
| `Canonical _
| `SubstAlias (_, _) as r' ->
    let (_, m) = resolve_resolved_module_reference env r' in r, Tools.signature_of_module_nopath env m
| `ModuleType (_, _) as r' ->
    let (_, m) = resolve_resolved_module_type_reference env r' in r, Tools.signature_of_module_type_nopath env m

and resolve_resolved_module_reference : Env.t -> Resolved.Module.t -> module_lookup_result =
  fun env r ->
    match r with
    | `Identifier i ->
      r, Env.lookup_module i env
    | `Module (parent, name) ->
      let (_, sg) = resolve_resolved_signature_reference env parent in
      r, Component.Find.module_in_sig sg name
    | `Canonical (p, _) ->
      let _, m = resolve_resolved_module_reference env p in r, m
    | `SubstAlias (_, p) ->
      let _, m = resolve_resolved_module_reference env p in r, m

and resolve_resolved_module_type_reference: Env.t -> Resolved.ModuleType.t -> module_type_lookup_result =
  fun env r ->
    match r with
    | `Identifier i ->
      r, Env.lookup_module_type i env
    | `ModuleType (parent, name) ->
      let (_, sg) = resolve_resolved_signature_reference env parent in
      r, Component.Find.module_type_in_sig sg name





and resolve_module_reference : Env.t -> Module.t -> module_lookup_result option =
  let open Tools.OptionMonad in
  fun env r ->
    match r with
    | `Resolved r ->
      Some (resolve_resolved_module_reference env r)
    | `Dot (parent, name) ->
      resolve_label_parent_reference env parent
      >>= signature_lookup_result_of_label_parent 
      >>= fun (parent', sg) -> Component.Find.opt_module_in_sig sg name
      >>= fun m -> return (`Module (parent', name), m)
    | `Module (parent, name) ->
      resolve_signature_reference env parent 
      >>= fun (parent', sg) -> Component.Find.opt_module_in_sig sg name
      >>= fun m -> return (`Module (parent', name), m)
    | `Root (name, _) -> begin
        match Env.lookup_module_by_name name env with
        | Some (`Module (id, m)) -> return (`Identifier id, m)
        | None -> begin
          let x = Env.lookup_root_module name env in
          match x with | Some (Env.Resolved (id,m)) -> return (`Identifier id, m) | _ -> None
        end
      end

      and resolve_module_type_reference : Env.t -> ModuleType.t -> module_type_lookup_result option =
  let open Tools.OptionMonad in
  fun env r ->
    match r with
    | `Resolved r ->
      Some (resolve_resolved_module_type_reference env r)
    | `Dot (parent, name) ->
      resolve_label_parent_reference env parent
      >>= signature_lookup_result_of_label_parent 
      >>= fun (parent', sg) -> Component.Find.opt_module_type_in_sig sg name
      >>= fun m -> return (`ModuleType (parent', name), m)
    | `ModuleType (parent, name) ->
      resolve_signature_reference env parent 
      >>= fun (parent', sg) -> Component.Find.opt_module_type_in_sig sg name
      >>= fun m -> return (`ModuleType (parent', name), m)
    | `Root (name, _) ->
        Env.lookup_module_type_by_name name env >>=
        fun (`ModuleType (id, m)) -> return (`Identifier id, m)

  
and resolve_label_parent_reference : Env.t -> LabelParent.t -> label_parent_lookup_result option =
      let open Tools.OptionMonad in
      fun env r ->
      let label_parent_res_of_sig_res x =
        x
        >>= (fun (r',sg) ->
        return ((r' :> Resolved.LabelParent.t), `S sg))
      in
        match r with
        | `Resolved _ -> failwith "unimplemented"
        | `Module _ 
        | `ModuleType _
        | `Root (_, #Odoc_model.Paths_types.Reference.tag_module) as sr ->
            resolve_signature_reference env sr |> label_parent_res_of_sig_res
        | `Dot (parent, name) ->
          resolve_label_parent_reference env parent
          >>= signature_lookup_result_of_label_parent
          >>= fun (parent', sg) ->
          choose [
            (fun () -> Component.Find.opt_module_in_sig sg name >>= fun m ->
              let sg = Tools.signature_of_module_nopath env m in
              return (`Module (parent', name), `S sg));
            (fun () -> Component.Find.opt_module_type_in_sig sg name >>= fun m ->
              let sg = Tools.signature_of_module_type_nopath env m in
              return (`ModuleType (parent', name), `S sg));]
        | _ -> None
      
           

and resolve_signature_reference : Env.t -> Signature.t -> signature_lookup_result option =
  let open Tools.OptionMonad in
  fun env r ->
    match r with
    | `Resolved r -> Some (resolve_resolved_signature_reference env r)
    | `Root (name, _) -> 
      choose [
        (fun () ->
          Env.lookup_module_by_name name env >>= fun (`Module (id, m)) ->
          let sg = Tools.signature_of_module_nopath env m in
          return (`Identifier (id :> Identifier.Signature.t), sg));
        (fun () ->
          Env.lookup_root_module name env >>= function
          | (Env.Resolved (id,m)) ->
            return (
              `Identifier (id :> Identifier.Signature.t),
              (Tools.signature_of_module_nopath env m))
          | _ -> None);
        (fun () ->
          Env.lookup_module_type_by_name name env >>= fun (`ModuleType (id, m)) ->
          let sg = Tools.signature_of_module_type_nopath env m in
          return (`Identifier (id :> Identifier.Signature.t), sg))
      ]
    | `Dot (parent, name) ->
      resolve_label_parent_reference env parent 
          >>= signature_lookup_result_of_label_parent
          >>= fun (parent', sg) -> Component.Find.opt_module_in_sig sg name
          >>= fun m ->
             let sg = Tools.signature_of_module_nopath env m in
             return (`Module (parent', name), sg)
    |`Module (parent,name) ->
      resolve_signature_reference env parent
        >>= fun (parent', sg) -> Component.Find.opt_module_in_sig sg name
        >>= fun m ->
        let sg = Tools.signature_of_module_nopath env m in
        return (`Module (parent', name), sg)
  | `ModuleType (parent,name) ->
      resolve_signature_reference env parent
        >>= fun (parent', sg) -> Component.Find.opt_module_type_in_sig sg name
        >>= fun m ->
        let sg = Tools.signature_of_module_type_nopath env m in
        return (`ModuleType (parent', name), sg)

and resolve_value_reference : Env.t -> Value.t -> value_lookup_result option =
  let open Tools.OptionMonad in
  fun env r ->
    match r with
    | `Root (name, _) -> begin
      Env.lookup_value_by_name name env >>= function
      | `Value (id, x) -> return (`Identifier id, x)
      end
    | `Dot (parent, name) -> begin
      resolve_label_parent_reference env parent 
          >>= signature_lookup_result_of_label_parent
          >>= fun (parent', sg) -> Component.Find.opt_value_in_sig sg name
          >>= fun v -> return (`Value (parent', name), v)
    end
    | _ -> failwith "erk"

and resolve_label_reference : Env.t -> Label.t -> Resolved.Label.t option =
    let open Tools.OptionMonad in
    fun env r ->
      match r with
      | `Resolved r -> Some r
      | `Root (name, _) -> begin
        Env.lookup_any_by_name name env >>= function
        | `Label id -> return (`Identifier id)
        | _ -> None
        end
      | `Dot (parent, name) -> begin
        resolve_label_parent_reference env parent >>= fun (p,sg) ->
        match sg with
        | `S sg -> Component.Find.opt_label_in_sig sg name >>= fun _ -> Some (`Label (p, name))
        | `CS _sg -> None
        end
      | `Label (parent, name) -> begin
        resolve_label_parent_reference env parent >>= fun (p,sg) ->
        match sg with
        | `S sg -> Component.Find.opt_label_in_sig sg name >>= fun _ -> Some (`Label (p, name))
        | `CS _sg -> None
        end
        


and resolve_reference : Env.t -> t -> Resolved.t option =
    let open Tools.OptionMonad in
    fun env r ->
    match r with
    | `Root (name, `TUnknown) -> begin
        Env.lookup_any_by_name name env >>= function
        | `Module (id,_) -> return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `ModuleType (id, _) -> return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `Value (id, _) -> return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `Type (id, _) -> return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `Label id -> return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `Class (id,_) -> return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        | `ClassType (id,_) -> return (`Identifier (id :> Odoc_model.Paths.Identifier.t))
        end
    | `Resolved r -> Some r
    | `Root (_, `TModule)
    | `Module (_,_) as r ->
        resolve_module_reference env r >>= fun (x,_) -> return (x :> Resolved.t)
    | `Root (_,`TModuleType)
    | `ModuleType (_,_) as r ->
        resolve_module_type_reference env r >>= fun (x, _) -> return (x :> Resolved.t)
    | `Root (_, `TType) 
    | `Type (_,_) as r ->
        resolve_type_reference env r >>= fun (x,_) -> return (x :> Resolved.t)
    | `Root (_, `TValue)
    | `Value (_,_) as r ->
        resolve_value_reference env r >>= fun (x,_) -> return (x :> Resolved.t)
    | `Root (_, `TLabel)
    | `Label (_, _) as r ->
        resolve_label_reference env r >>= fun x -> return (x :> Resolved.t)
    | `Dot (_,_) as r ->
        choose 
          [ (fun () -> resolve_type_reference env r >>= fun (x,_) -> return (x :> Resolved.t))
          ; (fun () -> resolve_module_reference env r >>= fun (x,_) -> return (x :> Resolved.t))
          ; (fun () -> resolve_module_type_reference env r >>= fun (x,_) -> return (x :> Resolved.t))
          ; (fun () -> resolve_value_reference env r >>= fun (x,_) -> return (x :> Resolved.t))
          ; (fun () -> resolve_label_reference env r >>= fun x -> return (x :> Resolved.t))
          ]
    | _ -> None


let _ = Tools.resolve_module_ref := resolve_module_reference