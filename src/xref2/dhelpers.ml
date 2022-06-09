(* DHelpers *)

(* Delayed helpers *)

module Module = struct
  type t = Component.Module.t Component.Delayed.t

  let rec canonical : t -> Odoc_model.Paths.Path.Module.t option = function
    | Val x -> x.Component.Module.canonical
    | OfLang (Module, x, _) -> x.canonical
    | Subst (Module, x, _) -> canonical x

  let rec doc : t -> Component.CComment.docs = function
    | Val x -> x.doc
    | OfLang (Module, x, map) -> Component.Of_Lang.docs map x.doc
    | Subst (Module, x, _) -> doc x

  let rec hidden : t -> bool = function
    | Val x -> x.hidden
    | OfLang (Module, x, _) -> x.hidden
    | Subst (Module, x, _) -> hidden x

  type modifiers =
    | AliasPath of Cpath.module_
    | ModuleTypePath of Cpath.module_type

  let rec m_path_modifiers : t -> modifiers option = function
    | Val x -> (
        match x.type_ with
        | Alias (p, _) -> Some (AliasPath p)
        | ModuleType (Path { p_path; _ }) -> Some (ModuleTypePath p_path)
        | _ -> None)
    | OfLang (Module, x, map) -> (
        match x.type_ with
        | Alias (p, _) -> Some (AliasPath (Component.Of_Lang.module_path map p))
        | ModuleType (Path { p_path; _ }) ->
            Some
              (ModuleTypePath (Component.Of_Lang.module_type_path map p_path))
        | _ -> None)
    | Subst (Module, x, sub) -> (
        match m_path_modifiers x with
        | Some (AliasPath p) -> Some (AliasPath (Subst.module_path sub p))
        | Some (ModuleTypePath p) -> (
            match Subst.module_type_path sub p with
            | Not_replaced p -> Some (ModuleTypePath p)
            | Replaced (Path { p_path; _ }) -> Some (ModuleTypePath p_path)
            | _ -> None)
        | None -> None)
end

module ModuleType = struct
  let rec doc :
      Component.ModuleType.t Component.Delayed.t -> Component.CComment.docs =
    function
    | Val x -> x.doc
    | OfLang (ModuleType, x, map) -> Component.Of_Lang.docs map x.doc
    | Subst (ModuleType, x, _) -> doc x

  let rec canonical :
      Component.ModuleType.t Component.Delayed.t ->
      Odoc_model.Paths.Path.ModuleType.t option = function
    | Val x -> x.Component.ModuleType.canonical
    | OfLang (ModuleType, x, _) -> x.canonical
    | Subst (ModuleType, x, _) -> canonical x

  let rec m_path_modifiers :
      Component.ModuleType.t Component.Delayed.t -> Cpath.module_type option =
    function
    | Val x -> (
        match x.expr with Some (Path { p_path; _ }) -> Some p_path | _ -> None)
    | OfLang (ModuleType, x, map) -> (
        match x.expr with
        | Some (Path { p_path; _ }) ->
            Some (Component.Of_Lang.module_type_path map p_path)
        | _ -> None)
    | Subst (ModuleType, x, sub) -> (
        match m_path_modifiers x with
        | Some p -> (
            match Subst.module_type_path sub p with
            | Not_replaced p -> Some p
            | Replaced (Path { p_path; _ }) -> Some p_path
            | _ -> None)
        | None -> None)
end

module Value = struct
  type t = Component.Value.t Component.Delayed.t

  let rec doc : t -> Component.CComment.docs = function
    | Val x -> x.doc
    | OfLang (Value, x, map) -> Component.Of_Lang.docs map x.doc
    | Subst (Value, x, _) -> doc x
end

module Signature = struct
  type t = Component.Signature.t Component.Delayed.t

  type ident =
    | IType of Ident.datatype
    | IModule of Ident.module_
    | IModuleType of Ident.module_type
    | IClass of Ident.class_
    | IClassType of Ident.class_type
  
  let rec get_idents : t -> ident list = function
    | Val sg ->
      let rec inner sg =
        List.fold_left (fun acc t ->
          match t with
          | Component.Signature.Type (id, _, _)
          | Component.Signature.TypeSubstitution (id, _) -> IType id :: acc
          | Module (id, _, _)
          | ModuleSubstitution (id, _) -> IModule id :: acc
          | ModuleType (id, _)
          | ModuleTypeSubstitution (id, _) -> IModuleType id :: acc
          | Class (id, _, _) -> IClass id :: acc
          | ClassType (id, _, _) -> IClassType id :: acc
          | Exception _
          | TypExt _
          | Value _
          | Comment _ -> acc
          | Include i -> acc @ (inner i.expansion_.items)
          | Open o -> acc @ (inner o.expansion.items)) [] sg
        in inner sg.items
    | OfLang (Signature, sg, map) ->
      let open Odoc_model.Paths.Identifier in
      let rec inner sg =
        List.fold_left (fun acc t ->
          match t with
          | Odoc_model.Lang.Signature.Type (_, t) ->
            let id = Maps.Type.find t.id map.types in
            IType id :: acc
          | Odoc_model.Lang.Signature.TypeSubstitution t ->
            let id = Maps.Type.find t.id map.types in
            IType id :: acc
          | Module (_, m) ->
            let id =
              Maps.Module.find
                (m.id :> Module.t)
                map.modules
            in
            IModule id :: acc
          | ModuleSubstitution m ->
            let id =
              Maps.Module.find
                (m.id :> Module.t)
                map.modules
            in
            IModule id :: acc
          | ModuleType m ->
            let id =
              Maps.ModuleType.find
                m.id
                map.module_types
            in
            IModuleType id :: acc
          | ModuleTypeSubstitution m -> 
            let id =
              Maps.ModuleType.find
                m.id
                map.module_types
            in
            IModuleType id :: acc
          | Class (_, c) ->
            let id =
              Maps.Class.find
              c.id
              map.classes
            in
            IClass id :: acc
          | ClassType (_, c) ->
            let id =
              Maps.ClassType.find
              c.id
              map.class_types
            in
            IClassType id :: acc
          | Exception _
          | TypExt _
          | Value _
          | Comment _ -> acc
          | Include i -> acc @ (inner i.expansion.content.items)
          | Open o -> acc @ (inner o.expansion.items)) [] sg
        in inner sg.items
      | Subst (Signature, _, _) -> failwith "Can't do it"
      | AddDoc (sg, _) -> get_idents sg
    
end