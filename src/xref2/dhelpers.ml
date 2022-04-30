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
