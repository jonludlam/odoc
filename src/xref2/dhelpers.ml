(* DHelpers *)

(* Delayed helpers *)

let filter_lang_sg : Odoc_model.Lang.Signature.t -> Odoc_model.Lang.Signature.t = fun sg ->
  let open Odoc_model.Lang.Signature in
  let filter_fn = function
    | Value _ -> false
    | Comment _ -> false
    | Exception _ -> false
    | _ -> true
  in
  { sg with items = List.filter filter_fn sg.items } 

let filter_lang_expr : Odoc_model.Lang.ModuleType.expr -> Odoc_model.Lang.ModuleType.expr = fun expr ->
  match expr with
  | Signature sg -> Signature (filter_lang_sg sg)
  | x -> x

let filter_expr : Component.ModuleType.expr -> Component.ModuleType.expr =
  let filter_comp_sg : Component.Signature.t -> Component.Signature.t = fun sg ->
    let fn =
      let open Component.Signature in
      function
      | Value _ -> false
      | Comment _ -> false
      | Exception _ -> false
      | _ -> true
    in
    { sg with items = List.filter fn sg.items }
  in
  function
  | Signature sg -> Signature (filter_comp_sg sg)
  | x -> x

module Module = struct
  type t = Component.Module.t Component.Delayed.t
  let rec canonical : t -> Odoc_model.Paths.Path.Module.t option = function
    | Val x -> x.Component.Module.canonical
    | OfLang (Module, x, _) -> x.canonical
    | Subst (Module, x, _) -> canonical x
    | ResolveFilter x -> canonical x

  let rec doc : t -> Component.CComment.docs = function
    | Val x -> x.doc
    | OfLang (Module, x, map) -> Component.Of_Lang.docs map x.doc
    | Subst (Module, x, _) -> doc x
    | ResolveFilter x -> doc x

  let rec hidden : t -> bool = function
    | Val x -> x.hidden
    | OfLang (Module, x, _) -> x.hidden
    | Subst (Module, x, _) -> hidden x
    | ResolveFilter x -> hidden x

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
    | ResolveFilter x -> m_path_modifiers x

  let type_ : t -> Component.Module.decl = 
    let rec inner filter = function
    | Component.Delayed.Val x -> x.type_
    | OfLang (Module, x, map) -> begin
      (* Avoid doing anything with the option expansion *)
      match x.type_ with
      | Alias (x, _) -> Alias (Component.Of_Lang.module_path map x, None)
      | ModuleType e -> ModuleType (Component.Of_Lang.module_type_expr map (if filter then filter_lang_expr e else e))
      end
    | Subst (Module, x, sub) -> begin
      match inner filter x with
      | Alias (x, _) -> Alias (Subst.module_path sub x, None)
      | ModuleType y -> ModuleType (Subst.module_type_expr sub (if filter then filter_expr y else y))
      end
    | ResolveFilter x -> inner true x
    in
    fun e -> inner false e

end

module ModuleType = struct
  type t = Component.ModuleType.t Component.Delayed.t
  let rec doc :
      t -> Component.CComment.docs =
    function
    | Val x -> x.doc
    | OfLang (ModuleType, x, map) -> Component.Of_Lang.docs map x.doc
    | Subst (ModuleType, x, _) -> doc x
    | ResolveFilter x -> doc x

  let rec canonical :
      t ->
      Odoc_model.Paths.Path.ModuleType.t option = function
    | Val x -> x.Component.ModuleType.canonical
    | OfLang (ModuleType, x, _) -> x.canonical
    | Subst (ModuleType, x, _) -> canonical x
    | ResolveFilter x -> canonical x

  let rec m_path_modifiers :
      t -> Cpath.module_type option =
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
    | ResolveFilter x -> m_path_modifiers x

  let expr : t -> Component.ModuleType.expr option = fun x ->
    let rec inner filter = function
      | Component.Delayed.Val x -> if filter then (match x.expr with Some y -> Some (filter_expr y) | None -> None) else x.expr
      | OfLang (ModuleType, x, map) -> begin
        match x.expr with
        | None -> None
        | Some (Signature sg) -> Some (Signature (Component.Of_Lang.signature map (if filter then filter_lang_sg sg else sg)))
        | Some (With w) -> Some (Component.Of_Lang.module_type_expr map (With {w with w_expansion=None}))
        | Some (Path p) -> Some (Component.Of_Lang.module_type_expr map (Path {p with p_expansion=None}))
        | Some ((Functor _) as f) -> Some (Component.Of_Lang.module_type_expr map f)
        | Some ((TypeOf _) as t) -> Some (Component.Of_Lang.module_type_expr map t)
      end
      | Subst (ModuleType, x, sub) -> (
        match inner filter x with
        | None -> None
        | Some y -> Some (Subst.module_type_expr sub y))
      | ResolveFilter x -> inner true x

      in inner false x
end

module Value = struct
  type t = Component.Value.t Component.Delayed.t

  let rec doc : t -> Component.CComment.docs = function
    | Val x -> x.doc
    | OfLang (Value, x, map) -> Component.Of_Lang.docs map x.doc
    | Subst (Value, x, _) -> doc x
    | ResolveFilter x -> doc x

end
