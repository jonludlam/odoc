(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Names

module Identifier = struct
  type t = Paths_types.Identifier.any

  let rec name_aux : t -> string = function
    | `Root (_, name) -> ModuleName.to_string name
    | `Page (_, name) -> PageName.to_string name
    | `LeafPage (_, name) -> PageName.to_string name
    | `Module (_, name) -> ModuleName.to_string name
    | `Parameter (_, name) -> ParameterName.to_string name
    | `Result x -> name_aux (x :> t)
    | `ModuleType (_, name) -> ModuleTypeName.to_string name
    | `Type (_, name) -> TypeName.to_string name
    | `CoreType name -> TypeName.to_string name
    | `Constructor (_, name) -> ConstructorName.to_string name
    | `Field (_, name) -> FieldName.to_string name
    | `Extension (_, name) -> ExtensionName.to_string name
    | `Exception (_, name) -> ExceptionName.to_string name
    | `CoreException name -> ExceptionName.to_string name
    | `Value (_, name) -> ValueName.to_string name
    | `Class (_, name) -> ClassName.to_string name
    | `ClassType (_, name) -> ClassTypeName.to_string name
    | `Method (_, name) -> MethodName.to_string name
    | `InstanceVariable (_, name) -> InstanceVariableName.to_string name
    | `Label (_, name) -> LabelName.to_string name

  let name : [< t ] -> string = fun n -> name_aux (n :> t)

  let rec label_parent_aux =
    let open Paths_types.Identifier in
    fun (n : any) ->
      match n with
      | `Result i -> label_parent_aux (i :> any)
      | `CoreType _ | `CoreException _ -> assert false
      | `Root _ as p -> (p :> label_parent)
      | `Page _ as p -> (p :> label_parent)
      | `LeafPage _ as p -> (p :> label_parent)
      | `Module (p, _)
      | `ModuleType (p, _)
      | `Parameter (p, _)
      | `Class (p, _)
      | `ClassType (p, _)
      | `Type (p, _)
      | `Extension (p, _)
      | `Exception (p, _)
      | `Value (p, _) ->
          (p : signature :> label_parent)
      | `Label (p, _) -> p
      | `Method (p, _) | `InstanceVariable (p, _) ->
          (p : class_signature :> label_parent)
      | `Constructor (p, _) -> (p : datatype :> label_parent)
      | `Field (p, _) -> (p : parent :> label_parent)

  let label_parent n = label_parent_aux (n :> t)

  let equal = ( = )

  let hash = Hashtbl.hash

  let compare = compare

  type any = t

  module Any = struct
    type t = any

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Signature = struct
    type t = Paths_types.Identifier.signature

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module ClassSignature = struct
    type t = Paths_types.Identifier.class_signature

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module DataType = struct
    type t = Paths_types.Identifier.datatype

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Parent = struct
    type t = Paths_types.Identifier.parent

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module LabelParent = struct
    type t = Paths_types.Identifier.label_parent

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module RootModule = struct
    type t = Paths_types.Identifier.root_module

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Module = struct
    type t = Paths_types.Identifier.module_

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module FunctorParameter = struct
    type t = Paths_types.Identifier.functor_parameter

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module FunctorResult = struct
    type t = Paths_types.Identifier.functor_result

    let equal x y = equal (x :> any) (y :> any)

    let hash x = hash (x :> any)

    let compare x y = compare (x :> any) (y :> any)
  end

  module ModuleType = struct
    type t = Paths_types.Identifier.module_type

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Type = struct
    type t = Paths_types.Identifier.type_

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Constructor = struct
    type t = Paths_types.Identifier.constructor

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Field = struct
    type t = Paths_types.Identifier.field

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Extension = struct
    type t = Paths_types.Identifier.extension

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Exception = struct
    type t = Paths_types.Identifier.exception_

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Value = struct
    type t = Paths_types.Identifier.value

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Class = struct
    type t = Paths_types.Identifier.class_

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module ClassType = struct
    type t = Paths_types.Identifier.class_type

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Method = struct
    type t = Paths_types.Identifier.method_

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module InstanceVariable = struct
    type t = Paths_types.Identifier.instance_variable

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Label = struct
    type t = Paths_types.Identifier.label

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Page = struct
    type t = Paths_types.Identifier.page

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module ContainerPage = struct
    type t = Paths_types.Identifier.container_page

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module OdocId = struct
    type t = Paths_types.Identifier.odoc_id

    let equal x y = equal (x :> any) (y :> any)

    let hash = Hashtbl.hash

    let compare x y = compare (x :> any) (y :> any)
  end

  module Path = struct
    module Module = struct
      type t = Paths_types.Identifier.path_module

      let equal x y = equal (x :> any) (y :> any)

      let hash = Hashtbl.hash

      let compare x y = compare (x :> any) (y :> any)
    end

    module ModuleType = struct
      type t = Paths_types.Identifier.path_module_type

      let equal x y = equal (x :> any) (y :> any)

      let hash = Hashtbl.hash

      let compare x y = compare (x :> any) (y :> any)
    end

    module Type = struct
      type t = Paths_types.Identifier.path_type

      let equal x y = equal (x :> any) (y :> any)

      let hash = Hashtbl.hash

      let compare x y = compare (x :> any) (y :> any)
    end

    module ClassType = struct
      type t = Paths_types.Identifier.path_class_type

      let equal x y = equal (x :> any) (y :> any)

      let hash = Hashtbl.hash

      let compare x y = compare (x :> any) (y :> any)
    end

    type t = Paths_types.Identifier.path_any
  end

  module Sets = struct
    module Signature = Set.Make (Signature)
    module ClassSignature = Set.Make (ClassSignature)
    module DataType = Set.Make (DataType)
    module Parent = Set.Make (Parent)
    module LabelParent = Set.Make (LabelParent)
    module RootModule = Set.Make (RootModule)
    module FunctorParameter = Set.Make (FunctorParameter)
    module Module = Set.Make (Module)
    module ModuleType = Set.Make (ModuleType)
    module Type = Set.Make (Type)
    module Constructor = Set.Make (Constructor)
    module Field = Set.Make (Field)
    module Extension = Set.Make (Extension)
    module Exception = Set.Make (Exception)
    module Value = Set.Make (Value)
    module Class = Set.Make (Class)
    module ClassType = Set.Make (ClassType)
    module Method = Set.Make (Method)
    module InstanceVariable = Set.Make (InstanceVariable)
    module Label = Set.Make (Label)
    module Page = Set.Make (Page)
    module ContainerPage = Set.Make (ContainerPage)

    module Path = struct
      module Module = Set.Make (Path.Module)
      module ModuleType = Set.Make (Path.ModuleType)
      module Type = Set.Make (Path.Type)
      module ClassType = Set.Make (Path.ClassType)
    end
  end

  module Maps = struct
    module Any = Map.Make (Any)
    module Signature = Map.Make (Signature)
    module ClassSignature = Map.Make (ClassSignature)
    module DataType = Map.Make (DataType)
    module Parent = Map.Make (Parent)
    module LabelParent = Map.Make (LabelParent)
    module RootModule = Map.Make (RootModule)
    module FunctorParameter = Map.Make (FunctorParameter)
    module Module = Map.Make (Module)
    module ModuleType = Map.Make (ModuleType)
    module Type = Map.Make (Type)
    module Constructor = Map.Make (Constructor)
    module Field = Map.Make (Field)
    module Extension = Map.Make (Extension)
    module Exception = Map.Make (Exception)
    module Value = Map.Make (Value)
    module Class = Map.Make (Class)
    module ClassType = Map.Make (ClassType)
    module Method = Map.Make (Method)
    module InstanceVariable = Map.Make (InstanceVariable)
    module Label = Map.Make (Label)
    module Page = Map.Make (Page)
    module ContainerPage = Map.Make (ContainerPage)

    module Path = struct
      module Module = Map.Make (Path.Module)
      module ModuleType = Map.Make (Path.ModuleType)
      module Type = Map.Make (Path.Type)
      module ClassType = Map.Make (Path.ClassType)
    end
  end
end

module Path = struct
  type t = Paths_types.Path.any

  type t_unhashed = Paths_types.Path.any_unhashed

  let rec is_resolved_hidden : Paths_types.Resolved_path.any -> bool =
   fun x ->
    let open Paths_types.Resolved_path in
    let rec inner_unhashed : Paths_types.Resolved_path.any_unhashed -> bool =
      function
      | `Identifier (`ModuleType (_, m)) when Names.ModuleTypeName.is_internal m
        ->
          true
      | `Identifier (`Type (_, t)) when Names.TypeName.is_internal t -> true
      | `Identifier (`Module (_, m)) when Names.ModuleName.is_internal m -> true
      | `Identifier _ -> false
      | `Canonical (_, { v = `Resolved _; _ }) -> false
      | `Canonical (x, _) -> inner (x : module_ :> any)
      | `Hidden _ -> true
      | `Subst (p1, p2) ->
          inner (p1 : module_type :> any) || inner (p2 : module_ :> any)
      | `Module (p, _) -> inner (p : module_ :> any)
      | `Apply (p, _) -> inner (p : module_ :> any)
      | `ModuleType (_, m) when Names.ModuleTypeName.is_internal m -> true
      | `ModuleType (p, _) -> inner (p : module_ :> any)
      | `Type (_, t) when Names.TypeName.is_internal t -> true
      | `Type (p, _) -> inner (p : module_ :> any)
      | `Class (p, _) -> inner (p : module_ :> any)
      | `ClassType (p, _) -> inner (p : module_ :> any)
      | `AliasRD (dest, { v = `Resolved src; _ }) ->
          inner (dest : module_ :> any) && inner (src : module_ :> any)
      | `AliasRS ({ v = `Resolved dest; _ }, src) ->
          inner (src : module_ :> any) && inner (dest : module_ :> any)
      | `AliasRD (dest, src) ->
          inner (dest : module_ :> any)
          && is_path_hidden (src :> Paths_types.Path.any)
      | `AliasRS (dest, src) ->
          inner (src : module_ :> any)
          && is_path_hidden (dest :> Paths_types.Path.any)
      | `AliasModuleType (p1, p2) ->
          inner (p1 : module_type :> any) && inner (p2 : module_type :> any)
      | `SubstT (p1, p2) -> inner (p1 :> any) || inner (p2 :> any)
      | `CanonicalModuleType (_, { v = `Resolved _; _ }) -> false
      | `CanonicalModuleType (x, _) -> inner (x : module_type :> any)
      | `CanonicalType (_, { v = `Resolved _; _ }) -> false
      | `CanonicalType (x, _) -> inner (x : type_ :> any)
      | `OpaqueModule m -> inner (m :> any)
      | `OpaqueModuleType mt -> inner (mt :> any)
    and inner x = inner_unhashed x.v in
    inner x

  and contains_double_underscore s =
    let len = String.length s in
    let rec aux i =
      if i > len - 2 then false
      else if s.[i] = '_' && s.[i + 1] = '_' then true
      else aux (i + 1)
    in
    aux 0

  and is_path_hidden : Paths_types.Path.any -> bool =
    let open Paths_types.Path in
    fun x ->
      match x.v with
      | `Resolved r -> is_resolved_hidden r
      | `Identifier (_, hidden) -> hidden
      | `Root s -> contains_double_underscore s
      | `Forward _ -> false
      | `Dot (p, _) -> is_path_hidden (p : module_ :> any)
      | `Apply (p1, p2) ->
          is_path_hidden (p1 : module_ :> any)
          || is_path_hidden (p2 : module_ :> any)

  module Resolved = struct
    type t = Paths_types.Resolved_path.any

    type t_unhashed = Paths_types.Resolved_path.any_unhashed

    let rec parent_module_type_identifier :
        Paths_types.Resolved_path.module_type -> Identifier.Signature.t =
     fun x ->
      match x.v with
      | `Identifier id ->
          (id : Identifier.ModuleType.t :> Identifier.Signature.t)
      | `ModuleType (m, n) -> `ModuleType (parent_module_identifier m, n)
      | `SubstT (m, _n) -> parent_module_type_identifier m
      | `CanonicalModuleType (_, { v = `Resolved p; _ }) ->
          parent_module_type_identifier p
      | `CanonicalModuleType (p, _) -> parent_module_type_identifier p
      | `OpaqueModuleType mt -> parent_module_type_identifier mt
      | `AliasModuleType (sub, orig) ->
          if is_resolved_hidden (sub :> t) then
            parent_module_type_identifier orig
          else parent_module_type_identifier sub

    and parent_module_identifier :
        Paths_types.Resolved_path.module_ -> Identifier.Signature.t =
     fun x ->
      match x.v with
      | `Identifier id ->
          (id : Identifier.Path.Module.t :> Identifier.Signature.t)
      | `Subst (sub, _) -> parent_module_type_identifier sub
      | `Hidden p -> parent_module_identifier p
      | `Module (m, n) -> `Module (parent_module_identifier m, n)
      | `Canonical (_, { v = `Resolved p; _ }) -> parent_module_identifier p
      | `Canonical (p, _) -> parent_module_identifier p
      | `Apply (m, _) -> parent_module_identifier m
      | `AliasRS ({ v = `Resolved dest; _ }, src) ->
          if is_resolved_hidden (src :> t) then parent_module_identifier dest
          else parent_module_identifier src
      | `AliasRD (dest, { v = `Resolved src; _ }) ->
          if is_resolved_hidden (dest :> t) then parent_module_identifier src
          else parent_module_identifier dest
      | `AliasRD (dest, _src) -> parent_module_identifier dest
      | `AliasRS (_dest, src) -> parent_module_identifier src
      | `OpaqueModule m -> parent_module_identifier m

    module Module = struct
      type t = Paths_types.Resolved_path.module_

      let is_hidden m =
        is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let rec identifier : t -> Identifier.Path.Module.t =
       fun x ->
        match x.v with
        | `Identifier id -> id
        | `Subst (_, p) -> identifier p
        | `Hidden p -> identifier p
        | `Module (m, n) -> `Module (parent_module_identifier m, n)
        | `Canonical (_, { v = `Resolved p; _ }) -> identifier p
        | `Canonical (p, _) -> identifier p
        | `Apply (m, _) -> identifier m
        | `AliasRS ({ v = `Resolved dest; _ }, src) ->
            if is_resolved_hidden (src :> Paths_types.Resolved_path.any) then
              identifier (dest :> t)
            else identifier (src :> t)
        | `AliasRD (dest, { v = `Resolved src; _ }) ->
            if is_resolved_hidden (dest :> Paths_types.Resolved_path.any) then
              identifier (src :> t)
            else identifier (dest :> t)
        | `AliasRS (_dest, src) -> identifier (src :> t)
        | `AliasRD (dest, _src) -> identifier (dest :> t)
        | `OpaqueModule m -> identifier m

      let rec canonical_ident : t -> Identifier.Path.Module.t option =
       fun x ->
        match x.v with
        | `Identifier _id -> None
        | `Subst (_, _) -> None
        | `Hidden p -> canonical_ident p
        | `Module (p, n) -> (
            match canonical_ident p with
            | Some x -> Some (`Module ((x :> Identifier.Signature.t), n))
            | None -> None)
        | `Canonical (_, { v = `Resolved p; _ }) -> Some (identifier p)
        | `Canonical (_, _) -> None
        | `Apply (_, _) -> None
        | `AliasRS (_, _) -> None
        | `AliasRD (_, _) -> None
        | `OpaqueModule m -> canonical_ident m

      module Mk = struct
        let identifier =
          let module M = Identifier.Maps.Path.Module in
          let tbl = ref M.empty in
          fun x ->
            if M.mem x !tbl then M.find x !tbl
            else
              let y = Hc.mk (`Identifier x) in
              tbl := M.add x y !tbl;
              y

        let subst : Paths_types.Resolved_path.module_type * t -> t =
          Hc.gen2 (fun (x, y) -> `Subst (x, y))

        let hidden : t -> t = Hc.gen1 (fun x -> `Hidden x)

        let module_ : t * ModuleName.t -> t =
          Hc.gen_named ModuleName.to_string (fun (p, n) -> `Module (p, n))

        let canonical : t * Paths_types.Path.module_ -> t =
          Hc.gen2 (fun (x, y) -> `Canonical (x, y))

        let apply : t * t -> t = Hc.gen2 (fun (x, y) -> `Apply (x, y))

        let aliasrs : Paths_types.Path.module_ * t -> t =
          Hc.gen2 (fun (x, y) -> `AliasRS (x, y))

        let aliasrd : t * Paths_types.Path.module_ -> t =
          Hc.gen2 (fun (x, y) -> `AliasRD (x, y))

        let opaquemodule : t -> t = Hc.gen1 (fun x -> `OpaqueModule x)
      end
    end

    module ModuleType = struct
      type t = Paths_types.Resolved_path.module_type

      let is_hidden m =
        is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let rec identifier : t -> Identifier.ModuleType.t =
       fun x ->
        match x.v with
        | `Identifier id -> id
        | `ModuleType (m, n) -> `ModuleType (parent_module_identifier m, n)
        | `SubstT (s, _) -> identifier s
        | `CanonicalModuleType (_, { v = `Resolved p; _ }) -> identifier p
        | `CanonicalModuleType (p, _) -> identifier p
        | `OpaqueModuleType mt -> identifier mt
        | `AliasModuleType (sub, orig) ->
            if is_resolved_hidden (sub :> Paths_types.Resolved_path.any) then
              identifier orig
            else identifier sub

      let rec canonical_ident : t -> Identifier.ModuleType.t option =
       fun x ->
        match x.v with
        | `Identifier _id -> None
        | `ModuleType (p, n) -> (
            match Module.canonical_ident p with
            | Some x -> Some (`ModuleType ((x :> Identifier.Signature.t), n))
            | None -> None)
        | `SubstT (_, _) -> None
        | `AliasModuleType (_, _) -> None
        | `CanonicalModuleType (_, { v = `Resolved p; _ }) ->
            Some (identifier p)
        | `CanonicalModuleType (_, _) -> None
        | `OpaqueModuleType m -> canonical_ident (m :> t)

      module Mk = struct
        let identifier : Identifier.Path.ModuleType.t -> t =
          let module M = Identifier.Maps.ModuleType in
          let tbl = ref M.empty in
          fun x ->
            if M.mem x !tbl then M.find x !tbl
            else
              let y = Hc.mk (`Identifier x) in
              tbl := M.add x y !tbl;
              y

        let module_type : Module.t * ModuleTypeName.t -> t =
          Hc.gen_named ModuleTypeName.to_string (fun (p, n) ->
              `ModuleType (p, n))

        let substt : t * t -> t = Hc.gen2 (fun (x, y) -> `SubstT (x, y))

        let aliasmoduletype : t * t -> t =
          Hc.gen2 (fun (x, y) -> `AliasModuleType (x, y))

        let canonicalmoduletype : t * Paths_types.Path.module_type -> t =
          Hc.gen2 (fun (x, y) -> `CanonicalModuleType (x, y))

        let opaquemoduletype : t -> t = Hc.gen1 (fun x -> `OpaqueModuleType x)
      end
    end

    module Type = struct
      type t = Paths_types.Resolved_path.type_

      let is_hidden m =
        is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let rec identifier : t -> Identifier.Path.Type.t =
       fun x ->
        match x.v with
        | `Identifier id -> id
        | `CanonicalType (_, { v = `Resolved t; _ }) -> identifier t
        | `CanonicalType (t, _) -> identifier t
        | `Type (m, n) -> `Type (parent_module_identifier m, n)
        | `Class (m, n) -> `Class (parent_module_identifier m, n)
        | `ClassType (m, n) -> `ClassType (parent_module_identifier m, n)

      let canonical_ident : t -> Identifier.Path.Type.t option =
        let parent m default fn =
          match Module.canonical_ident m with
          | Some x -> fn (x :> Identifier.Signature.t)
          | None -> default
        in
        fun x ->
          match x.v with
          | `Identifier _ -> None
          | `CanonicalType (_, { v = `Resolved t; _ }) -> Some (identifier t)
          | `CanonicalType (_, _) -> None
          | `Type (m, n) -> parent m None (fun sg -> Some (`Type (sg, n)))
          | `Class (m, n) -> parent m None (fun sg -> Some (`Class (sg, n)))
          | `ClassType (m, n) ->
              parent m None (fun sg -> Some (`ClassType (sg, n)))

      module Mk = struct
        let identifier : Identifier.Path.Type.t -> t =
          let module M = Identifier.Maps.Path.Type in
          let tbl = ref M.empty in
          fun x ->
            if M.mem x !tbl then M.find x !tbl
            else
              let y = Hc.mk (`Identifier x) in
              tbl := M.add x y !tbl;
              y

        let canonicaltype : t * Paths_types.Path.type_ -> t =
          Hc.gen2 (fun (x, y) -> `CanonicalType (x, y))

        let type_ : Module.t * TypeName.t -> t =
          Hc.gen_named TypeName.to_string (fun (x, y) -> `Type (x, y))

        let class_ : Module.t * ClassName.t -> t =
          Hc.gen_named ClassName.to_string (fun (x, y) -> `Class (x, y))

        let class_type : Module.t * ClassTypeName.t -> t =
          Hc.gen_named ClassTypeName.to_string (fun (x, y) -> `ClassType (x, y))
      end
    end

    module ClassType = struct
      type t = Paths_types.Resolved_path.class_type

      let is_hidden m =
        is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let identifier : t -> Identifier.Path.ClassType.t =
       fun x ->
        match x.v with
        | `Identifier id -> id
        | `Class (m, n) -> `Class (parent_module_identifier m, n)
        | `ClassType (m, n) -> `ClassType (parent_module_identifier m, n)

      module Mk = struct
        let identifier : Identifier.Path.ClassType.t -> t =
          let module M = Identifier.Maps.Path.ClassType in
          let tbl = ref M.empty in
          fun x ->
            if M.mem x !tbl then M.find x !tbl
            else
              let y = Hc.mk (`Identifier x) in
              tbl := M.add x y !tbl;
              y

        let class_ : Module.t * ClassName.t -> t =
          Hc.gen_named ClassName.to_string (fun (x, y) -> `Class (x, y))

        let class_type : Module.t * ClassTypeName.t -> t =
          Hc.gen_named ClassTypeName.to_string (fun (x, y) -> `ClassType (x, y))
      end
    end

    let rec identifier : t -> Identifier.t =
     fun x ->
      match x.v with
      | `Identifier id -> id
      | `Subst (_, p) -> identifier (p :> t)
      | `Hidden p -> identifier (p :> t)
      | `Module (m, n) -> `Module (parent_module_identifier m, n)
      | `Canonical (_, { v = `Resolved p; _ }) -> identifier (p :> t)
      | `Canonical (p, _) -> identifier (p :> t)
      | `Apply (m, _) -> identifier (m :> t)
      | `Type (m, n) -> `Type (parent_module_identifier m, n)
      | `ModuleType (m, n) -> `ModuleType (parent_module_identifier m, n)
      | `Class (m, n) -> `Class (parent_module_identifier m, n)
      | `ClassType (m, n) -> `ClassType (parent_module_identifier m, n)
      | `AliasRS ({ v = `Resolved dest; _ }, src) ->
          if is_resolved_hidden (src :> t) then identifier (dest :> t)
          else identifier (src :> t)
      | `AliasRD (dest, { v = `Resolved src; _ }) ->
          if is_resolved_hidden (dest :> t) then identifier (src :> t)
          else identifier (dest :> t)
      | `AliasRS (_dest, src) -> identifier (src :> t)
      | `AliasRD (dest, _src) -> identifier (dest :> t)
      | `AliasModuleType (sub, orig) ->
          if is_resolved_hidden (sub :> t) then identifier (orig :> t)
          else identifier (sub :> t)
      | `SubstT (p, _) -> identifier (p :> t)
      | `CanonicalModuleType (_, { v = `Resolved p; _ }) -> identifier (p :> t)
      | `CanonicalModuleType (p, _) -> identifier (p :> t)
      | `CanonicalType (_, { v = `Resolved p; _ }) -> identifier (p :> t)
      | `CanonicalType (p, _) -> identifier (p :> t)
      | `OpaqueModule m -> identifier (m :> t)
      | `OpaqueModuleType mt -> identifier (mt :> t)
  end

  module Module = struct
    type t = Paths_types.Path.module_

    module Mk = struct
      (* [ `Resolved of Resolved_path.module_
         | `Identifier of Identifier.path_module * bool
         | `Root of string
         | `Forward of string
         | `Dot of module_ * string
         | `Apply of module_ * module_ ] *)

      let resolved : Resolved.Module.t -> t = Hc.gen1 (fun x -> `Resolved x)

      let identifier : Identifier.Path.Module.t * bool -> t =
        let module M = Identifier.Maps.Path.Module in
        let tbl = ref M.empty in
        fun (x, b) ->
          if M.mem x !tbl then M.find x !tbl
          else
            let y = Hc.mk (`Identifier (x, b)) in
            tbl := M.add x y !tbl;
            y

      let root : string -> t =
        let tbl = Hashtbl.create 255 in
        fun root ->
          if Hashtbl.mem tbl root then Hashtbl.find tbl root
          else
            let y = Hc.mk (`Root root) in
            Hashtbl.add tbl root y;
            y

      let forward : string -> t =
        let tbl = Hashtbl.create 255 in
        fun f ->
          if Hashtbl.mem tbl f then Hashtbl.find tbl f
          else
            let y = Hc.mk (`Forward f) in
            Hashtbl.add tbl f y;
            y

      let dot : t * string -> t =
        Hc.gen_named (fun x -> x) (fun (x, y) -> `Dot (x, y))

      let apply : t * t -> t = Hc.gen2 (fun (x, y) -> `Apply (x, y))
    end
  end

  module ModuleType = struct
    type t = Paths_types.Path.module_type

    (* [ `Resolved of Resolved_path.module_type
       | `Identifier of Identifier.path_module_type * bool
       | `Dot of module_ * string ]
    *)

    module Mk = struct
      let resolved : Resolved.ModuleType.t -> t = Hc.gen1 (fun x -> `Resolved x)

      let identifier : Identifier.Path.ModuleType.t * bool -> t =
        let module M = Identifier.Maps.Path.ModuleType in
        let tbl = ref M.empty in
        fun (x, b) ->
          if M.mem x !tbl then M.find x !tbl
          else
            let y = Hc.mk (`Identifier (x, b)) in
            tbl := M.add x y !tbl;
            y

      let dot : Module.t * string -> t =
        Hc.gen_named (fun x -> x) (fun (x, y) -> `Dot (x, y))
    end
  end

  module Type = struct
    type t = Paths_types.Path.type_

    module Mk = struct
      let resolved : Resolved.Type.t -> t = Hc.gen1 (fun x -> `Resolved x)

      let identifier : Identifier.Path.Type.t * bool -> t =
        let module M = Identifier.Maps.Path.Type in
        let tbl = ref M.empty in
        fun (x, b) ->
          if M.mem x !tbl then M.find x !tbl
          else
            let y = Hc.mk (`Identifier (x, b)) in
            tbl := M.add x y !tbl;
            y

      let dot : Module.t * string -> t =
        Hc.gen_named (fun x -> x) (fun (x, y) -> `Dot (x, y))
    end
  end

  module ClassType = struct
    type t = Paths_types.Path.class_type

    module Mk = struct
      let resolved : Resolved.ClassType.t -> t = Hc.gen1 (fun x -> `Resolved x)

      let identifier : Identifier.Path.ClassType.t * bool -> t =
        let module M = Identifier.Maps.Path.ClassType in
        let tbl = ref M.empty in
        fun (x, b) ->
          if M.mem x !tbl then M.find x !tbl
          else
            let y = Hc.mk (`Identifier (x, b)) in
            tbl := M.add x y !tbl;
            y

      let dot : Module.t * string -> t =
        Hc.gen_named (fun x -> x) (fun (x, y) -> `Dot (x, y))
    end
  end

  let is_hidden = is_path_hidden
end

module Fragment = struct
  module Resolved = struct
    type t = Paths_types.Resolved_fragment.any

    type root = Paths_types.Resolved_fragment.root

    let sig_of_mod m =
      let open Paths_types.Resolved_fragment in
      (m : module_ :> signature)

    type base_name =
      | Base of root
      | Branch of ModuleName.t * Paths_types.Resolved_fragment.signature

    let rec split_parent : Paths_types.Resolved_fragment.signature -> base_name
        = function
      | `Root i -> Base i
      | `Subst (_, p) -> split_parent (sig_of_mod p)
      | `Alias (_, p) -> split_parent (sig_of_mod p)
      | `OpaqueModule m -> split_parent (sig_of_mod m)
      | `Module (p, name) -> (
          match split_parent p with
          | Base i -> Branch (name, `Root i)
          | Branch (base, m) -> Branch (base, `Module (m, name)))

    module Signature = struct
      type t = Paths_types.Resolved_fragment.signature

      let rec split : t -> string * t option = function
        | `Root _ -> ("", None)
        | `Subst (_, p) -> split (sig_of_mod p)
        | `Alias (_, p) -> split (sig_of_mod p)
        | `OpaqueModule m -> split (sig_of_mod m)
        | `Module (m, name) -> (
            match split_parent m with
            | Base _ -> (ModuleName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`Module (m, name))))

      let rec identifier : t -> Identifier.Signature.t = function
        | `Root (`ModuleType i) ->
            (Path.Resolved.ModuleType.identifier i :> Identifier.Signature.t)
        | `Root (`Module i) ->
            (Path.Resolved.Module.identifier i :> Identifier.Signature.t)
        | `Subst (s, _) ->
            (Path.Resolved.ModuleType.identifier s :> Identifier.Signature.t)
        | `Alias (i, _) ->
            (Path.Resolved.Module.identifier i :> Identifier.Signature.t)
        | `Module (m, n) -> `Module (identifier m, n)
        | `OpaqueModule m -> identifier (sig_of_mod m)
    end

    module Module = struct
      type t = Paths_types.Resolved_fragment.module_

      let rec split : t -> string * t option = function
        | `Subst (_, p) -> split p
        | `Alias (_, p) -> split p
        | `Module (m, name) -> (
            match split_parent m with
            | Base _ -> (ModuleName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`Module (m, name))))
        | `OpaqueModule m -> split m
    end

    module ModuleType = struct
      type t = Paths_types.Resolved_fragment.module_type

      let split : t -> string * t option = function
        | `Module_type (m, name) -> (
            match split_parent m with
            | Base _ -> (ModuleTypeName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`Module_type (m, name))))
    end

    module Type = struct
      type t = Paths_types.Resolved_fragment.type_

      let split : t -> string * t option = function
        | `Type (m, name) -> (
            match split_parent m with
            | Base _ -> (TypeName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`Type (m, name))))
        | `Class (m, name) -> (
            match split_parent m with
            | Base _ -> (ClassName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`Class (m, name))))
        | `ClassType (m, name) -> (
            match split_parent m with
            | Base _ -> (ClassTypeName.to_string name, None)
            | Branch (base, m) ->
                (ModuleName.to_string base, Some (`ClassType (m, name))))
    end

    type leaf = Paths_types.Resolved_fragment.leaf

    let rec identifier : t -> Identifier.t = function
      | `Root (`ModuleType _r) -> assert false
      | `Root (`Module _r) -> assert false
      | `Subst (s, _) -> Path.Resolved.identifier (s :> Path.Resolved.t)
      | `Alias (p, _) -> (Path.Resolved.Module.identifier p :> Identifier.t)
      | `Module (m, n) -> `Module (Signature.identifier m, n)
      | `Module_type (m, n) -> `ModuleType (Signature.identifier m, n)
      | `Type (m, n) -> `Type (Signature.identifier m, n)
      | `Class (m, n) -> `Class (Signature.identifier m, n)
      | `ClassType (m, n) -> `ClassType (Signature.identifier m, n)
      | `OpaqueModule m -> identifier (m :> t)

    let rec is_hidden : t -> bool = function
      | `Root (`ModuleType r) -> Path.is_resolved_hidden (r :> Path.Resolved.t)
      | `Root (`Module r) -> Path.is_resolved_hidden (r :> Path.Resolved.t)
      | `Subst (s, _) -> Path.is_resolved_hidden (s :> Path.Resolved.t)
      | `Alias (s, _) -> Path.is_resolved_hidden (s :> Path.Resolved.t)
      | `Module (m, _)
      | `Module_type (m, _)
      | `Type (m, _)
      | `Class (m, _)
      | `ClassType (m, _) ->
          is_hidden (m :> t)
      | `OpaqueModule m -> is_hidden (m :> t)
  end

  type t = Paths_types.Fragment.any

  type base_name =
    | Base of Resolved.root option
    | Branch of ModuleName.t * Paths_types.Fragment.signature

  let rec split_parent : Paths_types.Fragment.signature -> base_name = function
    | `Root -> Base None
    | `Resolved r -> (
        match Resolved.split_parent r with
        | Resolved.Base i -> Base (Some i)
        | Resolved.Branch (base, m) -> Branch (base, `Resolved m))
    | `Dot (m, name) -> (
        match split_parent m with
        | Base None -> Branch (ModuleName.make_std name, `Root)
        | Base (Some i) -> Branch (ModuleName.make_std name, `Resolved (`Root i))
        | Branch (base, m) -> Branch (base, `Dot (m, name)))

  module Signature = struct
    type t = Paths_types.Fragment.signature

    let split : t -> string * t option = function
      | `Root -> ("", None)
      | `Resolved r ->
          let base, m = Resolved.Signature.split r in
          let m = match m with None -> None | Some m -> Some (`Resolved m) in
          (base, m)
      | `Dot (m, name) -> (
          match split_parent m with
          | Base _ -> (name, None)
          | Branch (base, m) ->
              (ModuleName.to_string base, Some (`Dot (m, name))))
  end

  module Module = struct
    type t = Paths_types.Fragment.module_

    let split : t -> string * t option = function
      | `Resolved r ->
          let base, m = Resolved.Module.split r in
          let m = match m with None -> None | Some m -> Some (`Resolved m) in
          (base, m)
      | `Dot (m, name) -> (
          match split_parent m with
          | Base _ -> (name, None)
          | Branch (base, m) ->
              (ModuleName.to_string base, Some (`Dot (m, name))))
  end

  module ModuleType = struct
    type t = Paths_types.Fragment.module_type

    let split : t -> string * t option = function
      | `Resolved r ->
          let base, m = Resolved.ModuleType.split r in
          let m = match m with None -> None | Some m -> Some (`Resolved m) in
          (base, m)
      | `Dot (m, name) -> (
          match split_parent m with
          | Base _ -> (name, None)
          | Branch (base, m) ->
              (ModuleName.to_string base, Some (`Dot (m, name))))
  end

  module Type = struct
    type t = Paths_types.Fragment.type_

    let split : t -> string * t option = function
      | `Resolved r ->
          let base, m = Resolved.Type.split r in
          let m = match m with None -> None | Some m -> Some (`Resolved m) in
          (base, m)
      | `Dot (m, name) -> (
          match split_parent m with
          | Base _ -> (name, None)
          | Branch (base, m) ->
              (ModuleName.to_string base, Some (`Dot (m, name))))
  end

  type leaf = Paths_types.Fragment.leaf
end

module Reference = struct
  module Resolved = struct
    open Paths_types.Resolved_reference

    type t = Paths_types.Resolved_reference.any

    let rec parent_signature_identifier : signature -> Identifier.Signature.t =
      function
      | `Identifier id -> id
      | `Hidden s -> parent_signature_identifier (s :> signature)
      | `Alias (sub, orig) ->
          if Path.Resolved.Module.is_hidden sub then
            parent_signature_identifier (orig :> signature)
          else (Path.Resolved.Module.identifier sub :> Identifier.Signature.t)
      | `AliasModuleType (sub, orig) ->
          if Path.Resolved.ModuleType.is_hidden sub then
            parent_signature_identifier (orig :> signature)
          else
            (Path.Resolved.ModuleType.identifier sub :> Identifier.Signature.t)
      | `Module (m, n) -> `Module (parent_signature_identifier m, n)
      | `ModuleType (m, s) -> `ModuleType (parent_signature_identifier m, s)

    and parent_type_identifier : datatype -> Identifier.DataType.t = function
      | `Identifier id -> id
      | `Type (sg, s) -> `Type (parent_signature_identifier sg, s)

    and parent_class_signature_identifier :
        class_signature -> Identifier.ClassSignature.t = function
      | `Identifier id -> id
      | `Class (sg, s) -> `Class (parent_signature_identifier sg, s)
      | `ClassType (sg, s) -> `ClassType (parent_signature_identifier sg, s)

    and parent_identifier : parent -> Identifier.Parent.t = function
      | `Identifier id -> id
      | (`Hidden _ | `Alias _ | `AliasModuleType _ | `Module _ | `ModuleType _)
        as sg ->
          (parent_signature_identifier sg :> Identifier.Parent.t)
      | `Type _ as t -> (parent_type_identifier t :> Identifier.Parent.t)
      | (`Class _ | `ClassType _) as c ->
          (parent_class_signature_identifier c :> Identifier.Parent.t)

    and label_parent_identifier : label_parent -> Identifier.LabelParent.t =
      function
      | `Identifier id -> id
      | ( `Hidden _ | `Alias _ | `AliasModuleType _ | `Module _ | `ModuleType _
        | `Type _ | `Class _ | `ClassType _ ) as r ->
          (parent_identifier r :> Identifier.LabelParent.t)

    and identifier : t -> Identifier.t = function
      | `Identifier id -> id
      | ( `Alias _ | `AliasModuleType _ | `Module _ | `Hidden _ | `Type _
        | `Class _ | `ClassType _ | `ModuleType _ ) as r ->
          (label_parent_identifier r :> Identifier.t)
      | `Field (p, n) -> `Field (parent_identifier p, n)
      | `Constructor (s, n) -> `Constructor (parent_type_identifier s, n)
      | `Extension (p, q) -> `Extension (parent_signature_identifier p, q)
      | `Exception (p, q) -> `Exception (parent_signature_identifier p, q)
      | `Value (p, q) -> `Value (parent_signature_identifier p, q)
      | `Method (p, q) -> `Method (parent_class_signature_identifier p, q)
      | `InstanceVariable (p, q) ->
          `InstanceVariable (parent_class_signature_identifier p, q)
      | `Label (p, q) -> `Label (label_parent_identifier p, q)

    module Signature = struct
      type t = Paths_types.Resolved_reference.signature
    end

    module ClassSignature = struct
      type t = Paths_types.Resolved_reference.class_signature
    end

    module DataType = struct
      type t = Paths_types.Resolved_reference.datatype
    end

    module Parent = struct
      type t = Paths_types.Resolved_reference.parent
    end

    module LabelParent = struct
      type t = Paths_types.Resolved_reference.label_parent
    end

    module Module = struct
      type t = Paths_types.Resolved_reference.module_
    end

    module ModuleType = struct
      type t = Paths_types.Resolved_reference.module_type
    end

    module Type = struct
      type t = Paths_types.Resolved_reference.type_
    end

    module Constructor = struct
      type t = Paths_types.Resolved_reference.constructor
    end

    module Field = struct
      type t = Paths_types.Resolved_reference.field
    end

    module Extension = struct
      type t = Paths_types.Resolved_reference.extension
    end

    module Exception = struct
      type t = Paths_types.Resolved_reference.exception_
    end

    module Value = struct
      type t = Paths_types.Resolved_reference.value
    end

    module Class = struct
      type t = Paths_types.Resolved_reference.class_
    end

    module ClassType = struct
      type t = Paths_types.Resolved_reference.class_type
    end

    module Method = struct
      type t = Paths_types.Resolved_reference.method_
    end

    module InstanceVariable = struct
      type t = Paths_types.Resolved_reference.instance_variable
    end

    module Label = struct
      type t = Paths_types.Resolved_reference.label
    end

    module Page = struct
      type t = Paths_types.Resolved_reference.page
    end
  end

  type t = Paths_types.Reference.any

  type tag_any = Paths_types.Reference.tag_any

  module Signature = struct
    type t = Paths_types.Reference.signature
  end

  module ClassSignature = struct
    type t = Paths_types.Reference.class_signature
  end

  module DataType = struct
    type t = Paths_types.Reference.datatype
  end

  module Parent = struct
    type t = Paths_types.Reference.parent
  end

  module LabelParent = struct
    type t = Paths_types.Reference.label_parent
  end

  module Module = struct
    type t = Paths_types.Reference.module_
  end

  module ModuleType = struct
    type t = Paths_types.Reference.module_type
  end

  module Type = struct
    type t = Paths_types.Reference.type_
  end

  module Constructor = struct
    type t = Paths_types.Reference.constructor
  end

  module Field = struct
    type t = Paths_types.Reference.field
  end

  module Extension = struct
    type t = Paths_types.Reference.extension
  end

  module Exception = struct
    type t = Paths_types.Reference.exception_
  end

  module Value = struct
    type t = Paths_types.Reference.value
  end

  module Class = struct
    type t = Paths_types.Reference.class_
  end

  module ClassType = struct
    type t = Paths_types.Reference.class_type
  end

  module Method = struct
    type t = Paths_types.Reference.method_
  end

  module InstanceVariable = struct
    type t = Paths_types.Reference.instance_variable
  end

  module Label = struct
    type t = Paths_types.Reference.label
  end

  module Page = struct
    type t = Paths_types.Reference.page
  end
end
