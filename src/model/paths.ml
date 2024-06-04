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

module Ocaml_ident = Ident
module Ocaml_env = Env


type na_ty = |
type na = [ `Na of na_ty ]

open Names

module Identifier = struct
  type 'a id = 'a Paths_types.id = { iv : 'a; ihash : int; ikey : string }

  module Id = Paths_types.Identifier

  type t = Id.any

  type t_pv = Id.any_pv

  let rec name_aux : t -> string =
   fun x ->
    match x.iv with
    | `Root (_, name) -> ModuleName.to_string name
    | `Page (_, name) -> PageName.to_string name
    | `LeafPage (_, name) -> PageName.to_string name
    | `Module (_, name) -> ModuleName.to_string name
    | `Parameter (_, name) -> ModuleName.to_string name
    | `Result x -> name_aux (x :> t)
    | `ModuleType (_, name) -> ModuleTypeName.to_string name
    | `Type (_, name) -> TypeName.to_string name
    | `CoreType name -> TypeName.to_string name
    | `Constructor (_, name) -> ConstructorName.to_string name
    | `Field (_, name) -> FieldName.to_string name
    | `Extension (_, name) -> ExtensionName.to_string name
    | `ExtensionDecl (_, _, name) -> ExtensionName.to_string name
    | `Exception (_, name) -> ExceptionName.to_string name
    | `CoreException name -> ExceptionName.to_string name
    | `Value (_, name) -> ValueName.to_string name
    | `Class (_, name) -> TypeName.to_string name
    | `ClassType (_, name) -> TypeName.to_string name
    | `Method (_, name) -> MethodName.to_string name
    | `InstanceVariable (_, name) -> InstanceVariableName.to_string name
    | `Label (_, name) -> LabelName.to_string name
    | `SourcePage (dir, name) -> name_aux (dir :> t) ^ name
    | `SourceDir (({ iv = `SourceDir _; _ } as p), n) ->
        name_aux (p :> t) ^ n ^ "/"
    | `SourceDir (_, n) -> "./" ^ n ^ "/"
    | `SourceLocation (x, anchor) ->
        name_aux (x :> t) ^ "#" ^ DefName.to_string anchor
    | `SourceLocationMod x -> name_aux (x :> t)
    | `SourceLocationInternal (x, anchor) ->
        name_aux (x :> t) ^ "#" ^ LocalName.to_string anchor
    | `AssetFile (_, name) -> name

  let rec is_hidden : t -> bool =
   fun x ->
    match x.iv with
    | `Root (_, name) -> ModuleName.is_hidden name
    | `Page (_, _) -> false
    | `LeafPage (_, _) -> false
    | `Module (_, name) -> ModuleName.is_hidden name
    | `Parameter (_, name) -> ModuleName.is_hidden name
    | `Result x -> is_hidden (x :> t)
    | `ModuleType (_, name) -> ModuleTypeName.is_hidden name
    | `Type (_, name) -> TypeName.is_hidden name
    | `CoreType name -> TypeName.is_hidden name
    | `Constructor (parent, _) -> is_hidden (parent :> t)
    | `Field (parent, _) -> is_hidden (parent :> t)
    | `Extension (parent, _) -> is_hidden (parent :> t)
    | `ExtensionDecl (parent, _, _) -> is_hidden (parent :> t)
    | `Exception (parent, _) -> is_hidden (parent :> t)
    | `CoreException _ -> false
    | `Value (_, name) -> ValueName.is_hidden name
    | `Class (_, name) -> TypeName.is_hidden name
    | `ClassType (_, name) -> TypeName.is_hidden name
    | `Method (parent, _) -> is_hidden (parent :> t)
    | `InstanceVariable (parent, _) -> is_hidden (parent :> t)
    | `Label (parent, _) -> is_hidden (parent :> t)
    | `SourceDir _ | `SourceLocationMod _ | `SourceLocation _ | `SourcePage _
    | `SourceLocationInternal _ | `AssetFile _ ->
        false

  let name : [< t_pv ] id -> string = fun n -> name_aux (n :> t)

  let rec full_name_aux : t -> string list =
   fun x ->
    match x.iv with
    | `Root (_, name) -> [ ModuleName.to_string name ]
    | `Page (_, name) -> [ PageName.to_string name ]
    | `LeafPage (_, name) -> [ PageName.to_string name ]
    | `Module (parent, name) ->
        ModuleName.to_string name :: full_name_aux (parent :> t)
    | `Parameter (parent, name) ->
        ModuleName.to_string name :: full_name_aux (parent :> t)
    | `Result x -> full_name_aux (x :> t)
    | `ModuleType (parent, name) ->
        ModuleTypeName.to_string name :: full_name_aux (parent :> t)
    | `Type (parent, name) ->
        TypeName.to_string name :: full_name_aux (parent :> t)
    | `CoreType name -> [ TypeName.to_string name ]
    | `Constructor (parent, name) ->
        ConstructorName.to_string name :: full_name_aux (parent :> t)
    | `Field (parent, name) ->
        FieldName.to_string name :: full_name_aux (parent :> t)
    | `Extension (parent, name) ->
        ExtensionName.to_string name :: full_name_aux (parent :> t)
    | `ExtensionDecl (parent, _, name) ->
        ExtensionName.to_string name :: full_name_aux (parent :> t)
    | `Exception (parent, name) ->
        ExceptionName.to_string name :: full_name_aux (parent :> t)
    | `CoreException name -> [ ExceptionName.to_string name ]
    | `Value (parent, name) ->
        ValueName.to_string name :: full_name_aux (parent :> t)
    | `Class (parent, name) ->
        TypeName.to_string name :: full_name_aux (parent :> t)
    | `ClassType (parent, name) ->
        TypeName.to_string name :: full_name_aux (parent :> t)
    | `Method (parent, name) ->
        MethodName.to_string name :: full_name_aux (parent :> t)
    | `InstanceVariable (parent, name) ->
        InstanceVariableName.to_string name :: full_name_aux (parent :> t)
    | `Label (parent, name) ->
        LabelName.to_string name :: full_name_aux (parent :> t)
    | `SourceDir (parent, name) -> name :: full_name_aux (parent :> t)
    | `SourceLocation (parent, name) ->
        DefName.to_string name :: full_name_aux (parent :> t)
    | `SourceLocationInternal (parent, name) ->
        LocalName.to_string name :: full_name_aux (parent :> t)
    | `SourceLocationMod name -> full_name_aux (name :> t)
    | `SourcePage (parent, name) -> name :: full_name_aux (parent :> t)
    | `AssetFile (parent, name) -> name :: full_name_aux (parent :> t)

  let fullname : [< t_pv ] id -> string list =
   fun n -> List.rev @@ full_name_aux (n :> t)

  let is_hidden : [< t_pv ] id -> bool = fun n -> is_hidden (n :> t)

  let rec label_parent_aux =
    let open Id in
    fun (n : non_src) ->
      match n with
      | { iv = `Result i; _ } -> label_parent_aux (i :> non_src)
      | { iv = `CoreType _; _ } | { iv = `CoreException _; _ } -> assert false
      | { iv = `Root _; _ } as p -> (p :> label_parent)
      | { iv = `Page _; _ } as p -> (p :> label_parent)
      | { iv = `LeafPage _; _ } as p -> (p :> label_parent)
      | { iv = `Module (p, _); _ }
      | { iv = `ModuleType (p, _); _ }
      | { iv = `Parameter (p, _); _ }
      | { iv = `Class (p, _); _ }
      | { iv = `ClassType (p, _); _ }
      | { iv = `Type (p, _); _ }
      | { iv = `Extension (p, _); _ }
      | { iv = `ExtensionDecl (p, _, _); _ }
      | { iv = `Exception (p, _); _ }
      | { iv = `Value (p, _); _ } ->
          (p : signature :> label_parent)
      | { iv = `Label (p, _); _ } -> p
      | { iv = `Method (p, _); _ } | { iv = `InstanceVariable (p, _); _ } ->
          (p : class_signature :> label_parent)
      | { iv = `Constructor (p, _); _ } -> (p : datatype :> label_parent)
      | { iv = `Field (p, _); _ } -> (p : field_parent :> label_parent)

  let label_parent n = label_parent_aux (n :> Id.non_src)

  let equal x y = x.ihash = y.ihash && x.ikey = y.ikey

  let hash x = x.ihash

  let compare x y = compare x.ikey y.ikey

  type any = t

  type any_pv = t_pv

  module type IdSig = sig
    type t
    type t_pv
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module Any = struct
    type t = any
    type t_pv = any_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Signature = struct
    type t = Id.signature
    type t_pv = Id.signature_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module ClassSignature = struct
    type t = Id.class_signature
    type t_pv = Id.class_signature_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module DataType = struct
    type t = Id.datatype
    type t_pv = Id.datatype_pv
  end

  module FieldParent = struct
    type t = Paths_types.Identifier.field_parent
    type t_pv = Paths_types.Identifier.field_parent_pv
  end

  module LabelParent = struct
    type t = Id.label_parent
    type t_pv = Id.label_parent_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module RootModule = struct
    type t = Id.root_module
    type t_pv = Id.root_module_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Module = struct
    type t = Id.module_
    type t_pv = Id.module_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module FunctorParameter = struct
    type t = Id.functor_parameter
    type t_pv = Id.functor_parameter_pv
    let equal = equal
    let hash = hash
    let compare = compare

    let functor_arg_pos { iv = `Parameter (p, _); _ } =
      let rec inner_sig = function
        | `Result { iv = p; _ } -> 1 + inner_sig p
        | `Module _ | `ModuleType _ | `Root _ | `Parameter _ -> 1
      in
      inner_sig p.iv
  end

  module FunctorResult = struct
    type t = Id.functor_result
    type t_pv = Id.functor_result_pv
  end

  module ModuleType = struct
    type t = Id.module_type
    type t_pv = Id.module_type_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Type = struct
    type t = Id.type_
    type t_pv = Id.type_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Constructor = struct
    type t = Id.constructor
    type t_pv = Id.constructor_pv
  end

  module Field = struct
    type t = Id.field
    type t_pv = Id.field_pv
  end

  module Extension = struct
    type t = Id.extension
    type t_pv = Id.extension_pv
  end

  module ExtensionDecl = struct
    type t = Paths_types.Identifier.extension_decl

    type t_pv = Paths_types.Identifier.extension_decl_pv

    let equal = equal

    let hash = hash

    let compare = compare
  end

  module Exception = struct
    type t = Id.exception_
    type t_pv = Id.exception_pv
  end

  module Value = struct
    type t = Id.value
    type t_pv = Id.value_pv
  end

  module Class = struct
    type t = Id.class_
    type t_pv = Id.class_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module ClassType = struct
    type t = Id.class_type
    type t_pv = Id.class_type_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Method = struct
    type t = Id.method_
    type t_pv = Id.method_pv
  end

  module InstanceVariable = struct
    type t = Id.instance_variable
    type t_pv = Id.instance_variable_pv
  end

  module Label = struct
    type t = Paths_types.Identifier.label
    type t_pv = Paths_types.Identifier.label_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module Page = struct
    type t = Id.page
    type t_pv = Id.page_pv
  end

  module ContainerPage = struct
    type t = Id.container_page
    type t_pv = Id.container_page_pv
  end

  module NonSrc = struct
    type t = Paths_types.Identifier.non_src
    type t_pv = Paths_types.Identifier.non_src_pv
  end

  module SourceDir = struct
    type t = Id.source_dir
    type t_pv = Id.source_dir_pv
    let equal = equal
    let hash = hash
    let compare = compare
  end

  module SourcePage = struct
    type t = Id.source_page
    type t_pv = Id.source_page_pv
  end

  module SourceLocation = struct
    type t = Paths_types.Identifier.source_location
    type t_pv = Paths_types.Identifier.source_location_pv
  end

  module AssetFile = struct
    type t = Id.asset_file
    type t_pv = Id.asset_file_pv
  end

  module OdocId = struct
    type t = Id.odoc_id
    type t_pv = Id.odoc_id_pv
  end

  module Path = struct
    module Module = struct
      type t = Id.path_module
      type t_pv = Id.path_module_pv
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module ModuleType = struct
      type t = Id.path_module_type
      type t_pv = Id.module_type_pv
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module Type = struct
      type t = Id.path_type
      type t_pv = Id.path_type_pv
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module Value = struct
      type t = Id.path_value
      type t_pv = Id.value_pv
      let equal = equal
      let hash = hash
      let compare = compare
    end

    module ClassType = struct
      type t = Id.path_class_type
      type t_pv = Id.path_class_type_pv
      let equal = equal
      let hash = hash
      let compare = compare
    end

    type t = Id.path_any
  end

  module Maps = struct
    module Any = Map.Make (Any)
    module FunctorParameter = Map.Make (FunctorParameter)
    module Module = Map.Make (Module)
    module ModuleType = Map.Make (ModuleType)
    module Type = Map.Make (Type)
    module Class = Map.Make (Class)
    module ClassType = Map.Make (ClassType)
    module Label = Map.Make (Label)

    module Path = struct
      module Module = Map.Make (Path.Module)
      module ModuleType = Map.Make (Path.ModuleType)
      module Type = Map.Make (Path.Type)
      module ClassType = Map.Make (Path.ClassType)
    end
  end

  module Mk = struct
    let mk_fresh to_str ty f x =
      let ikey = Printf.sprintf "%s_%s" ty (to_str x) in
      let ihash = Hashtbl.hash ikey in
      { iv = f x; ihash; ikey }

    let mk_parent to_str ty f (parent, x) =
      let ikey = Printf.sprintf "%s_%s.%s" ty (to_str x) parent.ikey in
      let ihash = Hashtbl.hash ikey in

      { iv = f (parent, x); ihash; ikey }

    let mk_parent_opt to_str ty f (parent_opt, x) =
      let ikey =
        match parent_opt with
        | None -> Printf.sprintf "%s_%s" ty (to_str x)
        | Some p -> Printf.sprintf "%s_%s.%s" ty (to_str x) p.ikey
      in
      let ihash = Hashtbl.hash ikey in
      { iv = f (parent_opt, x); ihash; ikey }

    let page :
        ContainerPage.t option * PageName.t ->
        [> `Page of ContainerPage.t option * PageName.t ] id =
      mk_parent_opt PageName.to_string "p" (fun (p, n) -> `Page (p, n))

    let leaf_page :
        ContainerPage.t option * PageName.t ->
        [> `LeafPage of ContainerPage.t option * PageName.t ] id =
      mk_parent_opt PageName.to_string "lp" (fun (p, n) -> `LeafPage (p, n))

    let asset_file : Page.t * string -> AssetFile.t =
      mk_parent (fun k -> k) "asset" (fun (p, n) -> `AssetFile (p, n))

    let source_page (container_page, path) =
      let rec source_dir dir =
        match dir with
        | [] -> (container_page : ContainerPage.t :> SourceDir.t)
        | a :: q ->
            let parent = source_dir q in
            mk_parent
              (fun k -> k)
              "sd"
              (fun (p, dir) -> `SourceDir (p, dir))
              (parent, a)
      in
      match List.rev path with
      | [] -> assert false
      | file :: dir ->
          let parent = source_dir dir in
          mk_parent
            (fun x -> x)
            "sp"
            (fun (p, rp) -> `SourcePage (p, rp))
            (parent, file)

    let root :
        ContainerPage.t option * ModuleName.t ->
        [> `Root of ContainerPage.t option * ModuleName.t ] id =
      mk_parent_opt ModuleName.to_string "r" (fun (p, n) -> `Root (p, n))

    let implementation =
      mk_fresh
        (fun s -> s)
        "impl"
        (fun s -> `Implementation (ModuleName.make_std s))

    let module_ :
        Signature.t * ModuleName.t ->
        [> `Module of Signature.t * ModuleName.t ] id =
      mk_parent ModuleName.to_string "m" (fun (p, n) -> `Module (p, n))

    let parameter :
        Signature.t * ModuleName.t ->
        [> `Parameter of Signature.t * ModuleName.t ] id =
      mk_parent ModuleName.to_string "p" (fun (p, n) -> `Parameter (p, n))

    let result : Signature.t -> [> `Result of Signature.t ] id =
     fun s ->
      mk_parent (fun () -> "__result__") "" (fun (s, ()) -> `Result s) (s, ())

    let module_type :
        Signature.t * ModuleTypeName.t ->
        [> `ModuleType of Signature.t * ModuleTypeName.t ] id =
      mk_parent ModuleTypeName.to_string "mt" (fun (p, n) -> `ModuleType (p, n))

    let class_ :
        Signature.t * TypeName.t -> [> `Class of Signature.t * TypeName.t ] id
        =
      mk_parent TypeName.to_string "c" (fun (p, n) -> `Class (p, n))

    let class_type :
        Signature.t * TypeName.t ->
        [> `ClassType of Signature.t * TypeName.t ] id =
      mk_parent TypeName.to_string "ct" (fun (p, n) -> `ClassType (p, n))

    let type_ :
        Signature.t * TypeName.t -> [> `Type of Signature.t * TypeName.t ] id =
      mk_parent TypeName.to_string "t" (fun (p, n) -> `Type (p, n))

    let core_type =
      mk_fresh (fun s -> s) "coret" (fun s -> `CoreType (TypeName.make_std s))

    let constructor :
        DataType.t * ConstructorName.t ->
        [> `Constructor of DataType.t * ConstructorName.t ] id =
      mk_parent ConstructorName.to_string "ctor" (fun (p, n) ->
          `Constructor (p, n))

    let field :
        FieldParent.t * FieldName.t ->
        [> `Field of FieldParent.t * FieldName.t ] id =
      mk_parent FieldName.to_string "fld" (fun (p, n) -> `Field (p, n))

    let extension :
        Signature.t * ExtensionName.t ->
        [> `Extension of Signature.t * ExtensionName.t ] id =
      mk_parent ExtensionName.to_string "extn" (fun (p, n) -> `Extension (p, n))

    let extension_decl :
        Signature.t * (ExtensionName.t * ExtensionName.t) ->
        [> `ExtensionDecl of Signature.t * ExtensionName.t * ExtensionName.t ]
        id =
      mk_parent
        (fun (n, m) ->
          ExtensionName.to_string n ^ "." ^ ExtensionName.to_string m)
        "extn-decl"
        (fun (p, (n, m)) -> `ExtensionDecl (p, n, m))

    let exception_ :
        Signature.t * ExceptionName.t ->
        [> `Exception of Signature.t * ExceptionName.t ] id =
      mk_parent ExceptionName.to_string "exn" (fun (p, n) -> `Exception (p, n))

    let core_exception =
      mk_fresh
        (fun s -> s)
        "coreexn"
        (fun s -> `CoreException (ExceptionName.make_std s))

    let value :
        Signature.t * ValueName.t -> [> `Value of Signature.t * ValueName.t ] id
        =
      mk_parent ValueName.to_string "v" (fun (p, n) -> `Value (p, n))

    let method_ :
        ClassSignature.t * MethodName.t ->
        [> `Method of ClassSignature.t * MethodName.t ] id =
      mk_parent MethodName.to_string "m" (fun (p, n) -> `Method (p, n))

    let instance_variable :
        ClassSignature.t * InstanceVariableName.t ->
        [> `InstanceVariable of ClassSignature.t * InstanceVariableName.t ] id =
      mk_parent InstanceVariableName.to_string "iv" (fun (p, n) ->
          `InstanceVariable (p, n))

    let label :
        LabelParent.t * LabelName.t ->
        [> `Label of LabelParent.t * LabelName.t ] id =
      mk_parent LabelName.to_string "l" (fun (p, n) -> `Label (p, n))

    let source_location :
        SourcePage.t * DefName.t ->
        [> `SourceLocation of SourcePage.t * DefName.t ] id =
      mk_parent DefName.to_string "sl" (fun (p, n) -> `SourceLocation (p, n))

    let source_location_mod :
        SourcePage.t -> [> `SourceLocationMod of SourcePage.t ] id =
     fun s ->
      mk_parent
        (fun () -> "__slm__")
        ""
        (fun (s, ()) -> `SourceLocationMod s)
        (s, ())

    let source_location_int :
        SourcePage.t * LocalName.t ->
        [> `SourceLocationInternal of SourcePage.t * LocalName.t ] id =
      mk_parent LocalName.to_string "sli" (fun (p, n) ->
          `SourceLocationInternal (p, n))
  end
end

module Path = struct
  type ('lmod, 'lmodty, 'lcty, 'lty, 'lval) gen = ('lmod, 'lmodty, 'lcty, 'lty, 'lval) Paths_types.Path.any

  type t = (na, na, na, na, na) gen
  type rt = (na, na, na, na, na) Paths_types.Resolved_path.any
  type module_ = (na, na) Paths_types.Path.module_

  let rec is_resolved_hidden :
      weak_canonical_test:bool -> rt -> bool =
   fun ~weak_canonical_test x ->
    let rec inner : rt -> bool = function
      | `Identifier { iv = `ModuleType (_, m); _ }
        when Names.ModuleTypeName.is_hidden m ->
          true
      | `Identifier { iv = `Type (_, t); _ } when Names.TypeName.is_hidden t ->
          true
      | `Identifier { iv = `Module (_, m); _ } when Names.ModuleName.is_hidden m
        ->
          true
      | `Identifier _ -> false
      | `Canonical (_, `Resolved _) -> false
      | `Canonical (x, _) ->
          (not weak_canonical_test) && inner (x :> rt)
      | `Hidden _ -> true
      | `Subst (p1, p2) ->
          inner (p1 :> rt) || inner (p2 :> rt)
      | `Module (p, _) -> inner (p :> rt)
      | `Apply (p, _) -> inner (p :> rt)
      | `ModuleType (_, m) when Names.ModuleTypeName.is_hidden m -> true
      | `ModuleType (p, _) -> inner (p :> rt)
      | `Type (_, t) when Names.TypeName.is_hidden t -> true
      | `Type (p, _) -> inner (p :> rt)
      | `Value (_, t) when Names.ValueName.is_hidden t -> true
      | `Value (p, _) -> inner (p :> rt)
      | `Class (p, _) -> inner (p :> rt)
      | `ClassType (p, _) -> inner (p :> rt)
      | `Alias (dest, `Resolved src) ->
          inner (dest :> rt) && inner (src :> rt)
      | `Alias (dest, src) ->
          inner (dest :> rt)
          && is_path_hidden (src :> t)
      | `AliasModuleType (p1, p2) ->
          inner (p1 :> rt) && inner (p2 :> rt)
      | `SubstT (p1, p2) -> inner (p1 :> rt) || inner (p2 :> rt)
      | `Substituted m -> inner (m :> rt)
      | `SubstitutedMT m -> inner (m :> rt)
      | `SubstitutedT m -> inner (m :> rt)
      | `SubstitutedCT m -> inner (m :> rt)
      | `CanonicalModuleType (_, `Resolved _) -> false
      | `CanonicalModuleType (x, _) -> inner (x :> rt)
      | `CanonicalType (_, `Resolved _) -> false
      | `CanonicalType (x, _) -> inner (x :> rt)
      | `OpaqueModule m -> inner (m :> rt)
      | `OpaqueModuleType mt -> inner (mt :> rt)
      | `LocalMod (`Na _) -> .
      | `LocalModTy (`Na _) -> .
      | `LocalCty (`Na _) -> .
      | `LocalTy (`Na _) -> .
      | `LocalVal (`Na _) -> .
    in
    inner x

  and is_path_hidden : t -> bool =
    function
    | `Resolved r -> is_resolved_hidden ~weak_canonical_test:false r
    | `Identifier (_, hidden) -> hidden
    | `Substituted r -> is_path_hidden (r :> t)
    | `SubstitutedMT r -> is_path_hidden (r :> t)
    | `SubstitutedT r -> is_path_hidden (r :> t)
    | `SubstitutedCT r -> is_path_hidden (r :> t)
    | `Root s -> ModuleName.is_hidden s
    | `Forward _ -> false
    | `Dot (p, n) -> ModuleName.is_hidden n || is_path_hidden (p : module_ :> t)
    | `DotMT (p, n) -> ModuleTypeName.is_hidden n || is_path_hidden (p : module_ :> t)
    | `DotT (p, n) -> TypeName.is_hidden n || is_path_hidden (p : module_ :> t)
    | `DotV (p, n) -> ValueName.is_hidden n || is_path_hidden (p : module_ :> t)
    | `Apply (p1, p2) ->
        is_path_hidden (p1 :> t)
        || is_path_hidden (p2 :> t)
    | `LocalMod (`Na _)
    | `LocalModTy (`Na _)
    | `LocalCty (`Na _)
    | `LocalTy (`Na _)
    | `LocalVal (`Na _) -> .

  module Resolved = struct
    type ('lmod, 'lmodty, 'lcty, 'lty, 'lval) gen = ('lmod, 'lmodty, 'lcty, 'lty, 'lval) Paths_types.Resolved_path.any
    type t = (na, na, na, na, na) gen

    let rec parent_module_type_identifier :
        (na, na) Paths_types.Resolved_path.module_type -> Identifier.Signature.t =
      function
      | `Identifier id ->
          (id : Identifier.ModuleType.t :> Identifier.Signature.t)
      | `ModuleType (m, n) ->
          Identifier.Mk.module_type (parent_module_identifier m, n)
      | `SubstT (m, _n) -> parent_module_type_identifier m
      | `CanonicalModuleType (_, `Resolved p) -> parent_module_type_identifier p
      | `CanonicalModuleType (p, _) -> parent_module_type_identifier p
      | `OpaqueModuleType mt -> parent_module_type_identifier mt
      | `SubstitutedMT m -> parent_module_type_identifier m
      | `AliasModuleType (sub, orig) ->
          if is_resolved_hidden ~weak_canonical_test:false (sub :> t) then
            parent_module_type_identifier orig
          else parent_module_type_identifier sub
      | `LocalModTy (`Na _) -> .

    and parent_module_identifier :
        ('lmod, 'lmodty) Paths_types.Resolved_path.module_ -> Identifier.Signature.t = function
      | `Identifier id ->
          (id : Identifier.Path.Module.t :> Identifier.Signature.t)
      | `Subst (sub, _) -> parent_module_type_identifier sub
      | `Hidden p -> parent_module_identifier p
      | `Module (m, n) -> Identifier.Mk.module_ (parent_module_identifier m, n)
      | `Canonical (_, `Resolved p) -> parent_module_identifier p
      | `Canonical (p, _) -> parent_module_identifier p
      | `Apply (m, _) -> parent_module_identifier m
      | `Alias (dest, `Resolved src) ->
          if is_resolved_hidden ~weak_canonical_test:false (dest :> t) then
            parent_module_identifier src
          else parent_module_identifier dest
      | `Alias (dest, _src) -> parent_module_identifier dest
      | `Substituted m -> parent_module_identifier m
      | `OpaqueModule m -> parent_module_identifier m
      | `LocalMod (`Na _) -> .

    module Module = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_path.module_
      type t = (na, na) gen

      let is_hidden m =
        is_resolved_hidden (m : t :> rt)
    end

    module ModuleType = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_path.module_type
      type t = (na, na) gen
    end

    module Type = struct
      type ('lmod, 'lmodty, 'lcty, 'lty) gen = ('lmod, 'lmodty, 'lcty, 'lty) Paths_types.Resolved_path.type_
      type t = (na, na, na, na) gen
    end

    module Value = struct
      type ('lmod, 'lmodty, 'lval) gen = ('lmod, 'lmodty, 'lval) Paths_types.Resolved_path.value
      type t = (na, na, na) gen
    end

    module ClassType = struct
      type ('lmod, 'lmodty, 'lcty) gen = ('lmod, 'lmodty, 'lcty) Paths_types.Resolved_path.class_type
      type t = (na, na, na) gen
    end

    let rec identifier : rt -> Identifier.t = function
      | `Identifier id -> id
      | `Subst (sub, _) -> identifier (sub :> rt)
      | `Hidden p -> identifier (p :> rt)
      | `Module (m, n) -> Identifier.Mk.module_ (parent_module_identifier m, n)
      | `Canonical (_, `Resolved p) -> identifier (p :> rt)
      | `Canonical (p, _) -> identifier (p :> rt)
      | `Apply (m, _) -> identifier (m :> rt)
      | `Type (m, n) -> Identifier.Mk.type_ (parent_module_identifier m, n)
      | `Value (m, n) -> Identifier.Mk.value (parent_module_identifier m, n)
      | `ModuleType (m, n) ->
          Identifier.Mk.module_type (parent_module_identifier m, n)
      | `Class (m, n) -> Identifier.Mk.class_ (parent_module_identifier m, n)
      | `ClassType (m, n) ->
          Identifier.Mk.class_type (parent_module_identifier m, n)
      | `Alias (dest, `Resolved src) ->
          if is_resolved_hidden ~weak_canonical_test:false (dest :> rt) then
            identifier (src :> rt)
          else identifier (dest :> rt)
      | `Alias (dest, _src) -> identifier (dest :> rt)
      | `AliasModuleType (sub, orig) ->
          if is_resolved_hidden ~weak_canonical_test:false (sub :> rt) then
            identifier (orig :> rt)
          else identifier (sub :> rt)
      | `SubstT (p, _) -> identifier (p :> rt)
      | `CanonicalModuleType (_, `Resolved p) -> identifier (p :> rt)
      | `CanonicalModuleType (p, _) -> identifier (p :> rt)
      | `CanonicalType (_, `Resolved p) -> identifier (p :> rt)
      | `CanonicalType (p, _) -> identifier (p :> rt)
      | `OpaqueModule m -> identifier (m :> rt)
      | `OpaqueModuleType mt -> identifier (mt :> rt)
      | `Substituted m -> identifier (m :> rt)
      | `SubstitutedMT m -> identifier (m :> rt)
      | `SubstitutedCT m -> identifier (m :> rt)
      | `SubstitutedT m -> identifier (m :> rt)
      | `LocalMod (`Na _) -> .
      | `LocalModTy (`Na _) -> .
      | `LocalCty (`Na _) -> .
      | `LocalTy (`Na _) -> .
      | `LocalVal (`Na _) -> .

    let is_hidden r = is_resolved_hidden ~weak_canonical_test:false r
  end

  module Module = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Path.module_
    type t = (na, na) gen
  end

  module ModuleType = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Path.module_type
    type t = (na, na) gen
  end

  module Type = struct
    type ('lmod, 'lmodty, 'lcty, 'lty) gen = ('lmod, 'lmodty, 'lcty, 'lty) Paths_types.Path.type_
    type t = (na, na, na, na) gen
  end

  module Value = struct
    type ('lmod, 'lmodty, 'lval) gen = ('lmod, 'lmodty, 'lval) Paths_types.Path.value
    type t = (na, na, na) gen
  end

  module ClassType = struct
    type ('lmod, 'lmodty, 'lcty) gen = ('lmod, 'lmodty, 'lcty)  Paths_types.Path.class_type
    type t = (na, na, na) gen
  end

  let is_hidden = is_path_hidden
end

module Fragment = struct
  module Resolved = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_fragment.any
    type t = (na, na) gen

    type ('lmod, 'lmodty) root_gen = ('lmod, 'lmodty) Paths_types.Resolved_fragment.root
    type root = (na, na) root_gen
    module Signature = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_fragment.signature
      type t = (na, na) gen
      let rec sgidentifier : t -> Identifier.Signature.t = function
        | `Root (`ModuleType i) -> Path.Resolved.parent_module_type_identifier i
        | `Root (`Module i) -> Path.Resolved.parent_module_identifier i
        | `Subst (s, _) -> Path.Resolved.parent_module_type_identifier s
        | `Alias (i, _) -> Path.Resolved.parent_module_identifier i
        | `Module (m, n) -> Identifier.Mk.module_ (sgidentifier m, n)
        | `OpaqueModule m -> sgidentifier (m :> t)
    end

    module Module = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_fragment.module_
      type t = (na, na) gen
    end

    module ModuleType = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_fragment.module_type
      type t = (na, na) gen
    end

    module Type = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_fragment.type_
      type t = (na, na) gen
    end

    type ('lmod, 'lmodty) leaf_gen = ('lmod, 'lmodty) Paths_types.Resolved_fragment.leaf
    type leaf = (na, na) leaf_gen

    let rec identifier : t -> Identifier.t = function
      | `Root (`ModuleType _r) -> assert false
      | `Root (`Module _r) -> assert false
      | `Subst (s, _) -> Path.Resolved.identifier (s :> Path.Resolved.t)
      | `Alias (p, _) ->
          (Path.Resolved.parent_module_identifier p :> Identifier.t)
      | `Module (m, n) -> Identifier.Mk.module_ (Signature.sgidentifier m, n)
      | `Module_type (m, n) ->
          Identifier.Mk.module_type (Signature.sgidentifier m, n)
      | `Type (m, n) -> Identifier.Mk.type_ (Signature.sgidentifier m, n)
      | `Class (m, n) -> Identifier.Mk.class_ (Signature.sgidentifier m, n)
      | `ClassType (m, n) ->
          Identifier.Mk.class_type (Signature.sgidentifier m, n)
      | `OpaqueModule m -> identifier (m :> t)

    let rec is_hidden : t -> bool = function
      | `Root (`ModuleType r) -> Path.Resolved.(is_hidden (r :> t))
      | `Root (`Module r) -> Path.Resolved.(is_hidden (r :> t))
      | `Subst (s, _) -> Path.Resolved.(is_hidden (s :> t))
      | `Alias (s, _) -> Path.Resolved.(is_hidden (s :> t))
      | `Module (m, _)
      | `Module_type (m, _)
      | `Type (m, _)
      | `Class (m, _)
      | `ClassType (m, _) ->
          is_hidden (m :> t)
      | `OpaqueModule m -> is_hidden (m :> t)
  end

  type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Fragment.any
  type t = (na, na) gen

  module Signature = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Fragment.signature
    type t = (na, na) gen
  end

  module Module = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Fragment.module_
    type t = (na, na) gen
  end

  module ModuleType = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Fragment.module_type
    type t = (na, na) gen
  end

  module Type = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Fragment.type_
    type t = (na, na) gen
  end

  type ('lmod, 'lmodty) leaf_gen = ('lmod, 'lmodty) Paths_types.Fragment.leaf
  type leaf = (na, na) leaf_gen

end

module Reference = struct
  module Resolved = struct
    open Paths_types.Resolved_reference

    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.any
    type t = (na, na) gen

    let rec parent_signature_identifier : (na, na) signature -> Identifier.Signature.t =
      function
      | `Identifier id -> id
      | `Hidden s -> parent_signature_identifier (s :> (na, na) signature)
      | `Alias (sub, orig) ->
          if Path.Resolved.(is_hidden (sub :> t)) then
            parent_signature_identifier (orig :> (na, na) signature)
          else
            (Path.Resolved.parent_module_identifier sub
              :> Identifier.Signature.t)
      | `AliasModuleType (sub, orig) ->
          if Path.Resolved.(is_hidden (sub :> t)) then
            parent_signature_identifier (orig :> (na, na) signature)
          else
            (Path.Resolved.parent_module_type_identifier sub
              :> Identifier.Signature.t)
      | `Module (m, n) ->
          Identifier.Mk.module_ (parent_signature_identifier m, n)
      | `ModuleType (m, s) ->
          Identifier.Mk.module_type (parent_signature_identifier m, s)

    and parent_type_identifier : (na, na) datatype -> Identifier.DataType.t = function
      | `Identifier id -> id
      | `Type (sg, s) -> Identifier.Mk.type_ (parent_signature_identifier sg, s)

    and parent_class_signature_identifier :
    (na, na) class_signature -> Identifier.ClassSignature.t = function
      | `Identifier id -> id
      | `Class (sg, s) ->
          Identifier.Mk.class_ (parent_signature_identifier sg, s)
      | `ClassType (sg, s) ->
          Identifier.Mk.class_type (parent_signature_identifier sg, s)

    and field_parent_identifier : (na, na) field_parent -> Identifier.FieldParent.t =
      function
      | `Identifier id -> id
      | (`Hidden _ | `Alias _ | `AliasModuleType _ | `Module _ | `ModuleType _)
        as sg ->
          (parent_signature_identifier sg :> Identifier.FieldParent.t)
      | `Type _ as t -> (parent_type_identifier t :> Identifier.FieldParent.t)

    and label_parent_identifier : (na, na) label_parent -> Identifier.LabelParent.t =
      function
      | `Identifier id -> id
      | (`Class _ | `ClassType _) as c ->
          (parent_class_signature_identifier c :> Identifier.LabelParent.t)
      | ( `Hidden _ | `Alias _ | `AliasModuleType _ | `Module _ | `ModuleType _
        | `Type _ ) as r ->
          (field_parent_identifier r :> Identifier.LabelParent.t)

    and identifier : t -> Identifier.t = function
      | `Identifier id -> id
      | ( `Alias _ | `AliasModuleType _ | `Module _ | `Hidden _ | `Type _
        | `Class _ | `ClassType _ | `ModuleType _ ) as r ->
          (label_parent_identifier r :> Identifier.t)
      | `Field (p, n) -> Identifier.Mk.field (field_parent_identifier p, n)
      | `Constructor (s, n) ->
          Identifier.Mk.constructor
            ((parent_type_identifier s :> Identifier.DataType.t), n)
      | `Extension (p, q) ->
          Identifier.Mk.extension (parent_signature_identifier p, q)
      | `ExtensionDecl (p, q, r) ->
          Identifier.Mk.extension_decl (parent_signature_identifier p, (q, r))
      | `Exception (p, q) ->
          Identifier.Mk.exception_ (parent_signature_identifier p, q)
      | `Value (p, q) -> Identifier.Mk.value (parent_signature_identifier p, q)
      | `Method (p, q) ->
          Identifier.Mk.method_ (parent_class_signature_identifier p, q)
      | `InstanceVariable (p, q) ->
          Identifier.Mk.instance_variable
            (parent_class_signature_identifier p, q)
      | `Label (p, q) -> Identifier.Mk.label (label_parent_identifier p, q)

    module Signature = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.signature
      type t = (na, na) gen
    end

    module ClassSignature = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.class_signature
      type t = (na, na) gen
    end

    module DataType = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.datatype
      type t = (na, na) gen
    end

    module FieldParent = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.field_parent
      type t = (na, na) gen
    end

    module LabelParent = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.label_parent
      type t = (na, na) gen
    end

    module Module = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.module_
      type t = (na, na) gen
    end

    module ModuleType = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.module_type
      type t = (na, na) gen
    end

    module Type = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.type_
      type t = (na, na) gen
    end

    module Constructor = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.constructor
      type t = (na, na) gen
    end

    module Field = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.field
      type t = (na, na) gen
    end

    module Extension = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.extension
      type t = (na, na) gen
    end

    module ExtensionDecl = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.extension_decl
      type t = (na, na) gen
    end

    module Exception = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.exception_
      type t = (na, na) gen
    end

    module Value = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.value
      type t = (na, na) gen
    end

    module Class = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.class_
      type t = (na, na) gen
    end

    module ClassType = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.class_type
      type t = (na, na) gen
    end

    module Method = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.method_
      type t = (na, na) gen
    end

    module InstanceVariable = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.instance_variable
      type t = (na, na) gen
    end

    module Label = struct
      type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Resolved_reference.label
      type t = (na, na) gen
    end

    module Page = struct
      type t = Paths_types.Resolved_reference.page
    end
  end

  type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.any
  type t = (na, na) gen

  type tag_any = Paths_types.Reference.tag_any

  module Signature = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.signature
    type t = (na, na) gen
  end

  module ClassSignature = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.class_signature
    type t = (na, na) gen
  end

  module DataType = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.datatype
    type t = (na, na) gen
  end

  module FragmentTypeParent = struct
    type ('lmod, 'lmodty) gen =('lmod, 'lmodty)  Paths_types.Reference.fragment_type_parent
    type t = (na, na) gen
  end

  module LabelParent = struct
    type ('lmod, 'lmodty) gen =('lmod, 'lmodty)  Paths_types.Reference.label_parent
    type t = (na, na) gen
  end

  module Module = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.module_
    type t = (na, na) gen
  end

  module ModuleType = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.module_type
    type t = (na, na) gen
  end

  module Type = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.type_
    type t = (na, na) gen
  end

  module Constructor = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.constructor
    type t = (na, na) gen
  end

  module Field = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.field
    type t = (na, na) gen
  end

  module Extension = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.extension
    type t = (na, na) gen
  end

  module ExtensionDecl = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.extension_decl
    type t = (na, na) gen
  end

  module Exception = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.exception_
    type t = (na, na) gen
  end

  module Value = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.value
    type t = (na, na) gen
  end

  module Class = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.class_
    type t = (na, na) gen
  end

  module ClassType = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.class_type
    type t = (na, na) gen
  end

  module Method = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.method_
    type t = (na, na) gen
  end

  module InstanceVariable = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.instance_variable
    type t = (na, na) gen
  end

  module Label = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.label
    type t = (na, na) gen
  end

  module Page = struct
    type ('lmod, 'lmodty) gen = ('lmod, 'lmodty) Paths_types.Reference.page
    type t = (na, na) gen
  end
end
