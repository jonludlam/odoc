open Names
(** {1 Paths} *)

module Identifier = struct
  type container_page = [ `Page of container_page option * PageName.t ]
  (** @canonical Odoc_model.Paths.Identifier.ContainerPage.t *)

  type leaf_page = [ `LeafPage of container_page option * PageName.t ]
  (** @canonical Odoc_model.Paths.Identifier.LeafPage.t *)

  type page = [ container_page | leaf_page ]
  (** @canonical Odoc_model.Paths.Identifier.Page.t *)

  type source_page = [ `SourcePage of container_page * string ]
  (** The second argument is the filename.

      @canonical Odoc_model.Paths.Identifier.SourcePage.t *)

  type asset_file = [ `AssetFile of page * AssetName.t ]
  (** The second argument is the filename.

      @canonical Odoc_model.Paths.Identifier.AssetFile.t *)

  type source_location =
    [ `SourceLocationMod of source_page
    | `SourceLocation of source_page * DefName.t
    | `SourceLocationInternal of source_page * LocalName.t ]
  (** @canonical Odoc_model.Paths.Identifier.SourceLocation.t *)

  type odoc_id =
    [ page
    | source_page
    | asset_file
    | `Root of container_page option * ModuleName.t
    | `Implementation of ModuleName.t ]
  (** @canonical Odoc_model.Paths.Identifier.OdocId.t *)

  type signature =
    [ `Root of container_page option * ModuleName.t
    | `Module of signature * ModuleName.t
    | `Parameter of signature * ModuleName.t
    | `Result of signature
    | `ModuleType of signature * ModuleTypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Signature.t *)

  type class_signature =
    [ `Class of signature * TypeName.t | `ClassType of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.ClassSignature.t *)

  type datatype = [ `Type of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.DataType.t *)

  type field_parent = [ signature | datatype ]
  (** @canonical Odoc_model.Paths.Identifier.FieldParent.t *)

  (* fragment_type_parent in identifiers is for record fields parent. It’s type
     (for usual record fields) or [signature] for fields of inline records of
     extension constructor. *)
  type unboxed_field_parent = datatype
  (** @canonical Odoc_model.Paths.Identifier.UnboxedFieldParent.t *)

  type label_parent = [ field_parent | page | class_signature ]
  (** @canonical Odoc_model.Paths.Identifier.LabelParent.t *)

  type root_module = [ `Root of container_page option * ModuleName.t ]
  (** @canonical Odoc_model.Paths.Identifier.RootModule.t *)

  type module_ =
    [ root_module
    | `Module of signature * ModuleName.t
    | `Parameter of signature * ModuleName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Module.t *)

  type functor_parameter = [ `Parameter of signature * ModuleName.t ]
  (** @canonical Odoc_model.Paths.Identifier.FunctorParameter.t *)

  type functor_result = [ `Result of signature ]
  (** @canonical Odoc_model.Paths.Identifier.FunctorResult.t *)

  type module_type = [ `ModuleType of signature * ModuleTypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.ModuleType.t *)

  type type_ = [ `Type of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Type.t *)

  type constructor = [ `Constructor of datatype * ConstructorName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Constructor.t *)

  type field = [ `Field of field_parent * FieldName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Field.t *)

  type unboxed_field =
    [ `UnboxedField of unboxed_field_parent * UnboxedFieldName.t ]
  (** @canonical Odoc_model.Paths.Identifier.UnboxedField.t *)

  type extension = [ `Extension of signature * ExtensionName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Extension.t *)

  type extension_decl =
    [ `ExtensionDecl of signature * ExtensionName.t * ExtensionName.t ]
  (** @canonical Odoc_model.Paths.Identifier.ExtensionDecl.t *)

  type exception_ = [ `Exception of signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Exception.t *)

  type value = [ `Value of signature * ValueName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Value.t *)

  type class_ = [ `Class of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Class.t *)

  type class_type = [ `ClassType of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.ClassType.t *)

  type method_ = [ `Method of class_signature * MethodName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Method.t *)

  type instance_variable =
    [ `InstanceVariable of class_signature * InstanceVariableName.t ]
  (** @canonical Odoc_model.Paths.Identifier.InstanceVariable.t *)

  type label = [ `Label of label_parent * LabelName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Label.t *)

  type non_src =
    [ signature
    | class_signature
    | datatype
    | field_parent
    | unboxed_field_parent
    | label_parent
    | module_
    | functor_parameter
    | functor_result
    | module_type
    | type_
    | constructor
    | field
    | unboxed_field
    | extension
    | extension_decl
    | exception_
    | value
    | class_
    | class_type
    | method_
    | instance_variable
    | label
    | page ]
  (** @canonical Odoc_model.Paths.Identifier.NonSrc.t *)

  type any = [ non_src | source_page | source_location | asset_file ]
  (** @canonical Odoc_model.Paths.Identifier.t *)

  type path_module = [ module_ | functor_parameter | functor_result ]
  (** @canonical Odoc_model.Paths.Identifier.Path.Module.t *)

  type path_module_type = module_type
  (** @canonical Odoc_model.Paths.Identifier.Path.ModuleType.t *)

  type path_type = [ type_ | class_ | class_type ]
  (** @canonical Odoc_model.Paths.Identifier.Path.Type.t *)

  type path_value = value

  type path_class_type = [ class_ | class_type ]
  (** @canonical Odoc_model.Paths.Identifier.Path.ClassType.t *)

  type path_any =
    [ path_module | module_type | path_type | path_class_type | value ]
  (** @canonical Odoc_model.Paths.Identifier.Path.t *)

  type fragment_module = path_module

  type fragment_type = path_type

  type reference_module = path_module

  type reference_module_type = module_type

  type reference_type = path_type

  type reference_constructor = [ constructor | extension | exception_ ]

  type reference_field = field

  type reference_unboxed_field = unboxed_field

  type reference_extension = [ extension | exception_ ]

  type reference_extension_decl = extension_decl

  type reference_exception = exception_

  type reference_value = value

  type reference_class = class_

  type reference_class_type = [ class_ | class_type ]

  type reference_method = method_

  type reference_instance_variable = instance_variable

  type reference_label = label

  type reference_page = page
end

type na_ty = |
(* No value: the locals and typed parents of paths that are in a [.odoc]
   file, which have none. *)

type na = [ `Na of na_ty ]

(* Paths are generic in the identifiers of local items, ['lmod], ['lmodty]
   and ['lty], and in ['pty], which paths with a typed parent carry.
   The paths of the model instantiate them all with [na], so have none; the
   paths xref2 works on instantiate them with its own types. *)
module rec Path : sig
  type ('lmod, 'lmodty, 'pty, 'canon) module_ =
    [ `Resolved of ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.module_
    | `Identifier of Identifier.path_module * bool
    | `LocalMod of 'lmod * bool
    | `Substituted of ('lmod, 'lmodty, 'pty, 'canon) module_
    | `Root of ModuleName.t
    | `Dot of ('lmod, 'lmodty, 'pty, 'canon) module_ * ModuleName.t
    | `Module of
      'pty * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent * ModuleName.t
    | `Apply of
      ('lmod, 'lmodty, 'pty, 'canon) module_
      * ('lmod, 'lmodty, 'pty, 'canon) module_ ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >
  (** @canonical Odoc_model.Paths.Path.Module.gen *)

  type ('lmod, 'lmodty, 'pty, 'canon) module_type =
    [ `Resolved of ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.module_type
    | `SubstitutedMT of ('lmod, 'lmodty, 'pty, 'canon) module_type
    | `Identifier of Identifier.path_module_type * bool
    | `LocalModTy of 'lmodty * bool
    | `DotMT of ('lmod, 'lmodty, 'pty, 'canon) module_ * ModuleTypeName.t
    | `ModuleType of
      'pty
      * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent
      * ModuleTypeName.t ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >
  (** @canonical Odoc_model.Paths.Path.ModuleType.gen *)

  type ('lmod, 'lmodty, 'pty, 'lty, 'canon) type_ =
    [ `Resolved of ('lmod, 'lmodty, 'pty, 'lty, 'canon) Resolved_path.type_
    | `SubstitutedT of ('lmod, 'lmodty, 'pty, 'lty, 'canon) type_
    | `Identifier of Identifier.path_type * bool
    | `LocalTy of 'lty * bool
    | `DotT of ('lmod, 'lmodty, 'pty, 'canon) module_ * TypeName.t
    | `Type of
      'pty * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent * TypeName.t
    | `Class of
      'pty * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent * TypeName.t
    | `ClassType of
      'pty * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent * TypeName.t
    | `Unbox of ('lmod, 'lmodty, 'pty, 'lty, 'canon) type_ ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >
  (** @canonical Odoc_model.Paths.Path.Type.gen *)

  type ('lmod, 'lmodty, 'pty, 'canon) value =
    [ `Resolved of ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.value
    | `Identifier of Identifier.path_value * bool
    | `DotV of ('lmod, 'lmodty, 'pty, 'canon) module_ * ValueName.t
    | `Value of
      'pty * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent * ValueName.t
    ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >
  (** @canonical Odoc_model.Paths.Path.Value.gen *)

  type ('lmod, 'lmodty, 'pty, 'lty, 'canon) class_type =
    [ `Resolved of ('lmod, 'lmodty, 'pty, 'lty, 'canon) Resolved_path.class_type
    | `SubstitutedCT of ('lmod, 'lmodty, 'pty, 'lty, 'canon) class_type
    | `Identifier of Identifier.path_class_type * bool
    | `LocalTy of 'lty * bool
    | `DotT of ('lmod, 'lmodty, 'pty, 'canon) module_ * TypeName.t
    | `Class of
      'pty * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent * TypeName.t
    | `ClassType of
      'pty * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent * TypeName.t
    ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >
  (** @canonical Odoc_model.Paths.Path.ClassType.gen *)

  type ('lmod, 'lmodty, 'pty, 'lty, 'canon) any =
    [ `Resolved of ('lmod, 'lmodty, 'pty, 'lty, 'canon) Resolved_path.any
    | `SubstitutedT of ('lmod, 'lmodty, 'pty, 'lty, 'canon) type_
    | `SubstitutedMT of ('lmod, 'lmodty, 'pty, 'canon) module_type
    | `Substituted of ('lmod, 'lmodty, 'pty, 'canon) module_
    | `SubstitutedCT of ('lmod, 'lmodty, 'pty, 'lty, 'canon) class_type
    | `Identifier of Identifier.path_any * bool
    | `LocalMod of 'lmod * bool
    | `LocalModTy of 'lmodty * bool
    | `LocalTy of 'lty * bool
    | `Root of ModuleName.t
    | `Dot of ('lmod, 'lmodty, 'pty, 'canon) module_ * ModuleName.t
    | `DotT of ('lmod, 'lmodty, 'pty, 'canon) module_ * TypeName.t
    | `DotMT of ('lmod, 'lmodty, 'pty, 'canon) module_ * ModuleTypeName.t
    | `DotV of ('lmod, 'lmodty, 'pty, 'canon) module_ * ValueName.t
    | `Module of
      'pty * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent * ModuleName.t
    | `ModuleType of
      'pty
      * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent
      * ModuleTypeName.t
    | `Type of
      'pty * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent * TypeName.t
    | `Class of
      'pty * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent * TypeName.t
    | `ClassType of
      'pty * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent * TypeName.t
    | `Value of
      'pty * ('lmod, 'lmodty, 'pty, 'canon) Resolved_path.parent * ValueName.t
    | `Apply of
      ('lmod, 'lmodty, 'pty, 'canon) module_
      * ('lmod, 'lmodty, 'pty, 'canon) module_
    | `Unbox of ('lmod, 'lmodty, 'pty, 'lty, 'canon) type_ ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >
  (** @canonical Odoc_model.Paths.Path.gen *)
end =
  Path

and Resolved_path : sig
  type ('lmod, 'lmodty, 'pty, 'canon) parent =
    [ `Module of ('lmod, 'lmodty, 'pty, 'canon) module_
    | `ModuleType of ('lmod, 'lmodty, 'pty, 'canon) module_type * 'pty
    | `FragmentRoot of 'pty ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >

  and ('lmod, 'lmodty, 'pty, 'canon) module_ =
    [ `Identifier of Identifier.path_module
    | `LocalMod of 'lmod
    | `Subst of
      ('lmod, 'lmodty, 'pty, 'canon) module_type
      * ('lmod, 'lmodty, 'pty, 'canon) module_
    | `Substituted of ('lmod, 'lmodty, 'pty, 'canon) module_
    | `Hidden of ('lmod, 'lmodty, 'pty, 'canon) module_
    | `Module of ('lmod, 'lmodty, 'pty, 'canon) parent * ModuleName.t
    | `Canonical of ('lmod, 'lmodty, 'pty, 'canon) module_ * 'cm
      (** [`Canonical (mod, canonical)] *)
    | `Apply of
      ('lmod, 'lmodty, 'pty, 'canon) module_
      * ('lmod, 'lmodty, 'pty, 'canon) module_
      (** [`Apply (functor, argument)] *)
    | `Alias of
      ('lmod, 'lmodty, 'pty, 'canon) module_
      * ('lmod, 'lmodty, 'pty, 'canon) Path.module_
      * ('lmod, 'lmodty, 'pty, 'canon) module_ option
      (** [`Alias (dest, src, resolved src)] *)
    | `OpaqueModule of ('lmod, 'lmodty, 'pty, 'canon) module_ ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >
  (** @canonical Odoc_model.Paths.Path.Resolved.Module.gen *)

  and ('lmod, 'lmodty, 'pty, 'canon) module_type =
    [ `Identifier of Identifier.path_module_type
    | `LocalModTy of 'lmodty
    | `SubstT of
      ('lmod, 'lmodty, 'pty, 'canon) module_type
      * ('lmod, 'lmodty, 'pty, 'canon) module_type
    | `SubstitutedMT of ('lmod, 'lmodty, 'pty, 'canon) module_type
    | `CanonicalModuleType of ('lmod, 'lmodty, 'pty, 'canon) module_type * 'cmt
    | `AliasModuleType of
      ('lmod, 'lmodty, 'pty, 'canon) module_type
      * ('lmod, 'lmodty, 'pty, 'canon) module_type
    | `ModuleType of ('lmod, 'lmodty, 'pty, 'canon) parent * ModuleTypeName.t
    | `OpaqueModuleType of ('lmod, 'lmodty, 'pty, 'canon) module_type ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >
  (** @canonical Odoc_model.Paths.Path.Resolved.ModuleType.gen *)

  type ('lmod, 'lmodty, 'pty, 'canon) value =
    [ `Identifier of Identifier.path_value
    | `Value of ('lmod, 'lmodty, 'pty, 'canon) parent * ValueName.t ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >
  (** @canonical Odoc_model.Paths.Path.Resolved.Value.gen *)

  type ('lmod, 'lmodty, 'pty, 'lty, 'canon) class_type =
    [ `Identifier of Identifier.path_class_type
    | `LocalTy of 'lty
    | `SubstitutedCT of ('lmod, 'lmodty, 'pty, 'lty, 'canon) class_type
    | `Class of ('lmod, 'lmodty, 'pty, 'canon) parent * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty, 'canon) parent * TypeName.t ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >
  (** @canonical Odoc_model.Paths.Path.Resolved.ClassType.gen *)

  type ('lmod, 'lmodty, 'pty, 'lty, 'canon) type_ =
    [ `Identifier of Identifier.path_type
    | `LocalTy of 'lty
    | `SubstitutedT of ('lmod, 'lmodty, 'pty, 'lty, 'canon) type_
    | `SubstitutedCT of ('lmod, 'lmodty, 'pty, 'lty, 'canon) class_type
    | `CanonicalType of ('lmod, 'lmodty, 'pty, 'lty, 'canon) type_ * 'ct
    | `Type of ('lmod, 'lmodty, 'pty, 'canon) parent * TypeName.t
    | `Class of ('lmod, 'lmodty, 'pty, 'canon) parent * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty, 'canon) parent * TypeName.t
    | `CoreType of TypeName.t
    | `Unbox of ('lmod, 'lmodty, 'pty, 'lty, 'canon) type_ ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >
  (** @canonical Odoc_model.Paths.Path.Resolved.Type.gen *)

  type ('lmod, 'lmodty, 'pty, 'lty, 'canon) any =
    [ `Identifier of Identifier.any
    | `LocalMod of 'lmod
    | `LocalModTy of 'lmodty
    | `LocalTy of 'lty
    | `SubstitutedCT of ('lmod, 'lmodty, 'pty, 'lty, 'canon) class_type
    | `SubstitutedT of ('lmod, 'lmodty, 'pty, 'lty, 'canon) type_
    | `SubstitutedMT of ('lmod, 'lmodty, 'pty, 'canon) module_type
    | `Substituted of ('lmod, 'lmodty, 'pty, 'canon) module_
    | `Subst of
      ('lmod, 'lmodty, 'pty, 'canon) module_type
      * ('lmod, 'lmodty, 'pty, 'canon) module_
    | `Hidden of ('lmod, 'lmodty, 'pty, 'canon) module_
    | `Module of ('lmod, 'lmodty, 'pty, 'canon) parent * ModuleName.t
    | `Canonical of ('lmod, 'lmodty, 'pty, 'canon) module_ * 'cm
    | `Apply of
      ('lmod, 'lmodty, 'pty, 'canon) module_
      * ('lmod, 'lmodty, 'pty, 'canon) module_
    | `Alias of
      ('lmod, 'lmodty, 'pty, 'canon) module_
      * ('lmod, 'lmodty, 'pty, 'canon) Path.module_
      * ('lmod, 'lmodty, 'pty, 'canon) module_ option
    | `AliasModuleType of
      ('lmod, 'lmodty, 'pty, 'canon) module_type
      * ('lmod, 'lmodty, 'pty, 'canon) module_type
    | `OpaqueModule of ('lmod, 'lmodty, 'pty, 'canon) module_
    | `ModuleType of ('lmod, 'lmodty, 'pty, 'canon) parent * ModuleTypeName.t
    | `CanonicalModuleType of ('lmod, 'lmodty, 'pty, 'canon) module_type * 'cmt
    | `SubstT of
      ('lmod, 'lmodty, 'pty, 'canon) module_type
      * ('lmod, 'lmodty, 'pty, 'canon) module_type
    | `OpaqueModuleType of ('lmod, 'lmodty, 'pty, 'canon) module_type
    | `CanonicalType of ('lmod, 'lmodty, 'pty, 'lty, 'canon) type_ * 'ct
    | `Type of ('lmod, 'lmodty, 'pty, 'canon) parent * TypeName.t
    | `Class of ('lmod, 'lmodty, 'pty, 'canon) parent * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty, 'canon) parent * TypeName.t
    | `Value of ('lmod, 'lmodty, 'pty, 'canon) parent * ValueName.t
    | `CoreType of TypeName.t
    | `Unbox of ('lmod, 'lmodty, 'pty, 'lty, 'canon) type_ ]
    constraint 'canon = < m : 'cm ; mt : 'cmt ; t : 'ct >
  (** @canonical Odoc_model.Paths.Path.Resolved.gen *)
end =
  Resolved_path

type canon =
  < m : (na, na, na, canon) Path.module_
  ; mt : (na, na, na, canon) Path.module_type
  ; t : (na, na, na, na, canon) Path.type_ >
(** The canonical targets of model paths: model paths. *)

module rec Fragment : sig
  type signature =
    [ `Resolved of Resolved_fragment.signature
    | `Dot of signature * string
    | `Root ]
  (** @canonical Odoc_model.Paths.Fragment.Signature.t *)

  type module_ =
    [ `Resolved of Resolved_fragment.module_ | `Dot of signature * string ]
  (** @canonical Odoc_model.Paths.Fragment.Module.t *)

  type module_type =
    [ `Resolved of Resolved_fragment.module_type | `Dot of signature * string ]
  (** @canonical Odoc_model.Paths.Fragment.ModuleType.t *)

  type type_ =
    [ `Resolved of Resolved_fragment.type_ | `Dot of signature * string ]
  (** @canonical Odoc_model.Paths.Fragment.Type.t *)

  type leaf =
    [ `Resolved of Resolved_fragment.leaf | `Dot of signature * string ]
  (** @canonical Odoc_model.Paths.Fragment.leaf *)

  type any =
    [ `Resolved of Resolved_fragment.any | `Dot of signature * string | `Root ]
  (** @canonical Odoc_model.Paths.Fragment.t *)
end =
  Fragment

and Resolved_fragment : sig
  type root =
    [ `ModuleType of (na, na, na, canon) Resolved_path.module_type
    | `Module of (na, na, na, canon) Resolved_path.module_ ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.root *)

  type signature =
    [ `Root of root
    | `Subst of (na, na, na, canon) Resolved_path.module_type * module_
    | `Alias of (na, na, na, canon) Resolved_path.module_ * module_
    | `Module of signature * ModuleName.t
    | `OpaqueModule of module_ ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.Signature.t *)

  and module_ =
    [ `Subst of (na, na, na, canon) Resolved_path.module_type * module_
    | `Alias of (na, na, na, canon) Resolved_path.module_ * module_
    | `Module of signature * ModuleName.t
    | `OpaqueModule of module_ ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.Module.t *)

  type type_ =
    [ `Type of signature * TypeName.t
    | `Class of signature * TypeName.t
    | `ClassType of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.Type.t *)

  and module_type = [ `Module_type of signature * ModuleTypeName.t ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.ModuleType.t *)

  type leaf = [ module_ | module_type | type_ ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.leaf *)

  (* Absence of `Root here might make coersions annoying *)
  type any =
    [ `Root of root
    | `Subst of (na, na, na, canon) Resolved_path.module_type * module_
    | `Alias of (na, na, na, canon) Resolved_path.module_ * module_
    | `Module of signature * ModuleName.t
    | `Module_type of signature * ModuleTypeName.t
    | `Type of signature * TypeName.t
    | `Class of signature * TypeName.t
    | `ClassType of signature * TypeName.t
    | `OpaqueModule of module_ ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.t *)
end =
  Resolved_fragment

module rec Reference : sig
  type tag_only_module = [ `TModule ]

  type tag_only_module_type = [ `TModuleType ]

  type tag_only_type = [ `TType ]

  type tag_only_constructor = [ `TConstructor ]

  type tag_only_field = [ `TField ]

  type tag_only_unboxed_field = [ `TUnboxedField ]

  type tag_only_extension = [ `TExtension ]

  type tag_only_exception = [ `TException ]

  type tag_only_value = [ `TValue ]

  type tag_only_class = [ `TClass ]

  type tag_only_class_type = [ `TClassType ]

  type tag_only_method = [ `TMethod ]

  type tag_only_instance_variable = [ `TInstanceVariable ]

  type tag_only_label = [ `TLabel ]

  type tag_only_page = [ `TPage ]

  type tag_unknown = [ `TUnknown ]

  type tag_only_child_page = [ `TChildPage ]

  type tag_only_child_module = [ `TChildModule ]

  type tag_hierarchy =
    [ `TRelativePath  (** [{!identifier/}] *)
    | `TAbsolutePath  (** [{!/identifier}] *)
    | `TCurrentPackage  (** [{!//identifier}] *) ]
  (** @canonical Odoc_model.Paths.Reference.tag_hierarchy *)

  type tag_any =
    [ `TModule
    | `TModuleType
    | `TType
    | `TConstructor
    | `TField
    | `TUnboxedField
    | `TExtension
    | `TExtensionDecl
    | `TException
    | `TValue
    | `TClass
    | `TClassType
    | `TMethod
    | `TInstanceVariable
    | `TLabel
    | `TPage
    | `TAsset
    | `TChildPage
    | `TChildModule
    | `TUnknown ]
  (** @canonical Odoc_model.Paths.Reference.tag_any *)

  type tag_signature = [ `TUnknown | `TModule | `TModuleType ]

  type tag_class_signature = [ `TUnknown | `TClass | `TClassType ]

  type tag_datatype = [ `TUnknown | `TType ]

  type tag_parent = [ `TUnknown | `TModule | `TModuleType | `TType ]

  type tag_label_parent =
    [ `TUnknown
    | `TModule
    | `TModuleType
    | `TClass
    | `TClassType
    | `TType
    | `TPage
    | `TChildPage
    | `TChildModule ]

  type hierarchy = tag_hierarchy * string list
  (** @canonical Odoc_model.Paths.Reference.Hierarchy.t *)

  type signature =
    [ `Resolved of Resolved_reference.signature
    | `Root of string * tag_signature
    | `Dot of label_parent * string
    | `Module_path of hierarchy
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Signature.t *)

  and class_signature =
    [ `Resolved of Resolved_reference.class_signature
    | `Root of string * tag_class_signature
    | `Dot of label_parent * string
    | `Class of signature * TypeName.t
    | `ClassType of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.ClassSignature.t *)

  and datatype =
    [ `Resolved of Resolved_reference.datatype
    | `Root of string * tag_datatype
    | `Dot of label_parent * string
    | `Type of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.DataType.t *)

  (* Parent of fields and constructor. Can be either a type or [signature] *)
  and fragment_type_parent =
    [ `Resolved of Resolved_reference.field_parent
    | `Root of string * tag_parent
    | `Dot of label_parent * string
    | `Module_path of hierarchy
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t
    | `Type of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.FragmentTypeParent.t *)

  and label_parent =
    [ `Resolved of Resolved_reference.label_parent
    | `Root of string * tag_label_parent
    | `Dot of label_parent * string
    | `Page_path of hierarchy
    | `Module_path of hierarchy
    | `Any_path of hierarchy
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t
    | `Class of signature * TypeName.t
    | `ClassType of signature * TypeName.t
    | `Type of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.LabelParent.t *)

  type asset =
    [ `Resolved of Resolved_reference.asset | `Asset_path of hierarchy ]

  type module_ =
    [ `Resolved of Resolved_reference.module_
    | `Root of string * [ `TModule | `TUnknown ]
    | `Dot of label_parent * string
    | `Module_path of hierarchy
    | `Module of signature * ModuleName.t ]
  (** @canonical Odoc_model.Paths.Reference.Module.t *)

  type module_type =
    [ `Resolved of Resolved_reference.module_type
    | `Root of string * [ `TModuleType | `TUnknown ]
    | `Dot of label_parent * string
    | `ModuleType of signature * ModuleTypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.ModuleType.t *)

  type type_ =
    [ `Resolved of Resolved_reference.type_
    | `Root of string * [ `TType | `TClass | `TClassType | `TUnknown ]
    | `Dot of label_parent * string
    | `Class of signature * TypeName.t
    | `ClassType of signature * TypeName.t
    | `Type of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Type.t *)

  type constructor =
    [ `Resolved of Resolved_reference.constructor
    | `Root of string * [ `TConstructor | `TExtension | `TException | `TUnknown ]
    | `Dot of label_parent * string
    | `Constructor of fragment_type_parent * ConstructorName.t
    | `Extension of signature * ExtensionName.t
    | `Exception of signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Reference.Constructor.t *)

  type field =
    [ `Resolved of Resolved_reference.field
    | `Root of string * [ `TField | `TUnknown ]
    | `Dot of label_parent * string
    | `Field of fragment_type_parent * FieldName.t ]
  (** @canonical Odoc_model.Paths.Reference.Field.t *)

  type unboxed_field =
    [ `Resolved of Resolved_reference.unboxed_field
    | `Root of string * [ `TField | `TUnknown ]
    | `Dot of label_parent * string
    | `UnboxedField of fragment_type_parent * UnboxedFieldName.t ]
  (** @canonical Odoc_model.Paths.Reference.UnboxedField.t *)

  type extension =
    [ `Resolved of Resolved_reference.extension
    | `Root of string * [ `TExtension | `TException | `TUnknown ]
    | `Dot of label_parent * string
    | `Extension of signature * ExtensionName.t
    | `Exception of signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Reference.Extension.t *)

  type extension_decl =
    [ `Resolved of Resolved_reference.extension_decl
    | `Root of string * [ `TExtension | `TException | `TUnknown ]
    | `Dot of label_parent * string
    | `ExtensionDecl of signature * ExtensionName.t ]
  (** @canonical Odoc_model.Paths.Reference.ExtensionDecl.t *)

  type exception_ =
    [ `Resolved of Resolved_reference.exception_
    | `Root of string * [ `TException | `TUnknown ]
    | `Dot of label_parent * string
    | `Exception of signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Reference.Exception.t *)

  type value =
    [ `Resolved of Resolved_reference.value
    | `Root of string * [ `TValue | `TUnknown ]
    | `Dot of label_parent * string
    | `Value of signature * ValueName.t ]
  (** @canonical Odoc_model.Paths.Reference.Value.t *)

  type class_ =
    [ `Resolved of Resolved_reference.class_
    | `Root of string * [ `TClass | `TUnknown ]
    | `Dot of label_parent * string
    | `Class of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Class.t *)

  type class_type =
    [ `Resolved of Resolved_reference.class_type
    | `Root of string * [ `TClass | `TClassType | `TUnknown ]
    | `Dot of label_parent * string
    | `Class of signature * TypeName.t
    | `ClassType of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.ClassType.t *)

  type method_ =
    [ `Resolved of Resolved_reference.method_
    | `Root of string * [ `TMethod | `TUnknown ]
    | `Dot of label_parent * string
    | `Method of class_signature * MethodName.t ]
  (** @canonical Odoc_model.Paths.Reference.Method.t *)

  type instance_variable =
    [ `Resolved of Resolved_reference.instance_variable
    | `Root of string * [ `TInstanceVariable | `TUnknown ]
    | `Dot of label_parent * string
    | `InstanceVariable of class_signature * InstanceVariableName.t ]
  (** @canonical Odoc_model.Paths.Reference.InstanceVariable.t *)

  type label =
    [ `Resolved of Resolved_reference.label
    | `Root of string * [ `TLabel | `TUnknown ]
    | `Dot of label_parent * string
    | `Label of label_parent * LabelName.t ]
  (** @canonical Odoc_model.Paths.Reference.Label.t *)

  type page =
    [ `Resolved of Resolved_reference.page
    | `Root of string * [ `TPage | `TUnknown ]
    | `Page_path of hierarchy ]
  (** @canonical Odoc_model.Paths.Reference.Page.t *)

  type any =
    [ `Resolved of Resolved_reference.any
    | `Root of string * tag_any
    | `Dot of label_parent * string
    | `Page_path of hierarchy
    | `Module_path of hierarchy
    | `Asset_path of hierarchy
    | `Any_path of hierarchy
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t
    | `Type of signature * TypeName.t
    | `Constructor of fragment_type_parent * ConstructorName.t
    | `Field of fragment_type_parent * FieldName.t
    | `UnboxedField of fragment_type_parent * UnboxedFieldName.t
    | `Extension of signature * ExtensionName.t
    | `ExtensionDecl of signature * ExtensionName.t
    | `Exception of signature * ExceptionName.t
    | `Value of signature * ValueName.t
    | `Class of signature * TypeName.t
    | `ClassType of signature * TypeName.t
    | `Method of class_signature * MethodName.t
    | `InstanceVariable of class_signature * InstanceVariableName.t
    | `Label of label_parent * LabelName.t ]
  (** @canonical Odoc_model.Paths.Reference.t *)
end =
  Reference

and Resolved_reference : sig
  (* Note - many of these are effectively unions of previous types,
     but they are declared here explicitly because OCaml isn't yet
     smart enough to accept the more natural expression of this. Hence
     we define here all those types that ever appear on the right hand
     side of the constructors and then below we redefine many with
     the actual hierarchy made more explicit. *)
  type datatype =
    [ `Identifier of Identifier.datatype | `Type of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.DataType.t *)

  and module_ =
    [ `Identifier of Identifier.path_module
    | `Hidden of module_
    | `Alias of (na, na, na, canon) Resolved_path.module_ * module_
    | `Module of signature * ModuleName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Module.t *)

  (* Signature is [ module | moduletype ] *)
  and signature =
    [ `Identifier of Identifier.signature
    | `Hidden of module_
    | `Alias of (na, na, na, canon) Resolved_path.module_ * module_
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t
    | `AliasModuleType of
      (na, na, na, canon) Resolved_path.module_type * module_type ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Signature.t *)

  and class_signature =
    [ `Identifier of Identifier.class_signature
    | `Class of signature * TypeName.t
    | `ClassType of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.ClassSignature.t *)

  (* fragment_type_parent in resolved references is for record fields parent.
     It’s type (for usual record fields) or [signature] for fields of inline
     records of extension constructor. *)
  and field_parent =
    [ `Identifier of Identifier.field_parent
    | `Alias of (na, na, na, canon) Resolved_path.module_ * module_
    | `AliasModuleType of
      (na, na, na, canon) Resolved_path.module_type * module_type
    | `Module of signature * ModuleName.t
    | `Hidden of module_
    | `ModuleType of signature * ModuleTypeName.t
    | `Type of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.FragmentTypeParent.t *)

  and unboxed_field_parent =
    [ `Identifier of Identifier.unboxed_field_parent
    | `Type of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.FragmentTypeParent.t *)

  (* The only difference between parent and label_parent
     is that the Identifier allows more types *)
  and label_parent =
    [ `Identifier of Identifier.label_parent
    | `Alias of (na, na, na, canon) Resolved_path.module_ * module_
    | `AliasModuleType of
      (na, na, na, canon) Resolved_path.module_type * module_type
    | `Module of signature * ModuleName.t
    | `Hidden of module_
    | `ModuleType of signature * ModuleTypeName.t
    | `Class of signature * TypeName.t
    | `ClassType of signature * TypeName.t
    | `Type of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.LabelParent.t *)

  and module_type =
    [ `Identifier of Identifier.reference_module_type
    | `ModuleType of signature * ModuleTypeName.t
    | `AliasModuleType of
      (na, na, na, canon) Resolved_path.module_type * module_type ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.ModuleType.t *)

  type type_ =
    [ `Identifier of Identifier.reference_type
    | `Type of signature * TypeName.t
    | `Class of signature * TypeName.t
    | `ClassType of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Type.t *)

  type constructor =
    [ `Identifier of Identifier.reference_constructor
    | `Constructor of datatype * ConstructorName.t
    | `Extension of signature * ExtensionName.t
    | `Exception of signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Constructor.t *)

  type field =
    [ `Identifier of Identifier.reference_field
    | `Field of field_parent * FieldName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Field.t *)

  type unboxed_field =
    [ `Identifier of Identifier.reference_unboxed_field
    | `UnboxedField of unboxed_field_parent * UnboxedFieldName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.UnboxedField.t *)

  type extension =
    [ `Identifier of Identifier.reference_extension
    | `Extension of signature * ExtensionName.t
    | `Exception of signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Extension.t *)

  type extension_decl =
    [ `Identifier of Identifier.reference_extension_decl
    | `ExtensionDecl of
      signature
      * ExtensionName.t
        (* The extension_name used in the url.
           It is the extension_name of the first constructor of the extension (there is always at least 1). *)
      * ExtensionName.t (* displayed *) ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Extension.t *)

  type exception_ =
    [ `Identifier of Identifier.reference_exception
    | `Exception of signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Exception.t *)

  type value =
    [ `Identifier of Identifier.reference_value
    | `Value of signature * ValueName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Value.t *)

  type class_ =
    [ `Identifier of Identifier.reference_class
    | `Class of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Class.t *)

  type class_type =
    [ `Identifier of Identifier.reference_class_type
    | `Class of signature * TypeName.t
    | `ClassType of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.ClassType.t *)

  type method_ =
    [ `Identifier of Identifier.reference_method
    | `Method of class_signature * MethodName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Method.t *)

  type instance_variable =
    [ `Identifier of Identifier.reference_instance_variable
    | `InstanceVariable of class_signature * InstanceVariableName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.InstanceVariable.t *)

  type label =
    [ `Identifier of Identifier.reference_label
    | `Label of label_parent * LabelName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Label.t *)

  type page = [ `Identifier of Identifier.reference_page ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Page.t *)

  type asset = [ `Identifier of Identifier.asset_file ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Asset.t *)

  type any =
    [ `Identifier of Identifier.any
    | `Alias of (na, na, na, canon) Resolved_path.module_ * module_
    | `AliasModuleType of
      (na, na, na, canon) Resolved_path.module_type * module_type
    | `Module of signature * ModuleName.t
    | `Hidden of module_
    | `ModuleType of signature * ModuleTypeName.t
    | `Type of signature * TypeName.t
    | `Constructor of datatype * ConstructorName.t
    | `PolyConstructor of datatype * ConstructorName.t
    | `Field of field_parent * FieldName.t
    | `UnboxedField of unboxed_field_parent * UnboxedFieldName.t
    | `Extension of signature * ExtensionName.t
    | `ExtensionDecl of signature * ExtensionName.t * ExtensionName.t
    | `Exception of signature * ExceptionName.t
    | `Value of signature * ValueName.t
    | `Class of signature * TypeName.t
    | `ClassType of signature * TypeName.t
    | `Method of class_signature * MethodName.t
    | `InstanceVariable of class_signature * InstanceVariableName.t
    | `Label of label_parent * LabelName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.t *)
end =
  Resolved_reference
