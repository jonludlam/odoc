open Names
(** {1 Paths} *)

type 'a id = { iv : 'a; ihash : int; ikey : string }
(** @canonical Odoc_model.Paths.Identifier.id *)

module Identifier = struct
  type container_page_pv = [ `Page of container_page option * PageName.t ]
  (** @canonical Odoc_model.Paths.Identifier.ContainerPage.t_pv *)

  and container_page = container_page_pv id
  (** @canonical Odoc_model.Paths.Identifier.ContainerPage.t *)

  type page_pv =
    [ container_page_pv | `LeafPage of container_page option * PageName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Page.t_pv *)

  and page = page_pv id
  (** @canonical Odoc_model.Paths.Identifier.Page.t *)

  type source_dir_pv = [ container_page_pv | `SourceDir of source_dir * string ]
  (** @canonical Odoc_model.Paths.Identifier.SourceDir.t_pv *)

  and source_dir = source_dir_pv id
  (** @canonical Odoc_model.Paths.Identifier.SourceDir.t *)

  type source_page_pv = [ `SourcePage of source_dir * string ]
  (** The second argument is the filename.

     @canonical Odoc_model.Paths.Identifier.SourcePage.t_pv *)

  type source_page = source_page_pv id
  (** @canonical Odoc_model.Paths.Identifier.SourcePage.t *)

  type asset_file_pv = [ `AssetFile of page * string ]
  (** The second argument is the filename.

    @canonical Odoc_model.Paths.Identifier.AssetFile.t_pv *)

  type asset_file = asset_file_pv id
  (** @canonical Odoc_model.Paths.Identifier.AssetFile.t *)

  type source_location_pv =
    [ `SourceLocationMod of source_page
    | `SourceLocation of source_page * DefName.t
    | `SourceLocationInternal of source_page * LocalName.t ]
  (** @canonical Odoc_model.Paths.Identifier.SourceLocation.t *)

  and source_location = source_location_pv id
  (** @canonical Odoc_model.Paths.Identifier.SourceLocation.t_pv *)

  type odoc_id_pv =
    [ page_pv
    | source_page_pv
    | `Root of container_page option * ModuleName.t
    | `Implementation of ModuleName.t ]
  (** @canonical Odoc_model.Paths.Identifier.OdocId.t_pv *)

  and odoc_id = odoc_id_pv id
  (** @canonical Odoc_model.Paths.Identifier.OdocId.t *)

  type signature_pv =
    [ `Root of container_page option * ModuleName.t
    | `Module of signature * ModuleName.t
    | `Parameter of signature * ModuleName.t
    | `Result of signature
    | `ModuleType of signature * ModuleTypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Signature.t_pv *)

  and signature = signature_pv id
  (** @canonical Odoc_model.Paths.Identifier.Signature.t *)

  type class_signature_pv =
    [ `Class of signature * TypeName.t | `ClassType of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.ClassSignature.t_pv *)

  and class_signature = class_signature_pv id
  (** @canonical Odoc_model.Paths.Identifier.ClassSignature.t *)

  type datatype_pv =
    [ `Type of signature * TypeName.t | `CoreType of TypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.DataType.t_pv *)

  and datatype = datatype_pv id
  (** @canonical Odoc_model.Paths.Identifier.DataType.t *)

  type field_parent_pv = [ signature_pv | datatype_pv ]
  (** @canonical Odoc_model.Paths.Identifier.FieldParent.t_pv *)

  (* fragment_type_parent in identifiers is for record fields parent. It’s type
     (for usual record fields) or [signature] for fields of inline records of
     extension constructor. *)
  and field_parent = field_parent_pv id
  (** @canonical Odoc_model.Paths.Identifier.FieldParent.t *)

  type label_parent_pv = [ field_parent_pv | page_pv | class_signature_pv ]
  (** @canonical Odoc_model.Paths.Identifier.LabelParent.t_pv *)

  and label_parent = label_parent_pv id
  (** @canonical Odoc_model.Paths.Identifier.LabelParent.t *)

  type root_module_pv = [ `Root of container_page option * ModuleName.t ]
  (** @canonical Odoc_model.Paths.Identifier.RootModule.t_pv *)

  and root_module = root_module_pv id
  (** @canonical Odoc_model.Paths.Identifier.RootModule.t *)

  type module_pv =
    [ root_module_pv
    | `Module of signature * ModuleName.t
    | `Parameter of signature * ModuleName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Module.t_pv *)

  and module_ = module_pv id
  (** @canonical Odoc_model.Paths.Identifier.Module.t *)

  type functor_parameter_pv = [ `Parameter of signature * ModuleName.t ]
  (** @canonical Odoc_model.Paths.Identifier.FunctorParameter.t_pv *)

  and functor_parameter = functor_parameter_pv id
  (** @canonical Odoc_model.Paths.Identifier.FunctorParameter.t *)

  type functor_result_pv = [ `Result of signature ]
  (** @canonical Odoc_model.Paths.Identifier.FunctorResult.t_pv *)

  and functor_result = functor_result_pv id
  (** @canonical Odoc_model.Paths.Identifier.FunctorResult.t *)

  type module_type_pv = [ `ModuleType of signature * ModuleTypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.ModuleType.t_pv *)

  and module_type = module_type_pv id
  (** @canonical Odoc_model.Paths.Identifier.ModuleType.t *)

  type type_pv = [ `Type of signature * TypeName.t | `CoreType of TypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Type.t_pv *)

  and type_ = type_pv id
  (** @canonical Odoc_model.Paths.Identifier.Type.t *)

  type constructor_pv = [ `Constructor of datatype * ConstructorName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Constructor.t_pv *)

  and constructor = constructor_pv id
  (** @canonical Odoc_model.Paths.Identifier.Constructor.t *)

  type field_pv = [ `Field of field_parent * FieldName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Field.t_pv *)

  and field = field_pv id
  (** @canonical Odoc_model.Paths.Identifier.Field.t *)

  type extension_pv = [ `Extension of signature * ExtensionName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Extension.t_pv *)

  type extension_decl_pv =
    [ `ExtensionDecl of signature * ExtensionName.t * ExtensionName.t ]
  (** @canonical Odoc_model.Paths.Identifier.ExtensionDecl.t_pv *)

  and extension = extension_pv id
  (** @canonical Odoc_model.Paths.Identifier.Extension.t *)

  and extension_decl = extension_decl_pv id
  (** @canonical Odoc_model.Paths.Identifier.ExtensionDecl.t *)

  type exception_pv =
    [ `Exception of signature * ExceptionName.t
    | `CoreException of ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Exception.t_pv *)

  and exception_ = exception_pv id
  (** @canonical Odoc_model.Paths.Identifier.Exception.t *)

  type value_pv = [ `Value of signature * ValueName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Value.t_pv *)

  and value = value_pv id
  (** @canonical Odoc_model.Paths.Identifier.Value.t *)

  type class_pv = [ `Class of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Class.t_pv *)

  and class_ = class_pv id
  (** @canonical Odoc_model.Paths.Identifier.Class.t *)

  type class_type_pv = [ `ClassType of signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Identifier.ClassType.t_pv *)

  and class_type = class_type_pv id
  (** @canonical Odoc_model.Paths.Identifier.ClassType.t *)

  type method_pv = [ `Method of class_signature * MethodName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Method.t_pv *)

  and method_ = method_pv id
  (** @canonical Odoc_model.Paths.Identifier.Method.t *)

  type instance_variable_pv =
    [ `InstanceVariable of class_signature * InstanceVariableName.t ]
  (** @canonical Odoc_model.Paths.Identifier.InstanceVariable.t_pv *)

  and instance_variable = instance_variable_pv id
  (** @canonical Odoc_model.Paths.Identifier.InstanceVariable.t *)

  type label_pv = [ `Label of label_parent * LabelName.t ]
  (** @canonical Odoc_model.Paths.Identifier.Label.t_pv *)

  and label = label_pv id
  (** @canonical Odoc_model.Paths.Identifier.Label.t *)

  type non_src_pv =
    [ signature_pv
    | class_signature_pv
    | datatype_pv
    | field_parent_pv
    | label_parent_pv
    | module_pv
    | functor_parameter_pv
    | functor_result_pv
    | module_type_pv
    | type_pv
    | constructor_pv
    | field_pv
    | extension_pv
    | extension_decl_pv
    | exception_pv
    | value_pv
    | class_pv
    | class_type_pv
    | method_pv
    | instance_variable_pv
    | label_pv
    | page_pv ]
  (** @canonical Odoc_model.Paths.Identifier.NonSrc.t_pv *)

  and non_src = non_src_pv id
  (** @canonical Odoc_model.Paths.Identifier.NonSrc.t *)

  type any_pv =
    [ non_src_pv
    | source_page_pv
    | source_dir_pv
    | source_location_pv
    | asset_file_pv ]
  (** @canonical Odoc_model.Paths.Identifier.t_pv *)

  and any = any_pv id
  (** @canonical Odoc_model.Paths.Identifier.t *)

  type path_module_pv = [ module_pv | functor_parameter_pv | functor_result_pv ]
  (** @canonical Odoc_model.Paths.Identifier.Path.Module.t_pv *)

  and path_module = path_module_pv id
  (** @canonical Odoc_model.Paths.Identifier.Path.Module.t *)

  type path_module_type = module_type
  (** @canonical Odoc_model.Paths.Identifier.Path.ModuleType.t *)

  type path_type_pv = [ type_pv | class_pv | class_type_pv ]
  (** @canonical Odoc_model.Paths.Identifier.Path.Type.t_pv *)

  and path_type = path_type_pv id
  (** @canonical Odoc_model.Paths.Identifier.Path.Type.t *)

  type path_value = value

  type path_class_type_pv = [ class_pv | class_type_pv ]
  (** @canonical Odoc_model.Paths.Identifier.Path.ClassType.t_pv *)

  and path_class_type = path_class_type_pv id
  (** @canonical Odoc_model.Paths.Identifier.Path.ClassType.t *)

  type path_any =
    [ path_module_pv
    | module_type_pv
    | path_type_pv
    | path_class_type_pv
    | value_pv ]
    id
  (** @canonical Odoc_model.Paths.Identifier.Path.t *)

  type fragment_module = path_module

  type fragment_type = path_type

  type reference_module = path_module

  type reference_module_type = module_type

  type reference_type = path_type

  type reference_constructor =
    [ constructor_pv | extension_pv | exception_pv ] id

  type reference_field = field

  type reference_extension = [ extension_pv | exception_pv ] id

  type reference_extension_decl = extension_decl

  type reference_exception = exception_

  type reference_value = value

  type reference_class = class_

  type reference_class_type = [ class_pv | class_type_pv ] id

  type reference_method = method_

  type reference_instance_variable = instance_variable

  type reference_label = label

  type reference_page = page
end

module rec Path : sig
  type ('lmod, 'lmodty, 'pty) module_ =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_path.module_
    | `Identifier of Identifier.path_module * bool
    | `LocalMod of 'lmod
    | `Substituted of ('lmod, 'lmodty, 'pty) module_
    | `Root of ModuleName.t
    | `Forward of string
    | `Dot of ('lmod, 'lmodty, 'pty) module_ * ModuleName.t
    | `Module of
      'pty * ('lmod, 'lmodty, 'pty) Resolved_path.parent * ModuleName.t
    | `Apply of ('lmod, 'lmodty, 'pty) module_ * ('lmod, 'lmodty, 'pty) module_
    ]
  (** @canonical Odoc_model.Paths.Path.Module.t *)

  type ('lmod, 'lmodty, 'pty) module_type =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_path.module_type
    | `LocalModTy of 'lmodty
    | `SubstitutedMT of ('lmod, 'lmodty, 'pty) module_type
    | `Identifier of Identifier.path_module_type * bool
    | `DotMT of ('lmod, 'lmodty, 'pty) module_ * ModuleTypeName.t
    | `ModuleType of
      'pty * ('lmod, 'lmodty, 'pty) Resolved_path.parent * ModuleTypeName.t ]
  (** @canonical Odoc_model.Paths.Path.ModuleType.t *)

  type ('lmod, 'lmodty, 'pty, 'lcty) class_type =
    [ `Resolved of ('lmod, 'lmodty, 'pty, 'lcty) Resolved_path.class_type
    | `LocalCty of 'lcty
    | `SubstitutedCT of ('lmod, 'lmodty, 'pty, 'lcty) class_type
    | `Identifier of Identifier.path_class_type * bool
    | `DotT of ('lmod, 'lmodty, 'pty) module_ * TypeName.t
    | `Type of 'pty * ('lmod, 'lmodty, 'pty) Resolved_path.parent * TypeName.t
    ]
  (** @canonical Odoc_model.Paths.Path.ClassType.t *)

  type ('lmod, 'lmodty, 'pty, 'lcty, 'lty) type_ =
    [ `Resolved of ('lmod, 'lmodty, 'pty, 'lcty, 'lty) Resolved_path.type_
    | `LocalTy of 'lty
    | `LocalCty of 'lcty
    | `SubstitutedT of ('lmod, 'lmodty, 'pty, 'lcty, 'lty) type_
    | `SubstitutedCT of ('lmod, 'lmodty, 'pty, 'lcty) class_type
    | `Identifier of Identifier.path_type * bool
    | `DotT of ('lmod, 'lmodty, 'pty) module_ * TypeName.t
    | `Type of 'pty * ('lmod, 'lmodty, 'pty) Resolved_path.parent * TypeName.t
    ]
  (** @canonical Odoc_model.Paths.Path.Type.t *)

  type ('lmod, 'lmodty, 'pty, 'lval) value =
    [ `Resolved of ('lmod, 'lmodty, 'pty, 'lval) Resolved_path.value
    | `LocalVal of 'lval
    | `Identifier of Identifier.path_value * bool
    | `DotV of ('lmod, 'lmodty, 'pty) module_ * ValueName.t ]
  (** @canonical Odoc_model.Paths.Path.Value.t *)

  type ('lmod, 'lmodty, 'pty, 'lcty, 'lty, 'lval) any =
    [ `Resolved of ('lmod, 'lmodty, 'pty, 'lcty, 'lty, 'lval) Resolved_path.any
    | `SubstitutedT of ('lmod, 'lmodty, 'pty, 'lcty, 'lty) type_
    | `SubstitutedMT of ('lmod, 'lmodty, 'pty) module_type
    | `Substituted of ('lmod, 'lmodty, 'pty) module_
    | `SubstitutedCT of ('lmod, 'lmodty, 'pty, 'lcty) class_type
    | `Identifier of Identifier.path_any * bool
    | `Root of ModuleName.t
    | `Forward of string
    | `Dot of ('lmod, 'lmodty, 'pty) module_ * ModuleName.t
    | `DotT of ('lmod, 'lmodty, 'pty) module_ * TypeName.t
    | `DotMT of ('lmod, 'lmodty, 'pty) module_ * ModuleTypeName.t
    | `DotV of ('lmod, 'lmodty, 'pty) module_ * ValueName.t
    | `Apply of ('lmod, 'lmodty, 'pty) module_ * ('lmod, 'lmodty, 'pty) module_
    | `Type of 'pty * ('lmod, 'lmodty, 'pty) Resolved_path.parent * TypeName.t
    | `ModuleType of
      'pty * ('lmod, 'lmodty, 'pty) Resolved_path.parent * ModuleTypeName.t
    | `Module of
      'pty * ('lmod, 'lmodty, 'pty) Resolved_path.parent * ModuleName.t
    | `LocalMod of 'lmod
    | `LocalModTy of 'lmodty
    | `LocalVal of 'lval
    | `LocalTy of 'lty
    | `LocalCty of 'lcty ]
  (** @canonical Odoc_model.Paths.Path.t *)
end =
  Path

and Resolved_path : sig
  type ('lmod, 'lmodty, 'pty) parent =
    [ `Module of ('lmod, 'lmodty, 'pty) module_
    | `ModuleType of ('lmod, 'lmodty, 'pty) module_type * 'pty
    | `FragmentRoot of 'pty ]

  and ('lmod, 'lmodty, 'pty) module_ =
    [ `Identifier of Identifier.path_module
    | `LocalMod of 'lmod
    | `Subst of
      ('lmod, 'lmodty, 'pty) module_type * ('lmod, 'lmodty, 'pty) module_
    | `Substituted of ('lmod, 'lmodty, 'pty) module_
    | `Hidden of ('lmod, 'lmodty, 'pty) module_
    | `Module of ('lmod, 'lmodty, 'pty) parent * ModuleName.t
    | `Canonical of
      ('lmod, 'lmodty, 'pty) module_ * ('lmod, 'lmodty, 'pty) Path.module_
      (** [`Canonical (mod, canonical)] *)
    | `Apply of ('lmod, 'lmodty, 'pty) module_ * ('lmod, 'lmodty, 'pty) module_
      (** [`Apply (functor, argument)] *)
    | `Alias of
      ('lmod, 'lmodty, 'pty) module_
      * ('lmod, 'lmodty, 'pty) Path.module_
      * ('lmod, 'lmodty, 'pty) module_ option
      (** Resolved dest *)
    | `OpaqueModule of ('lmod, 'lmodty, 'pty) module_ ]
  (** @canonical Odoc_model.Paths.Path.Resolved.Module.t *)

  and ('lmod, 'lmodty, 'pty) module_type =
    [ `Identifier of Identifier.path_module_type
    | `LocalModTy of 'lmodty
    | `SubstT of
      ('lmod, 'lmodty, 'pty) module_type * ('lmod, 'lmodty, 'pty) module_type
    | `SubstitutedMT of ('lmod, 'lmodty, 'pty) module_type
    | `CanonicalModuleType of
      ('lmod, 'lmodty, 'pty) module_type
      * ('lmod, 'lmodty, 'pty) Path.module_type
    | `AliasModuleType of
      ('lmod, 'lmodty, 'pty) module_type * ('lmod, 'lmodty, 'pty) module_type
    | `ModuleType of ('lmod, 'lmodty, 'pty) parent * ModuleTypeName.t
    | `OpaqueModuleType of ('lmod, 'lmodty, 'pty) module_type ]
  (** @canonical Odoc_model.Paths.Path.Resolved.ModuleType.t *)

  type ('lmod, 'lmodty, 'pty, 'lval) value =
    [ `Identifier of Identifier.path_value
    | `LocalVal of 'lval
    | `Value of ('lmod, 'lmodty, 'pty) parent * ValueName.t ]
  (** @canonical Odoc_model.Paths.Path.Resolved.Value.t *)

  type ('lmod, 'lmodty, 'pty, 'lcty) class_type =
    [ `Identifier of Identifier.path_class_type
    | `LocalCty of 'lcty
    | `SubstitutedCT of ('lmod, 'lmodty, 'pty, 'lcty) class_type
    | `Class of ('lmod, 'lmodty, 'pty) parent * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) parent * TypeName.t ]

  type ('lmod, 'lmodty, 'pty, 'lcty, 'lty) type_ =
    [ `Identifier of Identifier.path_type
    | `LocalTy of 'lty
    | `LocalCty of 'lcty
    | `SubstitutedT of ('lmod, 'lmodty, 'pty, 'lcty, 'lty) type_
    | `SubstitutedCT of ('lmod, 'lmodty, 'pty, 'lcty) class_type
    | `CanonicalType of
      ('lmod, 'lmodty, 'pty, 'lcty, 'lty) type_
      * ('lmod, 'lmodty, 'pty, 'lcty, 'lty) Path.type_
    | `Type of ('lmod, 'lmodty, 'pty) parent * TypeName.t
    | `Class of ('lmod, 'lmodty, 'pty) parent * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) parent * TypeName.t ]
  (** @canonical Odoc_model.Paths.Path.Resolved.Type.t *)

  type ('lmod, 'lmodty, 'pty, 'lcty, 'lty, 'lval) any =
    [ `Identifier of Identifier.any
    | `SubstitutedCT of ('lmod, 'lmodty, 'pty, 'lcty) class_type
    | `SubstitutedT of ('lmod, 'lmodty, 'pty, 'lcty, 'lty) type_
    | `SubstitutedMT of ('lmod, 'lmodty, 'pty) module_type
    | `Substituted of ('lmod, 'lmodty, 'pty) module_
    | `Subst of
      ('lmod, 'lmodty, 'pty) module_type * ('lmod, 'lmodty, 'pty) module_
    | `Hidden of ('lmod, 'lmodty, 'pty) module_
    | `Module of ('lmod, 'lmodty, 'pty) parent * ModuleName.t
    | `Canonical of
      ('lmod, 'lmodty, 'pty) module_ * ('lmod, 'lmodty, 'pty) Path.module_
    | `Apply of ('lmod, 'lmodty, 'pty) module_ * ('lmod, 'lmodty, 'pty) module_
    | `Alias of
      ('lmod, 'lmodty, 'pty) module_
      * ('lmod, 'lmodty, 'pty) Path.module_
      * ('lmod, 'lmodty, 'pty) module_ option
    | `AliasModuleType of
      ('lmod, 'lmodty, 'pty) module_type * ('lmod, 'lmodty, 'pty) module_type
    | `OpaqueModule of ('lmod, 'lmodty, 'pty) module_
    | `ModuleType of ('lmod, 'lmodty, 'pty) parent * ModuleTypeName.t
    | `CanonicalModuleType of
      ('lmod, 'lmodty, 'pty) module_type
      * ('lmod, 'lmodty, 'pty) Path.module_type
    | `SubstT of
      ('lmod, 'lmodty, 'pty) module_type * ('lmod, 'lmodty, 'pty) module_type
    | `OpaqueModuleType of ('lmod, 'lmodty, 'pty) module_type
    | `CanonicalType of
      ('lmod, 'lmodty, 'pty, 'lcty, 'lty) type_
      * ('lmod, 'lmodty, 'pty, 'lcty, 'lty) Path.type_
    | `Type of ('lmod, 'lmodty, 'pty) parent * TypeName.t
    | `Class of ('lmod, 'lmodty, 'pty) parent * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) parent * TypeName.t
    | `Class of ('lmod, 'lmodty, 'pty) parent * TypeName.t
    | `Value of ('lmod, 'lmodty, 'pty) parent * ValueName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) parent * TypeName.t
    | `LocalMod of 'lmod
    | `LocalModTy of 'lmodty
    | `LocalVal of 'lval
    | `LocalTy of 'lty
    | `LocalCty of 'lcty ]
  (** @canonical Odoc_model.Paths.Path.Resolved.t *)
end =
  Resolved_path

module rec Fragment : sig
  type ('lmod, 'lmodty, 'pty) signature =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_fragment.signature
    | `Dot of ('lmod, 'lmodty, 'pty) signature * string
    | `Root ]
  (** @canonical Odoc_model.Paths.Fragment.Signature.t *)

  type ('lmod, 'lmodty, 'pty) module_ =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_fragment.module_
    | `Dot of ('lmod, 'lmodty, 'pty) signature * string ]
  (** @canonical Odoc_model.Paths.Fragment.Module.t *)

  type ('lmod, 'lmodty, 'pty) module_type =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_fragment.module_type
    | `Dot of ('lmod, 'lmodty, 'pty) signature * string ]
  (** @canonical Odoc_model.Paths.Fragment.ModuleType.t *)

  type ('lmod, 'lmodty, 'pty) type_ =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_fragment.type_
    | `Dot of ('lmod, 'lmodty, 'pty) signature * string ]
  (** @canonical Odoc_model.Paths.Fragment.Type.t *)

  type ('lmod, 'lmodty, 'pty) leaf =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_fragment.leaf
    | `Dot of ('lmod, 'lmodty, 'pty) signature * string ]
  (** @canonical Odoc_model.Paths.Fragment.leaf *)

  type ('lmod, 'lmodty, 'pty) any =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_fragment.any
    | `Dot of ('lmod, 'lmodty, 'pty) signature * string
    | `Root ]
  (** @canonical Odoc_model.Paths.Fragment.t *)
end =
  Fragment

and Resolved_fragment : sig
  type ('lmod, 'lmodty, 'pty) root =
    [ `ModuleType of ('lmod, 'lmodty, 'pty) Resolved_path.module_type
    | `Module of ('lmod, 'lmodty, 'pty) Resolved_path.module_ ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.root *)

  type ('lmod, 'lmodty, 'pty) signature =
    [ `Root of ('lmod, 'lmodty, 'pty) root
    | `Subst of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_type
      * ('lmod, 'lmodty, 'pty) module_
    | `Alias of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_
      * ('lmod, 'lmodty, 'pty) module_
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t
    | `OpaqueModule of ('lmod, 'lmodty, 'pty) module_ ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.Signature.t *)

  and ('lmod, 'lmodty, 'pty) module_ =
    [ `Subst of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_type
      * ('lmod, 'lmodty, 'pty) module_
    | `Alias of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_
      * ('lmod, 'lmodty, 'pty) module_
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t
    | `OpaqueModule of ('lmod, 'lmodty, 'pty) module_ ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.Module.t *)

  type ('lmod, 'lmodty, 'pty) type_ =
    [ `Type of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.Type.t *)

  and ('lmod, 'lmodty, 'pty) module_type =
    [ `Module_type of ('lmod, 'lmodty, 'pty) signature * ModuleTypeName.t ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.ModuleType.t *)

  type ('lmod, 'lmodty, 'pty) leaf =
    [ ('lmod, 'lmodty, 'pty) module_
    | ('lmod, 'lmodty, 'pty) module_type
    | ('lmod, 'lmodty, 'pty) type_ ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.leaf *)

  (* Absence of `Root here might make coersions annoying *)
  type ('lmod, 'lmodty, 'pty) any =
    [ `Root of ('lmod, 'lmodty, 'pty) root
    | `Subst of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_type
      * ('lmod, 'lmodty, 'pty) module_
    | `Alias of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_
      * ('lmod, 'lmodty, 'pty) module_
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t
    | `Module_type of ('lmod, 'lmodty, 'pty) signature * ModuleTypeName.t
    | `Type of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `OpaqueModule of ('lmod, 'lmodty, 'pty) module_ ]
  (** @canonical Odoc_model.Paths.Fragment.Resolved.t *)
end =
  Resolved_fragment

module rec Reference : sig
  type tag_only_module = [ `TModule ]

  type tag_only_module_type = [ `TModuleType ]

  type tag_only_type = [ `TType ]

  type tag_only_constructor = [ `TConstructor ]

  type tag_only_field = [ `TField ]

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

  type tag_any =
    [ `TModule
    | `TModuleType
    | `TType
    | `TConstructor
    | `TField
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
    | `TChildPage
    | `TChildModule
    | `TUnknown ]

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

  type ('lmod, 'lmodty, 'pty) signature =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.signature
    | `Root of string * tag_signature
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t
    | `ModuleType of ('lmod, 'lmodty, 'pty) signature * ModuleTypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Signature.t *)

  and ('lmod, 'lmodty, 'pty) class_signature =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.class_signature
    | `Root of string * tag_class_signature
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.ClassSignature.t *)

  and ('lmod, 'lmodty, 'pty) datatype =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.datatype
    | `Root of string * tag_datatype
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Type of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.DataType.t *)

  (* Parent of fields and constructor. Can be either a type or [signature] *)
  and ('lmod, 'lmodty, 'pty) fragment_type_parent =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.field_parent
    | `Root of string * tag_parent
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t
    | `ModuleType of ('lmod, 'lmodty, 'pty) signature * ModuleTypeName.t
    | `Type of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.FragmentTypeParent.t *)

  and ('lmod, 'lmodty, 'pty) label_parent =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.label_parent
    | `Root of string * tag_label_parent
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t
    | `ModuleType of ('lmod, 'lmodty, 'pty) signature * ModuleTypeName.t
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `Type of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.LabelParent.t *)

  type ('lmod, 'lmodty, 'pty) module_ =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.module_
    | `Root of string * [ `TModule | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t ]
  (** @canonical Odoc_model.Paths.Reference.Module.t *)

  type ('lmod, 'lmodty, 'pty) module_type =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.module_type
    | `Root of string * [ `TModuleType | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `ModuleType of ('lmod, 'lmodty, 'pty) signature * ModuleTypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.ModuleType.t *)

  type ('lmod, 'lmodty, 'pty) type_ =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.type_
    | `Root of string * [ `TType | `TClass | `TClassType | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `Type of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Type.t *)

  type ('lmod, 'lmodty, 'pty) constructor =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.constructor
    | `Root of string * [ `TConstructor | `TExtension | `TException | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Constructor of
      ('lmod, 'lmodty, 'pty) fragment_type_parent * ConstructorName.t
    | `Extension of ('lmod, 'lmodty, 'pty) signature * ExtensionName.t
    | `Exception of ('lmod, 'lmodty, 'pty) signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Reference.Constructor.t *)

  type ('lmod, 'lmodty, 'pty) field =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.field
    | `Root of string * [ `TField | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Field of ('lmod, 'lmodty, 'pty) fragment_type_parent * FieldName.t ]
  (** @canonical Odoc_model.Paths.Reference.Field.t *)

  type ('lmod, 'lmodty, 'pty) extension =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.extension
    | `Root of string * [ `TExtension | `TException | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Extension of ('lmod, 'lmodty, 'pty) signature * ExtensionName.t
    | `Exception of ('lmod, 'lmodty, 'pty) signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Reference.Extension.t *)

  type ('lmod, 'lmodty, 'pty) extension_decl =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.extension_decl
    | `Root of string * [ `TExtension | `TException | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `ExtensionDecl of ('lmod, 'lmodty, 'pty) signature * ExtensionName.t ]
  (** @canonical Odoc_model.Paths.Reference.ExtensionDecl.t *)

  type ('lmod, 'lmodty, 'pty) exception_ =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.exception_
    | `Root of string * [ `TException | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Exception of ('lmod, 'lmodty, 'pty) signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Reference.Exception.t *)

  type ('lmod, 'lmodty, 'pty) value =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.value
    | `Root of string * [ `TValue | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Value of ('lmod, 'lmodty, 'pty) signature * ValueName.t ]
  (** @canonical Odoc_model.Paths.Reference.Value.t *)

  type ('lmod, 'lmodty, 'pty) class_ =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.class_
    | `Root of string * [ `TClass | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Class.t *)

  type ('lmod, 'lmodty, 'pty) class_type =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.class_type
    | `Root of string * [ `TClass | `TClassType | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.ClassType.t *)

  type ('lmod, 'lmodty, 'pty) method_ =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.method_
    | `Root of string * [ `TMethod | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Method of ('lmod, 'lmodty, 'pty) class_signature * MethodName.t ]
  (** @canonical Odoc_model.Paths.Reference.Method.t *)

  type ('lmod, 'lmodty, 'pty) instance_variable =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.instance_variable
    | `Root of string * [ `TInstanceVariable | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `InstanceVariable of
      ('lmod, 'lmodty, 'pty) class_signature * InstanceVariableName.t ]
  (** @canonical Odoc_model.Paths.Reference.InstanceVariable.t *)

  type ('lmod, 'lmodty, 'pty) label =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.label
    | `Root of string * [ `TLabel | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Label of ('lmod, 'lmodty, 'pty) label_parent * LabelName.t ]
  (** @canonical Odoc_model.Paths.Reference.Label.t *)

  type ('lmod, 'lmodty, 'pty) page =
    [ `Resolved of Resolved_reference.page
    | `Root of string * [ `TPage | `TUnknown ]
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string ]
  (** @canonical Odoc_model.Paths.Reference.Page.t *)

  type ('lmod, 'lmodty, 'pty) any =
    [ `Resolved of ('lmod, 'lmodty, 'pty) Resolved_reference.any
    | `Root of string * tag_any
    | `Dot of ('lmod, 'lmodty, 'pty) label_parent * string
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t
    | `ModuleType of ('lmod, 'lmodty, 'pty) signature * ModuleTypeName.t
    | `Type of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `Constructor of
      ('lmod, 'lmodty, 'pty) fragment_type_parent * ConstructorName.t
    | `Field of ('lmod, 'lmodty, 'pty) fragment_type_parent * FieldName.t
    | `Extension of ('lmod, 'lmodty, 'pty) signature * ExtensionName.t
    | `ExtensionDecl of ('lmod, 'lmodty, 'pty) signature * ExtensionName.t
    | `Exception of ('lmod, 'lmodty, 'pty) signature * ExceptionName.t
    | `Value of ('lmod, 'lmodty, 'pty) signature * ValueName.t
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `Method of ('lmod, 'lmodty, 'pty) class_signature * MethodName.t
    | `InstanceVariable of
      ('lmod, 'lmodty, 'pty) class_signature * InstanceVariableName.t
    | `Label of ('lmod, 'lmodty, 'pty) label_parent * LabelName.t ]
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
  type ('lmod, 'lmodty, 'pty) datatype =
    [ `Identifier of Identifier.datatype
    | `Type of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.DataType.t *)

  and ('lmod, 'lmodty, 'pty) module_ =
    [ `Identifier of Identifier.path_module
    | `Hidden of ('lmod, 'lmodty, 'pty) module_
    | `Alias of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_
      * ('lmod, 'lmodty, 'pty) module_
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Module.t *)

  (* Signature is [ module | moduletype ] *)
  and ('lmod, 'lmodty, 'pty) signature =
    [ `Identifier of Identifier.signature
    | `Hidden of ('lmod, 'lmodty, 'pty) module_
    | `Alias of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_
      * ('lmod, 'lmodty, 'pty) module_
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t
    | `ModuleType of ('lmod, 'lmodty, 'pty) signature * ModuleTypeName.t
    | `AliasModuleType of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_type
      * ('lmod, 'lmodty, 'pty) module_type ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Signature.t *)

  and ('lmod, 'lmodty, 'pty) class_signature =
    [ `Identifier of Identifier.class_signature
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.ClassSignature.t *)

  (* fragment_type_parent in resolved references is for record fields parent.
     It’s type (for usual record fields) or [signature] for fields of inline
     records of extension constructor. *)
  and ('lmod, 'lmodty, 'pty) field_parent =
    [ `Identifier of Identifier.field_parent
    | `Alias of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_
      * ('lmod, 'lmodty, 'pty) module_
    | `AliasModuleType of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_type
      * ('lmod, 'lmodty, 'pty) module_type
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t
    | `Hidden of ('lmod, 'lmodty, 'pty) module_
    | `ModuleType of ('lmod, 'lmodty, 'pty) signature * ModuleTypeName.t
    | `Type of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.FragmentTypeParent.t *)

  (* The only difference between parent and label_parent
     is that the Identifier allows more types *)
  and ('lmod, 'lmodty, 'pty) label_parent =
    [ `Identifier of Identifier.label_parent
    | `Alias of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_
      * ('lmod, 'lmodty, 'pty) module_
    | `AliasModuleType of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_type
      * ('lmod, 'lmodty, 'pty) module_type
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t
    | `Hidden of ('lmod, 'lmodty, 'pty) module_
    | `ModuleType of ('lmod, 'lmodty, 'pty) signature * ModuleTypeName.t
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `Type of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.LabelParent.t *)

  and ('lmod, 'lmodty, 'pty) module_type =
    [ `Identifier of Identifier.reference_module_type
    | `ModuleType of ('lmod, 'lmodty, 'pty) signature * ModuleTypeName.t
    | `AliasModuleType of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_type
      * ('lmod, 'lmodty, 'pty) module_type ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.ModuleType.t *)

  type ('lmod, 'lmodty, 'pty) type_ =
    [ `Identifier of Identifier.reference_type
    | `Type of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Type.t *)

  type ('lmod, 'lmodty, 'pty) constructor =
    [ `Identifier of Identifier.reference_constructor
    | `Constructor of ('lmod, 'lmodty, 'pty) datatype * ConstructorName.t
    | `Extension of ('lmod, 'lmodty, 'pty) signature * ExtensionName.t
    | `Exception of ('lmod, 'lmodty, 'pty) signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Constructor.t *)

  type ('lmod, 'lmodty, 'pty) field =
    [ `Identifier of Identifier.reference_field
    | `Field of ('lmod, 'lmodty, 'pty) field_parent * FieldName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Field.t *)

  type ('lmod, 'lmodty, 'pty) extension =
    [ `Identifier of Identifier.reference_extension
    | `Extension of ('lmod, 'lmodty, 'pty) signature * ExtensionName.t
    | `Exception of ('lmod, 'lmodty, 'pty) signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Extension.t *)

  type ('lmod, 'lmodty, 'pty) extension_decl =
    [ `Identifier of Identifier.reference_extension_decl
    | `ExtensionDecl of
      ('lmod, 'lmodty, 'pty) signature
      * ExtensionName.t
        (* The extension_name used in the url.
           It is the extension_name of the first constructor of the extension (there is always at least 1). *)
      * ExtensionName.t (* displayed *) ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Extension.t *)

  type ('lmod, 'lmodty, 'pty) exception_ =
    [ `Identifier of Identifier.reference_exception
    | `Exception of ('lmod, 'lmodty, 'pty) signature * ExceptionName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Exception.t *)

  type ('lmod, 'lmodty, 'pty) value =
    [ `Identifier of Identifier.reference_value
    | `Value of ('lmod, 'lmodty, 'pty) signature * ValueName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Value.t *)

  type ('lmod, 'lmodty, 'pty) class_ =
    [ `Identifier of Identifier.reference_class
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Class.t *)

  type ('lmod, 'lmodty, 'pty) class_type =
    [ `Identifier of Identifier.reference_class_type
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) signature * TypeName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.ClassType.t *)

  type ('lmod, 'lmodty, 'pty) method_ =
    [ `Identifier of Identifier.reference_method
    | `Method of ('lmod, 'lmodty, 'pty) class_signature * MethodName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Method.t *)

  type ('lmod, 'lmodty, 'pty) instance_variable =
    [ `Identifier of Identifier.reference_instance_variable
    | `InstanceVariable of
      ('lmod, 'lmodty, 'pty) class_signature * InstanceVariableName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.InstanceVariable.t *)

  type ('lmod, 'lmodty, 'pty) label =
    [ `Identifier of Identifier.reference_label
    | `Label of ('lmod, 'lmodty, 'pty) label_parent * LabelName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Label.t *)

  type page = [ `Identifier of Identifier.reference_page ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.Page.t *)

  type ('lmod, 'lmodty, 'pty) any =
    [ `Identifier of Identifier.any
    | `Alias of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_
      * ('lmod, 'lmodty, 'pty) module_
    | `AliasModuleType of
      ('lmod, 'lmodty, 'pty) Resolved_path.module_type
      * ('lmod, 'lmodty, 'pty) module_type
    | `Module of ('lmod, 'lmodty, 'pty) signature * ModuleName.t
    | `Hidden of ('lmod, 'lmodty, 'pty) module_
    | `ModuleType of ('lmod, 'lmodty, 'pty) signature * ModuleTypeName.t
    | `Type of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `Constructor of ('lmod, 'lmodty, 'pty) datatype * ConstructorName.t
    | `Field of ('lmod, 'lmodty, 'pty) field_parent * FieldName.t
    | `Extension of ('lmod, 'lmodty, 'pty) signature * ExtensionName.t
    | `ExtensionDecl of
      ('lmod, 'lmodty, 'pty) signature * ExtensionName.t * ExtensionName.t
    | `Exception of ('lmod, 'lmodty, 'pty) signature * ExceptionName.t
    | `Value of ('lmod, 'lmodty, 'pty) signature * ValueName.t
    | `Class of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `ClassType of ('lmod, 'lmodty, 'pty) signature * TypeName.t
    | `Method of ('lmod, 'lmodty, 'pty) class_signature * MethodName.t
    | `InstanceVariable of
      ('lmod, 'lmodty, 'pty) class_signature * InstanceVariableName.t
    | `Label of ('lmod, 'lmodty, 'pty) label_parent * LabelName.t ]
  (** @canonical Odoc_model.Paths.Reference.Resolved.t *)
end =
  Resolved_reference
