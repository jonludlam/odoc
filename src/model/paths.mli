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

(** Identifiers for definitions *)

type 'a id = 'a Paths_types.id = { iv : 'a; ihash : int; ikey : string }

module Identifier : sig
  (** {2 Generic operations} *)

  module Any : sig
    type t = Paths_types.Identifier.any

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Signature : sig
    type t = Paths_types.Identifier.signature

    type t_pv = Paths_types.Identifier.signature_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module ClassSignature : sig
    type t = Paths_types.Identifier.class_signature

    type t_pv = Paths_types.Identifier.class_signature_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module DataType : sig
    type t = Paths_types.Identifier.datatype

    type t_pv = Paths_types.Identifier.datatype_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Parent : sig
    type t = Paths_types.Identifier.parent

    type t_pv = Paths_types.Identifier.parent_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module LabelParent : sig
    type t = Paths_types.Identifier.label_parent

    type t_pv = Paths_types.Identifier.label_parent_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module RootModule : sig
    type t = Paths_types.Identifier.root_module

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Module : sig
    type t = Paths_types.Identifier.module_

    type t_pv = Paths_types.Identifier.module_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module FunctorParameter : sig
    type t = Paths_types.Identifier.functor_parameter

    type t_pv = Paths_types.Identifier.functor_parameter_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module FunctorResult : sig
    type t = Paths_types.Identifier.functor_result

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module ModuleType : sig
    type t = Paths_types.Identifier.module_type

    type t_pv = Paths_types.Identifier.module_type_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Type : sig
    type t = Paths_types.Identifier.type_

    type t_pv = Paths_types.Identifier.type_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Constructor : sig
    type t = Paths_types.Identifier.constructor

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Field : sig
    type t = Paths_types.Identifier.field

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Extension : sig
    type t = Paths_types.Identifier.extension

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Exception : sig
    type t = Paths_types.Identifier.exception_

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Value : sig
    type t = Paths_types.Identifier.value

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Class : sig
    type t = Paths_types.Identifier.class_

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module ClassType : sig
    type t = Paths_types.Identifier.class_type

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Method : sig
    type t = Paths_types.Identifier.method_

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module InstanceVariable : sig
    type t = Paths_types.Identifier.instance_variable

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Label : sig
    type t = Paths_types.Identifier.label

    type t_pv = Paths_types.Identifier.label_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Page : sig
    type t = Paths_types.Identifier.page

    type t_pv = Paths_types.Identifier.page_pv

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module ContainerPage : sig
    type t = Paths_types.Identifier.container_page

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module OdocId : sig
    type t = Paths_types.Identifier.odoc_id

    val equal : t -> t -> bool

    val hash : t -> int

    val compare : t -> t -> int
  end

  module Path : sig
    module Module : sig
      type t = Paths_types.Identifier.path_module

      type t_pv = Paths_types.Identifier.path_module_pv

      val equal : t -> t -> bool

      val hash : t -> int

      val compare : t -> t -> int
    end

    module ModuleType : sig
      type t = Paths_types.Identifier.path_module_type

      val equal : t -> t -> bool

      val hash : t -> int

      val compare : t -> t -> int
    end

    module Type : sig
      type t = Paths_types.Identifier.path_type

      type t_pv = Paths_types.Identifier.path_type_pv

      val equal : t -> t -> bool

      val hash : t -> int

      val compare : t -> t -> int
    end

    module ClassType : sig
      type t = Paths_types.Identifier.path_class_type

      type t_pv = Paths_types.Identifier.path_class_type_pv

      val equal : t -> t -> bool

      val hash : t -> int

      val compare : t -> t -> int
    end

    type t = Paths_types.Identifier.path_any
  end

  type t = Paths_types.Identifier.any

  type t_pv = Paths_types.Identifier.any_pv

  val hash : t -> int

  val name : [< t_pv ] id -> string

  val compare : t -> t -> int

  val equal : ([< t_pv ] id as 'a) -> 'a -> bool

  val label_parent : [< t_pv ] id -> LabelParent.t

  module Maps : sig
    module Any : Map.S with type key = Any.t

    module FunctorParameter : Map.S with type key = FunctorParameter.t

    module Module : Map.S with type key = Module.t

    module ModuleType : Map.S with type key = ModuleType.t

    module Type : Map.S with type key = Type.t

    module Class : Map.S with type key = Class.t

    module ClassType : Map.S with type key = ClassType.t

    module Label : Map.S with type key = Label.t

    module Path : sig
      module Type : Map.S with type key = Path.Type.t

      module ClassType : Map.S with type key = Path.ClassType.t
    end
  end

  module Mk : sig
    open Names

    val page :
      ContainerPage.t option * PageName.t ->
      [> `Page of ContainerPage.t option * PageName.t ] id

    val leaf_page :
      ContainerPage.t option * PageName.t ->
      [> `LeafPage of ContainerPage.t option * PageName.t ] id

    val root :
      ContainerPage.t option * ModuleName.t ->
      [> `Root of ContainerPage.t option * ModuleName.t ] id

    val module_ :
      Signature.t * ModuleName.t ->
      [> `Module of Signature.t * ModuleName.t ] id

    val parameter :
      Signature.t * ParameterName.t ->
      [> `Parameter of Signature.t * ParameterName.t ] id

    val result : Signature.t -> [> `Result of Signature.t ] id

    val module_type :
      Signature.t * ModuleTypeName.t ->
      [> `ModuleType of Signature.t * ModuleTypeName.t ] id

    val class_ :
      Signature.t * ClassName.t -> [> `Class of Signature.t * ClassName.t ] id

    val class_type :
      Signature.t * ClassTypeName.t ->
      [> `ClassType of Signature.t * ClassTypeName.t ] id

    val type_ :
      Signature.t * TypeName.t -> [> `Type of Signature.t * TypeName.t ] id

    val core_type : string -> [> `CoreType of TypeName.t ] id

    val constructor :
      Type.t * ConstructorName.t ->
      [> `Constructor of Type.t * ConstructorName.t ] id

    val field :
      Parent.t * FieldName.t -> [> `Field of Parent.t * FieldName.t ] id

    val extension :
      Signature.t * ExtensionName.t ->
      [> `Extension of Signature.t * ExtensionName.t ] id

    val exception_ :
      Signature.t * ExceptionName.t ->
      [> `Exception of Signature.t * ExceptionName.t ] id

    val core_exception : string -> [> `CoreException of ExceptionName.t ] id

    val value :
      Signature.t * ValueName.t -> [> `Value of Signature.t * ValueName.t ] id

    val method_ :
      ClassSignature.t * MethodName.t ->
      [> `Method of ClassSignature.t * MethodName.t ] id

    val instance_variable :
      ClassSignature.t * InstanceVariableName.t ->
      [> `InstanceVariable of ClassSignature.t * InstanceVariableName.t ] id

    val label :
      LabelParent.t * LabelName.t ->
      [> `Label of LabelParent.t * LabelName.t ] id
  end
end

(** Normal OCaml paths (i.e. the ones present in types) *)
module rec Path : sig
  module Resolved : sig
    module Module : sig
      type t = Paths_types.Resolved_path.module_

      val is_hidden : t -> weak_canonical_test:bool -> bool

      (* val identifier : t -> Identifier.Path.Module.t *)

      (* val canonical_ident : t -> Identifier.Path.Module.t option *)

      module Mk : sig
        val identifier : Identifier.Path.Module.t -> t

        val subst : Paths_types.Resolved_path.module_type * t -> t

        val hidden : t -> t

        val module_ : t * Names.ModuleName.t -> t

        val canonical : t * Paths_types.Path.module_ -> t

        val apply : t * t -> t

        val aliasrs : Paths_types.Path.module_ * t -> t

        val aliasrd : t * Paths_types.Path.module_ -> t

        val opaquemodule : t -> t
      end
    end

    module ModuleType : sig
      type t = Paths_types.Resolved_path.module_type

      val is_hidden : t -> weak_canonical_test:bool -> bool

      (* val identifier : t -> Identifier.Path.ModuleType.t *)

      (* val canonical_ident : t -> Identifier.Path.ModuleType.t option *)

      module Mk : sig
        val identifier : Identifier.Path.ModuleType.t -> t

        val module_type : Module.t * Names.ModuleTypeName.t -> t

        val substt : t * t -> t

        val aliasmoduletype : t * t -> t

        val canonicalmoduletype : t * Paths_types.Path.module_type -> t

        val opaquemoduletype : t -> t
      end
    end

    module Type : sig
      type t = Paths_types.Resolved_path.type_

      val is_hidden : t -> bool

      (* val identifier : t -> Identifier.Path.Type.t *)

      (* val canonical_ident : t -> Identifier.Path.Type.t option *)

      module Mk : sig
        val identifier : Identifier.Path.Type.t -> t

        val canonicaltype : t * Paths_types.Path.type_ -> t

        val type_ : Module.t * Names.TypeName.t -> t

        val class_ : Module.t * Names.ClassName.t -> t

        val class_type : Module.t * Names.ClassTypeName.t -> t
      end
    end

    module ClassType : sig
      type t = Paths_types.Resolved_path.class_type

      val is_hidden : t -> bool

      (* val identifier : t -> Identifier.Path.ClassType.t *)

      module Mk : sig
        val identifier : Identifier.Path.ClassType.t -> t

        val class_ : Module.t * Names.ClassName.t -> t

        val class_type : Module.t * Names.ClassTypeName.t -> t
      end
    end

    type t = Paths_types.Resolved_path.any

    type t_unhashed = Paths_types.Resolved_path.any_unhashed

    val identifier : t -> Identifier.t
  end

  module Module : sig
    type t = Paths_types.Path.module_

    module Mk : sig
      val resolved : Resolved.Module.t -> t

      val identifier : Identifier.Path.Module.t * bool -> t

      val forward : string -> t

      val dot : t * string -> t

      val root : string -> t

      val apply : t * t -> t
    end
  end

  module ModuleType : sig
    type t = Paths_types.Path.module_type

    module Mk : sig
      val resolved : Resolved.ModuleType.t -> t

      val identifier : Identifier.Path.ModuleType.t * bool -> t

      val dot : Module.t * string -> t
    end
  end

  module Type : sig
    type t = Paths_types.Path.type_

    module Mk : sig
      val resolved : Resolved.Type.t -> t

      val identifier : Identifier.Path.Type.t * bool -> t

      val dot : Module.t * string -> t
    end
  end

  module ClassType : sig
    type t = Paths_types.Path.class_type

    module Mk : sig
      val resolved : Resolved.ClassType.t -> t

      val identifier : Identifier.Path.ClassType.t * bool -> t

      val dot : Module.t * string -> t
    end
  end

  type t = Paths_types.Path.any

  type t_unhashed = Paths_types.Path.any_unhashed

  val is_hidden : t -> bool
end

(** OCaml path fragments for specifying module substitutions *)
module Fragment : sig
  module Resolved : sig
    module Signature : sig
      type t = Paths_types.Resolved_fragment.signature

      val split : t -> string * t option
    end

    module Module : sig
      type t = Paths_types.Resolved_fragment.module_

      val split : t -> string * t option
    end

    module ModuleType : sig
      type t = Paths_types.Resolved_fragment.module_type

      val split : t -> string * t option
    end

    module Type : sig
      type t = Paths_types.Resolved_fragment.type_

      val split : t -> string * t option
    end

    type leaf = Paths_types.Resolved_fragment.leaf

    type root = Paths_types.Resolved_fragment.root

    type t = Paths_types.Resolved_fragment.any

    val identifier : t -> Identifier.t

    val is_hidden : t -> bool
  end

  module Signature : sig
    type t = Paths_types.Fragment.signature

    val split : t -> string * t option
  end

  module Module : sig
    type t = Paths_types.Fragment.module_

    val split : t -> string * t option
  end

  module ModuleType : sig
    type t = Paths_types.Fragment.module_type

    val split : t -> string * t option
  end

  module Type : sig
    type t = Paths_types.Fragment.type_

    val split : t -> string * t option
  end

  type leaf = Paths_types.Fragment.leaf

  type t = Paths_types.Fragment.any
end

(** References present in documentation comments ([{!Foo.Bar}]) *)
module rec Reference : sig
  module Resolved : sig
    module Signature : sig
      type t = Paths_types.Resolved_reference.signature
    end

    module ClassSignature : sig
      type t = Paths_types.Resolved_reference.class_signature
    end

    module DataType : sig
      type t = Paths_types.Resolved_reference.datatype
    end

    module Parent : sig
      type t = Paths_types.Resolved_reference.parent
    end

    module LabelParent : sig
      type t = Paths_types.Resolved_reference.label_parent
    end

    module Module : sig
      type t = Paths_types.Resolved_reference.module_
    end

    module ModuleType : sig
      type t = Paths_types.Resolved_reference.module_type
    end

    module Type : sig
      type t = Paths_types.Resolved_reference.type_
    end

    module Constructor : sig
      type t = Paths_types.Resolved_reference.constructor
    end

    module Field : sig
      type t = Paths_types.Resolved_reference.field
    end

    module Extension : sig
      type t = Paths_types.Resolved_reference.extension
    end

    module Exception : sig
      type t = Paths_types.Resolved_reference.exception_
    end

    module Value : sig
      type t = Paths_types.Resolved_reference.value
    end

    module Class : sig
      type t = Paths_types.Resolved_reference.class_
    end

    module ClassType : sig
      type t = Paths_types.Resolved_reference.class_type
    end

    module Method : sig
      type t = Paths_types.Resolved_reference.method_
    end

    module InstanceVariable : sig
      type t = Paths_types.Resolved_reference.instance_variable
    end

    module Label : sig
      type t = Paths_types.Resolved_reference.label
    end

    module Page : sig
      type t = Paths_types.Resolved_reference.page
    end

    type t = Paths_types.Resolved_reference.any

    val identifier : t -> Identifier.t
  end

  module Signature : sig
    type t = Paths_types.Reference.signature
  end

  module ClassSignature : sig
    type t = Paths_types.Reference.class_signature
  end

  module DataType : sig
    type t = Paths_types.Reference.datatype
  end

  module Parent : sig
    type t = Paths_types.Reference.parent
  end

  module LabelParent : sig
    type t = Paths_types.Reference.label_parent
  end

  module Module : sig
    type t = Paths_types.Reference.module_
  end

  module ModuleType : sig
    type t = Paths_types.Reference.module_type
  end

  module Type : sig
    type t = Paths_types.Reference.type_
  end

  module Constructor : sig
    type t = Paths_types.Reference.constructor
  end

  module Field : sig
    type t = Paths_types.Reference.field
  end

  module Extension : sig
    type t = Paths_types.Reference.extension
  end

  module Exception : sig
    type t = Paths_types.Reference.exception_
  end

  module Value : sig
    type t = Paths_types.Reference.value
  end

  module Class : sig
    type t = Paths_types.Reference.class_
  end

  module ClassType : sig
    type t = Paths_types.Reference.class_type
  end

  module Method : sig
    type t = Paths_types.Reference.method_
  end

  module InstanceVariable : sig
    type t = Paths_types.Reference.instance_variable
  end

  module Label : sig
    type t = Paths_types.Reference.label
  end

  module Page : sig
    type t = Paths_types.Reference.page
  end

  type t = Paths_types.Reference.any

  type tag_any = Paths_types.Reference.tag_any
end
