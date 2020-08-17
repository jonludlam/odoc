(** Component module *)

module ModuleMap : Map.S with type key = Ident.module_

module TypeMap : Map.S with type key = Ident.type_

module PathModuleMap : Map.S with type key = Ident.path_module
(** Useful maps *)

module ModuleTypeMap : Map.S with type key = Ident.module_type

module PathTypeMap : Map.S with type key = Ident.path_type

module PathClassTypeMap : Map.S with type key = Ident.path_class_type

module IdentMap : Map.S with type key = Ident.any

(** Delayed is a bit like Lazy.t but may in the future offer the chance to peek inside
    to be able to optimize the calculation *)
module Delayed : sig
  val eager : bool ref
  (** If [eager] is true then no delaying is done. Most useful for testing and
        documentation *)

  type 'a t = { mutable v : 'a option; mutable get : (unit -> 'a) option }

  val get : 'a t -> 'a

  val put : (unit -> 'a) -> 'a t

  val put_val : 'a -> 'a t
end

module Opt : sig
  val map : ('a -> 'b) -> 'a option -> 'b option
end

(** {2 Components}
  
    The modules/types here are very similar to those in {!module:Odoc_model.Lang},
    in most cases the only difference being that we have {{!module:Ident}Idents} 
    (which are local) rather than {{!module:Odoc_model.Paths.Identifier}Identifiers}
    (which are global), {{!module:Cpath}Cpaths} instead of {{!module:Odoc_model.Paths.Path}Paths},
    and {{!module:Cfrag}Cfrags} rather than {{!module:Odoc_model.Paths.Fragment}Fragments}.
    All of these are to support the use of local idents.
    
    Note that we still use global {{!module:Odoc_model.Paths.Reference}References}
    rather than local ones - this is because at the point these components are being
    used all the references are unresolved, and hence do not contain any resolved
    global identifiers. When resolving references, we construct paths in parallel,
    which also helps avoid the need for local references.

    These idents of items are kept outside of the types themselves in
    order to help with laziness.

    There are a few other minor differences:
    
    - Signatures keep track of removed items. These items are removed during
      destructive substitution.

*)

module rec Module : sig
  type expansion =
    | AlreadyASig
    | Signature of Signature.t
    | Functor of FunctorParameter.t list * Signature.t

  type decl = Alias of Cpath.module_ | ModuleType of ModuleType.expr

  type t = {
    doc : CComment.docs;
    type_ : decl;
    canonical : (Cpath.module_ * Odoc_model.Paths.Reference.Module.t) option;
    hidden : bool;
    display_type : decl option;
    expansion : expansion option;
  }
end

and ModuleSubstitution : sig
  type t = { doc : CComment.docs; manifest : Cpath.module_ }
end

and TypeExpr : sig
  module Polymorphic_variant : sig
    type kind = Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind

    module Constructor : sig
      type t = {
        name : string;
        constant : bool;
        arguments : TypeExpr.t list;
        doc : CComment.docs;
      }
    end

    type element = Type of TypeExpr.t | Constructor of Constructor.t

    type t = { kind : kind; elements : element list }
  end

  module Object : sig
    type method_ = { name : string; type_ : TypeExpr.t }

    type field = Method of method_ | Inherit of TypeExpr.t

    type t = { fields : field list; open_ : bool }
  end

  module Package : sig
    type substitution = Cfrag.type_ * TypeExpr.t

    type t = { path : Cpath.module_type; substitutions : substitution list }
  end

  type label = Odoc_model.Lang.TypeExpr.label

  type t =
    | Var of string
    | Any
    | Alias of t * string
    | Arrow of label option * t * t
    | Tuple of t list
    | Constr of Cpath.type_ * t list
    | Polymorphic_variant of TypeExpr.Polymorphic_variant.t
    | Object of TypeExpr.Object.t
    | Class of Cpath.class_type * t list
    | Poly of string list * t
    | Package of TypeExpr.Package.t
end

and Extension : sig
  module Constructor : sig
    type t = {
      name : string;
      doc : CComment.docs;
      args : TypeDecl.Constructor.argument;
      res : TypeExpr.t option;
    }
  end

  type t = {
    type_path : Cpath.type_;
    doc : CComment.docs;
    type_params : TypeDecl.param list;
    private_ : bool;
    constructors : Constructor.t list;
  }
end

and Exception : sig
  type t = {
    doc : CComment.docs;
    args : TypeDecl.Constructor.argument;
    res : TypeExpr.t option;
  }
end

and FunctorParameter : sig
  type parameter = {
    id : Ident.functor_parameter;
    expr : ModuleType.expr;
    expansion : Module.expansion option;
  }

  type t = Named of parameter | Unit
end

and ModuleType : sig
  type substitution =
    | ModuleEq of Cfrag.module_ * Module.decl
    | ModuleSubst of Cfrag.module_ * Cpath.module_
    | TypeEq of Cfrag.type_ * TypeDecl.Equation.t
    | TypeSubst of Cfrag.type_ * TypeDecl.Equation.t

  type type_of_desc =
    | MPath of Cpath.module_
    | Struct_include of Cpath.module_

  type expr =
    | Path of Cpath.module_type
    | Signature of Signature.t
    | With of expr * substitution list
    | Functor of FunctorParameter.t * expr
    | TypeOf of type_of_desc

  type t = {
    doc : CComment.docs;
    expr : expr option;
    expansion : Module.expansion option;
  }
end

and TypeDecl : sig
  module Field : sig
    type t = {
      name : string;
      doc : CComment.docs;
      mutable_ : bool;
      type_ : TypeExpr.t;
    }
  end

  module Constructor : sig
    type argument = Tuple of TypeExpr.t list | Record of Field.t list

    type t = {
      name : string;
      doc : CComment.docs;
      args : argument;
      res : TypeExpr.t option;
    }
  end

  module Representation : sig
    type t =
      | Variant of Constructor.t list
      | Record of Field.t list
      | Extensible
  end

  type param = Odoc_model.Lang.TypeDecl.param

  module Equation : sig
    type t = {
      params : param list;
      private_ : bool;
      manifest : TypeExpr.t option;
      constraints : (TypeExpr.t * TypeExpr.t) list;
    }
  end

  type t = {
    doc : CComment.docs;
    equation : Equation.t;
    representation : Representation.t option;
  }
end

and Value : sig
  type t = { doc : CComment.docs; type_ : TypeExpr.t }
end

and Signature : sig
  type recursive = Odoc_model.Lang.Signature.recursive

  type item =
    | Module of Ident.module_ * recursive * Module.t Delayed.t
    | ModuleSubstitution of Ident.module_ * ModuleSubstitution.t
    | ModuleType of Ident.module_type * ModuleType.t Delayed.t
    | Type of Ident.type_ * recursive * TypeDecl.t Delayed.t
    | TypeSubstitution of Ident.type_ * TypeDecl.t
    | Exception of Ident.exception_ * Exception.t
    | TypExt of Extension.t
    | Value of Ident.value * Value.t Delayed.t
    | External of Ident.value * External.t
    | Class of Ident.class_ * recursive * Class.t
    | ClassType of Ident.class_type * recursive * ClassType.t
    | Include of Include.t
    | Open of Open.t
    | Comment of CComment.docs_or_stop

  (* When doing destructive substitution we keep track of the items that have been removed,
       and the path they've been substituted with *)
  type removed_item =
    | RModule of Ident.module_ * Cpath.Resolved.module_
    | RType of Ident.type_ * TypeExpr.t

  type t = { items : item list; removed : removed_item list }
end

and Open : sig
  type t = { expansion : Signature.t }
end

and Include : sig
  type t = {
    parent : Odoc_model.Paths.Identifier.Signature.t;
    doc : CComment.docs;
    shadowed : Odoc_model.Lang.Include.shadowed;
    expansion_ : Signature.t;
    decl : Module.decl;
  }
end

and External : sig
  type t = { doc : CComment.docs; type_ : TypeExpr.t; primitives : string list }
end

and Class : sig
  type decl =
    | ClassType of ClassType.expr
    | Arrow of TypeExpr.label option * TypeExpr.t * decl

  type t = {
    doc : CComment.docs;
    virtual_ : bool;
    params : TypeDecl.param list;
    type_ : decl;
    expansion : ClassSignature.t option;
  }
end

and ClassType : sig
  type expr =
    | Constr of Cpath.class_type * TypeExpr.t list
    | Signature of ClassSignature.t

  type t = {
    doc : CComment.docs;
    virtual_ : bool;
    params : TypeDecl.param list;
    expr : expr;
    expansion : ClassSignature.t option;
  }
end

and ClassSignature : sig
  type item =
    | Method of Ident.method_ * Method.t
    | InstanceVariable of Ident.instance_variable * InstanceVariable.t
    | Constraint of TypeExpr.t * TypeExpr.t
    | Inherit of ClassType.expr
    | Comment of CComment.docs_or_stop

  type t = { self : TypeExpr.t option; items : item list }
end

and Method : sig
  type t = {
    doc : CComment.docs;
    private_ : bool;
    virtual_ : bool;
    type_ : TypeExpr.t;
  }
end

and InstanceVariable : sig
  type t = {
    doc : CComment.docs;
    mutable_ : bool;
    virtual_ : bool;
    type_ : TypeExpr.t;
  }
end

and Substitution : sig
  type subst_module =
    [ `Prefixed of Cpath.module_ * Cpath.Resolved.module_
    | `Substituted
    | `Renamed of Ident.path_module ]

  type subst_module_type =
    [ `Prefixed of Cpath.module_type * Cpath.Resolved.module_type
    | `Renamed of Ident.module_type ]

  type subst_type =
    [ `Prefixed of Cpath.type_ * Cpath.Resolved.type_
    | `Renamed of Ident.path_type ]

  type subst_class_type =
    [ `Prefixed of Cpath.class_type * Cpath.Resolved.class_type
    | `Renamed of Ident.path_class_type ]

  type t = {
    module_ : subst_module PathModuleMap.t;
    module_type : subst_module_type ModuleTypeMap.t;
    type_ : subst_type PathTypeMap.t;
    class_type : subst_class_type PathClassTypeMap.t;
    type_replacement : TypeExpr.t PathTypeMap.t;
    invalidated_modules : Ident.path_module list;
  }
end

and CComment : sig
  type block_element =
    [ Odoc_model.Comment.nestable_block_element
    | `Heading of
      Odoc_model.Comment.heading_level
      * Ident.label
      * Odoc_model.Comment.link_content
    | `Tag of Odoc_model.Comment.tag ]

  type docs = block_element Odoc_model.Comment.with_location list

  type docs_or_stop = [ `Docs of docs | `Stop ]
end

module Element : sig
  open Odoc_model.Paths

  type module_ = [ `Module of Identifier.Path.Module.t * Module.t Delayed.t ]

  type module_type = [ `ModuleType of Identifier.ModuleType.t * ModuleType.t ]

  type type_ = [ `Type of Identifier.Type.t * TypeDecl.t ]

  type value = [ `Value of Identifier.Value.t * Value.t ]

  type label = [ `Label of Identifier.Label.t ]

  type class_ = [ `Class of Identifier.Class.t * Class.t ]

  type class_type = [ `ClassType of Identifier.ClassType.t * ClassType.t ]

  type datatype = [ type_ | class_ | class_type ]

  type signature = [ module_ | module_type ]

  type external_ = [ `External of Identifier.Value.t * External.t ]

  type constructor =
    [ `Constructor of Identifier.Constructor.t * TypeDecl.Constructor.t ]

  type exception_ = [ `Exception of Identifier.Exception.t * Exception.t ]

  type extension =
    [ `Extension of Identifier.Extension.t * Extension.Constructor.t ]

  type field = [ `Field of Identifier.Field.t * TypeDecl.Field.t ]

  type label_parent = [ signature | datatype ]

  type any =
    [ signature
    | value
    | type_
    | label
    | class_
    | class_type
    | external_
    | constructor
    | exception_
    | extension
    | field ]
end

(** Formatting functions for components *)
module Fmt : sig
  val signature : Format.formatter -> Signature.t -> unit

  val removed_item : Format.formatter -> Signature.removed_item -> unit

  val removed_item_list :
    Format.formatter -> Signature.removed_item list -> unit

  val external_ : Format.formatter -> External.t -> unit

  val class_ : Format.formatter -> Class.t -> unit

  val class_type : Format.formatter -> ClassType.t -> unit

  val include_ : Format.formatter -> Include.t -> unit

  val value : Format.formatter -> Value.t -> unit

  val module_decl : Format.formatter -> Module.decl -> unit

  val module_ : Format.formatter -> Module.t -> unit

  val module_expansion : Format.formatter -> Module.expansion -> unit

  val module_type : Format.formatter -> ModuleType.t -> unit

  val module_type_expr : Format.formatter -> ModuleType.expr -> unit

  val functor_parameter : Format.formatter -> FunctorParameter.t -> unit

  val functor_parameter_parameter :
    Format.formatter -> FunctorParameter.parameter -> unit

  val type_decl : Format.formatter -> TypeDecl.t -> unit

  val type_equation : Format.formatter -> TypeDecl.Equation.t -> unit

  val type_equation2 : Format.formatter -> TypeDecl.Equation.t -> unit

  val exception_ : Format.formatter -> Exception.t -> unit

  val extension : Format.formatter -> Extension.t -> unit

  val substitution : Format.formatter -> ModuleType.substitution -> unit

  val substitution_list :
    Format.formatter -> ModuleType.substitution list -> unit

  val type_expr_list : Format.formatter -> TypeExpr.t list -> unit

  val type_object : Format.formatter -> TypeExpr.Object.t -> unit

  val type_class :
    Format.formatter -> Cpath.class_type * TypeExpr.t list -> unit

  val type_package : Format.formatter -> TypeExpr.Package.t -> unit

  val type_expr_polymorphic_variant :
    Format.formatter -> TypeExpr.Polymorphic_variant.t -> unit

  val type_expr : Format.formatter -> TypeExpr.t -> unit

  val resolved_module_path : Format.formatter -> Cpath.Resolved.module_ -> unit

  val module_path : Format.formatter -> Cpath.module_ -> unit

  val resolved_module_type_path :
    Format.formatter -> Cpath.Resolved.module_type -> unit

  val module_type_path : Format.formatter -> Cpath.module_type -> unit

  val resolved_type_path : Format.formatter -> Cpath.Resolved.type_ -> unit

  val resolved_parent_path : Format.formatter -> Cpath.Resolved.parent -> unit

  val type_path : Format.formatter -> Cpath.type_ -> unit

  val resolved_class_type_path :
    Format.formatter -> Cpath.Resolved.class_type -> unit

  val class_type_path : Format.formatter -> Cpath.class_type -> unit

  val model_path : Format.formatter -> Odoc_model.Paths_types.Path.any -> unit

  val model_resolved_path :
    Format.formatter -> Odoc_model.Paths_types.Resolved_path.any -> unit

  val model_identifier :
    Format.formatter -> Odoc_model.Paths_types.Identifier.any -> unit

  val model_fragment :
    Format.formatter -> Odoc_model.Paths_types.Fragment.any -> unit

  val model_resolved_fragment :
    Format.formatter -> Odoc_model.Paths_types.Resolved_fragment.any -> unit

  val resolved_root_fragment : Format.formatter -> Cfrag.root -> unit

  val resolved_signature_fragment :
    Format.formatter -> Cfrag.resolved_signature -> unit

  val resolved_module_fragment :
    Format.formatter -> Cfrag.resolved_module -> unit

  val resolved_type_fragment : Format.formatter -> Cfrag.resolved_type -> unit

  val signature_fragment : Format.formatter -> Cfrag.signature -> unit

  val module_fragment : Format.formatter -> Cfrag.module_ -> unit

  val type_fragment : Format.formatter -> Cfrag.type_ -> unit

  val model_resolved_reference :
    Format.formatter -> Odoc_model.Paths_types.Resolved_reference.any -> unit

  val model_reference :
    Format.formatter -> Odoc_model.Paths_types.Reference.any -> unit
end

module Of_Lang : sig
  type map

  val empty : map

  val identifier :
    ('a -> 'b -> 'c) -> 'b -> 'a -> [> `Identifier of 'a | `Local of 'c ]

  val resolved_module_path :
    map ->
    Odoc_model.Paths_types.Resolved_path.module_ ->
    Cpath.Resolved.module_

  val resolved_module_type_path :
    map ->
    Odoc_model.Paths_types.Resolved_path.module_type ->
    Cpath.Resolved.module_type

  val resolved_type_path :
    map -> Odoc_model.Paths_types.Resolved_path.type_ -> Cpath.Resolved.type_

  val resolved_class_type_path :
    map ->
    Odoc_model.Paths_types.Resolved_path.class_type ->
    Cpath.Resolved.class_type

  val module_path : map -> Odoc_model.Paths_types.Path.module_ -> Cpath.module_

  val module_type_path :
    map -> Odoc_model.Paths_types.Path.module_type -> Cpath.module_type

  val type_path : map -> Odoc_model.Paths_types.Path.type_ -> Cpath.type_

  val class_type_path :
    map -> Odoc_model.Paths_types.Path.class_type -> Cpath.class_type

  val resolved_signature_fragment :
    map ->
    Odoc_model.Paths_types.Resolved_fragment.signature ->
    Cfrag.resolved_signature

  val resolved_module_fragment :
    map ->
    Odoc_model.Paths_types.Resolved_fragment.module_ ->
    Cfrag.resolved_module

  val resolved_type_fragment :
    map -> Odoc_model.Paths_types.Resolved_fragment.type_ -> Cfrag.resolved_type

  val signature_fragment :
    map -> Odoc_model.Paths_types.Fragment.signature -> Cfrag.signature

  val module_fragment :
    map -> Odoc_model.Paths_types.Fragment.module_ -> Cfrag.module_

  val type_fragment :
    map -> Odoc_model.Paths_types.Fragment.type_ -> Cfrag.type_

  val type_decl : map -> Odoc_model.Lang.TypeDecl.t -> TypeDecl.t

  val type_decl_representation :
    map ->
    Odoc_model.Lang.TypeDecl.Representation.t ->
    TypeDecl.Representation.t

  val type_decl_constructor :
    map -> Odoc_model.Lang.TypeDecl.Constructor.t -> TypeDecl.Constructor.t

  val type_decl_constructor_argument :
    map ->
    Odoc_model.Lang.TypeDecl.Constructor.argument ->
    TypeDecl.Constructor.argument

  val type_decl_field :
    map -> Odoc_model.Lang.TypeDecl.Field.t -> TypeDecl.Field.t

  val type_equation :
    map -> Odoc_model.Lang.TypeDecl.Equation.t -> TypeDecl.Equation.t

  val type_expr_polyvar :
    map ->
    Odoc_model.Lang.TypeExpr.Polymorphic_variant.t ->
    TypeExpr.Polymorphic_variant.t

  val type_object :
    map -> Odoc_model.Lang.TypeExpr.Object.t -> TypeExpr.Object.t

  val type_package :
    map -> Odoc_model.Lang.TypeExpr.Package.t -> TypeExpr.Package.t

  val type_expression : map -> Odoc_model.Lang.TypeExpr.t -> TypeExpr.t

  val module_decl : map -> Odoc_model.Lang.Module.decl -> Module.decl

  val canonical :
    map ->
    ( Odoc_model.Paths_types.Path.module_
    * Odoc_model.Paths_types.Reference.module_ )
    option ->
    (Cpath.module_ * Odoc_model.Paths_types.Reference.module_) option

  val module_expansion :
    map -> Odoc_model.Lang.Module.expansion -> Module.expansion

  val module_ : map -> Odoc_model.Lang.Module.t -> Module.t

  val module_type_substitution :
    map -> Odoc_model.Lang.ModuleType.substitution -> ModuleType.substitution

  val functor_parameter :
    map ->
    Ident.functor_parameter ->
    Odoc_model.Lang.FunctorParameter.parameter ->
    FunctorParameter.parameter

  val extension : map -> Odoc_model.Lang.Extension.t -> Extension.t

  val extension_constructor :
    map -> Odoc_model.Lang.Extension.Constructor.t -> Extension.Constructor.t

  val exception_ : map -> Odoc_model.Lang.Exception.t -> Exception.t

  val module_type_expr :
    map -> Odoc_model.Lang.ModuleType.expr -> ModuleType.expr

  val module_type : map -> Odoc_model.Lang.ModuleType.t -> ModuleType.t

  val value : map -> Odoc_model.Lang.Value.t -> Value.t

  val external_ : map -> Odoc_model.Lang.External.t -> External.t

  val include_ : map -> Odoc_model.Lang.Include.t -> Include.t

  val class_ : map -> Odoc_model.Lang.Class.t -> Class.t

  val class_decl : map -> Odoc_model.Lang.Class.decl -> Class.decl

  val class_type_expr : map -> Odoc_model.Lang.ClassType.expr -> ClassType.expr

  val class_type : map -> Odoc_model.Lang.ClassType.t -> ClassType.t

  val class_signature :
    map -> Odoc_model.Lang.ClassSignature.t -> ClassSignature.t

  val method_ : map -> Odoc_model.Lang.Method.t -> Method.t

  val instance_variable :
    map -> Odoc_model.Lang.InstanceVariable.t -> InstanceVariable.t

  val module_substitution :
    map -> Odoc_model.Lang.ModuleSubstitution.t -> ModuleSubstitution.t

  val module_of_module_substitution :
    map -> Odoc_model.Lang.ModuleSubstitution.t -> Module.t

  val signature : map -> Odoc_model.Lang.Signature.t -> Signature.t

  val open_ : map -> Odoc_model.Lang.Open.t -> Open.t

  val apply_sig_map : map -> Odoc_model.Lang.Signature.t -> Signature.t

  val block_element :
    map -> Odoc_model.Comment.block_element -> CComment.block_element

  val docs : map -> Odoc_model.Comment.docs -> CComment.docs

  val docs_or_stop :
    map -> Odoc_model.Comment.docs_or_stop -> CComment.docs_or_stop
end

val module_of_functor_argument : FunctorParameter.parameter -> Module.t
