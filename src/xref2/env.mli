(* Env.mli *)

open Odoc_model
open Odoc_model.Paths

type lookup_unit_result = Forward_reference | Found of Lang.Compilation_unit.t

type path_query = [ `Path of Reference.Hierarchy.t | `Name of string ]

type lookup_error = [ `Not_found ]

module Library_scope : sig
  type t = private
    | Scoped of string list
        (** Among these libraries alone. The list is never empty. *)
    | Any  (** Anywhere on the search path. *)

  val any : t

  val of_libraries : string list -> t
  (** [of_libraries libs] is [Scoped libs], or [Any] when [libs] is empty: a
      unit that recorded no libraries is not limited. *)
end

type resolver = {
  open_units : string list;
  lookup_unit_by_name :
    scope:Library_scope.t -> string -> (lookup_unit_result, lookup_error) result;
      (** Finds the unit a name refers to, whether the name comes from a path, a
          canonical path or a reference. A path wants [Scoped] with the
          libraries the unit the name is in recorded: a path can only mean a
          module the compiler gave the unit. A reference wants [Any], the whole
          search path. Where several units match, the lookup warns. *)
  lookup_unit_by_path :
    Reference.Hierarchy.t -> (Lang.Compilation_unit.t, lookup_error) result;
      (** Finds the unit a path reference names, such as [{!/libname/Module}].
          The reference says which library, so there is no scope. *)
  lookup_unit_by_id : Identifier.RootModule.t -> Lang.Compilation_unit.t option;
      (** Finds the unit an identifier names, wherever it is on the search path.
          There is nothing to choose between, so no ambiguity. *)
  scope_of_unit : Identifier.RootModule.t -> string list;
      (** The libraries a unit was compiled against, as it recorded them. *)
  lookup_page : path_query -> (Lang.Page.t, lookup_error) result;
  lookup_asset : path_query -> (Lang.Asset.t, lookup_error) result;
  lookup_impl : string -> Lang.Implementation.t option;
      (** Finds an implementation by name, among the libraries of the unit being
          linked. The names come from shapes, which only name units in the
          compiler's dependency closure of that unit: its recorded libraries
          hold them all, and a closure has one unit of each name. *)
  shape_reducer : Shape_reducer.t;
}

type root =
  | Resolved of (Root.t * Identifier.Module.t * Component.Module.t)
  | Forward

type lookup_type =
  | Module of Identifier.Path.Module.t
  | ModuleType of Identifier.ModuleType.t
  | RootModule of
      Odoc_model.Names.ModuleName.t
      * Library_scope.t
      * [ `Forward | `Resolved of Digest.t ] option
  | ModuleByName of string * Identifier.Path.Module.t
  | FragmentRoot of int

module LookupTypeSet : Set.S with type elt = lookup_type

val pp_lookup_type_list : Format.formatter -> lookup_type list -> unit

type t

val is_linking : t -> bool

val with_recorded_lookups : t -> (t -> 'a) -> LookupTypeSet.t * 'a

val set_resolver : t -> resolver -> t

val with_scope : Library_scope.t -> t -> t
(** [with_scope scope env] resolves names in [scope] from now on. Resolution
    that has reached a unit continues in that unit's libraries, since those are
    what the compiler could see when the unit was built.

    The scope limits one kind of lookup only: the root module of a path, looked
    up by name ({!lookup_root_module}). A path is written by the compiler, or in
    a [@canonical] tag, so it can only mean a module the unit could see. The
    scope does not limit the root of a reference ({!lookup_by_name}), which an
    author may write for anything on the search path, nor a lookup by identifier
    ({!lookup_by_id}), which names its unit. A reference that goes into a unit
    and meets a path there, such as an alias, resolves that path in the unit's
    scope. *)

val scope_of_unit : Identifier.RootModule.t -> t -> Library_scope.t
(** The libraries the given unit recorded at compile time. *)

val has_resolver : t -> bool

val resolver : t -> resolver option

val id : t -> int

val empty : t

val add_fragment_root : Component.Signature.t -> t -> t

val add_module :
  Identifier.Path.Module.t ->
  Component.Module.t Component.Delayed.t ->
  Component.CComment.docs ->
  t ->
  t

val add_type : Identifier.Type.t -> Component.TypeDecl.t -> t -> t

val add_module_type :
  Identifier.Path.ModuleType.t -> Component.ModuleType.t -> t -> t

val add_value : Identifier.Value.t -> Component.Value.t -> t -> t

val add_label :
  Identifier.Label.t -> Component.Label.t -> t -> unboxed:bool -> t

val add_class : Identifier.Class.t -> Component.Class.t -> t -> t

val add_class_type : Identifier.ClassType.t -> Component.ClassType.t -> t -> t

val add_exception : Identifier.Exception.t -> Component.Exception.t -> t -> t

val add_extension_constructor :
  Identifier.Extension.t ->
  Component.Extension.Constructor.t ->
  Component.Extension.t ->
  t ->
  t

val add_docs : Comment.docs -> t -> t

val add_comment : Comment.docs_or_stop -> t -> t

val add_method : Identifier.Method.t -> Component.Method.t -> t -> t

val add_module_functor_args :
  Component.Module.t -> Identifier.Path.Module.t -> t -> t

val add_module_type_functor_args :
  Component.ModuleType.t -> Identifier.ModuleType.t -> t -> t

val lookup_fragment_root : t -> (int * Component.Signature.t) option

val lookup_page_by_name : string -> t -> (Lang.Page.t, lookup_error) result
val lookup_page_by_path :
  Reference.Hierarchy.t -> t -> (Lang.Page.t, lookup_error) result

val lookup_asset_by_path :
  Reference.Hierarchy.t -> t -> (Lang.Asset.t, lookup_error) result

val lookup_asset_by_name : string -> t -> (Lang.Asset.t, lookup_error) result

val lookup_impl : string -> t -> Lang.Implementation.t option

val lookup_unit_by_path :
  Reference.Hierarchy.t -> t -> (Component.Element.module_, lookup_error) result
(** For a path reference, such as [{!/libname/Module}]. The author names the
    library, so the library scope does not apply. *)

val module_of_unit : Lang.Compilation_unit.t -> Component.Module.t

val lookup_root_module_in :
  Library_scope.t -> Odoc_model.Names.ModuleName.t -> t -> root option
(** For a path whose root name records where it was read from: looks up the root
    module by name in that unit's libraries. *)

val lookup_root_module : Odoc_model.Names.ModuleName.t -> t -> root option
(** For a path: looks up its root module by name, in the current library scope
    (see {!with_scope}). A reference's root is looked up by {!lookup_by_name}
    instead. *)

type 'a scope constraint 'a = [< Component.Element.any ]
(** Target of a lookup *)

type 'a maybe_ambiguous =
  ('a, [ `Ambiguous of 'a * 'a list | `Not_found ]) result

val lookup_by_name : 'a scope -> string -> t -> 'a maybe_ambiguous
(** For a reference: looks up an element in Env depending on the given [scope].
    Returns [Error (`Ambiguous _)] when two or more elements match the given
    scope and name. When nothing in Env has the name, {!s_module},
    {!s_signature} and {!s_any} look for a root module of that name along the
    whole search path, whatever the {!Library_scope.t}. *)

val lookup_by_id : 'a scope -> [< Identifier.t ] -> t -> 'a option
(** Like [lookup_by_name] but uses an identifier as key, for paths and
    references alike. A root identifier names its unit, which is looked up as
    itself, so the {!Library_scope.t} does not apply. *)

val s_any : Component.Element.any scope

val s_signature : Component.Element.signature scope

val s_module : Component.Element.module_ scope

val s_module_type : Component.Element.module_type scope

val s_type : Component.Element.type_ scope

val s_datatype : Component.Element.datatype scope

val s_class : Component.Element.class_ scope

val s_class_type : Component.Element.class_type scope

val s_value : Component.Element.value scope

val s_label : Component.Element.label scope

val s_constructor : Component.Element.constructor scope

val s_exception : Component.Element.exception_ scope

val s_extension : Component.Element.extension scope

val s_field : Component.Element.field scope

val s_unboxed_field : Component.Element.unboxed_field scope

val s_label_parent : Component.Element.label_parent scope

val s_fragment_type_parent : Component.Element.fragment_type_parent scope

(* val open_component_signature :
   Paths_types.Identifier.signature -> Component.Signature.t -> t -> t *)

val add_functor_parameter : Lang.FunctorParameter.t -> t -> t

val add_module_arg : Lang.TypeExpr.Module.t -> t -> t

val open_class_signature : Lang.ClassSignature.t -> t -> t

val open_signature : Lang.Signature.t -> t -> t

val open_type_substitution : Lang.TypeDecl.t -> t -> t

val open_module_substitution : Lang.ModuleSubstitution.t -> t -> t

val open_module_type_substitution : Lang.ModuleTypeSubstitution.t -> t -> t

val open_page : Lang.Page.t -> t -> t
(** Add a page content to the env. *)

val env_of_unit : Lang.Compilation_unit.t -> linking:bool -> resolver -> t
(** Create a new env with a module initially opened. *)

val env_of_page : Lang.Page.t -> resolver -> t
(** Create a new env for a page. *)

val env_of_impl : Lang.Implementation.t -> resolver -> t
(** Create a new env for an implementation. *)

val env_for_reference : resolver -> t
(** Create a new env for a reference. *)

val env_for_testing : linking:bool -> t
(** Create a new env for testing purposes *)

val inherit_resolver : t -> t
(** Create an empty environment reusing the same resolver. *)

val len : int ref

val n : int ref

val should_suppress_warnings : t -> string option -> bool

val set_warnings_tags : t -> string list -> t

val verify_lookups : t -> LookupTypeSet.t -> bool
