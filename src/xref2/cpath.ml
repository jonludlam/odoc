open Odoc_model.Paths
open Odoc_model.Names

(* Component paths are model paths that can also hold local identifiers: an
   identifier with a [`Local n] parent. The [na] cases let a model path be used
   as a component path. [`U] marks the typed parents that only component paths
   use. Canonical targets are model paths ([canon]), so they cannot hold local
   identifiers. *)

type pty = [ `U | na ]

(* The identifiers that component paths hold: a local one, with a [`Local n]
   parent, or a global one. The views split them into an [Ident] or a global
   identifier, which is what the environment looks up. *)
module Id = struct
  type module_ = local_parent Identifier.Id.path_module_gen

  type module_type = local_parent Identifier.Id.path_module_type_gen

  type type_ = local_parent Identifier.Id.path_type_gen

  type class_type = local_parent Identifier.Id.path_class_type_gen

  type ('l, 'g) view = Local of 'l | Global of 'g

  let module_ : module_ -> (Ident.module_, Identifier.Path.Module.t) view =
    function
    | `Module (`Local i, n) | `Parameter (`Local i, n) ->
        Local (`Module (`Local i, n))
    | `Module ((#global_parent as p), n) -> Global (`Module (p, n))
    | `Parameter ((#global_parent as p), n) -> Global (`Parameter (p, n))
    | (`Root _ | `Result _) as x -> Global x

  let module_type :
      module_type -> (Ident.module_type, Identifier.Path.ModuleType.t) view =
    function
    | `ModuleType (`Local i, n) -> Local (`ModuleType (`Local i, n))
    | `ModuleType ((#global_parent as p), n) -> Global (`ModuleType (p, n))

  let class_type : class_type -> (Ident.type_, Identifier.Path.ClassType.t) view
      = function
    | `Class (`Local i, n) | `ClassType (`Local i, n) ->
        Local (`Type (`Local i, n))
    | `Class ((#global_parent as p), n) -> Global (`Class (p, n))
    | `ClassType ((#global_parent as p), n) -> Global (`ClassType (p, n))

  let type_ : type_ -> (Ident.type_, Identifier.Path.Type.t) view = function
    | `Type (`Local i, n) -> Local (`Type (`Local i, n))
    | `Type ((#global_parent as p), n) -> Global (`Type (p, n))
    | (`Class _ | `ClassType _) as c -> (
        match class_type c with
        | Local l -> Local l
        | Global g -> Global (g :> Identifier.Path.Type.t))
end

module Resolved = struct
  type parent = (local_parent, pty, canon) Path.Resolved.parent_gen

  type module_ = (local_parent, pty, canon) Path.Resolved.Module.gen

  type module_type = (local_parent, pty, canon) Path.Resolved.ModuleType.gen

  type type_ = (local_parent, pty, canon) Path.Resolved.Type.gen

  type value = (local_parent, pty, canon) Path.Resolved.Value.gen

  type class_type = (local_parent, pty, canon) Path.Resolved.ClassType.gen

  type any = (local_parent, pty, canon) Path.Resolved.gen
end

module Cpath = struct
  type module_ = (local_parent, pty, canon) Path.Module.gen

  type module_type = (local_parent, pty, canon) Path.ModuleType.gen

  type type_ = (local_parent, pty, canon) Path.Type.gen

  type value = (local_parent, pty, canon) Path.Value.gen

  type class_type = (local_parent, pty, canon) Path.ClassType.gen

  type any = (local_parent, pty, canon) Path.gen
end

include Cpath

(* The parent that stands for the root of the signature a [with] constraint
   changes, while its fragments are resolved. *)
let fragment_root : Resolved.parent =
  `Module (`Identifier (Ident.fragment_root :> Id.module_))

let is_fragment_root : Resolved.parent -> bool = function
  | `Module (`Identifier (`Module (`Local i, _))) ->
      let (`Module (`Local j, _)) = Ident.fragment_root in
      i = j
  | _ -> false

let rec is_resolved_module_substituted : Resolved.module_ -> bool = function
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `Subst (_a, _) -> false (* is_resolved_module_type_substituted a*)
  | `Hidden a | `Apply (a, _) | `Alias (a, _, _) | `Canonical (a, _) ->
      is_resolved_module_substituted a
  | `Module (a, _) -> is_resolved_parent_substituted a
  | `OpaqueModule a -> is_resolved_module_substituted a

and is_resolved_parent_substituted = function
  | `Module m -> is_resolved_module_substituted m
  | `ModuleType (m, _) -> is_resolved_module_type_substituted m

and is_resolved_module_type_substituted : Resolved.module_type -> bool =
  function
  | `SubstitutedMT _ -> true
  | `Identifier _ -> false
  | `ModuleType (a, _) -> is_resolved_parent_substituted a
  | `SubstT _ -> false
  | `AliasModuleType (m1, _) -> is_resolved_module_type_substituted m1
  | `CanonicalModuleType (m, _) | `OpaqueModuleType m ->
      is_resolved_module_type_substituted m

and is_resolved_type_substituted : Resolved.type_ -> bool = function
  | `CoreType _ -> false
  | `Unbox _ -> false
  | `SubstitutedT _ | `SubstitutedCT _ -> true
  | `Identifier _ -> false
  | `CanonicalType (t, _) -> is_resolved_type_substituted t
  | `Type (a, _) | `Class (a, _) | `ClassType (a, _) ->
      is_resolved_parent_substituted a

and is_resolved_class_type_substituted : Resolved.class_type -> bool = function
  | `SubstitutedCT _ -> true
  | `Identifier _ -> false
  | `Class (a, _) | `ClassType (a, _) -> is_resolved_parent_substituted a

let rec is_module_substituted : module_ -> bool = function
  | `Resolved a -> is_resolved_module_substituted a
  | `Identifier _ -> false
  | `Substituted _ -> true
  | `Dot (a, _) | `Apply (a, _) -> is_module_substituted a
  | `Root _ -> false
  | `Module (_, a, _) -> is_resolved_parent_substituted a

let is_module_type_substituted : module_type -> bool = function
  | `Resolved a -> is_resolved_module_type_substituted a
  | `Identifier _ -> false
  | `SubstitutedMT _ -> true
  | `DotMT (a, _) -> is_module_substituted a
  | `ModuleType (_, a, _) -> is_resolved_parent_substituted a

let is_type_substituted : type_ -> bool = function
  | `Resolved a -> is_resolved_type_substituted a
  | `Identifier _ -> false
  | `Unbox _ -> false
  | `SubstitutedT _ -> true
  | `DotT (a, _) -> is_module_substituted a
  | `Type (_, a, _) | `Class (_, a, _) | `ClassType (_, a, _) ->
      is_resolved_parent_substituted a

let is_class_type_substituted : class_type -> bool = function
  | `Resolved a -> is_resolved_class_type_substituted a
  | `Identifier _ -> false
  | `SubstitutedCT _ -> true
  | `DotT (a, _) -> is_module_substituted a
  | `Class (_, a, _) | `ClassType (_, a, _) -> is_resolved_parent_substituted a

(* The hidden checks are those of the model, which take paths with any local
   identifiers. *)

let is_resolved_module_hidden ~weak_canonical_test (p : Resolved.module_) =
  Path.is_resolved_hidden_gen ~weak_canonical_test (p :> Resolved.any)

let is_resolved_module_type_hidden ?(weak_canonical_test = false)
    (p : Resolved.module_type) =
  Path.is_resolved_hidden_gen ~weak_canonical_test (p :> Resolved.any)

let is_resolved_type_hidden ?(weak_canonical_test = false) (p : Resolved.type_)
    =
  Path.is_resolved_hidden_gen ~weak_canonical_test (p :> Resolved.any)

let is_resolved_class_type_hidden ?(weak_canonical_test = false)
    (p : Resolved.class_type) =
  Path.is_resolved_hidden_gen ~weak_canonical_test (p :> Resolved.any)

let is_module_hidden (p : module_) = Path.is_hidden_gen (p :> any)

let is_module_type_hidden (p : module_type) = Path.is_hidden_gen (p :> any)

let is_type_hidden (p : type_) = Path.is_hidden_gen (p :> any)

let is_class_type_hidden (p : class_type) = Path.is_hidden_gen (p :> any)

(* A class path used as a type path. *)
let rec type_of_class_type : class_type -> type_ = function
  | `Resolved r -> `Resolved (r :> Resolved.type_)
  | `SubstitutedCT p -> `SubstitutedT (type_of_class_type p)
  | `Identifier (i, b) -> `Identifier ((i :> Id.type_), b)
  | `DotT _ as p -> p
  | (`Class _ | `ClassType _) as p -> p

let rec resolved_module_of_resolved_module_reference :
    Reference.Resolved.Module.t -> Resolved.module_ = function
  | `Module (parent, name) ->
      `Module
        (`Module (resolved_module_of_resolved_signature_reference parent), name)
  | `Identifier x -> `Identifier (x :> Id.module_)
  | `Alias (_m1, _m2) -> failwith "gah"
  | `Hidden s -> `Hidden (resolved_module_of_resolved_module_reference s)

and resolved_module_of_resolved_signature_reference :
    Reference.Resolved.Signature.t -> Resolved.module_ = function
  | `Identifier (#Identifier.Module.t as i) -> `Identifier (i :> Id.module_)
  | (`Alias _ | `Module _ | `Hidden _) as r' ->
      resolved_module_of_resolved_module_reference r'
  | `ModuleType (_, n) ->
      failwith ("Not a module reference: " ^ ModuleTypeName.to_string n)
  | `AliasModuleType _ -> failwith "Not a module reference: aliasmoduletype"
  | `Identifier _ -> failwith "Not a module reference : identifier"

and module_of_module_reference : Reference.Module.t -> module_ = function
  | `Resolved r -> `Resolved (resolved_module_of_resolved_module_reference r)
  | `Root (_, _) -> failwith "unhandled"
  | `Dot
      ( (( `Resolved (`Identifier #Identifier.Module.t)
         | `Dot (_, _)
         | `Module (_, _) ) as parent),
        name ) ->
      `Dot (module_of_module_reference parent, ModuleName.make_std name)
  | `Module
      ( (( `Resolved (`Identifier #Identifier.Module.t)
         | `Dot (_, _)
         | `Module (_, _) ) as parent),
        name ) ->
      `Dot (module_of_module_reference parent, name)
  | _ -> failwith "Not a module reference"

let rec unresolve_resolved_module_path : Resolved.module_ -> module_ = function
  | `Hidden (`Identifier x) -> `Identifier (x, true)
  | `Identifier x ->
      let hidden =
        match x with
        | `Module (`Local _, _) -> false
        | `Module (_, n) -> Odoc_model.Names.ModuleName.is_hidden n
        | _ -> false
      in
      `Identifier (x, hidden)
  | `Substituted x -> unresolve_resolved_module_path x
  | `Subst (_, x) -> unresolve_resolved_module_path x
  | `Hidden x -> unresolve_resolved_module_path x (* should assert false here *)
  | `Module (p, m) -> `Dot (unresolve_resolved_parent_path p, m)
  | `Canonical (m, _) -> unresolve_resolved_module_path m
  | `Apply (m, a) ->
      `Apply (unresolve_resolved_module_path m, unresolve_resolved_module_path a)
  | `Alias (_, `Resolved m, _) -> unresolve_resolved_module_path m
  | `Alias (_, m, _) -> m
  | `OpaqueModule m -> unresolve_resolved_module_path m

and unresolve_module_path : module_ -> module_ = function
  | `Resolved x -> unresolve_resolved_module_path x
  | `Substituted x -> unresolve_module_path x
  | `Identifier _ as x -> x
  | `Root _ as x -> x
  | `Dot (p, x) -> `Dot (unresolve_module_path p, x)
  | `Module (_, p, x) -> `Dot (unresolve_resolved_parent_path p, x)
  | `Apply (x, y) -> `Apply (unresolve_module_path x, unresolve_module_path y)

and unresolve_resolved_module_type_path : Resolved.module_type -> module_type =
  function
  | `Identifier _ as p -> `Resolved p
  | `SubstitutedMT x -> unresolve_resolved_module_type_path x
  | `ModuleType (p, n) -> `DotMT (unresolve_resolved_parent_path p, n)
  | `SubstT (_, m) -> unresolve_resolved_module_type_path m
  | `AliasModuleType (_, m2) -> unresolve_resolved_module_type_path m2
  | `CanonicalModuleType (p, _) -> unresolve_resolved_module_type_path p
  | `OpaqueModuleType m -> unresolve_resolved_module_type_path m

and unresolve_resolved_parent_path : Resolved.parent -> module_ = function
  | p when is_fragment_root p -> assert false
  | `Module m -> unresolve_resolved_module_path m
  | `ModuleType _ -> assert false

and unresolve_resolved_type_path : Resolved.type_ -> type_ = function
  | (`Identifier _ | `CoreType _) as p -> `Resolved p
  | `SubstitutedT x -> unresolve_resolved_type_path x
  | `SubstitutedCT x -> unresolve_resolved_type_path (x :> Resolved.type_)
  | `CanonicalType (t1, _) -> unresolve_resolved_type_path t1
  | `Type (p, n) -> `DotT (unresolve_resolved_parent_path p, n)
  | `Class (p, n) -> `DotT (unresolve_resolved_parent_path p, n)
  | `ClassType (p, n) -> `DotT (unresolve_resolved_parent_path p, n)
  | `Unbox p -> `Unbox (unresolve_resolved_type_path p)

and unresolve_resolved_class_type_path : Resolved.class_type -> class_type =
  function
  | `Identifier _ as p -> `Resolved p
  | `SubstitutedCT x -> unresolve_resolved_class_type_path x
  | `Class (p, n) -> `DotT (unresolve_resolved_parent_path p, n)
  | `ClassType (p, n) -> `DotT (unresolve_resolved_parent_path p, n)

and unresolve_module_type_path : module_type -> module_type = function
  | `Resolved m -> unresolve_resolved_module_type_path m
  | y -> y

and unresolve_type_path : type_ -> type_ = function
  | `Resolved m -> (unresolve_resolved_type_path m :> type_)
  | y -> y

and unresolve_class_type_path : class_type -> class_type = function
  | `Resolved m -> unresolve_resolved_class_type_path m
  | y -> y

(*
These are left here for later. The idea is to alter a resolved path
such that all the identifiers in it are turned into fully-qualified
resolved paths. This is intended to be used to turn the 'original
path' of `module type of` expressions that was resolved in the
original context into a path that is valid in _all_ contexts.

let rec original_path_cpath : module_ -> module_ option = function
  | `Resolved p ->
      original_path_cpath (unresolve_resolved_module_path p)
  | `Root name -> Some (`Root name)
  | `Forward _ -> None
  | `Dot (p, s) -> (
      match original_path_cpath p with
      | Some p -> Some (`Dot (p, s))
      | None -> None)
  | `Apply (p1, p2) -> (
      match (original_path_cpath p1, original_path_cpath p2) with
      | Some p1', Some p2' -> Some (`Apply (p1', p2'))
      | _ -> None)
  | `Identifier (i, _) -> (
      match original_path_module_identifier i with
      | Some i -> Some (`Resolved i)
      | None -> None)
  | `Substituted p -> original_path_cpath p
  | `Local _ ->
      None
  | `Module _ ->
      None


and original_path_module_identifier :
    Odoc_model.Paths.Identifier.Path.Module.t -> Resolved.module_ option =
 fun id ->
  match id with
  | `Module (sg, name) -> (
      match original_path_parent_identifier sg with
      | Some sg' -> Some (`Module (sg', name))
      | None -> None)
  | `Root _ -> Some (`Identifier id)
  | _ ->
      None

and original_path_parent_identifier :
    Odoc_model.Paths.Identifier.Signature.t -> Resolved.parent option =
 fun id ->
  match id with
  | `Module _ | `Root _ | `Parameter _ | `Result _ as mid -> (
      match original_path_module_identifier mid with
      | Some m -> Some (`Module m)
      | None -> None)
  | `ModuleType _ as mtid -> (
      match original_path_module_type_identifier mtid with
      | Some m -> Some (`ModuleType m)
      | None -> None)

and original_path_module_type_identifier :
    Odoc_model.Paths.Identifier.ModuleType.t -> Resolved.module_type option =
 fun id ->
  match id with
  | `ModuleType (sg, name) -> (
      match original_path_parent_identifier sg with
      | Some sg' -> Some (`ModuleType (sg', name))
      | None -> None)
*)
