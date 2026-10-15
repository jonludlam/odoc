open Odoc_model.Paths
open Odoc_model.Names

(* Component paths are model paths that can also hold local identifiers. The
   [na] cases let a model path be used as a component path. [`U] marks the
   typed parents that only component paths use. Canonical targets are model
   paths ([canon]), so they cannot hold local identifiers. *)

type lmod = [ Ident.module_ | na ]

type lmodty = [ Ident.module_type | na ]

type lty = [ Ident.type_ | na ]

type pty = [ `U | na ]

module Resolved = struct
  type parent = (lmod, lmodty, pty, canon) Path.Resolved.parent_gen

  type module_ = (lmod, lmodty, pty, canon) Path.Resolved.Module.gen

  type module_type = (lmod, lmodty, pty, canon) Path.Resolved.ModuleType.gen

  type type_ = (lmod, lmodty, pty, lty, canon) Path.Resolved.Type.gen

  type value = (lmod, lmodty, pty, na, canon) Path.Resolved.Value.gen

  type class_type = (lmod, lmodty, pty, lty, canon) Path.Resolved.ClassType.gen
end

module Cpath = struct
  type module_ = (lmod, lmodty, pty, canon) Path.Module.gen

  type module_type = (lmod, lmodty, pty, canon) Path.ModuleType.gen

  type type_ = (lmod, lmodty, pty, lty, canon) Path.Type.gen

  type value = (lmod, lmodty, pty, na, canon) Path.Value.gen

  type class_type = (lmod, lmodty, pty, lty, canon) Path.ClassType.gen
end

include Cpath

let rec is_resolved_module_substituted : Resolved.module_ -> bool = function
  | `LocalMod _ -> false
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
  | `FragmentRoot _ -> false

and is_resolved_module_type_substituted : Resolved.module_type -> bool =
  function
  | `LocalModTy _ -> false
  | `SubstitutedMT _ -> true
  | `Identifier _ -> false
  | `ModuleType (a, _) -> is_resolved_parent_substituted a
  | `SubstT _ -> false
  | `AliasModuleType (m1, _) -> is_resolved_module_type_substituted m1
  | `CanonicalModuleType (m, _) | `OpaqueModuleType m ->
      is_resolved_module_type_substituted m

and is_resolved_type_substituted : Resolved.type_ -> bool = function
  | `LocalTy _ -> false
  | `CoreType _ -> false
  | `Unbox _ -> false
  | `SubstitutedT _ | `SubstitutedCT _ -> true
  | `Identifier _ -> false
  | `CanonicalType (t, _) -> is_resolved_type_substituted t
  | `Type (a, _) | `Class (a, _) | `ClassType (a, _) ->
      is_resolved_parent_substituted a

and is_resolved_class_type_substituted : Resolved.class_type -> bool = function
  | `LocalTy _ -> false
  | `SubstitutedCT _ -> true
  | `Identifier _ -> false
  | `Class (a, _) | `ClassType (a, _) -> is_resolved_parent_substituted a

let rec is_module_substituted : module_ -> bool = function
  | `Resolved a -> is_resolved_module_substituted a
  | `Identifier _ -> false
  | `LocalMod _ -> false
  | `Substituted _ -> true
  | `Dot (a, _) | `Apply (a, _) -> is_module_substituted a
  | `Root _ -> false
  | `Module (_, a, _) -> is_resolved_parent_substituted a

let is_module_type_substituted : module_type -> bool = function
  | `Resolved a -> is_resolved_module_type_substituted a
  | `Identifier _ -> false
  | `LocalModTy _ -> false
  | `SubstitutedMT _ -> true
  | `DotMT (a, _) -> is_module_substituted a
  | `ModuleType (_, a, _) -> is_resolved_parent_substituted a

let is_type_substituted : type_ -> bool = function
  | `Resolved a -> is_resolved_type_substituted a
  | `Identifier _ -> false
  | `LocalTy _ -> false
  | `Unbox _ -> false
  | `SubstitutedT _ -> true
  | `DotT (a, _) -> is_module_substituted a
  | `Type (_, a, _) | `Class (_, a, _) | `ClassType (_, a, _) ->
      is_resolved_parent_substituted a

let is_class_type_substituted : class_type -> bool = function
  | `Resolved a -> is_resolved_class_type_substituted a
  | `Identifier _ -> false
  | `LocalTy _ -> false
  | `SubstitutedCT _ -> true
  | `DotT (a, _) -> is_module_substituted a
  | `Class (_, a, _) | `ClassType (_, a, _) -> is_resolved_parent_substituted a

let rec is_module_hidden : module_ -> bool = function
  | `Resolved r -> is_resolved_module_hidden ~weak_canonical_test:false r
  | `Substituted p | `Dot (p, _) | `Apply (p, _) -> is_module_hidden p
  | `Identifier (id, b) -> b || Identifier.is_hidden id
  | `LocalMod (_, b) -> b
  | `Root n -> ModuleName.is_hidden n
  | `Module (_, p, _) -> is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_module_hidden :
    weak_canonical_test:bool -> Resolved.module_ -> bool =
 fun ~weak_canonical_test ->
  let rec inner = function
    | `LocalMod _ -> false
    | `Identifier _ as p ->
        Odoc_model.Paths.Path.Resolved.Module.is_hidden ~weak_canonical_test p
    | `Hidden _ -> true
    | `Canonical (_, `Resolved _) -> false
    | `Canonical (p, _) -> (not weak_canonical_test) && inner p
    | `Substituted p -> inner p
    | `Module (p, _) -> is_resolved_parent_hidden ~weak_canonical_test p
    | `Subst (p1, p2) ->
        is_resolved_module_type_hidden ~weak_canonical_test p1 || inner p2
    | `Alias (p1, `Resolved p2, _) -> inner p1 && inner p2
    | `Alias (p1, _p2, _) -> inner p1
    | `Apply (p1, p2) -> inner p1 || inner p2
    | `OpaqueModule m -> inner m
  in
  inner

and is_resolved_parent_hidden :
    weak_canonical_test:bool -> Resolved.parent -> bool =
 fun ~weak_canonical_test -> function
  | `Module m -> is_resolved_module_hidden ~weak_canonical_test m
  | `ModuleType (m, _) -> is_resolved_module_type_hidden ~weak_canonical_test m
  | `FragmentRoot _ -> false

and is_module_type_hidden : module_type -> bool = function
  | `Resolved r -> is_resolved_module_type_hidden r
  | `Identifier (`ModuleType (_, t), b) -> b || ModuleTypeName.is_hidden t
  | `LocalModTy (_, b) -> b
  | `SubstitutedMT p -> is_module_type_hidden p
  | `DotMT (p, _) -> is_module_hidden p
  | `ModuleType (_, p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_module_type_hidden :
    ?weak_canonical_test:bool -> Resolved.module_type -> bool =
 fun ?(weak_canonical_test = false) p ->
  let inner = is_resolved_module_type_hidden ~weak_canonical_test in
  match p with
  | `LocalModTy _ -> false
  | `Identifier _ as p -> Odoc_model.Paths.Path.Resolved.(is_hidden (p :> t))
  | `SubstitutedMT p -> inner p
  | `ModuleType (_, n) when ModuleTypeName.is_hidden n -> true
  | `ModuleType (p, _) -> is_resolved_parent_hidden ~weak_canonical_test p
  | `SubstT (p1, p2) -> inner p1 || inner p2
  | `AliasModuleType (p1, p2) -> inner p1 && inner p2
  | `CanonicalModuleType (_, `Resolved _) -> false
  | `CanonicalModuleType (p, _) -> inner p
  | `OpaqueModuleType m -> inner m

and is_type_hidden : type_ -> bool = function
  | `Resolved r -> is_resolved_type_hidden r
  | `Identifier (`Type (_, t), b) -> b || TypeName.is_hidden t
  | `Identifier (`ClassType (_, t), b) -> b || TypeName.is_hidden t
  | `Identifier (`Class (_, t), b) -> b || TypeName.is_hidden t
  | `LocalTy (_, b) -> b
  | `SubstitutedT p -> is_type_hidden (p :> type_)
  | `DotT (p, _) -> is_module_hidden p
  | `Unbox p -> is_type_hidden p
  | `Type (_, p, _) | `Class (_, p, _) | `ClassType (_, p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_type_hidden :
    ?weak_canonical_test:bool -> Resolved.type_ -> bool =
 fun ?(weak_canonical_test = false) p ->
  match p with
  | `CoreType n -> TypeName.is_hidden n
  | `LocalTy _ -> false
  | `Identifier _ as p -> Odoc_model.Paths.Path.Resolved.(is_hidden (p :> t))
  | `SubstitutedT p -> is_resolved_type_hidden ~weak_canonical_test p
  | `SubstitutedCT p -> is_resolved_class_type_hidden ~weak_canonical_test p
  | `CanonicalType (_, `Resolved _) -> false
  | `CanonicalType (p, _) -> is_resolved_type_hidden ~weak_canonical_test p
  | `Unbox p -> is_resolved_type_hidden ~weak_canonical_test p
  (* A shadowed or otherwise hidden name makes the path hidden even when its
     parent is visible - as in [Odoc_model.Paths.Path.Resolved.is_hidden]. *)
  | (`Type (_, n) | `Class (_, n) | `ClassType (_, n)) when TypeName.is_hidden n
    ->
      true
  | `Type (p, _) | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test p

and is_resolved_class_type_hidden :
    ?weak_canonical_test:bool -> Resolved.class_type -> bool =
 fun ?(weak_canonical_test = false) p ->
  match p with
  | `LocalTy _ -> false
  | `Identifier _ as p -> Odoc_model.Paths.Path.Resolved.(is_hidden (p :> t))
  | `SubstitutedCT p -> is_resolved_class_type_hidden ~weak_canonical_test p
  | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test p

and is_class_type_hidden : class_type -> bool = function
  | `Resolved r -> is_resolved_class_type_hidden r
  | `Identifier (id, b) -> b || Identifier.is_hidden id
  | `LocalTy (_, b) -> b
  | `SubstitutedCT p -> is_class_type_hidden p
  | `DotT (p, _) -> is_module_hidden p
  | `Class (_, p, _) | `ClassType (_, p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

(* A class path used as a type path. *)
let rec type_of_class_type : class_type -> type_ = function
  | `Resolved r -> `Resolved (r :> Resolved.type_)
  | `SubstitutedCT p -> `SubstitutedT (type_of_class_type p)
  | `Identifier (i, b) -> `Identifier ((i :> Identifier.Path.Type.t), b)
  | `LocalTy _ as p -> p
  | `DotT _ as p -> p
  | (`Class _ | `ClassType _) as p -> p

let rec resolved_module_of_resolved_module_reference :
    Reference.Resolved.Module.t -> Resolved.module_ = function
  | `Module (parent, name) ->
      `Module
        (`Module (resolved_module_of_resolved_signature_reference parent), name)
  | `Identifier x -> `Identifier x
  | `Alias (_m1, _m2) -> failwith "gah"
  | `Hidden s -> `Hidden (resolved_module_of_resolved_module_reference s)

and resolved_module_of_resolved_signature_reference :
    Reference.Resolved.Signature.t -> Resolved.module_ = function
  | `Identifier (#Identifier.Module.t as i) -> `Identifier i
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
        | `Module (_, n) -> Odoc_model.Names.ModuleName.is_hidden n
        | _ -> false
      in
      `Identifier (x, hidden)
  | `Hidden (`LocalMod x) -> `LocalMod (x, true)
  | `LocalMod x -> `LocalMod (x, false)
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
  | `LocalMod (_, _) as x -> x
  | `Identifier _ as x -> x
  | `Root _ as x -> x
  | `Dot (p, x) -> `Dot (unresolve_module_path p, x)
  | `Module (_, p, x) -> `Dot (unresolve_resolved_parent_path p, x)
  | `Apply (x, y) -> `Apply (unresolve_module_path x, unresolve_module_path y)

and unresolve_resolved_module_type_path : Resolved.module_type -> module_type =
  function
  | (`LocalModTy _ | `Identifier _) as p -> `Resolved p
  | `SubstitutedMT x -> unresolve_resolved_module_type_path x
  | `ModuleType (p, n) -> `DotMT (unresolve_resolved_parent_path p, n)
  | `SubstT (_, m) -> unresolve_resolved_module_type_path m
  | `AliasModuleType (_, m2) -> unresolve_resolved_module_type_path m2
  | `CanonicalModuleType (p, _) -> unresolve_resolved_module_type_path p
  | `OpaqueModuleType m -> unresolve_resolved_module_type_path m

and unresolve_resolved_parent_path : Resolved.parent -> module_ = function
  | `Module m -> unresolve_resolved_module_path m
  | `FragmentRoot _ | `ModuleType _ -> assert false

and unresolve_resolved_type_path : Resolved.type_ -> type_ = function
  | (`Identifier _ | `LocalTy _ | `CoreType _) as p -> `Resolved p
  | `SubstitutedT x -> unresolve_resolved_type_path x
  | `SubstitutedCT x -> unresolve_resolved_type_path (x :> Resolved.type_)
  | `CanonicalType (t1, _) -> unresolve_resolved_type_path t1
  | `Type (p, n) -> `DotT (unresolve_resolved_parent_path p, n)
  | `Class (p, n) -> `DotT (unresolve_resolved_parent_path p, n)
  | `ClassType (p, n) -> `DotT (unresolve_resolved_parent_path p, n)
  | `Unbox p -> `Unbox (unresolve_resolved_type_path p)

and unresolve_resolved_class_type_path : Resolved.class_type -> class_type =
  function
  | (`LocalTy _ | `Identifier _) as p -> `Resolved p
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
