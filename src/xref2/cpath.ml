open Odoc_model.Paths
open Odoc_model.Names

type lmod = [ Ident.module_ | Odoc_model.Paths.na ]
type lmodty = [ Ident.module_type | Odoc_model.Paths.na ]
type lty = [ Ident.type_ | Odoc_model.Paths.na ]
type lval = [ Ident.value | Odoc_model.Paths.na ]
type pty = [ `U | Odoc_model.Paths.na ]

module rec Resolved : sig
  type parent = (lmod, lmodty, pty) Odoc_model.Paths.Path.Resolved.parent_gen

  type module_ = (lmod, lmodty, pty) Odoc_model.Paths.Path.Resolved.Module.gen

  type module_type =
    (lmod, lmodty, pty) Odoc_model.Paths.Path.Resolved.ModuleType.gen

  type type_ =
    (lmod, lmodty, pty, lty) Odoc_model.Paths.Path.Resolved.Type.gen

  type class_type =
    (lmod, lmodty, pty, lty) Odoc_model.Paths.Path.Resolved.ClassType.gen

  type value =
    (lmod, lmodty, pty, lval) Odoc_model.Paths.Path.Resolved.Value.gen

  type any =
    (lmod, lmodty, pty, lty, lval) Odoc_model.Paths.Path.Resolved.gen
end =
  Resolved

and Cpath : sig
  type module_ = (lmod, lmodty, pty) Odoc_model.Paths.Path.Module.gen

  type module_type = (lmod, lmodty, pty) Odoc_model.Paths.Path.ModuleType.gen
  type type_ = (lmod, lmodty, pty, lty) Odoc_model.Paths.Path.Type.gen

  type class_type =
    (lmod, lmodty, pty, lty) Odoc_model.Paths.Path.ClassType.gen

  type value = (lmod, lmodty, pty, lval) Odoc_model.Paths.Path.Value.gen

  type any = (lmod, lmodty, pty, lty, lval) Odoc_model.Paths.Path.gen
end =
  Cpath

include Cpath

let hidden_fns :
    (lmod, lmodty, pty, lty, lval, bool) Odoc_model.Paths.Path.genfn5 =
  {
    g =
      {
        lmod =
          (function `LModule (n, _) -> ModuleName.is_hidden n | `Na _ -> .);
        lmodty =
          (function
          | `LModuleType (n, _) -> ModuleTypeName.is_hidden n | `Na _ -> .);
        pty = (function _ -> false);
      };
    lty = (function `LType (n, _) -> TypeName.is_hidden n | `Na _ -> .);
    lval = (function `LValue (n, _) -> ValueName.is_hidden n | `Na _ -> .);
  }

let is_hidden : any -> bool =
  Odoc_model.Paths.Path.is_path_hidden_gen hidden_fns

let is_resolved_hidden : weak_canonical_test:bool -> Resolved.any -> bool =
  Odoc_model.Paths.Path.is_resolved_hidden_gen hidden_fns

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
  | `ModuleType (m, `U) -> is_resolved_module_type_substituted m
  | `FragmentRoot `U -> false
  | `ModuleType (_, `Na _) -> .
  | `FragmentRoot (`Na _) -> .

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
  | `SubstitutedT _ -> true
  | `SubstitutedCT _ -> true
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
  | `Module (_, p, _) -> is_resolved_parent_substituted p
  | `Forward _ -> false
  | `Root _ -> false

let is_module_type_substituted : module_type -> bool = function
  | `Resolved a -> is_resolved_module_type_substituted a
  | `Identifier _ -> false
  | `LocalModTy _ -> false
  | `SubstitutedMT _ -> true
  | `DotMT (a, _) -> is_module_substituted a
  | `ModuleType (_, p, _) -> is_resolved_parent_substituted p

let is_type_substituted : type_ -> bool = function
  | `Resolved a -> is_resolved_type_substituted a
  | `Identifier _ -> false
  | `LocalTy _ -> false
  | `SubstitutedT _ -> true
  | `SubstitutedCT _ -> true
  | `DotT (a, _) -> is_module_substituted a
  | `Type (_, p, _) -> is_resolved_parent_substituted p

let is_class_type_substituted : class_type -> bool = function
  | `Resolved a -> is_resolved_class_type_substituted a
  | `Identifier _ -> false
  | `SubstitutedCT _ -> true
  | `DotT (a, _) -> is_module_substituted a
  | `LocalTy _ -> false
  | `Type (_, p, _) -> is_resolved_parent_substituted p

let rec is_module_forward : module_ -> bool = function
  | `Forward _ -> true
  | `Resolved _ -> false
  | `Root _ -> false
  | `Identifier _ -> false
  | `LocalMod _ -> false
  | `Substituted p | `Dot (p, _) | `Apply (p, _) -> is_module_forward p
  | `Module (_, _, _) -> false

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
  | `Identifier ({ iv = #Identifier.Module.t_pv; _ } as i) -> `Identifier i
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
      ( (( `Resolved (`Identifier { iv = #Identifier.Module.t_pv; _ })
         | `Dot (_, _)
         | `Module (_, _) ) as parent),
        name ) ->
      `Dot (module_of_module_reference parent, ModuleName.make_std name)
  | `Module
      ( (( `Resolved (`Identifier { iv = #Identifier.Module.t_pv; _ })
         | `Dot (_, _)
         | `Module (_, _) ) as parent),
        name ) ->
      `Dot (module_of_module_reference parent, name)
  | _ -> failwith "Not a module reference"

let rec unresolve_resolved_module_path : Resolved.module_ -> module_ = function
  | `Hidden (`Identifier x) -> `Identifier (x, true)
  | `Identifier x ->
      let hidden =
        match x.iv with
        | `Module (_, n) -> Odoc_model.Names.ModuleName.is_hidden n
        | _ -> false
      in
      `Identifier (x, hidden)
  | `Hidden (`LocalMod x) -> `LocalMod x
  | `LocalMod x -> `LocalMod x
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
  | `LocalMod _ as x -> x
  | `Identifier _ as x -> x
  | `Root _ as x -> x
  | `Forward _ as x -> x
  | `Dot (p, x) -> `Dot (unresolve_module_path p, x)
  | `Module (_, p, x) -> `Dot (unresolve_resolved_parent_path p, x)
  | `Apply (x, y) -> `Apply (unresolve_module_path x, unresolve_module_path y)

and unresolve_resolved_module_type_path : Resolved.module_type -> module_type =
  function
  | `LocalModTy _ as p -> p
  | `Identifier x -> `Identifier (x, false)
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
  | (`LocalTy _ | `Identifier _) as p -> `Resolved p
  | `SubstitutedT x -> unresolve_resolved_type_path x
  | `SubstitutedCT _x -> failwith "unhandled"
  | `CanonicalType (t1, _) -> unresolve_resolved_type_path t1
  | `Type (p, n) -> `DotT (unresolve_resolved_parent_path p, n)
  | `Class (p, n) -> `DotT (unresolve_resolved_parent_path p, n)
  | `ClassType (p, n) -> `DotT (unresolve_resolved_parent_path p, n)

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
  | `Resolved m -> unresolve_resolved_type_path m
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
  match id.iv with
  | `Module (sg, name) -> (
      match original_path_parent_identifier sg with
      | Some sg' -> Some (`Module (sg', name))
      | None -> None)
  | `Root _ -> Some (`Gpath (`Identifier id))
  | _ ->
      None

and original_path_parent_identifier :
    Odoc_model.Paths.Identifier.Signature.t -> Resolved.parent option =
 fun id ->
  match id with
  | { iv = `Module _ | `Root _ | `Parameter _ | `Result _; _ } as mid -> (
      match original_path_module_identifier mid with
      | Some m -> Some (`Module m)
      | None -> None)
  | { iv = `ModuleType _; _ } as mtid -> (
      match original_path_module_type_identifier mtid with
      | Some m -> Some (`ModuleType m)
      | None -> None)

and original_path_module_type_identifier :
    Odoc_model.Paths.Identifier.ModuleType.t -> Resolved.module_type option =
 fun id ->
  match id.iv with
  | `ModuleType (sg, name) -> (
      match original_path_parent_identifier sg with
      | Some sg' -> Some (`ModuleType (sg', name))
      | None -> None)
*)
