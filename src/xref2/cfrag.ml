(*open Odoc_model.Paths*)
open Odoc_model.Names

type root = 
  [ `ModuleType of Cpath.Resolved.module_type
  | `Module of Cpath.Resolved.module_ ]

type resolved_signature =
  [ `Root of root
  | `Subst of Cpath.Resolved.module_type * resolved_module
  | `SubstAlias of Cpath.Resolved.module_ * resolved_module
  | `Module of resolved_signature * ModuleName.t
  | `OpaqueModule of resolved_module]

and resolved_module =
  [ `Subst of Cpath.Resolved.module_type * resolved_module
  | `SubstAlias of Cpath.Resolved.module_ * resolved_module
  | `Module of resolved_signature * ModuleName.t
  | `OpaqueModule of resolved_module ]

and resolved_type =
  [ `Type of resolved_signature * TypeName.t
  | `Class of resolved_signature * ClassName.t
  | `ClassType of resolved_signature * ClassTypeName.t ]

(* and signature = [ `Resolved of resolved_signature ] *)


type signature = [
  | `Resolved of resolved_signature
  | `Dot of signature * string
  | `Root
]

and module_ = [
  | `Resolved of resolved_module
  | `Dot of signature * string
]

and type_ = [
  | `Resolved of resolved_type
  | `Dot of signature * string
]

type resolved_base_name =
  | RBase of root
  | RBranch of ModuleName.t * resolved_signature

type base_name =
  | Base of root option
  | Branch of ModuleName.t * signature

let rec resolved_signature_split_parent : resolved_signature -> resolved_base_name = function
  | `Root i -> RBase i
  | `Subst(_, p) -> resolved_signature_split_parent (p :> resolved_signature)
  | `SubstAlias(_, p) -> resolved_signature_split_parent (p :> resolved_signature)
  | `OpaqueModule m -> resolved_signature_split_parent (m :> resolved_signature)
  | `Module(p, name) ->
    match resolved_signature_split_parent p with
    | RBase i -> RBranch(name, `Root i)
    | RBranch(base, m) -> RBranch(base, `Module(m, name))


let rec signature_split_parent : signature -> base_name =
  function
  | `Root -> Base None
  | `Resolved r -> begin
      match resolved_signature_split_parent r with
      | RBase i -> Base (Some i)
      | RBranch(base, m) -> Branch(base, `Resolved m)
    end
  | `Dot(m,name) -> begin
      match signature_split_parent m with
      | Base None -> Branch(ModuleName.of_string name, `Root)
      | Base (Some i) -> Branch(ModuleName.of_string name, `Resolved (`Root i))
      | Branch(base,m) -> Branch(base, `Dot(m,name))
    end

let rec resolved_module_split : resolved_module -> string * resolved_module option = function
| `Subst(_,p) -> resolved_module_split p
| `SubstAlias(_,p) -> resolved_module_split p
| `Module (m, name) -> begin
    match resolved_signature_split_parent m with
    | RBase _ -> (ModuleName.to_string name, None)
    | RBranch(base,m) -> ModuleName.to_string base, Some (`Module(m,name))
  end
| `OpaqueModule m -> resolved_module_split m

  let module_split : module_ -> string * module_ option = function
  | `Resolved r ->
    let base, m = resolved_module_split r in
    let m =
      match m with
      | None -> None
      | Some m -> Some (`Resolved m)
    in
    base, m
  | `Dot(m, name) ->
    match signature_split_parent m with
    | Base _ -> name, None
    | Branch(base, m) -> ModuleName.to_string base, Some(`Dot(m, name))

let resolved_type_split : resolved_type -> string * resolved_type option =
function
| `Type (m,name) -> begin
    match resolved_signature_split_parent m with
    | RBase _ -> TypeName.to_string name, None
    | RBranch(base, m) -> ModuleName.to_string base, Some (`Type(m, name))
  end
| `Class(m, name) -> begin
    match resolved_signature_split_parent m with
    | RBase _ -> ClassName.to_string name, None
    | RBranch(base, m) -> ModuleName.to_string base, Some (`Class(m, name))
  end
| `ClassType(m, name) -> begin
    match resolved_signature_split_parent m with
    | RBase _ -> ClassTypeName.to_string name, None
    | RBranch(base, m) -> ModuleName.to_string base, Some (`ClassType(m, name))
  end

  let type_split : type_ -> string * type_ option = function
      | `Resolved r ->
        let base, m = resolved_type_split r in
        let m =
          match m with
          | None -> None
          | Some m -> Some (`Resolved m)
        in
        base, m
      | `Dot(m, name) ->
        match signature_split_parent m with
        | Base _ -> name, None
        | Branch(base, m) -> ModuleName.to_string base, Some(`Dot(m, name))
