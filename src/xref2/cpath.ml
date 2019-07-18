open Odoc_model.Paths

(* Again, keep one Path.t rather than typed ones for now *)
type resolved = [
    | `Local of Ident.t
    | `Substituted of resolved
    | `Identifier of Odoc_model.Paths.Identifier.t
    | `Subst of resolved * resolved
    | `SubstAlias of resolved * resolved
    | `Hidden of resolved
    | `Module of resolved * Odoc_model.Names.ModuleName.t
    | `Canonical of resolved * t
    | `Apply of resolved * t
    | `Alias of resolved * resolved
    | `ModuleType of resolved * Odoc_model.Names.ModuleTypeName.t
    | `Type of resolved * Odoc_model.Names.TypeName.t
]
and t = [
    | `Substituted of t
    | `Resolved of resolved
    | `Dot of t * string
    | `Apply of t * t
]

exception LocalPath of t
exception TypesNeedRefining

let rec resolved_module_path_of_cpath : resolved -> Path.Resolved.Module.t =
    function
    | `Local _ as y -> raise (LocalPath (`Resolved y))
    | `Substituted y -> resolved_module_path_of_cpath y
    | `Identifier (#Identifier.Module.t as x) -> `Identifier x
    | `Subst (a, b) -> `Subst (resolved_module_type_path_of_cpath a, resolved_module_path_of_cpath b)
    | `SubstAlias (a, b) -> `SubstAlias (resolved_module_path_of_cpath a, resolved_module_path_of_cpath b)
    | `Hidden x -> `Hidden (resolved_module_path_of_cpath x)
    | `Canonical (a, b) -> `Canonical (resolved_module_path_of_cpath a, module_path_of_cpath b)
    | `Apply (a, b) -> `Apply (resolved_module_path_of_cpath a, module_path_of_cpath b)
    | `Alias (a, b) -> `Alias (resolved_module_path_of_cpath a, resolved_module_path_of_cpath b)
    | `Module (p, m) -> `Module (resolved_module_path_of_cpath p, m)
    | _ -> raise TypesNeedRefining

and resolved_module_type_path_of_cpath : resolved -> Path.Resolved.ModuleType.t =
    function
    | `Local _ as y -> raise (LocalPath (`Resolved y))
    | `Substituted y -> resolved_module_type_path_of_cpath y
    | `Identifier (#Identifier.ModuleType.t as x) -> `Identifier x
    | `ModuleType (p, m) -> `ModuleType (resolved_module_path_of_cpath p, m)
    | _ -> raise TypesNeedRefining

and resolved_type_path_of_cpath : resolved -> Path.Resolved.Type.t =
    function
    | `Identifier (#Identifier.Type.t as x) -> `Identifier x
    | `Local _ as y -> raise (LocalPath (`Resolved y))
    | `Substituted y -> resolved_type_path_of_cpath y
    | `Type (p, m) -> `Type (resolved_module_path_of_cpath p, m)
    | _ -> raise TypesNeedRefining

and module_path_of_cpath : t -> Path.Module.t =
    function
    | `Resolved r -> `Resolved (resolved_module_path_of_cpath r)
    | `Dot (p, x) -> `Dot (module_path_of_cpath p, x)
    | `Substituted p -> module_path_of_cpath p
    | _ -> raise TypesNeedRefining

and module_type_path_of_cpath : t -> Path.ModuleType.t =
    function
    | `Resolved r -> `Resolved (resolved_module_type_path_of_cpath r)
    | `Dot (p, x) -> `Dot (module_path_of_cpath p, x)
    | `Substituted p -> module_type_path_of_cpath p
    | _ -> raise TypesNeedRefining

and type_path_of_cpath : t -> Path.Type.t =
    function
    | `Resolved r -> `Resolved (resolved_type_path_of_cpath r)
    | `Dot (p, x) -> `Dot (module_path_of_cpath p, x)
    | `Substituted p -> type_path_of_cpath p
    | _ -> raise TypesNeedRefining

