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
