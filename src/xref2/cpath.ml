(* Again, keep one Path.t rather than typed ones for now *)
type resolved = [
    | `Local of Ident.t
    | `Substituted of resolved
    | `Apply of resolved * t
    | `Alias of resolved * resolved
    | `Module of resolved * Odoc_model.Names.ModuleName.t
    | `ModuleType of resolved * Odoc_model.Names.ModuleTypeName.t
    | `Type of resolved * Odoc_model.Names.TypeName.t
    | `Identifier of Odoc_model.Paths.Identifier.t

]
and t = [
    | `Resolved of resolved
    | `Dot of t * string
    | `Apply of t * t
]
