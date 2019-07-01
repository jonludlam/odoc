(* Again, keep one Path.t rather than typed ones for now *)
type t = [
    | `Local of Ident.t
    | `Dot of t * string
    | `Substituted of t
    | `Apply of t * t
    | `Global of Odoc_model.Paths.Path.t
]
