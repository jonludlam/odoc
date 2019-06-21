(* Again, keep one Path.t rather than typed ones for now *)
type t = [
    | `Local of Ident.t
    | `Dot of t * string
    | `Global of Odoc_model.Paths.Path.t
]
