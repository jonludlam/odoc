(* Again, keep one Path.t rather than typed ones for now *)
type t = [
    | `Local of Ident.t
    | `Ldot of t * string
    | `Global of Model.Paths.Path.t
]
