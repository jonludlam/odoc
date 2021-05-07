type sexp = Sexplib0.Sexp.t = Atom of string | List of sexp list

val gen_targets : Fpath.t list -> sexp list
(** Generates targets for a corresponding backend end specified in [odoc <backend>-generate ...] *)

val gen_backend_rules :
  string -> (Fpath.t -> Fpath.t list -> sexp) -> Fpath.t list -> sexp list

val files : Fpath.t list
