type t

val sexp_of_t : unit
val f : unit
val of_i : unit

module Q : sig
  module U : sig end

  val to_u : unit
  val of_u : unit
end

module U : sig end

val of_u : unit
val to_u : unit
val to_u_exn : unit
