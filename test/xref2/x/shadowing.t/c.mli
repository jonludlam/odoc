(* Can also inline the module types [B] and [B.Q] here. *)

include module type of struct
  include B
end

module Q : sig
  include module type of struct
    include B.Q
  end

  module U : sig end

  val to_u : unit
  val of_u : unit
end

module U : sig end

val of_u : unit
val to_u : unit
