module type Infix = sig
  type t

  val equal : t -> t -> bool
end

module type Has_t = sig
  type t = int

  module R : sig
    include Infix with type t := t
  end
end

module M : sig
  include Has_t

  (** Shadows the [t] above - [R]'s substitution still refers to the first one. *)
  include sig
    type nonrec t = t
  end
end

(** Mirrors core's [std_internal.ml], which does [include Int.Replace_polymorphic_compare]. *)
module N : sig
  include module type of struct
    include M.R
  end
end
