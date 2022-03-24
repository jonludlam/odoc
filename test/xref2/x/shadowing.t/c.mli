(* Can also inline the module types [B] and [B.Q] here. *)

include module type of struct
  include B
end

module Q : sig
  include module type of struct
    include B.Q
  end

  module U : sig end

end

module U : sig end
