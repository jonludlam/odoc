module type S = sig
  module X : sig
    type t
  end
  (** @canonical Lib.X *)
end

module type P = sig
  module X : sig
    type t
  end
  (** @canonical /w/Lib.X *)
end
