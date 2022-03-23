type t = int


let f : t = 5

module type B = sig
  module A : sig
    type t = int
  end
  include module type of struct include A end
  type t = float
end
