module type S = sig
  module A : sig type t = int end
  module B = A
end

module type T = S with type B.t = int
