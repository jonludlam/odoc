module X : sig
  type t
end

module type S = sig
  type t
  module type M = sig type u end
  module A : sig type t end
  module N : M
  type v = A.t
end

module type T1 = S with type t = int
module type T2 = S with type A.t = X.t
module type T3 = S with module A = X
module type T4 = S with type t := int
module type T5 = S with module type M = sig type u end
module type T6 = S with type N.u = int
