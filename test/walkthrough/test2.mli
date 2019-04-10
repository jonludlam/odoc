module type S1 = sig type t1 end
module type S2 = sig type t2 end

module T(X : S1) (Y : S2) : sig
  type v = X.t1
  type w = Y.t2
  type z = v
end

module A : sig
  type t1 = Hello
end

module B : sig
  type t2 = Hi
end

type w = T(A)(B).v
