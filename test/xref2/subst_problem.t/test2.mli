module type S1 = sig
  type t0
  type 'a t := unit

  val x : t0 t
end

module type S2 = sig
  type t (* must be the same name as [S1.t] *)

  include S1 with type t0 := t
end

module type S3 = sig
  type t1

  include S2 with type t := t1
end
