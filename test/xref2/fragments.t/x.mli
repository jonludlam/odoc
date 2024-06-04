module type S = sig
  module type S
  module T : S
end

module type T2 = sig
  type t
end

module type T3 = S with module type S = T2 and type T.t = int

(*
Resolution of the `T3` module type:

Step 1:

Create expansion:

module type T3 = sig
  module type S = T2
  module T : S with type t = int
end

Resolve expansion of T3.

No expansion for `module type S = T2` as it's a 'module type alias'

Resolve expansion of `module T : S with type t = int`:

Create expansion:

module T = sig
  type t = int
end

resove module type expr `S with type t = int`

Question: why is `S` an opaque module type??

Answer: because it was an opaque module type in the original `S` module type. It should have been invalidated by the substitution!










*)
(* 
module M : sig
  module F : sig
    module type X
    module type S = sig
      module N : X
    end
    module Z : S
  end
  module type T = sig
    module type S = sig type t end
  end
  module type O = module type of F with module type X = T
end *)