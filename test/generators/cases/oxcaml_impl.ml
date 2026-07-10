let[@zero_alloc] add b x y = if b then x + y else x

module To_be_included = struct
  let[@zero_alloc] add b x y = if b then x + y else x
  (* [add] has a zero alloc annotation that it shouldn't loose *)
end

module Including = struct
  include To_be_included
end

(** {1 Include functor on structures} *)

module Include_functor = struct
(** This module demonstrates the [include functor] functionality *)
  module Make (T : sig type t end) = struct type included end
  type t
  include functor Make
end

module Include_functor_desugared = struct
(** This module is the desugared version from above *)
  module Make (T : sig type t end) = struct type included end
  module DUMMY__ = struct
    type t
  end
  include DUMMY__
  include Make(DUMMY__)
end

module Resolve_functor = struct
  module F ( I : sig type t end ) = struct
    type myt = I.t
  end

  module M = struct
    type t = float
    include functor F
  end
end
