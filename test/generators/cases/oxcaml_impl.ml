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
(** This module demonstrates the [include functor] functionality.  [Make] uses
    its argument, so [included] has to come out equal to [t] rather than
    abstract. *)
  module Make (T : sig type t end) = struct type included = T.t end
  type t
  include functor Make
end

module Include_functor_desugared = struct
(** This module is the desugared version from above *)
  module Make (T : sig type t end) = struct type included = T.t end
  type t
  module DUMMY__ = struct
    type nonrec t = t
  end
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

module Multiple_include_functors = struct
(** Two [include functor]s in the same structure, with an item defined between
    them. *)
  module First (T : sig type t end) = struct type first = T.t end

  module Second (T : sig type t type first type between end) = struct
    type second = T.first
    type third = T.between
  end

  type t
  include functor First
  type between
  include functor Second
end

module Include_functor_not_last = struct
(** An [include functor] that is not the last item of the structure. *)
  module Make (T : sig type t end) = struct type included = T.t end

  type t
  include functor Make
  type after = string
end

module Anonymous_functor = struct
(** The functor is defined inline, so there is no path for odoc to apply: it
    has to bind the functor to a module first, as it does for every
    [include functor] in a signature. *)
  type t
  include functor (functor (T : sig type t end) -> struct type included = T.t end)
end
