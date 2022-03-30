module CanonicalTest : sig
  module Base__List : sig
    type 'a t

    val id : 'a t -> 'a t
  end

  module Base__ : sig
    module List = Base__List
    (** @canonical M.CanonicalTest.Base.List *)
  end

  module Base : sig
    module List = Base__.List
  end

  module Base_Tests : sig
    module C : module type of Base__.List

    open Base__
    module L = List

    val foo : int L.t -> float L.t

    val bar : 'a List.t -> 'a List.t
    (* This is just {!List.id}, or rather {!L.id} *)

    val baz : 'a Base__.List.t -> unit
    (* We can't reference [Base__] because it's hidden.
        {!List.t} ([List.t]) should resolve. *)
  end

  module List_modif : module type of Base.List with type 'c t = 'c Base__.List.t
end

(* val test : 'a CanonicalTest.Base__.List.t -> unit *)
(** Some ref to {!CanonicalTest.Base_Tests.C.t} and {!CanonicalTest.Base_Tests.L.id}.
    But also to {!CanonicalTest.Base.List} and {!CanonicalTest.Base.List.t} *)


