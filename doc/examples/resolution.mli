(** Examples of Path, Fragment and Reference Resolution *)

(** This module contains examples of some of the features of Resolution
as described in the page {!page-features}. See the explanations there for
details on what each of these demonstrates. *)

module Alias : sig
  (** Demonstrates a reference to an item in a module that's an alias *)

  module A : sig
    type t
  end
  
  module B = A
  
  type t = B.t
end

module HiddenAlias : sig
  (** Demonstrates a reference to an item in a module that's an alias of a
  hidden module. *)

  (**/**)

  module A : sig
    type t
  end

  (**/**)

  module B = A
  
  type t = B.t

end

module Canonical : sig
  (** Demonstrates the use of canonical tags *)

  module A : sig
    (** @canonical Odoc_examples.Resolution.Canonical.B *)

    type t
  end

  module B = A

  type t = A.t
end
