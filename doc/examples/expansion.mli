(** Examples of different features of Expansion *)

(** For details on what each of the following examples is showing,
see the explanations in the {{!page-features}Features page} *)

module Simple : sig
    (** Demonstrates simple expansion with a type equality *)

    module StringSet : Stdlib.Set.S with type t = string
end

module Aliases : sig
    (** Demonstrates expansion when a module is an alias to a hidden module. *)

    module Hidden__module : sig
        type t
        val f : t -> t
    end

    module Alias = Hidden__module
end

module DeepConstraint : sig
    (** Demonstrates expansion involving a constraint on a submodule *)

    module type SIG = sig
        type t
    end
    
    module type MODTYPE = sig
        module X : SIG
        module Y : SIG
    end
    
    type foo
    
    module M : MODTYPE with type X.t = foo
end

module DeepConstraint2 : sig
    (** Demonstrates expansion involving a constraint on a submodule, but the submodule is already a simple signature *)

    module type MODTYPE = sig
        module X : sig type t end
        module Y : sig type t end
    end
    
    type foo
    
    module M : MODTYPE with type X.t = foo
end

module TypeSubstitution : sig
    module type SIG = sig
        module M : sig
            type t
            val func : t -> t
        end
        module N : sig
            val func2 : M.t -> M.t
        end
    end
    
    module X : SIG with type M.t = string
end

module ModuleTypeOf : sig
    (** Demonstrates expanding after recovering the type of a module *)

    module A : sig

        (** This comment for [type t] is written in module [A] *)
        type t
      
    end
      
    module M : module type of A
    module M' : module type of struct include A end
end

module ModuleTypeOfComplications : sig
    (** Demonstrates the interaction of [module type of] and destructive module substitution *)

    module type S = sig
        module X : sig
            type t
        end

        module type Y = module type of X
        module type Z = module type of struct include X end
    end

    module X1 : sig
        type t
        type u
    end
    
    module type T = S with module X := X1
end
