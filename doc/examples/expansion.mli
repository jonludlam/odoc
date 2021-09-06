module Simple : sig
    module StringSet : Stdlib.Set.S with type t = string
end

module Aliases : sig

    module Hidden__module : sig
        type t
        val f : t -> t
    end

    module Alias = Hidden__module
end

module DeepConstraint : sig
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
