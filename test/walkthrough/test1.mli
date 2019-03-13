(* This is my type *)
(*type t = int*)

(*module F ( X : sig module type S module T : S end ) : sig module N : X.S end*)

module type S = sig
    type sx
end

module type T = sig
    type st
end

module F (IS : S) (IT : T) : sig
    type res

    type mysx = IS.sx

    type myit = IT.st
end

module M : sig
    module type S = sig
        type t
    end
    
    module T : S

end

(*type s = F(M).N.t*)

type v = M.T.t

