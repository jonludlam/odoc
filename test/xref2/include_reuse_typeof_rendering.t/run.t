Members that come from `include module type of X`, where X is an alias to a
module containing an include, render as aliases
([Make = X0.Make]) — unless a `with type t := ...` substitution triggers a
re-derivation that expands them in place instead. [User] and [User2] below
differ only in the substitution, and render differently. Both forms are
sound: `module type of <alias>` is fully strengthened by the compiler.
Note the abstract type [u]: re-derivation loses its [= X0.u] manifest in
[User], while [User2] keeps it.
The include matters because re-derivation happens per include: members
declared directly in X0 render as aliases either way.

  $ ocamlc -c -bin-annot repro.mli
  $ odoc compile repro.cmti
  $ odoc link repro.odoc

  $ odoc_print --short --show-include-expansions repro.odocl
  module type S = 
    sig
      type t
      type u
      val v : u
      module type H = sig val h : int end
      module Make : (X/11 : H) -> sig val mk : t end
    end
  module X0 : 
    sig
      include S
        (sig :
          type t
          type u
          val v : u
          module type H = sig val h : int end
          module Make : (X/20 : H) -> sig val mk : t end
         end)
    end module X = X0
  module User : 
    sig
      type t = X.t
      include module type of X with [t(params ) = X.t]
        (sig :
          include S with [t(params ) = X.t]
            (sig :
              type u
              val v : u
              module type H = sig val h : int end
              module Make : (X/29 : H) -> sig val mk : X.t end
             end)
         end)
    end
  module User2 : 
    sig
      include module type of X
        (sig :
          include S
            (sig :
              type t = X0.t
              type u = X0.u
              val v : u
              module type H = X0.H
              module Make = X0.Make
             end)
         end)
    end
