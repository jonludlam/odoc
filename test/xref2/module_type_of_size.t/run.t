Generate some code that's pretty bad for the odoc 2.0-2.3 implentation of
`module type of`. The problem is that the size of the files becomes
enormous very quickly.

Start with a base module:

  $ cat m1.mli
  module type M = sig
    type t
  end
  
  module N : sig type t end
  module T : sig type t type u end

Generate a chain of `module type of`s:

  $ N=14
  $ for i in `seq 2 $N`; do echo "module type M = module type of M$((i-1)) with module N = M1.T" > m$i.mli; echo "module N : sig type t end" >> m$i.mli; done

Compile the modules:

  $ for i in `seq 1 $N`; do ocamlc -c "m$i.mli" -bin-annot; done

  $ for i in `seq 1 $N`; do odoc compile -I . "m$i.cmti"; done

None of the files really ought to be bigger than 1M!

  $ find . -size +1000000c | sort 
  ./m12.odoc
  ./m13.odoc
  ./m14.odoc


