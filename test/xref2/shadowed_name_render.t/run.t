When a signature item is shadowed by a later one, odoc gives the shadowed item
a disambiguated internal name so that it still has a unique identifier. That
name must never reach the output.

Here `M`'s first `t` is shadowed by the `include` at the end, but `M.R`'s
destructive substitution still refers to the first one, and `N` includes `M.R`:

  $ cat test.mli
  module type Infix = sig
    type t
  
    val equal : t -> t -> bool
  end
  
  module type Has_t = sig
    type t = int
  
    module R : sig
      include Infix with type t := t
    end
  end
  
  module M : sig
    include Has_t
  
    (** Shadows the [t] above - [R]'s substitution still refers to the first one. *)
    include sig
      type nonrec t = t
    end
  end
  
  (** Mirrors core's [std_internal.ml], which does [include Int.Replace_polymorphic_compare]. *)
  module N : sig
    include module type of struct
      include M.R
    end
  end

  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  $ odoc link test.odoc
  $ odoc html-generate --indent -o html test.odocl

The shadowed `t` is `int`, and the compiler only permits the shadowing because
that makes it re-expressible. odoc should say so, rather than falling back on
the internal disambiguated name:

  $ grep -A4 'val</span> equal' html/Test/N/index.html
            <span><span class="keyword">val</span> equal : 
             <span>int <span class="arrow">&#45;&gt;</span></span> 
             <span>int <span class="arrow">&#45;&gt;</span></span> bool
            </span>
           </code>

Nothing anywhere in the output should contain the internal form:

  $ grep -rl 'shadowed/(' html/ || echo "no internal names leaked"
  no internal names leaked
