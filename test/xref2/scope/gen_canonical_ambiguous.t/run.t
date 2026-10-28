Rules S1 and S2 (section 5), with R3 (section 4.2): a [@canonical] tag is a
reference. If it has two candidates, it is ambiguous: odoc should warn, and
show the path as it is (S2).

Libraries [p] and [q] are unwrapped. Each has a hidden unit [Gamma__Impl],
and a unit [Gamma] with [module Impl = Gamma__Impl] and the tag
[@canonical Gamma.Impl]. [p]'s [Gamma__Impl] has [module type S], and
[p]'s [Gamma] has [module M : Impl.S]. [q]'s [Gamma__Impl] has only
[type u]. Neither library depends on the other, so no unit's Λc holds two
[Gamma]s (D3). Both are given at link.

  $ cat p/gamma__Impl.mli
  module type S = sig end
  $ cat q/gamma__Impl.mli
  type u
  $ cat p/gamma.mli
  (** @canonical Gamma.Impl *)
  module Impl = Gamma__Impl
  
  module M : Impl.S
  $ cat q/gamma.mli
  (** @canonical Gamma.Impl *)
  module Impl = Gamma__Impl
  $ for l in p q; do
  >   ocamlc -bin-annot -no-alias-deps -I $l -c $l/gamma__Impl.mli
  >   ocamlc -bin-annot -no-alias-deps -I $l -c $l/gamma.mli
  >   odoc compile --output-dir h --parent-id pkg/$l -L $l:h/pkg/$l $l/gamma__Impl.cmti
  >   odoc compile --output-dir h --parent-id pkg/$l -L $l:h/pkg/$l $l/gamma.cmti
  > done

How to read [odoc_print --short --long-paths --show-root-parents]. Each line
is one item of the signature. [M/0] is the item [M], with a number odoc adds
to tell apart items of the same name. [resolved(...)] is a resolved path,
and [r(...)] a resolved module type path. [root(pkg/p/Gamma)] is the unit
[Gamma] of the library [p] in the package [pkg]. [hidden(...)] marks a path
to a hidden unit, and [canonical(p,resolved(q))] a path [p] whose canonical
path is [q].

The tag [Gamma.Impl] has two candidates in Λl, [p]'s [Gamma] and [q]'s
[Gamma]. odoc should warn that it is ambiguous. [Impl.S] should then be
shown as it is, with no canonical path, and link to [p]'s own page for
[S] ([Impl/module-type-S/index.html]).

A canonical path, to either library's [Gamma.Impl], would be wrong, even
one that links to the right page. So would a link with no warning.

  $ odoc link -L p:h/pkg/p -L q:h/pkg/q h/pkg/p/gamma.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/p/gamma.odocl
  module Impl/1 = resolved(hidden(root(pkg/p/Gamma__Impl))) 
  module M/0 : r(canonical(Impl/1,resolved(root(pkg/p/Gamma).Impl)).S) 
  $ odoc html-generate -o html h/pkg/p/gamma.odocl
  $ grep -o 'href="[^"]*module-type-S[^"]*"' html/pkg/p/Gamma/index.html
  href="Impl/module-type-S/index.html"
