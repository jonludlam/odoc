Rules S1 to S3 (doc/scope_model.mld, section 5): how a path is shown. A
[@canonical] tag is a reference, resolved as in section 4. If it resolves,
paths to the module are shown as its target, which odoc trusts (S1). If it
does not resolve, the path is shown as it is, and odoc warns (S2, and section
4.5). Hidden units are not linked to (S3). None of this changes what a path
means.

Library [x] is a dune wrapped library. Its alias module [X] is written by
hand here, as dune generates it, with one alias for each hidden unit
[X__M], [X__F], and so on. Most aliases have a [@canonical] tag. The members
are compiled with [-open X], as dune does when there is no separate [X__].
[x]'s unit [Use] has one value for each alias. Library [z] is built against
[x], and has its own aliases to [X.F], [X.L] and [X.I.K], and a module
[Unrelated].
Every compile uses dune's [-no-alias-deps], and [-L] for the unit's own
library and its dependencies.

The source files:

  $ cat x/x__F.mli
  type t
  $ cat x/x__L.mli
  type t
  $ cat x/x__M.mli
  type t
  $ cat x/x__T.mli
  type t

  $ ocamlc -bin-annot -no-alias-deps -w -49 -c x/x.ml
  $ for m in M F W T H L I; do ocamlc -bin-annot -no-alias-deps -I x -open X -c x/x__$m.mli; done
  $ ocamlc -bin-annot -no-alias-deps -I x -open X -c x/use.mli
  $ ocamlc -bin-annot -no-alias-deps -I x -c z/z.mli

  $ for m in M F W T H L I; do odoc compile --output-dir h --parent-id pkg/x -L x:h/pkg/x x/x__$m.cmti; done
  $ odoc compile --output-dir h --parent-id pkg/x -L x:h/pkg/x x/x.cmt
  $ odoc compile --output-dir h --parent-id pkg/x -L x:h/pkg/x x/use.cmti
  $ odoc compile --output-dir h --parent-id pkg/z -L z:h/pkg/z -L x:h/pkg/x z/z.cmti

First, every library is given at link.

  $ for u in x/x__M x/x__F x/x__W x/x__T x/x__H x/x__L x/x__I x/x x/use z/z; do odoc link -L x:h/pkg/x -L z:h/pkg/z h/pkg/$u.odoc; done

How to read [odoc_print --short --long-paths --show-root-parents]. Each line
is one item of the signature. [x/0] is the item [x], with a number odoc adds
to tell apart items of the same name. [resolved(...)] is a resolved path, and
[root(pkg/x/X)] is the unit [X] of the library [x] in the package [pkg].
[canonical(P,resolved(Q))] is the path [P], whose canonical tag resolved to
[Q]: [P] is shown as [Q]. [canonical(P,Q)], where [Q] is not [resolved(...)],
is the path [P], whose canonical tag did not resolve. [root(Z)] is a root
[Z] that is not resolved. [hidden(P)] is a path [P] to a hidden module.

[show NAME] prints the line of the value [NAME] of [Use].

  $ show () { odoc_print --short --long-paths --show-root-parents h/pkg/x/use.odocl | grep "^val $1/"; }

S1.1. A canonical within one library. [m]'s path [X.M] should be shown as
[X.Pub], the target of [X.M]'s tag.

  $ cat x/x.ml
  (** @canonical X.Pub *)
  module M = X__M
  
  module Pub = X__M
  
  (** @canonical Z.F *)
  module F = X__F
  
  (** @canonical Z.Unrelated *)
  module W = X__W
  
  (** @canonical Z.Mispelt *)
  module T = X__T
  
  module H = X__H
  
  (** @canonical /z/Z.L *)
  module L = X__L
  
  module I = X__I
  $ show m
  val m/1 : resolved(canonical(root(pkg/x/X).M,resolved(root(pkg/x/X).Pub)).t)

S1.2. A forward canonical, as eio uses. [x] names [Z.F] as the canonical of
[X.F], and [z] is built against [x], after it. R3 looks in the libraries
given at link, so the tag should resolve to [z]'s [Z.F], and [f]'s path
should be shown as [Z.F].

  $ cat x/x__I.mli
  module K : sig
    type t
  end
  (** @canonical Z.K *)
  $ cat x/use.mli
  val m : M.t
  
  val f : F.t
  
  val w : W.t
  
  val t : T.t
  
  val h : H.t
  
  val h2 : X__H.t
  
  val l : L.t
  
  type k = X__I.K.t
  $ cat z/z.mli
  module F = X.F
  
  module Unrelated : sig
    type u
  end
  
  module L = X.L
  
  module K = X.I.K
  $ show f
  val f/2 : resolved(canonical(root(pkg/x/X).F,resolved(root(pkg/z/Z).F)).t)

The same holds for a tag inside a hidden unit, the shape of eio's internal
libraries. [X__I] has a module [K] with the tag [Z.K], and [Use] has
[type k = X__I.K.t]. The path names the hidden unit [X__I]. The tag should
resolve to [z]'s [Z.K], so [k]'s path should be shown as [Z.K], and the
equation of [k] should be kept. (Link removes the equation of a type whose
path stays hidden, so a tag that fails to resolve would lose it.)

  $ odoc_print --short --long-paths --show-root-parents h/pkg/x/use.odocl | grep "^type k/"
  type k/0 = resolved(canonical(hidden(root(pkg/x/X__I)).K,resolved(root(pkg/z/Z).K)).t)

S1.3. A canonical that is wrong. [X.W]'s tag names [Z.Unrelated], which is
not [X__W]. odoc trusts the tag, so [w]'s path should be shown as
[Z.Unrelated], with no warning.

  $ cat x/x__W.mli
  type t
  $ show w
  val w/3 : resolved(canonical(root(pkg/x/X).W,resolved(root(pkg/z/Z).Unrelated)).t)

S2.2. A misspelt canonical. [X.T]'s tag names [Z.Mispelt], which does not
exist. [t]'s path should be shown as [X.T], as it is. The tag should get a
warning when [Use] is linked (section 4.5).

A link of [Use] above with no warning for [Z.Mispelt] would be wrong.

  $ show t
  val t/4 : resolved(canonical(root(pkg/x/X).T,root(Z).Mispelt).t)

S1.4. A canonical tag with the [/library/] syntax, [/z/Z.L]. S1 resolves a
tag as a reference, and R3 restricts [{!/library/N}] to that library. So the
tag should resolve to [z]'s [Z.L], and [l]'s path should be shown as [Z.L].

  $ show l
  val l/7 : resolved(canonical(root(pkg/x/X).L,root((/z/Z)).L).t)

S3.1. Hidden units are not linked to. [h]'s path [X.H] is an alias of the
hidden unit [X__H] with no tag, and [h2]'s path names [X__H] itself. Neither
link should go to a page of [X__H]. The links are read from the HTML: [code
NAME] prints the HTML of the type of the value [NAME].

  $ cat x/x__H.mli
  type t
  $ odoc html-generate -o html h/pkg/x/use.odocl
  $ code () { grep -oE "id=\"val-$1\">.*" html/pkg/x/Use/index.html | grep -oE '<code>.*</code>' | sed 's|</code>.*||'; }
  $ code h
  <code><span><span class="keyword">val</span> h : <a href="../X/H/index.html#type-t">X.H.t</a></span>
  $ code h2
  <code><span><span class="keyword">val</span> h2 : <span class="xref-unresolved">X__H.t</span></span>

S2.1. A canonical whose target library is not given at link. [Use] is linked
again, with only [x]. The tag of [X.F] names [Z.F], and [z] is not given, so
the tag should not resolve. [f]'s path should be shown as [X.F], as it is,
and the tag should get a warning (section 4.5). [z] is not in [Λc] of [Use],
so the warning of D6 does not apply.

A link with no warning for [Z.F] would be wrong.

  $ odoc link -L x:h/pkg/x h/pkg/x/use.odoc
  $ show f
  val f/2 : resolved(canonical(root(pkg/x/X).F,root(Z).F).t)
