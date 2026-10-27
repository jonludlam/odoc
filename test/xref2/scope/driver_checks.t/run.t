The conditions on the driver (doc/scope_model.mld, section 2), and what odoc
checks of them.

The libraries:

- [ext] has [Ext], with [type t].
- [l] has [L], with [val x : Ext.t], as an interface and an implementation.
It is built against [ext].
- [n] has [module X : module type of L]. It is built against [l], and so, by
closure, [ext].
- [ext2] has [Ext2], with [type t].
- [r] has [module Y = Ext2]. It is built against [ext2], with
[-no-alias-deps], so [r] records no digest for [Ext2].
- [p] has a comment with the reference [{!R.Y.t}]. It is built against no
library: the reference is a forward reference to [r].

Every compile uses dune's [-no-alias-deps], and [-L] for the unit's own
library and its dependencies.

  $ cat ext/ext.mli
  type t
  $ cat n/n.mli
  module X : module type of L
  $ cat p/p.mli
  (** See {!R.Y.t}. *)
  val v : int
  $ ocamlc -bin-annot -no-alias-deps -c ext/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext -c l/l.mli
  $ ocamlc -bin-annot -no-alias-deps -I l -I ext -c l/l.ml
  $ ocamlc -bin-annot -no-alias-deps -I l -I ext -c n/n.mli
  $ ocamlc -bin-annot -no-alias-deps -c ext2/ext2.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext2 -c r/r.mli
  $ ocamlc -bin-annot -no-alias-deps -c p/p.mli

  $ odoc compile --output-dir h --parent-id pkg/ext -L ext:h/pkg/ext ext/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext:h/pkg/ext l/l.cmti
  $ odoc compile-impl --output-dir h --parent-id pkg/l --source-id pkg/l/l.ml -L l:h/pkg/l -L ext:h/pkg/ext l/l.cmt
  $ odoc compile --output-dir h --parent-id pkg/n -L n:h/pkg/n -L l:h/pkg/l -L ext:h/pkg/ext n/n.cmti
  $ odoc compile --output-dir h --parent-id pkg/ext2 -L ext2:h/pkg/ext2 ext2/ext2.cmti
  $ odoc compile --output-dir h --parent-id pkg/r -L r:h/pkg/r -L ext2:h/pkg/ext2 r/r.cmti
  $ odoc compile --output-dir h --parent-id pkg/p -L p:h/pkg/p p/p.cmti

D.1. The D6 check (section 2): at the link of a unit, every library that a
unit whose paths are resolved was compiled with should be given, and odoc
should warn about each one that is missing. [n] shows the expansion of [L],
whose origin [l] was compiled with [ext]. Without [ext] at link, odoc should
warn once that [ext] is missing.

  $ cat l/l.mli
  val x : Ext.t
  $ cat l/l.ml
  let x : Ext.t = Obj.magic 0
  $ odoc link -L n:h/pkg/n -L l:h/pkg/l h/pkg/n/n.odoc
  File "N":
  Library ext was used to compile this unit but is not given with -L. Paths into it are not resolved.

Without [l] and [ext], it should warn once about each of them.

  $ odoc link -L n:h/pkg/n h/pkg/n/n.odoc
  File "N":
  Library ext was used to compile this unit but is not given with -L. Paths into it are not resolved.
  File "N":
  Library l was used to compile this unit but is not given with -L. Paths into it are not resolved.

The D6 message is a warning (section 2). So it should follow the options for
warnings, as other warnings do. With [--warn-error], it should be an error,
and odoc should fail. With [--print-warnings false], it should not be
printed.

  $ odoc link --warn-error -L n:h/pkg/n -L l:h/pkg/l h/pkg/n/n.odoc
  File "N":
  Library ext was used to compile this unit but is not given with -L. Paths into it are not resolved.
  $ odoc link --print-warnings false -L n:h/pkg/n -L l:h/pkg/l h/pkg/n/n.odoc
  File "N":
  Library ext was used to compile this unit but is not given with -L. Paths into it are not resolved.

D.2. D6 for a unit reached by a reference (section 2). [p]'s reference
[{!R.Y.t}] is projected through [r], whose paths are resolved at the link of
[p]: [R.Y] is an alias to [Ext2], which [r] was compiled with. D6 covers the
units reached by projecting a reference, so without [ext2] at link, odoc
should warn that [ext2] is missing. The reference fails, so it should also
get an ordinary warning (section 4.5).

  $ cat ext2/ext2.mli
  type t
  $ cat r/r.mli
  module Y = Ext2
  $ odoc link -L p:h/pkg/p -L r:h/pkg/r h/pkg/p/p.odoc
  File "p/p.mli", line 1, characters 8-16:
  Warning: Failed to resolve reference unresolvedroot(R).Y.t Parent_sig: Unresolved module path unresolvedroot(Ext2) (Lookup failure (root module): Ext2)

With [ext2] given, the reference should resolve, with no warning.

  $ odoc link -L p:h/pkg/p -L r:h/pkg/r -L ext2:h/pkg/ext2 h/pkg/p/p.odoc

D.3. A unit should record its [Λc], the libraries given with [-L] when it
was compiled (section 1). D8 (section 2): a unit made by [odoc compile-impl]
has the same [Λc] as the interface of the same module. The driver gave [l]
and [ext] to both [odoc compile] and [odoc compile-impl] of [L], so each
unit should record [l] and [ext]. [odoc_print ... | jq -c .libraries] shows
the libraries that a unit records as its [Λc].

  $ odoc_print h/pkg/l/l.odoc | jq -c .libraries
  ["l","ext"]
  $ odoc_print h/pkg/l/impl-l.odoc | jq -c .libraries
  ["l","ext"]

D.4. D1 (section 2) says that a unit's own library is in its [Λc]. Here it
is broken: in library [k], [A2] has [val y : A1.t], and is compiled with
[-L] for another library, but not for [k]. The spec makes no claim when D1
is broken. This records what odoc does.

  $ cat k/a1.mli
  type t
  $ cat k/a2.mli
  val y : A1.t
  $ ocamlc -bin-annot -no-alias-deps -c k/a1.mli
  $ ocamlc -bin-annot -no-alias-deps -I k -c k/a2.mli
  $ odoc compile --output-dir h --parent-id pkg/k -L k:h/pkg/k k/a1.cmti
  $ odoc compile --output-dir h --parent-id pkg/k -L ext:h/pkg/ext k/a2.cmti
  $ odoc_print h/pkg/k/a2.odoc | jq -c .libraries
  ["ext"]
  $ odoc_print --short --long-paths --show-root-parents h/pkg/k/a2.odoc
  val y/0 : unresolvedroot(A1).t
  $ odoc link -L k:h/pkg/k -L ext:h/pkg/ext h/pkg/k/a2.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/k/a2.odocl
  val y/0 : resolved(root(pkg/k/A1).t)

D.5. A library is a name bound to one directory (section 1), and a name
means the same directory in every run (D7). A [-L] set that binds one name
to two directories breaks this, and odoc should reject it with an error, at
compile as at link.

A link that accepts it would be wrong.

  $ odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext:h/pkg/ext -L ext:h/pkg/ext2 l/l.cmti
  ERROR: Library ext is given with -L for two directories
  [1]
  $ odoc link -L l:h/pkg/l -L ext:h/pkg/ext -L ext:h/pkg/ext2 h/pkg/l/l.odoc

The spec does not say what odoc should do with one directory given under
two names, or with the same [-L] given twice. This records what odoc does.

  $ odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext:h/pkg/ext -L other:h/pkg/ext l/l.cmti
  $ odoc link -L l:h/pkg/l -L ext:h/pkg/ext -L other:h/pkg/ext h/pkg/l/l.odoc
  ERROR: Paths given to all -L options must be disjoint
  [1]
  $ odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext:h/pkg/ext -L ext:h/pkg/ext l/l.cmti
  $ odoc link -L l:h/pkg/l -L ext:h/pkg/ext -L ext:h/pkg/ext h/pkg/l/l.odoc
  ERROR: Paths given to all -L options must be disjoint
  [1]
