Rule P1 (doc/scope_model.mld, section 3.2): a root that its origin records
with a digest means the unit of that name and digest among the libraries the
origin was compiled with, Λc(o).

Libraries [ext1] and [ext2] each have a module [Ext], with different
contents. [l] is built against [ext1] and [m] against [ext2]; each uses
[Ext.t], so each records the digest of the [Ext] it was built against. [n]
is built against [l] and so, by closure, [ext1]. Every compile uses dune's
[-no-alias-deps], and [-L] for the unit's own library and its dependencies.

  $ cat ext1/ext.mli
  type t = A
  $ cat ext2/ext.mli
  type t = B
  $ cat l/l.mli
  val x : Ext.t
  $ cat m/m.mli
  val y : Ext.t
  $ cat n/n.mli
  module X : module type of L
  $ ocamlc -bin-annot -no-alias-deps -c ext1/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -c ext2/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext1 -c l/l.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext2 -c m/m.mli
  $ ocamlc -bin-annot -no-alias-deps -I l -I ext1 -c n/n.mli

  $ odoc compile --output-dir h --parent-id pkg/ext1 -L ext1:h/pkg/ext1 ext1/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/ext2 -L ext2:h/pkg/ext2 ext2/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext1:h/pkg/ext1 l/l.cmti
  $ odoc compile --output-dir h --parent-id pkg/m -L m:h/pkg/m -L ext2:h/pkg/ext2 m/m.cmti
  $ odoc compile --output-dir h --parent-id pkg/n -L n:h/pkg/n -L l:h/pkg/l -L ext1:h/pkg/ext1 n/n.cmti

How to read [odoc_print --short --long-paths --show-root-parents]. Each line
is one item of the signature. [x/0] is the item [x], with a number odoc adds
to tell apart items of the same name. [resolved(...)] is a resolved path, and
[root(pkg/ext1/Ext)] is the unit [Ext] of the library [ext1] in the package
[pkg]. With [--show-expansions], [=>] is followed by the signature odoc
computed for a module.

P1 applies when the unit is compiled, as the dependency was compiled before
it (D5). So the path should already be resolved, to [ext1]'s [Ext], in the
[.odoc] file:

  $ odoc_print --short --long-paths --show-root-parents h/pkg/l/l.odoc
  val x/0 : resolved(root(pkg/ext1/Ext).t)

Every library is given at link, so both [Ext]s are there. Each path should
still mean the [Ext] its unit recorded: [ext1]'s for [l], [ext2]'s for [m].
Only the libraries a unit was compiled with are searched, and the digest
picks the unit, so the other [Ext] is never a candidate.

  $ odoc link -L ext1:h/pkg/ext1 -L ext2:h/pkg/ext2 -L l:h/pkg/l -L m:h/pkg/m -L n:h/pkg/n h/pkg/l/l.odoc
  $ odoc link -L ext1:h/pkg/ext1 -L ext2:h/pkg/ext2 -L l:h/pkg/l -L m:h/pkg/m -L n:h/pkg/n h/pkg/m/m.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/l/l.odocl
  val x/0 : resolved(root(pkg/ext1/Ext).t)
  $ odoc_print --short --long-paths --show-root-parents h/pkg/m/m.odocl
  val y/0 : resolved(root(pkg/ext2/Ext).t)

A root keeps its origin when its term is shown in another unit (O1, section
3.3). [n]'s [X] has the signature of [L], so the [Ext] in it was written in
[l], and should be [ext1]'s:

  $ odoc link -L ext1:h/pkg/ext1 -L ext2:h/pkg/ext2 -L l:h/pkg/l -L m:h/pkg/m -L n:h/pkg/n h/pkg/n/n.odoc
  $ odoc_print --short --long-paths --show-root-parents --show-expansions h/pkg/n/n.odocl
  module X/0 : module type of resolved(root(pkg/l/L))
    => sig val x/1 : resolved(root(pkg/ext1/Ext).t) end

When [ext1] is not given at link, odoc should warn once that the library is
missing (D6). [x] was resolved at compile, so it should stay resolved.

  $ odoc link -L ext2:h/pkg/ext2 -L l:h/pkg/l h/pkg/l/l.odoc
  File "L":
  Warning: Library ext1 was used to compile this unit but is not given with -L.
  $ odoc_print --short --long-paths --show-root-parents h/pkg/l/l.odocl
  val x/0 : resolved(root(pkg/ext1/Ext).t)
