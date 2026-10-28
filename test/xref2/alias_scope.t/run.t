Rule P2 (doc/scope_model.mld, section 3.2) and O1 (section 7): a root that
its origin records with no digest means the unit of that name among the
libraries the origin was compiled with, Λc(o). The origin of a root is the
unit whose source it came from (section 3.1), and it does not change when
another unit shows the term. Λc(u) is the set of libraries given with [-L]
when the unit [u] was compiled (section 1).

Library [x] has a module [Inner] and a main module [X] that aliases it as
[H], compiled with [-no-alias-deps], so [X] does not import [Inner] with a
digest. Library [y] has a different module [Inner]. Library [z] depends on
both, and aliases [X.H], again with [-no-alias-deps]: it imports [X] but not
[Inner].

  $ cat x/inner.mli
  type t
  val in_x : t
  $ cat x/x.mli
  module H = Inner
  $ cat y/inner.mli
  type u
  val in_y : u
  $ cat z/z.mli
  module M = X.H
  $ (cd x && ocamlc -bin-annot -no-alias-deps -c inner.mli x.mli)
  $ (cd y && ocamlc -bin-annot -c inner.mli)
  $ (cd z && ocamlc -bin-annot -no-alias-deps -I ../x -I ../y -c z.mli)

[z] is compiled before [x], so the path into [X] is left for link to
resolve. This breaks D5 (section 2), which matters only for resolution at
compile.

  $ mkdir -p h/x h/y h/z
  $ odoc compile --output-dir h --parent-id y -L y:h/y y/inner.cmti
  $ odoc compile --output-dir h --parent-id z -L z:h/z -L x:h/x -L y:h/y z/z.cmti
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/inner.cmti
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/x.cmti

How to read [odoc_print --short --long-paths --show-root-parents]. Each line
is one item of the signature. [M/0] is the item [M], with a number odoc adds
to tell apart items of the same name. [unresolvedroot(X)] is a root that is
not resolved yet.

After compile, [z]'s root [X] should be unresolved: when [z] was compiled,
[x]'s [.odoc] did not exist yet. A resolved root would be wrong.

  $ odoc_print --short --long-paths --show-root-parents h/z/z.odoc
  module M/0 = unresolvedroot(X).H

Link follows [M] into [X] and meets the root [Inner] of [H], whose origin is
[X]. Λc(z) holds two modules [Inner], one from [x] and one from [y]. This
setup breaks D3 (section 2), so the spec makes no promise. It records what
odoc does. Section 3.3 says that odoc may then link the author's choice,
which is [x]'s [Inner]: P2 over Λc(X), which holds only [x]'s.

The [roots] helper prints every resolved root in the linked unit, as
[library/Unit], with no repeats.

  $ odoc link h/z/z.odoc -L z:h/z -L x:h/x -L y:h/y
  File "Inner":
  Ambiguous lookup. Possible units: x/Inner, y/Inner
  $ roots() { odoc_print $1 | jq -c '[.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'; }
  $ roots h/z/z.odocl
  ["x/X","y/Inner","z/Z"]
