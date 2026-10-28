Rules P1 and P2 (doc/scope_model.mld, section 3.2), and section 3.3 (aliases
bound late). P1 looks up a root that its origin records with a digest, P2 one
with no digest, among the libraries the origin was compiled with, Λc(o). The
origin of a root is the unit whose source it came from (section 3.1). Λc(u)
is the set of libraries given with [-L] when the unit [u] was compiled
(section 1).

Library [x] has a module [Inner] and a main module [X] that aliases it as
[H], compiled with [-no-alias-deps]. Library [y] has a different module
[Inner]. Library [z] depends on both, uses [y]'s [Inner] itself, and aliases
[X.H]. With [-no-alias-deps], the compiler resolves [X.H] where it is used: in
[z], whose load path has [y] first, [X.H] is [y]'s [Inner].

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
  
  val v : Inner.u
  $ (cd x && ocamlc -bin-annot -no-alias-deps -c inner.mli x.mli)
  $ (cd y && ocamlc -bin-annot -c inner.mli)
  $ (cd z && ocamlc -bin-annot -no-alias-deps -I ../y -I ../x -c z.mli)

[z] should record the digest of [y]'s [Inner], not [x]'s (section 3.3). The
[digest] helper prints the digest a compiled file records for [Inner]:

  $ digest() { ocamlobjinfo $1 | grep -A20 'Interfaces imported' | awk '$2 == "Inner" {print $1}'; }
  $ for d in x y; do [ "$(digest z/z.cmi)" = "$(digest $d/inner.cmi)" ] && echo "z imports $d's Inner"; done
  z imports y's Inner

[X] is compiled before [x]'s [Inner], so its alias [H] stays an unresolved
root, and [z] is compiled before [X], so the path into [X] is left for link.
This breaks D5 (section 2), which matters only for resolution at compile.
The [jq] filter prints each unresolved root in [X]'s file, by name:

  $ mkdir -p h/x h/y h/z
  $ odoc compile --output-dir h --parent-id y -L y:h/y y/inner.cmti
  $ odoc compile --output-dir h --parent-id z -L z:h/z -L y:h/y -L x:h/x z/z.cmti
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/x.cmti
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/inner.cmti
  $ odoc_print h/x/x.odoc | jq -c '[.. | objects | .["`Root"]? | select(type == "string")]'
  ["Inner"]

Link follows [M] into [X] and meets the root [Inner] of [H], whose origin is
[X]. Λc(z) holds two modules [Inner], one from [x] and one from [y]. This
setup breaks D3 (section 2), so the spec makes no promise. It records what
odoc does. This is the example of section 3.3: the compiler bound [X.H] in
[z] to [y]'s [Inner], and section 3.3 says that odoc may link the author's
choice instead, which is [x]'s [Inner]: P2 over Λc(X).

The [roots] helper prints every resolved root in the linked unit, as
[library/Unit], with no repeats. The last [jq] filter does the same for the
target of the alias [M] only.

  $ odoc link h/z/z.odoc -L z:h/z -L y:h/y -L x:h/x
  $ roots() { odoc_print $1 | jq -c '[.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'; }
  $ roots h/z/z.odocl
  ["x/X","y/Inner","z/Z"]
  $ odoc_print h/z/z.odocl | jq -c '.content.Module.items[] | .Module? | select(. != null) | select(.[1].id["`Module"][1] == "M") | .[1].type_.Alias[0] | [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'
  ["x/X","y/Inner"]
