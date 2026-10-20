Library [x] has a module [Inner] and a main module [X] that aliases it as
[H], compiled with [-no-alias-deps]. Library [y] has a different module
[Inner]. Library [z] depends on both, uses [y]'s [Inner] itself, and aliases
[X.H]. With [-no-alias-deps], the compiler resolves [X.H] where it is used: in
[z], whose load path has [y] first, [X.H] is [y]'s [Inner].

  $ (cd x && ocamlc -bin-annot -no-alias-deps -c inner.mli x.mli)
  $ (cd y && ocamlc -bin-annot -c inner.mli)
  $ (cd z && ocamlc -bin-annot -no-alias-deps -I ../y -I ../x -c z.mli)

[z] imports [y]'s [Inner] with its digest, not [x]'s:

  $ digest() { ocamlobjinfo $1 | grep -A20 'Interfaces imported' | awk '$2 == "Inner" {print $1}'; }
  $ for d in x y; do [ "$(digest z/z.cmi)" = "$(digest $d/inner.cmi)" ] && echo "z imports $d's Inner"; done
  z imports y's Inner

[X] is compiled before [x]'s [Inner], so its alias [H] stays an unresolved
root, and [z] is compiled before [X], so the path into [X] is left for link.

  $ mkdir -p h/x h/y h/z
  $ odoc compile --output-dir h --parent-id y -L y:h/y y/inner.cmti
  $ odoc compile --output-dir h --parent-id z -L z:h/z -L y:h/y -L x:h/x z/z.cmti
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/x.cmti
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/inner.cmti
  $ odoc_print h/x/x.odoc | jq -c '[.. | objects | .["`Root"]? | select(type == "string")]'
  ["Inner"]

Link follows [M] into [X] and finds the root [Inner], read from [X], whose
libraries hold only [x]'s [Inner]. But [z] imports an [Inner] with a digest,
and the digest is looked up first, so [M] is [y]'s [Inner]. This is the module
the compiler used for [X.H] in [z].

  $ odoc link h/z/z.odoc -L z:h/z -L y:h/y -L x:h/x
  $ roots() { odoc_print $1 | jq -c '[.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'; }
  $ roots h/z/z.odocl
  ["x/X","y/Inner","z/Z"]
  $ odoc_print h/z/z.odocl | jq -c '.content.Module.items[] | .Module? | select(. != null) | select(.[1].id["`Module"][1] == "M") | .[1].type_.Alias[0] | [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'
  ["x/X","y/Inner"]
