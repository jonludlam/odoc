Library [x] has a module [Inner] and a main module [X] that aliases it as
[H], compiled with [-no-alias-deps], so [X] does not import [Inner] with a
digest. Library [y] has a different module [Inner]. Library [z] depends on
both, and aliases [X.H], again with [-no-alias-deps]: it imports [X] but not
[Inner].

  $ (cd x && ocamlc -bin-annot -no-alias-deps -c inner.mli x.mli)
  $ (cd y && ocamlc -bin-annot -c inner.mli)
  $ (cd z && ocamlc -bin-annot -no-alias-deps -I ../x -I ../y -c z.mli)

[z] is compiled before [x], so the path into [X] is left for link to resolve.

  $ mkdir -p h/x h/y h/z
  $ odoc compile --output-dir h --parent-id y -L y:h/y y/inner.cmti
  $ odoc compile --output-dir h --parent-id z -L z:h/z -L x:h/x -L y:h/y z/z.cmti
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/inner.cmti
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/x.cmti

[H] is [X]'s, and can only mean [x]'s [Inner]: that is all [x] could see. Link
follows [M] into [X], and resolves what it finds there among the libraries
[X] recorded, not those of [z], which hold two modules [Inner].

  $ odoc link h/z/z.odoc -L z:h/z -L x:h/x -L y:h/y
  $ roots() { odoc_print $1 | jq -c '[.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'; }
  $ roots h/z/z.odocl
  ["x/Inner","x/X","z/Z"]
