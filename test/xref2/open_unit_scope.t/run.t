Library [x] has a module [Inner] and a module [Op] that aliases it as [H],
compiled with [-no-alias-deps], so [Op] does not import [Inner] with a digest.
Library [y] has a different module [Inner]. Library [z] does not depend on [x]
at all: it opens [Op] with [--open], and refers to [H.t] in a comment.

  $ (cd x && ocamlc -bin-annot -no-alias-deps -c inner.mli op.mli)
  $ (cd y && ocamlc -bin-annot -c inner.mli)
  $ (cd z && ocamlc -bin-annot -c z.mli)

[Op] is compiled before [Inner], so the path in [H] is left for link to resolve.

  $ mkdir -p h/x h/y h/z
  $ odoc compile --output-dir h --parent-id y -L y:h/y y/inner.cmti
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/op.cmti
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/inner.cmti
  $ odoc compile --output-dir h --parent-id z -L z:h/z -L y:h/y z/z.cmti

[H] is [Op]'s, and can only mean [x]'s [Inner]: that is all [x] could see.
Link should resolve it among the libraries [Op] recorded, not those of [z].

  $ odoc link h/z/z.odoc -L z:h/z -L x:h/x -L y:h/y --open Op
  $ odoc_print h/z/z.odocl | jq -c '[.. | objects | .["`Alias"]? | select(. != null) | .[0]["`Identifier"]["`Root"] | .[0].Some["`Page"][1] + "/" + .[1]] | unique'
  ["x/Inner"]
