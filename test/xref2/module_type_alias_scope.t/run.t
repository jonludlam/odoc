Library [x] has a main module [X], compiled with [-no-alias-deps], whose
module type [S] aliases [Inner] of library [ix] as [H]. Library [y]'s [Y]
declares [module type S = X.S], and [y] also records a library [iy] with a
different [Inner]. [H] was written in [X], so it can only mean [ix]'s [Inner].

  $ (cd ix && ocamlc -bin-annot -c inner.mli)
  $ (cd iy && ocamlc -bin-annot -c inner.mli)
  $ (cd x && ocamlc -bin-annot -no-alias-deps -I ../ix -c x.mli)
  $ (cd y && ocamlc -bin-annot -no-alias-deps -I ../x -c y.mli)
  $ odoc compile --output-dir h --parent-id pkg/ix -L ix:h/pkg/ix ix/inner.cmti
  $ odoc compile --output-dir h --parent-id pkg/iy -L iy:h/pkg/iy iy/inner.cmti
  $ odoc compile --output-dir h --parent-id pkg/x -L x:h/pkg/x -L ix:h/pkg/ix x/x.cmti
  $ odoc compile --output-dir h --parent-id pkg/y -L y:h/pkg/y -L x:h/pkg/x -L iy:h/pkg/iy y/y.cmti
  $ refs() { odoc_print $1 | jq -c '[.. | objects | select(has("`Reference")) | .["`Reference"][0] | [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique | join(" ")]'; }

A page refers to [H] through [Y.S]. It should reach [ix]'s [Inner], not
[iy]'s.

  $ odoc compile --output-dir h --parent-id pkg p.mld
  $ odoc link -P pkg:h/pkg -L ix:h/pkg/ix -L iy:h/pkg/iy -L x:h/pkg/x -L y:h/pkg/y h/pkg/page-p.odoc
  File "Inner":
  Ambiguous lookup. Possible files: Inner
  Inner
  $ refs h/pkg/page-p.odocl
  ["iy/Inner x/X y/Y"]
