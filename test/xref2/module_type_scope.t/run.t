Library [x] has a module [Inner] and a main module [X], compiled with
[-no-alias-deps], whose module type [S] and module [H] both alias [Inner].
Library [y] has a different module [Inner]. The [H] in [X.S] was written in
[X], so like [X.H] it can only mean [x]'s [Inner].

  $ (cd x && ocamlc -bin-annot -no-alias-deps -c inner.mli x.mli)
  $ (cd y && ocamlc -bin-annot -c inner.mli)
  $ odoc compile --output-dir h --parent-id pkg/x -L x:h/pkg/x x/inner.cmti
  $ odoc compile --output-dir h --parent-id pkg/x -L x:h/pkg/x x/x.cmti
  $ odoc compile --output-dir h --parent-id pkg/y -L y:h/pkg/y y/inner.cmti
  $ refs() { odoc_print $1 | jq -c '[.. | objects | select(has("`Reference")) | .["`Reference"][0] | [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique | join(" ")]'; }

A page refers to [H] through the module type and through the module. Both
should reach [x]'s [Inner], and neither lookup should be ambiguous.

  $ odoc compile --output-dir h --parent-id pkg p.mld
  $ odoc link -P pkg:h/pkg -L x:h/pkg/x -L y:h/pkg/y h/pkg/page-p.odoc
  $ refs h/pkg/page-p.odocl
  ["x/Inner x/X","x/Inner x/X"]
