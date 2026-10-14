Library [l2] has a hidden module [Inner__Hidden], whose [N] aliases [A] of
library [a2]. Library [l1]'s [Outer] aliases [Inner__Hidden] as [M], and [l1]
also records a library [o] with a different [A]. All are compiled with
[-no-alias-deps]. [N] was written in [l2], so it can only mean [a2]'s [A].

  $ (cd a2 && ocamlc -bin-annot -c a.mli)
  $ (cd o && ocamlc -bin-annot -c a.mli)
  $ (cd l2 && ocamlc -bin-annot -no-alias-deps -I ../a2 -c inner__Hidden.mli)
  $ (cd l1 && ocamlc -bin-annot -no-alias-deps -I ../l2 -c outer.mli)
  $ odoc compile --output-dir h --parent-id pkg/a2 -L a2:h/pkg/a2 a2/a.cmti
  $ odoc compile --output-dir h --parent-id pkg/o -L o:h/pkg/o o/a.cmti
  $ odoc compile --output-dir h --parent-id pkg/l2 -L l2:h/pkg/l2 -L a2:h/pkg/a2 l2/inner__Hidden.cmti
  $ odoc compile --output-dir h --parent-id pkg/l1 -L l1:h/pkg/l1 -L l2:h/pkg/l2 -L o:h/pkg/o l1/outer.cmti
  $ refs() { odoc_print $1 | jq -c '[.. | objects | select(has("`Reference")) | .["`Reference"][0] | [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique | join(" ")]'; }

A page refers to [Outer.M.N]. Following [M] leads into [l2]'s
[Inner__Hidden], so [N] should reach [a2]'s [A], not [o]'s.

  $ odoc compile --output-dir h --parent-id pkg p.mld
  $ odoc link -P pkg:h/pkg -L a2:h/pkg/a2 -L o:h/pkg/o -L l2:h/pkg/l2 -L l1:h/pkg/l1 h/pkg/page-p.odoc
  File "A":
  Ambiguous lookup. Possible files: A
  A
  $ refs h/pkg/page-p.odocl
  ["l1/Outer l2/Inner__Hidden o/A"]
