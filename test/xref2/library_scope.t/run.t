Two libraries of one package, [client] and [server], each built against a
different library that defines a module [A]: [x] for the client, [y] for the
server. Each aliases the [A] it was given, with [-no-alias-deps], so neither
imports it and no digest says which [A] was meant.

  $ ocamlc -bin-annot -no-alias-deps -I x -c x/a.mli
  $ ocamlc -bin-annot -no-alias-deps -I y -c y/a.mli
  $ ocamlc -bin-annot -no-alias-deps -I x -I client -c client/content.mli
  $ ocamlc -bin-annot -no-alias-deps -I y -I server -c server/content.mli

Compiled with [-I], nothing says afterwards what each unit could see. A page
naming both aliases sends them to the same [A], so one of the two is wrong,
and odoc says the lookup was ambiguous.

  $ odoc compile --output-dir i --parent-id pkg/x -I i/pkg/x x/a.cmti
  $ odoc compile --output-dir i --parent-id pkg/y -I i/pkg/y y/a.cmti
  $ odoc compile --output-dir i --parent-id pkg/client -I i/pkg/client -I i/pkg/x client/content.cmti
  $ odoc compile --output-dir i --parent-id pkg/server -I i/pkg/server -I i/pkg/y server/content.cmti
  $ odoc compile --output-dir i --parent-id pkg all.mld
  $ odoc link -P pkg:i/pkg -L client:i/pkg/client -L server:i/pkg/server -L x:i/pkg/x -L y:i/pkg/y i/pkg/page-all.odoc
  File "A":
  Ambiguous lookup. Possible files: A
  A
  $ refs() { odoc_print $1 | jq -c '[.. | objects | select(has("`Reference")) | .["`Reference"][0] | [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique | join(" ")]'; }
  $ refs i/pkg/page-all.odocl
  ["client/Content y/A","server/Content y/A"]

Compiled with [-L], each unit records the libraries it was built against, and
the page sends each alias to the [A] its own library was given.

  $ odoc compile --output-dir h --parent-id pkg/x -L x:h/pkg/x x/a.cmti
  $ odoc compile --output-dir h --parent-id pkg/y -L y:h/pkg/y y/a.cmti
  $ odoc compile --output-dir h --parent-id pkg/client -L client:h/pkg/client -L x:h/pkg/x client/content.cmti
  $ odoc compile --output-dir h --parent-id pkg/server -L server:h/pkg/server -L y:h/pkg/y server/content.cmti
  $ odoc compile --output-dir h --parent-id pkg all.mld
  $ odoc link -P pkg:h/pkg -L client:h/pkg/client -L server:h/pkg/server -L x:h/pkg/x -L y:h/pkg/y h/pkg/page-all.odoc
  $ refs h/pkg/page-all.odocl
  ["client/Content x/A","server/Content y/A"]

What each unit recorded.

  $ odoc_print h/pkg/client/content.odoc | jq -c '.libraries'
  ["client","x"]
  $ odoc_print h/pkg/server/content.odoc | jq -c '.libraries'
  ["server","y"]
