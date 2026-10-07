A library [y] whose module [B] aliases [A] of library [x]. A third library,
[z], has a module of the same name and nothing else in common. [B] is
compiled with [-no-alias-deps], so [A] is not among its imports with a digest:
nothing but the libraries [B] records says which [A] the alias means. [B]'s
documentation also refers to [W] of library [w], which [y] does not depend
on.

  $ ocamlc -bin-annot -c x/a.mli
  $ ocamlc -bin-annot -c z/a.mli
  $ ocamlc -bin-annot -c w/w.mli
  $ ocamlc -bin-annot -no-alias-deps -I x -c y/b.mli

  $ mkdir -p h/x h/z h/w h/y
  $ odoc compile --output-dir h --parent-id x -L x:h/x x/a.cmti
  $ odoc compile --output-dir h --parent-id z -L z:h/z z/a.cmti
  $ odoc compile --output-dir h --parent-id w -L w:h/w w/w.cmti
  $ odoc compile --output-dir h --parent-id y -L y:h/y -L x:h/x y/b.cmti

[B] records the libraries it was compiled against.

  $ odoc_print h/y/b.odoc | jq -c '.libraries'
  ["y","x"]

  $ alias_target() { odoc_print $1 | jq -c '[.. | objects | select(has("Alias")) | .Alias[0]]'; }
  $ reference() { odoc_print $1 | jq -c '[.. | objects | select(has("`Reference")) | .["`Reference"][0]]'; }

Linked with [x], the alias names the module the compiler gave it.

  $ odoc link h/y/b.odoc -o h/y/b.odocl -L y:h/y -L x:h/x -L w:h/w --custom-layout
  $ alias_target h/y/b.odocl
  [{"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","x"]}},"A"]}}}]

The reference is not limited to the libraries [B] records. An author may name
anything in the reference scope, which is wider than what the compiler saw, so
the reference to [W] goes on to the whole search path and finds it.

  $ reference h/y/b.odocl
  [{"`Resolved":{"`Type":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","w"]}},"W"]}},"u"]}}]

Linked without [x], and with [z] in its place, the alias does not resolve.
[z]'s [A] is a different module, and a path can only mean what the compiler
saw, so odoc says it is missing rather than taking the one it can reach.

  $ odoc link h/y/b.odoc -o h/y/b.odocl -L y:h/y -L z:h/z -L w:h/w --custom-layout --enable-missing-root-warning
  File "h/y/b.odoc":
  Warning: Couldn't find the following modules:
    A
  $ alias_target h/y/b.odocl
  [{"`Root":"A"}]
