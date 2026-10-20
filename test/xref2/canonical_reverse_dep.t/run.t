Library [core] has a module [Core__inner.X] whose canonical path is [Pub.X].
[Pub] is in library [pub], which depends on [core]: the canonical path names
the place where users find [X], which [core] itself cannot see. This is how
the internal libraries of a package such as eio name its public module.

  $ (cd core && ocamlc -bin-annot -c core__inner.mli user.mli)
  $ (cd pub && ocamlc -bin-annot -I ../core -c pub.mli)
  $ odoc compile --output-dir h --parent-id core -L core:h/core core/core__inner.cmti
  $ odoc compile --output-dir h --parent-id core -L core:h/core core/user.cmti
  $ odoc compile --output-dir h --parent-id pub -L pub:h/pub -L core:h/core pub/pub.cmti
  $ canonical() { odoc_print $1 | jq -c '[.. | objects | .["`Canonical"]? | select(. != null) | .[1] | .. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'; }

[User] is in [core] and refers to [Core__inner.X.t]. Its canonical path
[Pub.X] should resolve to [pub]'s [Pub]. If it does not, the path stays
hidden, and link removes the equation of [User.t].

  $ odoc link h/core/user.odoc -L core:h/core -L pub:h/pub
  $ canonical h/core/user.odocl
  ["pub/Pub"]
  $ odoc_print h/core/user.odocl | jq -c '.content.Module.items[0].Type[1].equation.manifest | if . == "None" then "no equation" else "equation" end'
  "equation"
