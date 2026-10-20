Library [w] has a module [Lib] with a submodule [X], and library [y] has a
different [Lib] with an [X]. Library [a] records [w], and its module type [S]
has a module [X] whose canonical path is [Lib.X]. Library [b] records [a] and
[y], not [w]. Its units [Early] and [Late] each declare [M : A.S] and refer to
[M.X.t].

  $ (cd w && ocamlc -bin-annot -c lib.mli)
  $ (cd y && ocamlc -bin-annot -c lib.mli)
  $ (cd a && ocamlc -bin-annot -c a.mli)
  $ (cd b && ocamlc -bin-annot -I ../a -c early.mli late.mli prefixed.mli)
  $ odoc compile --output-dir h --parent-id w -L w:h/w w/lib.cmti
  $ odoc compile --output-dir h --parent-id y -L y:h/y y/lib.cmti

[Early] and [Prefixed] are compiled before [A], so [A.S] and [A.P] are left for
link to expand. [Late] is compiled after it, so compile copies the expansion of
[A.S], with the canonical path, into [Late].

  $ odoc compile --output-dir h --parent-id b -L b:h/b -L a:h/a -L y:h/y b/early.cmti
  $ odoc compile --output-dir h --parent-id b -L b:h/b -L a:h/a -L y:h/y b/prefixed.cmti
  $ odoc compile --output-dir h --parent-id a -L a:h/a -L w:h/w a/a.cmti
  $ odoc compile --output-dir h --parent-id b -L b:h/b -L a:h/a -L y:h/y b/late.cmti
  $ canonical() { odoc_print $1 | jq -c '[.. | objects | .["`Canonical"]? | select(. != null) | .[1] | .. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'; }

The author of [a] means [w]'s [Lib]. But the root of a canonical path is looked
up in every library, like the root of a reference, so [Lib] is ambiguous: odoc
says so, and takes one. The current release does the same. In [Early], [M.X]
comes from [A] at link time.

  $ odoc link h/b/early.odoc -L b:h/b -L a:h/a -L w:h/w -L y:h/y
  File "Lib":
  Ambiguous lookup. Possible units: y/Lib, w/Lib
  $ canonical h/b/early.odocl
  ["y/Lib"]

In [Late], the canonical path is part of [Late]'s own content, copied by
compile. It is ambiguous in the same way.

  $ odoc link h/b/late.odoc -L b:h/b -L a:h/a -L w:h/w -L y:h/y
  File "Lib":
  Ambiguous lookup. Possible units: y/Lib, w/Lib
  $ canonical h/b/late.odocl
  ["y/Lib"]

The author of [a] can name the library of the canonical path with a prefix.
[A.P] is like [A.S], but the canonical path of its [X] is [/w/Lib.X], and
[Prefixed] declares [M : A.P]. The prefix should select [w]'s [Lib], with no
ambiguity. The roots of the canonical paths as [a] stores them:

  $ odoc_print h/a/a.odoc | jq -c '[.. | objects | .canonical? | select(. != null and . != "None") | .Some | .. | objects | (.["`Root"]? // (.["`LibraryRoot"]? | select(. != null) | "/" + .[0] + "/" + .[1])) | select(. != null)]'
  ["Lib","/w/Lib"]

  $ odoc link h/b/prefixed.odoc -L b:h/b -L a:h/a -L w:h/w -L y:h/y
  $ canonical h/b/prefixed.odocl
  ["w/Lib"]
