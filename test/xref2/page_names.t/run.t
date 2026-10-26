This test is about the names of pages. Pages are out of the scope of
doc/scope_model.mld (section 1), so no rule of it applies here.

Two packages, laid out as dune and the reference driver lay them out. Each
package has its pages at the top of its directory, and each library has a
directory of its own in the package, with a landing page [index]:

The source files:

  $ cat pa/alib/index.mld
  {0 Library alib}
  $ cat pa/index.mld
  {0 Index of pa}
  $ cat pa/sub/guide.mld
  {0 Guide}
  $ cat pa/sub/index.mld
  {0 Index of pa/sub}
  $ cat pz/index.mld
  {0 Index of pz}
  $ cat pz/zlib/index.mld
  {0 Library zlib}
  $ cat pz/zlib/znote.mld
  {0 A note in zlib}
  $ cat pz/zonly.mld
  {0 Only in pz}

  $ find . -name '*.mld' -o -name '*.mli' | sort
  ./pa/alib/index.mld
  ./pa/alib/m.mli
  ./pa/index.mld
  ./pa/other.mld
  ./pa/sub/guide.mld
  ./pa/sub/index.mld
  ./pz/index.mld
  ./pz/zlib/index.mld
  ./pz/zlib/znote.mld
  ./pz/zonly.mld

  $ cat pa/alib/m.mli
  (** See {!page-guide} and {!page-znote}. *)
  type t
  $ for p in pa pa/sub pa/alib pz pz/zlib; do
  >   for f in $p/*.mld; do
  >     odoc compile --output-dir h --parent-id $p $f
  >   done
  > done
  $ ocamlc -bin-annot -c pa/alib/m.mli
  $ odoc compile --output-dir h --parent-id pa/alib -L alib:h/pa/alib pa/alib/m.cmti

How to read [odoc_print --refs --show-root-parents]. Each line is one
reference. [r(pa/sub/guide)] is a reference that resolved to the page
[guide] in the directory [sub] of the package [pa]. [unresolvedroot(guide)]
is a reference to the name [guide] that did not resolve. A path that did not
resolve is printed as it was written.

The page [other] of [pa] refers to [page-guide], [page-index], [page-zonly]
and [page-znote] by name, then to [//index] and [/pz/zonly] by path:

  $ cat pa/other.mld
  {0 Other page of pa}
  
  See {!page-guide}, {!page-index}, {!page-zonly} and {!page-znote}.
  
  See {!//index} and {!/pz/zonly}.

A page name means a page of the current package, wherever it is in the
package. [pa] has one page [guide], in [sub], so [page-guide] should resolve
to [pa/sub/guide]. [pa] has three pages [index], so [page-index] should not
resolve, and odoc should warn and name the three. [zonly] and [znote] are
pages of [pz] only, so neither name should resolve: another package's pages
need a path. The two paths should resolve to [pa/index] and [pz/zonly].

  $ odoc link -P pa:h/pa -P pz:h/pz -L alib:h/pa/alib -L zlib:h/pz/zlib --current-package pa h/pa/page-other.odoc
  File "page-index":
  Ambiguous lookup. Possible pages: pa/alib/index, pa/index, pa/sub/index
  File "pa/other.mld", line 3, characters 52-65:
  Warning: Failed to resolve reference unresolvedroot(znote) Couldn't find page "znote"
  File "pa/other.mld", line 3, characters 34-47:
  Warning: Failed to resolve reference unresolvedroot(zonly) Couldn't find page "zonly"
  File "pa/other.mld", line 3, characters 19-32:
  Warning: Failed to resolve reference unresolvedroot(index) Couldn't find page "index"
  $ odoc_print --refs --show-root-parents h/pa/page-other.odocl
  r(pa/sub/guide)
  unresolvedroot(index)
  unresolvedroot(zonly)
  unresolvedroot(znote)
  r(pa/index)
  r(pz/zonly)

A module of [pa]'s library refers to pages by name in the same way.
[page-guide] should resolve to [pa/sub/guide], and [page-znote], a page in
another package's library directory, should not resolve.

  $ odoc link -P pa:h/pa -P pz:h/pz -L alib:h/pa/alib -L zlib:h/pz/zlib --current-package pa h/pa/alib/m.odoc
  File "pa/alib/m.mli", line 1, characters 26-39:
  Warning: Failed to resolve reference unresolvedroot(znote) Couldn't find page "znote"
  $ odoc_print --refs --show-root-parents h/pa/alib/m.odocl
  r(pa/sub/guide)
  unresolvedroot(znote)

Without [-P], there is no current package. odoc should then do what it has
always done, and take the first page of each name along the search path
without a warning, whichever package it is in. The paths need [-P], so they
should not resolve. A library's directory, given with [-L], holds no page a
name can mean, so [page-znote] should not resolve either.

  $ odoc link -I h/pa -I h/pz -L alib:h/pa/alib -L zlib:h/pz/zlib h/pa/page-other.odoc
  File "pa/other.mld", line 5, characters 19-31:
  Warning: Failed to resolve reference /pz/zonly Path '/pz/zonly' not found
  File "pa/other.mld", line 5, characters 4-14:
  Warning: Failed to resolve reference //index Path '//index' not found
  File "pa/other.mld", line 3, characters 52-65:
  Warning: Failed to resolve reference unresolvedroot(znote) Couldn't find page "znote"
  File "pa/other.mld", line 3, characters 4-17:
  Warning: Failed to resolve reference unresolvedroot(guide) Couldn't find page "guide"
  $ odoc_print --refs --show-root-parents h/pa/page-other.odocl
  unresolvedroot(guide)
  r(pz/index)
  r(pz/zonly)
  unresolvedroot(znote)
  //index
  /pz/zonly
