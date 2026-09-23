Two libraries of one package define modules with the same names, like
eliom.client and eliom.server. Each has a main module [Content], a
[Content__Html] canonically named [Content.Html], and an alias [Wrap.C]
compiled with [-no-alias-deps], so [Wrap] does not import [Content].

  $ for lib in client server; do
  >   for m in content__Html content wrap; do
  >     ocamlc -bin-annot -no-alias-deps -I $lib -c $lib/$m.mli
  >   done
  > done

  $ for lib in client server; do
  >   for m in content__Html content wrap; do
  >     odoc compile --output-dir h --parent-id pkg/$lib -I h/pkg/$lib $lib/$m.cmti
  >   done
  > done

Both libraries are in the reference scope of every unit, with [-L]. The
[-I] path of a unit holds only its own library, as the compiler's did.

  $ L="-L client:h/pkg/client -L server:h/pkg/server"
  $ for lib in client server; do
  >   for m in content__Html content wrap; do
  >     odoc link -I h/pkg/$lib $L h/pkg/$lib/$m.odoc
  >   done
  > done

The libraries the root modules named in a unit come from. A name is looked up
among the [-I] directories first, so the alias [Content.Html], the canonical
path of [Content__Html], the alias [Wrap.C] and the reference [{!Content.Html}]
written in the documentation all name a module of the unit's own library, and
no lookup is ambiguous. Searching both libraries at once made them name the
server's modules, since [Wrap] does not import [Content] and the digests that
tell same-named modules apart were not there to be checked.

  $ roots() { odoc_print $1 | jq -c '[.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'; }
  $ roots h/pkg/client/content.odocl
  ["client/Content","client/Content__Html"]
  $ roots h/pkg/client/wrap.odocl
  ["client/Content","client/Wrap"]
  $ roots h/pkg/server/content.odocl
  ["server/Content","server/Content__Html"]
  $ roots h/pkg/server/wrap.odocl
  ["server/Content","server/Wrap"]

A page listing the modules of one library, given that library's [-I]. The
reference is a path, so it names the library, and the synopsis shown for each
module is the module's own documentation. A reference that documentation makes,
[{!Content.Html}] here, names a module of the library the page is about, the
way the module's own page does.

  $ odoc compile --output-dir h --parent-id pkg page.mld
  $ odoc link -P pkg:h/pkg $L -I h/pkg/client h/pkg/page-page.odoc
  $ odoc_print h/pkg/page-page.odocl | jq -c '[.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'
  ["client/Content","client/Content__Html"]
