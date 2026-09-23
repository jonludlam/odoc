Two libraries of one package define modules with the same names, as
eliom.client and eliom.server do.

A name is tied to a particular file only where the unit's imports record it
with a digest, so each library here has three modules that between them
produce the three kinds of name no import records. [Wrap] aliases [Content] as
[C] and is compiled with [-no-alias-deps], so it does not import what the
alias names. [Content] aliases [Content__Html] as [Html] the same way, and its
documentation refers to [{!Content.Html}] as well. [Content__Html] carries the
canonical path [Content.Html], whose root is the library's own main module,
which a submodule never imports.

eliom is unwrapped, and the modules that collide between its two libraries are
plain ones like [Content] and [Wrap]. [Content__Html] is the shape a wrapped
library has instead, and it is what carries the canonical path, the other half
of what the issue reports.

This test records what odoc does today, for issue #1450. The units of each
library come out right; what a page of the package can say about them does
not, and the text below says where.

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

Which library the root modules named in each unit come from. Each unit stays
on its own side: the alias [Html], the canonical path written on
[Content__Html], the alias [C] and the reference [{!Content.Html}] in the
documentation all name a module of the unit's own library, and no lookup is
ambiguous. None of these names is among the imports of the unit that mentions
it, so no digest tells the two libraries apart; what does is that the [-I]
directories are searched before the [-L] ones, and a unit's [-I] holds the
library it was built against.

  $ roots() { odoc_print $1 | jq -c '[.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'; }
  $ roots h/pkg/client/content.odocl
  ["client/Content","client/Content__Html"]
  $ roots h/pkg/client/wrap.odocl
  ["client/Content","client/Wrap"]
  $ roots h/pkg/server/content.odocl
  ["server/Content","server/Content__Html"]
  $ roots h/pkg/server/wrap.odocl
  ["server/Content","server/Wrap"]

A page of the package rather than of one library. It names a root module of
each library, one by plain name, and a submodule of each.

  $ odoc compile --output-dir h --parent-id pkg all.mld
  $ odoc link -P pkg:h/pkg $L h/pkg/page-all.odoc
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  File "Content__Html":
  Ambiguous lookup. Possible files: Content__Html
  Content__Html
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  File "Content__Html":
  Ambiguous lookup. Possible files: Content__Html
  Content__Html
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  $ refs() { odoc_print $1 | jq -c '[.. | objects | select(has("`Reference")) | .["`Reference"][0] | [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique | join(" ")]'; }
Where each reference on the page points, in the order they are written. The
first two name a root module one library at a time, and a path reference says
which library it means, so both land where they were aimed. The third is the
plain [{!Content}], which is ambiguous, and odoc says so and takes one.

The fourth is [{!/client/Content.Html}], and it is wrong. Naming the library
covers the root and no more: [Content.Html] is an alias, so odoc resolves
[Content__Html] by name once the root is found, and a page about two libraries
has no [-I] to answer that. Both submodule references land on the server, so
there is nothing an author can write on this page to reach the client's.

  $ refs h/pkg/page-all.odocl
  ["client/Content","server/Content","server/Content","client/Content server/Content server/Content__Html","server/Content server/Content__Html","server/Content server/Content__Html"]
  $ modules() { odoc_print $1 | jq -c 'def libs: [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique; [.. | objects | select(has("`Modules")) | .["`Modules"][] | {shown: (.[0] | libs), says: (.[1] | libs)}]'; }
A module list shows, beside each module it names, that module's own first
paragraph. The paragraph is resolved as the page is linked, not as the module
was, so it points where the page can reach rather than where its author meant.

  $ modules h/pkg/page-all.odocl
  [{"shown":["server/Content"],"says":["server/Content","server/Content__Html"]}]
