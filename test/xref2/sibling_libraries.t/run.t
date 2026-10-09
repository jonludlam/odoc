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

This test is for issue #1450. The units of each library name their own
library's modules. A reference that does not say which library it means is
ambiguous, and odoc says so.

  $ for lib in client server; do
  >   for m in content__Html content wrap; do
  >     ocamlc -bin-annot -no-alias-deps -I $lib -c $lib/$m.mli
  >   done
  > done

  $ for lib in client server; do
  >   for m in content__Html content wrap; do
  >     odoc compile --output-dir h --parent-id pkg/$lib -L $lib:h/pkg/$lib $lib/$m.cmti
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
[Content__Html] and the alias [C] all name a module of the unit's own library,
and no lookup is ambiguous. None of these names is among the imports of the
unit that mentions it, so no digest tells the two libraries apart. What does
is the libraries each unit recorded when it was compiled, which are its own.
The reference [{!Content.Html}] in [Content]'s documentation names the unit
itself, so it is not ambiguous either.

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
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  File "Content":
  Ambiguous lookup. Possible files: Content
  Content
  $ refs() { odoc_print $1 | jq -c '[.. | objects | select(has("`Reference")) | .["`Reference"][0] | [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique | join(" ")]'; }
Where each reference on the page points, in the order they are written. The
first two name a root module one library at a time, and a path reference says
which library it means, so both land where they were aimed. The third is the
plain [{!Content}], which is ambiguous, and odoc says so and takes one.

The fourth and fifth name a submodule of each. Naming the library covers the
root and no more, since [Content.Html] is an alias and [Content__Html] is
looked up once the root is found. A page records no libraries, so what
settles it is the libraries each unit recorded when it was compiled:
resolution that has reached the client's [Content] carries on among the
client's libraries. The sixth and seventh are in the module list's
paragraphs, below.

  $ refs h/pkg/page-all.odocl
  ["client/Content","server/Content","server/Content","client/Content client/Content__Html","server/Content server/Content__Html","server/Content server/Content__Html","server/Content server/Content__Html"]
  $ modules() { odoc_print $1 | jq -c 'def libs: [.. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique; [.. | objects | select(has("`Modules")) | .["`Modules"][] | {shown: (.[0] | libs), says: (.[1] | libs)}]'; }
A module list shows, beside each module it names, that module's own first
paragraph. The paragraph is resolved as the page is linked, not as the module
was. Its [{!Content.Html}] is a reference, looked up along the whole search
path, where both libraries have a [Content]. That is ambiguous, and it is two
of the warnings above: the client's paragraph names the server's module. An
author who means the client's writes [{!/client/Content.Html}].

  $ modules h/pkg/page-all.odocl
  [{"shown":["client/Content"],"says":["server/Content","server/Content__Html"]},{"shown":["server/Content"],"says":["server/Content","server/Content__Html"]}]
