Rule S2 (doc/scope_model.mld, section 5), its known limit: dune's
[@canonical] tags name no library. Until dune writes tags with a library, a
tag naming [N] that is written in a unit dune names after [N] ([N], [N__],
or [N__] followed by a module name) means [N] in that unit's own library.
[odoc compile] knows that library from the [-L] whose directory holds its
output. This rule is temporary, and so is this test.

This is the shape of eliom. Libraries [client] and [server] are two dune
wrapped libraries with the same main module [Eliom], written by hand. dune
generates the alias module [Eliom__] of each, with
[module Content = Eliom__Content] and the tag [@canonical Eliom.Content],
and compiles the member [content.mli] as the hidden unit [Eliom__Content].
Their [t]s differ. The library [app_client] is built against [client] only,
and [app_server] against [server] only. Each has a unit [App] with
[type v = Eliom.Content.t]. No unit's Λc holds both [Eliom]s (D3).

As dune does, each alias module is compiled first, with
[-nopervasives -nostdlib -w -49], and the members and the main module with
[-open Eliom__]. Every compile uses dune's [-no-alias-deps], and [-L] for the
unit's own library and its dependencies.

  $ cat client/eliom__.ml-gen
  (** @canonical Eliom.Content *)
  module Content = Eliom__Content
  $ cat client/content.mli
  type t
  
  val client_only : t
  $ cat client/eliom.mli
  (** The client side. *)
  
  module Content = Content
  $ cat server/eliom__.ml-gen
  (** @canonical Eliom.Content *)
  module Content = Eliom__Content
  $ cat server/content.mli
  type t
  
  val server_only : t
  $ cat server/eliom.mli
  (** The server side. *)
  
  module Content = Content
  $ cat app_client/app.mli
  type v = Eliom.Content.t
  $ cat app_server/app.mli
  type v = Eliom.Content.t
  $ for l in client server; do
  >   ocamlc -bin-annot -no-alias-deps -nopervasives -nostdlib -w -49 -I $l -o $l/eliom__.cmo -c -impl $l/eliom__.ml-gen
  >   ocamlc -bin-annot -no-alias-deps -I $l -open Eliom__ -o $l/eliom__Content.cmi -c -intf $l/content.mli
  >   ocamlc -bin-annot -no-alias-deps -I $l -open Eliom__ -o $l/eliom.cmi -c -intf $l/eliom.mli
  >   ocamlc -bin-annot -no-alias-deps -I $l -o app_$l/app.cmi -c -intf app_$l/app.mli
  > done
  $ for l in client server; do
  >   for u in eliom__.cmt eliom__Content.cmti eliom.cmti; do
  >     odoc compile --output-dir h --parent-id pkg/$l -L $l:h/pkg/$l $l/$u
  >   done
  >   odoc compile --output-dir h --parent-id pkg/app_$l -L app_$l:h/pkg/app_$l -L $l:h/pkg/$l app_$l/app.cmti
  > done

Every library is given at link, as for a package that has both. [$ALL]
holds the [-L] options for that.

Each tag [Eliom.Content] is written in an [Eliom__], so it should mean the
[Eliom] of the library of that [Eliom__]. Linking should give no warning.
An "Ambiguous lookup" warning for [Eliom] would be wrong.

  $ ALL="-L client:h/pkg/client -L server:h/pkg/server -L app_client:h/pkg/app_client -L app_server:h/pkg/app_server"
  $ for u in client/eliom__ client/eliom__Content client/eliom server/eliom__ server/eliom__Content server/eliom app_client/app app_server/app; do odoc link $ALL h/pkg/$u.odoc; done

How to read [odoc_print --short --long-paths --show-root-parents]. Each line
is one item of the signature. [v/0] is the item [v], with a number odoc adds
to tell apart items of the same name. [resolved(...)] is a resolved path,
and [root(pkg/client/Eliom)] is the unit [Eliom] of the library [client] in
the package [pkg]. [hidden(...)] marks a path to a hidden unit.
[alias(A,B)] is the path [B], an alias of [A], and [identifier(X,false)] is
the unit [X] as the path names it.
[canonical(A,resolved(B))] is the path [A] shown as its [@canonical] target
[B].

[app_client]'s [v] goes through [client]'s [Eliom.Content], which is an
alias of [client]'s hidden [Eliom__Content]. It should be shown as
[client]'s [Eliom.Content.t]. [app_server]'s [v] should be shown as
[server]'s [Eliom.Content.t].

Showing either [v] as, or linking it to, the other library's
[Eliom.Content.t] would be wrong.

  $ odoc_print --short --long-paths --show-root-parents h/pkg/app_client/app.odocl
  type v/0 = resolved(alias(canonical(hidden(root(pkg/client/Eliom__)).Content,resolved(root(pkg/client/Eliom).Content)),identifier(root(pkg/client/Eliom),false).Content).t)
  $ odoc_print --short --long-paths --show-root-parents h/pkg/app_server/app.odocl
  type v/0 = resolved(alias(canonical(hidden(root(pkg/server/Eliom__)).Content,resolved(root(pkg/server/Eliom).Content)),identifier(root(pkg/server/Eliom),false).Content).t)

The links, in the HTML of each [App]:

  $ for l in client server; do odoc html-generate -o html h/pkg/app_$l/app.odocl; done
  $ grep -o 'href="[^"]*Content[^"]*"' html/pkg/app_client/App/index.html
  href="../../client/Eliom/Content/index.html#type-t"
  $ grep -o 'href="[^"]*Content[^"]*"' html/pkg/app_server/App/index.html
  href="../../server/Eliom/Content/index.html#type-t"
