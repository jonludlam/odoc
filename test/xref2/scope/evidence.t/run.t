Evidence for the claims about the compiler that doc/scope_model.mld relies on
(section 9). No odoc rule is tested here, except in E6, which checks that
odoc reads the digest the compiler records.

Every compile uses dune's [-bin-annot -no-alias-deps]. The two libraries
[ext1] and [ext2] each have a module [Ext], with different contents.

  $ cat ext1/ext.mli
  type t
  val a : t
  $ cat ext2/ext.mli
  type t
  val b : t

  $ ocamlc -bin-annot -no-alias-deps -c ext1/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -c ext2/ext.mli

How to read the output. The file [digests.sh] defines shell helpers that
read the import table of a compiled file with [ocamlobjinfo]. Each row of
that table is a digest and a name; a digest of dashes means no digest. For a
[.cmt] or [.cmti], the helpers read the table headed "Cmt interfaces
imported", which is the one odoc reads ([cmt_imports] in
[src/loader/odoc_loader.ml]). The helpers never print a digest, since
digests change between compiler versions. Instead they compare digests:

[entry FILE N] prints [digest], [no digest] or [no entry]: what [FILE]
records for the name [N]. [from FILE N CMI...] prints each [CMI] whose own
digest (its own row for [N]) is the one [FILE] records for [N], or [none].
[same A B] prints [same] or [different] for two digests, or [missing] if
one of them is empty. [digest FILE N] gives the digest [FILE] records for
[N], and [iface FILE] the interface digest a [.cmt] records for itself.

  $ cat digests.sh
  # Helpers that read the import table of a compiled file with ocamlobjinfo.
  # For a .cmt or .cmti they read the "Cmt interfaces imported" table, which
  # is the one odoc reads (cmt_imports); for other files, "Interfaces
  # imported". Each row of a table is a digest, a tab and a name. A digest of
  # dashes means no digest. The helpers never print a digest, as digests
  # change between compiler versions.
  
  table () {
    case "$1" in
      *.cmt|*.cmti) h="Cmt interfaces imported:" ;;
      *) h="Interfaces imported:" ;;
    esac
    ocamlobjinfo "$1" | awk -v h="$h" '$0 == h { t = 1; next } t && !/^\t/ { t = 0 } t'
  }
  
  # [entry FILE NAME]: "digest", "no digest" or "no entry" for NAME in FILE.
  entry () {
    table "$1" | awk -F'\t' -v n="$2" '$3 == n { f = 1; print ($2 ~ /^-+$/ ? "no digest" : "digest") } END { if (!f) print "no entry" }'
  }
  
  # [digest FILE NAME]: the digest FILE records for NAME, if any.
  digest () {
    table "$1" | awk -F'\t' -v n="$2" '$3 == n && $2 !~ /^-+$/ { print $2 }'
  }
  
  # [iface FILE]: the interface digest a .cmt records for its own unit.
  iface () {
    ocamlobjinfo "$1" | sed -n 's/^cmt interface digest: *//p'
  }
  
  # [same A B]: "same" or "different"; "missing" if either is empty.
  same () {
    if [ -z "$1" ] || [ -z "$2" ]; then echo missing
    elif [ "$1" = "$2" ]; then echo same
    else echo different; fi
  }
  
  # [from FILE NAME CMI...]: each CMI whose own digest FILE records for NAME.
  from () {
    f=$1; n=$2; shift 2; d=$(digest "$f" "$n"); r=none
    for c in "$@"; do
      if [ -n "$d" ] && [ "$d" = "$(digest "$c" "$n")" ]; then echo "$c"; r=; fi
    done
    if [ -n "$r" ]; then echo "$r"; fi
  }

  $ . ./digests.sh

E1 (section 3.4). At [-c], when two directories in [-I] both hold an [Ext],
the compiler binds [Ext] to the first one, and does not reject the unit.
This holds for an interface and for an implementation. [c.mli] and [d.ml]
both use [Ext.t]. Built with [ext1] first, each should record [ext1]'s
[Ext]; built with [ext2] first, [ext2]'s.

  $ cat c/c.mli
  val x : Ext.t
  $ cat c/d.ml
  let f (x : Ext.t) = x

  $ ocamlc -bin-annot -no-alias-deps -I ext1 -I ext2 -c c/c.mli
  $ from c/c.cmti Ext ext1/ext.cmi ext2/ext.cmi
  ext1/ext.cmi
  $ ocamlc -bin-annot -no-alias-deps -I ext2 -I ext1 -c c/c.mli
  $ from c/c.cmti Ext ext1/ext.cmi ext2/ext.cmi
  ext2/ext.cmi

  $ ocamlc -bin-annot -no-alias-deps -I ext1 -I ext2 -c c/d.ml
  $ from c/d.cmt Ext ext1/ext.cmi ext2/ext.cmi
  ext1/ext.cmi
  $ ocamlc -bin-annot -no-alias-deps -I ext2 -I ext1 -c c/d.ml
  $ from c/d.cmt Ext ext1/ext.cmi ext2/ext.cmi
  ext2/ext.cmi

E2 (sections 1 and 3.3). A pure alias compiled with [-no-alias-deps]
records no digest. [l.mli] is [module M = Ext], built against [ext1]. The
compiler should not look [Ext] up, so [l]'s [.cmi] and [.cmti] should have
an entry for [Ext] with no digest.

  $ cat l/l.mli
  module M = Ext

  $ ocamlc -bin-annot -no-alias-deps -I ext1 -c l/l.mli
  $ entry l/l.cmi Ext
  no digest
  $ entry l/l.cmti Ext
  no digest

E3 (section 3.3). A unit that goes through the alias binds it again, with
its own [-I]. [u.mli] is [val y : L.M.t]. To check it, the compiler must
load an [Ext], so [u] should record the digest of the [Ext] it loaded: the
first in [-I] order. With [ext1] only, that is [ext1]'s. With [ext2] before
[ext1], it is [ext2]'s, which [l]'s author never saw.

  $ cat u/u.mli
  val y : L.M.t

  $ ocamlc -bin-annot -no-alias-deps -I l -I ext1 -c u/u.mli
  $ from u/u.cmti Ext ext1/ext.cmi ext2/ext.cmi
  ext1/ext.cmi
  $ ocamlc -bin-annot -no-alias-deps -I l -I ext2 -I ext1 -c u/u.mli
  $ from u/u.cmti Ext ext1/ext.cmi ext2/ext.cmi
  ext2/ext.cmi

E4 (section 1). The compiler adds to a unit's import table every name that
has a digest in every [.cmi] it loads, with the same digest, even if the
unit never uses that name. [b.mli] has [type other = int] and [val y :
Ext.t], built against [ext1]. [w.mli] is [val z : B.other]: it never uses
[Ext], and is built with [-I b] only, so the compiler cannot even find an
[Ext]. Still, [w] should record [ext1]'s [Ext], copied from [b]'s table.

  $ cat b/b.mli
  type other = int
  val y : Ext.t
  $ cat w/w.mli
  val z : B.other

  $ ocamlc -bin-annot -no-alias-deps -I ext1 -c b/b.mli
  $ ocamlc -bin-annot -no-alias-deps -I b -c w/w.mli
  $ entry w/w.cmti Ext
  digest
  $ same "$(digest w/w.cmti Ext)" "$(digest b/b.cmi Ext)"
  same

E5 (section 3.1). [include La] and [module type of La] bring in [La]'s
[module M = Ext], but the including unit should have no entry for [Ext].
A name with no digest in a loaded [.cmi] is not copied, unlike E4. So a root
[Ext] in such a signature has a meaning only through the origin [La].
[la] is [module M = Ext], built against [ext1]. [k.mli] is [include module
type of La]. [j.ml] has [include La] and [module type S = module type of
La]; [ocamlc -i] shows that both bring in [module M = Ext].

  $ cat la/la.mli
  module M = Ext
  $ cat la/la.ml
  module M = Ext
  $ cat k/k.mli
  include module type of La
  $ cat k/j.ml
  include La
  module type S = module type of La

  $ ocamlc -bin-annot -no-alias-deps -I ext1 -c la/la.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext1 -I la -c la/la.ml
  $ ocamlc -bin-annot -no-alias-deps -I la -I ext1 -c k/k.mli
  $ entry k/k.cmti Ext
  no entry
  $ ocamlc -bin-annot -no-alias-deps -I la -I ext1 -i k/j.ml
  module M = Ext
  module type S = sig module M = Ext end
  $ ocamlc -bin-annot -no-alias-deps -I la -I ext1 -c k/j.ml
  $ entry k/j.cmt Ext
  no entry

E6 (sections 1 and 3.2). For a [.cmt] with no [.mli], [dg(u)] (the digest
odoc gives the unit) should be the digest other units record when they
import it. [noi.ml] is [type t = int], with no [.mli]. odoc takes [dg(u)]
from the interface digest in the [.cmt] ([cmt_interface_digest] in
[src/loader/odoc_loader.ml]). [usr.mli] is [val y : Noi.t], built against
[noi]. The digest [usr] records for [Noi] should be that one.

  $ cat noi/noi.ml
  type t = int
  $ cat usr/usr.mli
  val y : Noi.t

  $ ocamlc -bin-annot -no-alias-deps -c noi/noi.ml
  $ ocamlc -bin-annot -no-alias-deps -I noi -c usr/usr.mli
  $ same "$(digest usr/usr.cmti Noi)" "$(iface noi/noi.cmt)"
  same

odoc itself should agree. P1 (section 3.2) picks a unit by its digest, so
the path [Noi.t] in [usr] resolves only if odoc's [dg(u)] for [Noi] is the
one [usr] records.

How to read [odoc_print --short --long-paths --show-root-parents]. Each line
is one item of the signature. [y/0] is the item [y], with a number odoc adds
to tell apart items of the same name. [resolved(...)] is a resolved path,
and [root(pkg/noi/Noi)] is the unit [Noi] of the library [noi] in the
package [pkg]. [unresolvedroot(Noi)] is a root that is not resolved.

[noi] is compiled first (D5), so the path should be resolved in the
[.odoc] already, to [noi]'s [Noi]:

  $ odoc compile --output-dir h --parent-id pkg/noi -L noi:h/pkg/noi noi/noi.cmt
  $ odoc compile --output-dir h --parent-id pkg/usr -L usr:h/pkg/usr -L noi:h/pkg/noi usr/usr.cmti
  $ odoc_print --short --long-paths --show-root-parents h/pkg/usr/usr.odoc
  val y/0 : resolved(root(pkg/noi/Noi).t)

As a control, [noi] is rebuilt with other contents, and so another digest,
while [usr] is not. [noi2/noi.ml] is [type t = string]; its [.odoc] takes
the place of the old one in [h/pkg/noi]. The path should then not resolve,
since no unit has the digest [usr] records.

  $ cat noi2/noi.ml
  type t = string

  $ ocamlc -bin-annot -no-alias-deps -c noi2/noi.ml
  $ same "$(digest usr/usr.cmti Noi)" "$(iface noi2/noi.cmt)"
  different
  $ odoc compile --output-dir h --parent-id pkg/noi -L noi:h/pkg/noi noi2/noi.cmt
  $ odoc compile --output-dir h --parent-id pkg/usr -L usr:h/pkg/usr -L noi:h/pkg/noi usr/usr.cmti
  $ odoc_print --short --long-paths --show-root-parents h/pkg/usr/usr.odoc
  val y/0 : unresolvedroot(Noi).t

E7 (section 3.1). In a [.cmt] with no [.mli], the inferred type of a value
can name a unit the source never names. The compiler made that root, and
its origin is the unit whose [.cmt] holds it, so that unit's table must
have an entry, with a digest. [ty.mli] has [type t] and [val v : t]. [mk.ml]
is [let f () = Ty.v]. [inf.ml] is [let x = Mk.f ()]: it never names [Ty],
but [x] has the type [Ty.t]. [inf.cmt] should record [ty]'s [Ty].

  $ cat ty/ty.mli
  type t
  val v : t
  $ cat mk/mk.ml
  let f () = Ty.v
  $ cat inf/inf.ml
  let x = Mk.f ()

  $ ocamlc -bin-annot -no-alias-deps -c ty/ty.mli
  $ ocamlc -bin-annot -no-alias-deps -I ty -c mk/mk.ml
  $ ocamlc -bin-annot -no-alias-deps -I mk -I ty -i inf/inf.ml
  val x : Ty.t
  $ ocamlc -bin-annot -no-alias-deps -I mk -I ty -c inf/inf.ml
  $ from inf/inf.cmt Ty ty/ty.cmi
  ty/ty.cmi

E8 (section 3.5). dune compiles each implementation of a virtual library
against the virtual library's [.cmi], with [-cmi-file]. So the
implementation, and every unit that uses the virtual module, should record
the virtual library's digest. [vlib/vm.mli] is the virtual module, and
[impl/vm.ml] its implementation. [vuser.mli] is [val y : Vm.t], built
against [vlib].

  $ cat vlib/vm.mli
  type t
  val v : t
  $ cat impl/vm.ml
  type t = int let v = 1
  $ cat vuser/vuser.mli
  val y : Vm.t

  $ ocamlc -bin-annot -no-alias-deps -c vlib/vm.mli
  $ ocamlc -bin-annot -no-alias-deps -I vlib -cmi-file vlib/vm.cmi -c impl/vm.ml
  $ ocamlc -bin-annot -no-alias-deps -I vlib -c vuser/vuser.mli
  $ same "$(digest impl/vm.cmt Vm)" "$(digest vlib/vm.cmi Vm)"
  same
  $ same "$(digest vuser/vuser.cmti Vm)" "$(digest vlib/vm.cmi Vm)"
  same

The implementation should have no [.cmi] of its own, and its [.cmt] should
hold no interface digest. So a driver that documents the interface again
for [impl] (section 3.5) can only take it from [vlib]'s files.

  $ ls impl
  vm.cmo
  vm.cmt
  vm.ml
  $ same "$(iface impl/vm.cmt)" "$(digest vlib/vm.cmi Vm)"
  missing
