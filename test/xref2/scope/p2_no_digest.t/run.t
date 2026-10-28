Rule P2 (doc/scope_model.mld, section 3.2): a root that its origin records
with no digest means the unit of that name among the libraries the origin
was compiled with, Λc(o). Pure aliases compiled with [-no-alias-deps] give
such roots (section 3.3). Every compile uses dune's [-no-alias-deps], and
[-L] for the unit's own library and its dependencies.

The file [digests.sh] defines [entry FILE N], which reads the import table
of [FILE] with [ocamlobjinfo] and prints [digest], [no digest] or [no
entry] for the name [N], and [from FILE N CMI...], which prints each [CMI]
whose own digest is the one [FILE] records for [N]. Digests are never
printed, as they change between compiler versions.

The source files:

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

How to read [odoc_print --short --long-paths --show-root-parents]. Each line
is one item of the signature. [U/0] is the item [U], with a number odoc adds
to tell apart items of the same name. [resolved(...)] is a resolved path,
and [root(pkg/dep/Dep)] is the unit [Dep] of the library [dep] in the
package [pkg]. [unresolvedroot(Dep)] is a root that is not resolved.
[alias(T,P)] is the path [P], which is an alias to [T].
[identifier(root(pkg/n/Na),false)] is the path to [Na] given by its
identifier; [false] says it is not hidden.

P2.2. A pure alias to a unit of a dependency library, in the style of
[module U = Unix]. The library [dep] has the unit [Dep]. The library [w] is
built against [dep]; [w.mli] is [module U = Dep], and should record no
digest for [Dep].

  $ cat dep/dep.mli
  type t
  $ cat w/w.mli
  module U = Dep
  $ ocamlc -bin-annot -no-alias-deps -c dep/dep.mli
  $ ocamlc -bin-annot -no-alias-deps -I dep -c w/w.mli
  $ entry w/w.cmti Dep
  no digest
  $ odoc compile --output-dir h --parent-id pkg/dep -L dep:h/pkg/dep dep/dep.cmti
  $ odoc compile --output-dir h --parent-id pkg/w -L w:h/pkg/w -L dep:h/pkg/dep w/w.cmti

The spec lets odoc resolve this root at compile or leave it for link
(section 7). At link, [U] should mean [dep]'s [Dep].

  $ odoc_print --short --long-paths --show-root-parents h/pkg/w/w.odoc
  module U/0 = unresolvedroot(Dep)
  $ odoc link -L dep:h/pkg/dep -L w:h/pkg/w h/pkg/w/w.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/w/w.odocl
  module U/0 = resolved(root(pkg/dep/Dep))

P2.4. A chain of aliases across two libraries. The library [m] has [Mc],
with [type t], and [Mb], which is [module B = Mc]. The library [n] is built
against [m]; [Na] is [module A = Mb]. Neither alias records a digest. The
library [o] is built against [n] and [m]; [o.mli] is [val z : Na.A.B.t],
so [o] records a digest for each of the three units.

  $ cat m/mc.mli
  type t
  $ cat m/mb.mli
  module B = Mc
  $ cat n/na.mli
  module A = Mb
  $ cat o/o.mli
  val z : Na.A.B.t
  $ ocamlc -bin-annot -no-alias-deps -c m/mc.mli
  $ ocamlc -bin-annot -no-alias-deps -I m -c m/mb.mli
  $ ocamlc -bin-annot -no-alias-deps -I m -c n/na.mli
  $ ocamlc -bin-annot -no-alias-deps -I n -I m -c o/o.mli
  $ entry n/na.cmti Mb
  no digest
  $ entry m/mb.cmti Mc
  no digest
  $ odoc compile --output-dir h --parent-id pkg/m -L m:h/pkg/m m/mc.cmti
  $ odoc compile --output-dir h --parent-id pkg/m -L m:h/pkg/m m/mb.cmti
  $ odoc compile --output-dir h --parent-id pkg/n -L n:h/pkg/n -L m:h/pkg/m n/na.cmti
  $ odoc compile --output-dir h --parent-id pkg/o -L o:h/pkg/o -L n:h/pkg/n -L m:h/pkg/m o/o.cmti

The root [Na] of [z]'s path has a digest, so P1 gives [n]'s [Na].
Projection then meets the alias [Mb], whose origin is [Na], and the alias
[Mc], whose origin is [Mb] (section 3.2). Each should be resolved by P2 in
the scope of its own origin: [Mb] in Λc(Na), [Mc] in Λc(Mb). So [Na.A]
should mean [m]'s [Mb], and [Na.A.B] [m]'s [Mc].

  $ odoc link -L m:h/pkg/m -L n:h/pkg/n -L o:h/pkg/o h/pkg/o/o.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/o/o.odocl
  val z/0 : resolved(alias(alias(root(pkg/m/Mc),identifier(root(pkg/m/Mb),false).B),identifier(root(pkg/n/Na),false).A.B).t)

P2.6 (and section 3.6). A pure alias to a unit that does not exist. The
compiler gives warning 49 for it; [-w -49] silences it, as dune does for
its alias modules. [g.mli] is [module X = Nope]. It should record no digest
for [Nope].

  $ cat g/g.mli
  module X = Nope
  $ ocamlc -bin-annot -no-alias-deps -w -49 -c g/g.mli
  $ entry g/g.cmti Nope
  no digest
  $ odoc compile --output-dir h --parent-id pkg/g -L g:h/pkg/g g/g.cmti

No unit in Λc(G) is named [Nope], so [X] should be unresolved. This is a
missing root: it should not be reported by default, and should be reported
with [--enable-missing-root-warning].

  $ odoc link -L g:h/pkg/g h/pkg/g/g.odoc
  $ odoc_print --short --long-paths --show-root-parents h/pkg/g/g.odocl
  module X/0 = unresolvedroot(Nope)
  $ odoc link --enable-missing-root-warning -L g:h/pkg/g h/pkg/g/g.odoc
  File "h/pkg/g/g.odoc":
  Warning: Couldn't find the following modules:
    Nope

P2.7. The example of section 3.3, which breaks D3. The libraries [ext1] and
[ext2] are [(wrapped false)], and each has a unit [Ext], with different
contents. [l] is built against [ext1]; [l.mli] is [module M = Ext]. [c]
depends on [l] and [ext2], so it is built against [l], [ext1] and [ext2],
with [ext2] first in [-I]. [c.mli] is [val x : L.M.t]. To check it, the
compiler loads the first [Ext] in [-I], so [c] should record [ext2]'s
[Ext] (as in evidence.t, E3).

  $ cat ext1/ext.mli
  type t = A
  $ cat ext2/ext.mli
  type t = B
  $ cat l/l.mli
  module M = Ext
  $ cat c/c.mli
  val x : L.M.t
  $ ocamlc -bin-annot -no-alias-deps -c ext1/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -c ext2/ext.mli
  $ ocamlc -bin-annot -no-alias-deps -I ext1 -c l/l.mli
  $ ocamlc -bin-annot -no-alias-deps -I l -I ext2 -I ext1 -c c/c.mli
  $ from c/c.cmti Ext ext1/ext.cmi ext2/ext.cmi
  ext2/ext.cmi

  $ odoc compile --output-dir h --parent-id pkg/ext1 -L ext1:h/pkg/ext1 ext1/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/ext2 -L ext2:h/pkg/ext2 ext2/ext.cmti
  $ odoc compile --output-dir h --parent-id pkg/l -L l:h/pkg/l -L ext1:h/pkg/ext1 l/l.cmti
  $ odoc compile --output-dir h --parent-id pkg/c -L c:h/pkg/c -L l:h/pkg/l -L ext1:h/pkg/ext1 -L ext2:h/pkg/ext2 c/c.cmti
  $ odoc link -L ext1:h/pkg/ext1 -L ext2:h/pkg/ext2 -L l:h/pkg/l -L c:h/pkg/c h/pkg/l/l.odoc
  File "Ext":
  Ambiguous lookup. Possible units: pkg/ext1/Ext, pkg/ext2/Ext
  $ odoc link -L ext1:h/pkg/ext1 -L ext2:h/pkg/ext2 -L l:h/pkg/l -L c:h/pkg/c h/pkg/c/c.odoc

On [l]'s page, [M] should mean [ext1]'s [Ext], the author's target: Λc(L)
holds only that [Ext].

  $ odoc_print --short --long-paths --show-root-parents h/pkg/l/l.odocl
  module M/0 = resolved(root(pkg/ext2/Ext))

This is a known limit (section 3.3): Λc(C) holds two units named [Ext],
which D3 rules out, so the claims of section 3.7 do not hold for [c]. The
alias [Ext] in [x]'s path has the origin [L], so P2 gives [ext1]'s [Ext],
the author's choice, while the compiler took [ext2]'s for [c]. On [c]'s
page odoc may link either.

  $ odoc_print --short --long-paths --show-root-parents h/pkg/c/c.odocl
  val x/0 : resolved(alias(root(pkg/ext2/Ext),identifier(root(pkg/l/L),false).M).t)
