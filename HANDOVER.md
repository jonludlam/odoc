# Driver work in progress — delete this file before opening a PR

Scratch notes for whoever picks up the `driver-library-graph` branch. The commit
messages carry the reasoning for what has been done; this file only covers what
they cannot: where the work is going, what is known to be broken, and how to
check that a change to the driver did not break anything.

## The model, in one paragraph

The library graph — each library's declared `META` `requires` — is the only
source of cross-library dependency information. Module interface digests are
used for exactly two local purposes: ordering the modules of one library against
each other, and identifying which `.cmti` provides a virtual library's
interface. Two things are derived from the graph and must not be confused: the
**reference scope** (`-L`/`-P`, the *direct* requires, per package) and the
**search path** (`-I`, the *transitive closure*, per library). Between driver
runs, everything a later run needs about an earlier one travels in the markers
(`Marker.Pkg`, `Marker.Lib`) written beside the odoc output.

## Remaining plan

1. **Coarsen the compile schedule.** Prune each unit's `deps` to its own
   library's modules (~25 lines in `odoc_units_of.ml`, applied and reverted
   twice, trivially redone), then replace the recursive `compile_mod` and its
   promise table in `compile.ml` with two levels of Kahn waves: libraries
   ordered by the graph, modules within a library by their imports. Compute the
   order up front rather than awaiting promises, so a cycle cannot deadlock.
   **Do not** apply the pruning without the scheduler — the tree is incoherent
   in between, as unit deps no longer describe cross-library ordering.
2. **Delete the partials.** `__odoc_partial.m` has no readers left once (1)
   lands: `find_partials`, `mk_byhash`, `build_all_hashes`, `marshal`,
   `unmarshal`, and the `?partial`/`partial_dir` parameters all go. The file is
   a `Marshal` dump of live internal types, so any change to `Odoc_unit.t`
   silently poisons a warm cache — removing it closes that hazard.
3. **Make `odoc_driver` work package-by-package**, the way
   `odoc_driver_voodoo` does, so there is one orchestration rather than two and
   every run exercises the marker path. Blocked on the virtual-library gap
   below. Open question: one process looping over packages in dependency order
   (simpler, keeps the shared worker pool, process packages in waves to keep
   parallelism) versus a process per package (true isolation, natural
   incremental rebuild, what docs-ci already does).

## Known gaps, not fixed

- **A virtual library is never discovered in voodoo mode.** Its `META` entry has
  an empty `archive` and no `directory`, so it reaches neither `odoc classify`
  nor `cmi_only_libs`. Nothing stashes its `.cmti`, the stash branch of
  `Packages.remap_virtual` is dormant, and implementations get documented from
  their `.cmt` files without the interface documentation. Opam mode does the
  right thing (`checkseum.cmti` is compiled three times). This predates the
  branch — a control run at `6de44e215` behaves identically — but it blocks
  step 3, because per-package processing would newly expose opam mode to it.
- **`odoc classify` misattributes modules.** It assigns a `compiler-libs.common`
  module that imports `compiler-libs.bytecomp`, producing an apparent edge the
  declared graph cannot contain. Deliberately not worked around here; it costs
  nothing today only because the two are libraries of one package, so the
  package-wide `-L` still carries `bytecomp` at link time. It would bite for a
  misattribution across a package boundary. Fix belongs in `classify`.
- **The driver has no tests at all.** The marker format is a versioned on-disk
  contract spanning invocations and warm caches, verified only by a throwaway
  executable. A cram test building a two-package prep tree and running
  `odoc_driver_voodoo` over it in dependency order would cover the marker
  write/read cycle, the cross-package closure and the format's legacy
  tolerance — the area with zero coverage and most of this branch's risk.

## Verifying a driver change

Compare against a baseline **re-run in the same session**. The environment
drifts; a stored baseline from an earlier session produced a 1365-file phantom
diff that vanished when the baseline was re-run.

- **Compare HTML, not `.odocl`.** `.odocl` files embed their own output
  directory, so two runs into different directories always differ. Confirm with
  a control run of unchanged code into a second directory.
- **Count `class="xref-unresolved"` in the HTML.** This is the real gate. A
  genuine regression went through with odoc's warning output byte-identical.
- **Normalise trailing slashes** when diffing `-I` sets. `Fpath.parent` yields
  one and `//` does not; 814 units once looked like they had lost include
  directories purely because of this.
- `Fmt.(list string)` has a cut separator that prints nothing unboxed, so debug
  logs render `["num.core"; "parsexp"]` as `num.coreparsexp`.

Pin the tools, and make sure the `odoc` binary and the `.cmi` files it reads
come from the same compiler — a mismatch shows up as `Cmi_format.Error` inside
`odoc classify`:

```sh
B=$PWD/_build/install/default/bin
$B/odoc_driver --odoc $B/odoc --odoc-md $B/odoc-md \
  --odoc-dir /tmp/t/odoc --odocl-dir /tmp/t/odocl --index-dir /tmp/t/index \
  --mld-dir /tmp/t/mld --html-dir /tmp/t/html checkseum digestif tyxml
```

`checkseum` and `digestif` are virtual-library packages and `tyxml` exercises
the wrapper-package case; that trio is a good default target.

## Exercising voodoo mode

There is no fixture, so build a prep tree by hand. Voodoo compiles one package
per invocation and reads what earlier invocations left behind, so run the
packages in dependency order:

```sh
W=/tmp/voodoo; L=$(ocamlfind printconf destdir)
for pkg in optint checkseum; do
  ver=$(opam list -i --columns=version -s $pkg | tr -d ' \n')
  mkdir -p $W/prep/universes/u1/$pkg/$ver/lib/$pkg
  cp -R $L/$pkg/. $W/prep/universes/u1/$pkg/$ver/lib/$pkg/
done
cd $W && for pkg in optint checkseum; do
  $B/odoc_driver_voodoo --blessed --odoc-dir $W/odoc --odocl-dir $W/odocl \
    --html-dir $W/html $pkg
done
```

Check `$W/odoc/p/*/*/doc/.odoc_pkg_marker` for the declared graph and that a
`checkseum` unit's `-I` contains `optint`'s directory, which can only have come
from the markers `optint`'s run wrote.
