# Driver rewrite and name resolution: where we are

Branch `driver-per-package`, replacing PR #1461. This note records the state of
the work, the options considered for issue #1450, and what each one costs. It is
a working document, not a design doc for merging.

## 1. The branch

`odoc_driver` builds one package at a time, the way `odoc_driver_voodoo` does,
instead of assembling every unit into one structure and building it in parallel.
The main consequences:

- **No partials and no markers.** Every location is a function of a name, so a
  run finds what earlier runs built without any record of it.
- **The odoc directory mirrors the switch.** A library's `.odoc` files sit at
  `lib/<findlib dir>`, a package's pages below `doc/<pkg>`. Libraries installed
  in one directory share one odoc directory, so a single `-I` finds them, and
  `--odoc-dir <switch prefix>` is legitimate. This replaces the digest-based
  recovery of undeclared dependencies that `fix_missing_deps` did.
- **Scope from findlib and opam.** `-L` is the package's libraries, their direct
  `META` requires and the `odoc-config.sexp` additions; `-P` is the package, the
  packages providing those libraries and the config additions; `-I` per library
  is its dependency cone.
- **Ordering by recursion, not sorting.** A memoised recursive function compiles
  a library after the libraries it requires. Before a package is linked, every
  package its scope names is compiled, which is the compile/link split voodoo
  mode exposes as `--actions`.
- `odoc_driver_monorepo` is gone. Every module has an `.mli` with documentation,
  and `doc/driver.mld` is a walkthrough of a standard package followed by the
  other kinds.

The driver is 4981 lines across 45 files, against 5421 across 40 on master.

### Validation

Whole 5.4.1 switch, 434 packages including eliom, same machine:

| | master | branch |
|---|---|---|
| HTML pages | 32335 | 32335 |
| wall clock | 463s | 289s |
| errors | 0 | 0 |

Pages differing: 167, out of 32335. A page can differ in more than one way:
39 gain a resolved link, 50 change a link target, 114 differ in some other way,
mostly a synopsis link that now resolves, and 2 lose a link. See section 5 for
the two.

Voodoo mode has not been run locally. It needs a docs-ci run.

## 2. Issue #1450, and what it actually is

Vincent Balat reported that eliom's client documentation shows the server's
modules: `eliom.client` and `eliom.server` define the same module names on
purpose, and with both on the link path the wrong one wins. He named two parts,
the modules picked by first-found order and the canonical paths.

Odoc resolves a root module name at link time. Where the name is among the
unit's imports, the digest settles which file is meant. Three kinds of lookup
have no digest to go on:

1. **Paths in a signature.** The expansion of a module, and aliases, where the
   root is not an import. Common under `-no-alias-deps`, which dune passes.
2. **Canonical paths.** The root of `@canonical Eliom_content.Html` is the
   library's own main module, which the submodule never imports.
3. **References in doc comments.** Never imports, by construction.

Only the third is something an author can qualify, with
`{!/eliom.client/Eliom_content}`. A path in a signature is the compiler's, not
the author's. A canonical tag takes a dotted path: a bare root is rejected and
there is no library-qualified form. So advice to authors cannot fix the reported
bug, whatever else it is good for.

**PR #1461 did not fix this.** Its per-library `-I` addressed a neighbouring
problem, alternative implementations of a virtual library. Its commit message
says `-L` and `-P` remain package-wide, which is exactly the condition the
report describes. Vincent asked on the issue whether the plan would tell sibling
libraries apart, suggesting the hash; that question was never answered.

## 3. What the branch does now

**In odoc**, one lookup with an order: a module name is looked up in the `-I`
directories first, and in the `-L` directories only if that finds nothing. The
`-I` directories hold what the compiler gave the unit, so a name they answer
means the module the unit was built against. This covers all three kinds above.
Some thirty lines in `src/odoc/resolver.ml`, a documentation line on the `-L`
option and one on the resolver's interface, plus a cram test with two mirrored
libraries. Nothing else in odoc changes.

**In the driver**, the page written for a library is linked with that library's
`-I`. The page lists the library's modules and shows a synopsis for each, lifted
from the module and resolved as the page is linked; without the search path
those synopses resolve in a context that has neither the module nor its
dependencies.

Measured on eliom 12.1.0:

| | content links from client pages into eliom.server | ambiguous lookups |
|---|---|---|
| master | 116, on 41 pages | 320 |
| branch | 0 | 193 |

## 4. Designs tried and rejected

### A hard split: `-I` for paths, `-L` and `-P` for references

The first attempt, and the rule we agreed on before measuring it. Rejected
because resolution does not stay in one category. A reference has to keep
resolving after it lands: the client's own comment `{!Content.Html}` finds the
root locally, then follows an alias whose target is a path, and that path was
resolved in the reference's context, which reaches both siblings. The client's
own page then linked to the server's module.

Making paths always use `-I` instead breaks the other direction: a reference
into another library cannot be followed at all, because that library's cone is
not on our `-I`. Measured: the package index for `fix` lost every synopsis,
since a synopsis needs the module's expansion. A hard split cannot have both.
An ordered search can.

- **For:** each flag has one clear job, easy to describe.
- **Against:** does not survive contact with nested resolution, as above.

### A narrower scope for the library's page

Second attempt at the page problem: link it with the library's own scope rather
than the package's. It fixed eliom, and cost three pages elsewhere, where a
low-level library's page pointed up at the package's wrapper library, which
requires it rather than the other way round: `progress.engine`,
`capnp-rpc.proto`, `ocsigenserver.baselib.base`. Replaced by giving the page the
library's `-I`, which fixes eliom and keeps those three.

- **For:** no odoc change at all.
- **Against:** loses upward references; makes a generated page resolve in a
  narrower world than the modules it describes, which is hard to justify.

## 5. Known regressions against master

Two eliom.server source pages lose their links into js_of_ocaml. The cause is a
`META` gap rather than any decision here: `eliom.server` requires
`js_of_ocaml.deriving`, whose own `META` declares no requirements at all, so
js_of_ocaml is not in the dependency cone and not on the include path. Master
recovered it with `fix_missing_deps`, which this branch drops by design. The
interface pages are unaffected, because the fallback to `-L` still finds the
module; only source links need the implementation on the include path.

Worth reporting upstream to both packages.

## 6. Options not taken

### Resolve at compile time and record the answer

Compile has the compiler's search path and currently refuses to resolve any name
the imports do not vouch for, which is why these names reach link at all.
Letting compile resolve them against its own `-I` would settle signature paths
where the information is authoritative.

- **For:** the answer is computed where it is known, and link stops guessing.
- **Against:** reaches only the first of the three kinds. Canonical paths are
  resolved during link, in `reresolve_module`, and references are link-time by
  nature. Half the report, none of the page.

### Prefer the current library

Odoc already derives the current library from where the input sits under a `-L`
root, so module units would need no new input; a generated page would need a
flag naming its library.

- **For:** no reliance on the include path; explicit.
- **Against:** covers less. A module in library A referring to a module of its
  dependency B, where a sibling C defines that name too, is in neither A nor C's
  favour and stays ambiguous. The include path answers that, because B is on it.

### Stop resolving references inside lifted synopses

A synopsis on an index page is a one-line summary, and the links inside it are
worth little. Rendering them as plain text removes the whole cross-context class
at a stroke, library page and package index alike.

- **For:** no scope reasoning anywhere; kills the class rather than managing it.
- **Against:** a visible change to every index page in the world; on eliom it
  costs exactly one link, the one that was wrong.

### A hidden-module heuristic

Prefer the closest candidate when the ambiguous name is a hidden module.

- **For:** obviously safe, since a hidden module is an implementation detail of
  one library and nobody refers to another library's.
- **Against:** inert on the actual problem. Eliom ships no hidden modules: zero
  files with `__` across its 34 client and 53 server interfaces, and all 320 of
  its ambiguous lookups are visible names. Collisions inside one package need
  unwrapped libraries, and unwrapped libraries have no hidden modules; wrapped
  libraries cannot collide, because two libraries in a package get different
  wrapper names and their hidden modules inherit those prefixes. The one place
  the two meet is a virtual library and its implementations, which the
  per-library `-I` already separates.

### Author discipline alone

Tell authors to write `{!/eliom.client/Eliom_content}`.

- **For:** no tool change; unambiguous by construction; the only thing that
  works for references from pages, which have no include path.
- **Against:** cannot reach paths or canonical paths, which is what was
  reported. And the eliom comment that broke the landing page,
  `(** ... Cf. {!Eliom_content_core.Xml}. *)`, resolves correctly on its own
  module's page and only misresolves when the driver lifts it elsewhere. Asking
  authors to write for a context they cannot see is a poor trade.

Worth documenting regardless, for the cases it does cover.

## 7. Open question

Preferring the include path resolves silently what odoc used to flag. With both
libraries searched it warned about an ambiguous lookup and picked one; now the
unit's own library wins with no warning. Someone in `eliom.client` who
deliberately meant the server's module quietly gets the client's.

The suggestion on the table is to warn when the wider search would also have
matched: say which was chosen and that a path reference makes it explicit. The
default stays right and the author is still told. The noise lands on packages
like eliom, which is where it belongs. Not implemented.

## 8. Next

- Decide on the warning above.
- Re-cut the ten WIP commits into the reviewable series.
- Reply on #1450, which can now say the report is fixed in full.
- Run voodoo mode through docs-ci.
