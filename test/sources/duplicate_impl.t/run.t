Two libraries each define a module `Common`, but with different contents (and
therefore different interface digests). A third module `User` in `liba`
re-exports `liba`'s `Common` via `include`. When linking `User` with both
`liba` and `libb` on the include path, odoc must pick `liba`'s implementation
of `Common` (matched by digest) to resolve the source links of the included
values — not `libb`'s same-named implementation.

  $ cat liba/common.ml
  let x = 1
  $ cat libb/common.ml
  let x = 1
  let y = 2
  $ cat liba/user.ml
  include Common

  $ ocamlc -c -bin-annot liba/common.ml
  $ ocamlc -c -bin-annot -I liba liba/user.ml
  $ ocamlc -c -bin-annot libb/common.ml

Compile the two libraries into separate directories:

  $ odoc compile-impl --output-dir _odoc --parent-id liba --source-id src/liba/common.ml liba/common.cmt
  $ odoc compile --output-dir _odoc --parent-id liba liba/common.cmt
  $ odoc compile-impl --output-dir _odoc --parent-id liba --source-id src/liba/user.ml -I _odoc/liba liba/user.cmt
  $ odoc compile --output-dir _odoc --parent-id liba -I _odoc/liba liba/user.cmt
  $ odoc compile-impl --output-dir _odoc --parent-id libb --source-id src/libb/common.ml libb/common.cmt
  $ odoc compile --output-dir _odoc --parent-id libb libb/common.cmt

Link `User` with BOTH `liba` and `libb` on the include path. `libb`'s
same-named `Common` must not shadow `liba`'s when resolving source links:

  $ odoc link -L liba:_odoc/liba -L libb:_odoc/libb -I _odoc/liba -I _odoc/libb _odoc/liba/impl-user.odoc
  $ odoc link -L liba:_odoc/liba -L libb:_odoc/libb -I _odoc/liba -I _odoc/libb _odoc/liba/user.odoc

  $ odoc html-generate-source --impl _odoc/liba/impl-user.odocl -o html liba/user.ml
  $ odoc html-generate -o html _odoc/liba/user.odocl

The source link for the included value `x` must point to `liba`'s `common.ml`,
not `libb`'s. Before the digest-aware implementation lookup, odoc resolved this
by module name alone and picked `libb`'s `Common`, producing
`src/libb/common.ml.html#val-x`:

  $ grep -o 'src/lib./common.ml.html#val-x' html/liba/User/index.html
  src/liba/common.ml.html#val-x
