  $ ocamlc -c -bin-annot chain.mli
  $ /usr/bin/time -l odoc compile chain.cmti
  $ /usr/bin/time -l odoc link chain.odoc -I .
  $ ls -l
  total 73856
  -rw-r--r--  1 jon  staff     16511 Dec  6 19:58 chain.cmi
  -rw-r--r--  1 jon  staff     85001 Dec  6 19:58 chain.cmti
  lrwxr-xr-x  1 jon  staff        59 Dec  6 19:58 chain.mli -> ../../../../../../default/test/xref2/aliaschain.t/chain.mli
  -rw-r--r--  1 jon  staff     18554 Dec  6 19:58 chain.odoc
  -rw-r--r--  1 jon  staff  37685606 Dec  6 19:58 chain.odocl
  lrwxr-xr-x  1 jon  staff        55 Dec  6 19:58 run.t -> ../../../../../../default/test/xref2/aliaschain.t/run.t

