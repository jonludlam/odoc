This test generates documentation using odoc for a library:

  $ dune build
  $ odocmkgen > Makefile
  $ odocmkgen compile --dir . > Makefile.gen
  odocmkgen: unknown option `--dir'.
  Usage: odocmkgen compile [OPTION]... 
  Try `odocmkgen compile --help' or `odocmkgen --help' for more information.
  [2]
  $ gmake
  gmake[1]: Entering directory '$TESTCASE_ROOT'
  mkdir odocs
  mkdir odocls
  gmake[1]: Leaving directory '$TESTCASE_ROOT'

