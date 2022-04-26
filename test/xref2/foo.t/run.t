Here's a little test

  $ ocamlc -c -bin-annot m.mli
  $ odoc compile m.cmti
  $ odoc link m.odoc
  module: (root M).CanonicalTest
  module: (root M).CanonicalTest.{Base__List}1
  module: (root M).CanonicalTest.{Base__}2
  module: (root M).CanonicalTest.Base
  module: (root M).CanonicalTest.Base.List
  module: (root M).CanonicalTest.Base_Tests
  module: (root M).CanonicalTest.Base_Tests.C
  module: (root M).CanonicalTest.Base_Tests.L
  module: (root M).CanonicalTest.List_modif
  $ odoc html-generate -o html m.odocl
  $ odoc support-files -o html
  $ rsync -avz html /tmp/html/
  building file list ... done
  html/
  html/highlight.pack.js
  html/odoc.css
  html/M/
  html/M/index.html
  html/M/CanonicalTest/
  html/M/CanonicalTest/index.html
  html/M/CanonicalTest/Base/
  html/M/CanonicalTest/Base/index.html
  html/M/CanonicalTest/Base/List/
  html/M/CanonicalTest/Base/List/index.html
  html/M/CanonicalTest/Base_Tests/
  html/M/CanonicalTest/Base_Tests/index.html
  html/M/CanonicalTest/Base_Tests/C/
  html/M/CanonicalTest/Base_Tests/C/index.html
  html/M/CanonicalTest/List_modif/
  html/M/CanonicalTest/List_modif/index.html
  
  sent 15237 bytes  received 266 bytes  31006.00 bytes/sec
  total size is 39340  speedup is 2.54
  $ odoc_print m.odo
  odoc_print: PATH argument: no 'm.odo' file or directory
  Usage: odoc_print [-r VAL] [OPTION]… PATH
  Try 'odoc_print --help' for more information.
  [124]
