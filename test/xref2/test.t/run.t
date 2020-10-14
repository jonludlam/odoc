This test generates documentation using odoc for a library:

  $ dune build
  $ odocmkgen > Makefile
  $ odocmkgen compile --dir . > Makefile.gen
  Warning, couldn't find dep Stdlib of file _build/default/.bar.objs/byte/bar.cmt
  Warning, couldn't find dep CamlinternalFormatBasics of file _build/default/.bar.objs/byte/bar.cmt
  Warning, couldn't find dep Stdlib of file _build/default/.foo.objs/byte/foo2.cmt
  Warning, couldn't find dep CamlinternalFormatBasics of file _build/default/.foo.objs/byte/foo2.cmt
  Warning, couldn't find dep Stdlib of file _build/default/.foo.objs/byte/foo3.cmt
  Warning, couldn't find dep CamlinternalFormatBasics of file _build/default/.foo.objs/byte/foo3.cmt
  Warning, couldn't find dep Stdlib of file _build/default/.foo.objs/byte/foo.cmt
  Warning, couldn't find dep CamlinternalFormatBasics of file _build/default/.foo.objs/byte/foo.cmt
  Warning, couldn't find dep Stdlib of file _build/default/.foo_byte.objs/byte/foo_byte.cmt
  Warning, couldn't find dep CamlinternalFormatBasics of file _build/default/.foo_byte.objs/byte/foo_byte.cmt
  Warning, couldn't find dep Stdlib of file _build/install/default/lib/bar/bar.cmt
  Warning, couldn't find dep CamlinternalFormatBasics of file _build/install/default/lib/bar/bar.cmt
  Warning, couldn't find dep Stdlib of file _build/install/default/lib/foo/foo3.cmt
  Warning, couldn't find dep CamlinternalFormatBasics of file _build/install/default/lib/foo/foo3.cmt
  Warning, couldn't find dep Stdlib of file _build/install/default/lib/foo/foo2.cmt
  Warning, couldn't find dep CamlinternalFormatBasics of file _build/install/default/lib/foo/foo2.cmt
  Warning, couldn't find dep Stdlib of file _build/install/default/lib/foo/foo.cmt
  Warning, couldn't find dep CamlinternalFormatBasics of file _build/install/default/lib/foo/foo.cmt
  Warning, couldn't find dep Stdlib of file _build/install/default/lib/foo/byte/foo_byte.cmt
  Warning, couldn't find dep CamlinternalFormatBasics of file _build/install/default/lib/foo/byte/foo_byte.cmt
  Warning, couldn't find dep CamlinternalFormatBasics of file _build/install/default/lib/foo/.private/foo3.cmi
  Warning, couldn't find dep Stdlib of file _build/install/default/lib/foo/.private/foo3.cmi
  $ gmake
  /usr/bin/time -l odoc compile --package _build _build/default/.bar.objs/byte/bar.cmt  -o odocs/_build/default/.bar.objs/byte/bar.odoc
          0.00 real         0.00 user         0.00 sys
     9166848  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2258  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           8  involuntary context switches
  /usr/bin/time -l odoc compile --package _build _build/default/.foo.objs/byte/foo2.cmt  -o odocs/_build/default/.foo.objs/byte/foo2.odoc
          0.00 real         0.00 user         0.00 sys
     9199616  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2266  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           8  involuntary context switches
  /usr/bin/time -l odoc compile --package _build _build/default/.foo.objs/byte/foo3.cmt  -o odocs/_build/default/.foo.objs/byte/foo3.odoc
          0.00 real         0.00 user         0.00 sys
     9129984  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2249  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           1  involuntary context switches
  /usr/bin/time -l odoc compile --package _build _build/default/.foo.objs/byte/foo.cmt  -o odocs/_build/default/.foo.objs/byte/foo.odoc
          0.00 real         0.00 user         0.00 sys
     9129984  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2249  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           2  involuntary context switches
  /usr/bin/time -l odoc compile --package _build _build/default/.foo_byte.objs/byte/foo_byte.cmt  -o odocs/_build/default/.foo_byte.objs/byte/foo_byte.odoc
          0.00 real         0.00 user         0.00 sys
     9134080  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2250  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           1  involuntary context switches
  /usr/bin/time -l odoc compile --package _build _build/install/default/lib/bar/bar.cmt  -o odocs/_build/install/default/lib/bar/bar.odoc
          0.00 real         0.00 user         0.00 sys
     9134080  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2250  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           2  involuntary context switches
  /usr/bin/time -l odoc compile --package _build _build/install/default/lib/foo/foo3.cmt  -o odocs/_build/install/default/lib/foo/foo3.odoc
          0.00 real         0.00 user         0.00 sys
     9134080  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2250  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           1  involuntary context switches
  /usr/bin/time -l odoc compile --package _build _build/install/default/lib/foo/foo2.cmt  -o odocs/_build/install/default/lib/foo/foo2.odoc
          0.00 real         0.00 user         0.00 sys
     9129984  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2249  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           2  involuntary context switches
  /usr/bin/time -l odoc compile --package _build _build/install/default/lib/foo/foo.cmt  -o odocs/_build/install/default/lib/foo/foo.odoc
          0.01 real         0.00 user         0.00 sys
     9129984  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2249  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           1  involuntary context switches
  /usr/bin/time -l odoc compile --package _build _build/install/default/lib/foo/byte/foo_byte.cmt  -o odocs/_build/install/default/lib/foo/byte/foo_byte.odoc
          0.00 real         0.00 user         0.00 sys
     9134080  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2250  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           2  involuntary context switches
  /usr/bin/time -l odoc compile --package _build _build/install/default/lib/foo/.private/foo3.cmi  -o odocs/_build/install/default/lib/foo/.private/foo3.odoc
          0.00 real         0.00 user         0.00 sys
     9109504  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2244  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           2  involuntary context switches
  /usr/bin/time -l odoc link odocs/_build/default/.bar.objs/byte/bar.odoc -o odocls/_build/default/.bar.objs/byte/bar.odocl 
  Starting link
          0.00 real         0.00 user         0.00 sys
     9089024  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2239  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           2  involuntary context switches
  /usr/bin/time -l odoc link odocs/_build/default/.foo.objs/byte/foo.odoc -o odocls/_build/default/.foo.objs/byte/foo.odocl 
  Starting link
          0.00 real         0.00 user         0.00 sys
     9089024  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2239  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           1  involuntary context switches
  /usr/bin/time -l odoc link odocs/_build/default/.foo.objs/byte/foo3.odoc -o odocls/_build/default/.foo.objs/byte/foo3.odocl 
  Starting link
          0.00 real         0.00 user         0.00 sys
     9080832  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2237  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           1  involuntary context switches
  /usr/bin/time -l odoc link odocs/_build/default/.foo.objs/byte/foo2.odoc -o odocls/_build/default/.foo.objs/byte/foo2.odocl 
  Starting link
          0.01 real         0.00 user         0.00 sys
     9080832  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2237  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           3  involuntary context switches
  /usr/bin/time -l odoc link odocs/_build/default/.foo_byte.objs/byte/foo_byte.odoc -o odocls/_build/default/.foo_byte.objs/byte/foo_byte.odocl 
  Starting link
          0.01 real         0.00 user         0.00 sys
     9121792  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2247  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
          17  involuntary context switches
  /usr/bin/time -l odoc link odocs/_build/install/default/lib/bar/bar.odoc -o odocls/_build/install/default/lib/bar/bar.odocl 
  Starting link
          0.00 real         0.00 user         0.00 sys
     9089024  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2239  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           3  involuntary context switches
  /usr/bin/time -l odoc link odocs/_build/install/default/lib/foo/foo.odoc -o odocls/_build/install/default/lib/foo/foo.odocl 
  Starting link
          0.00 real         0.00 user         0.00 sys
     9109504  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2244  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           2  involuntary context switches
  /usr/bin/time -l odoc link odocs/_build/install/default/lib/foo/foo3.odoc -o odocls/_build/install/default/lib/foo/foo3.odocl 
  Starting link
          0.00 real         0.00 user         0.00 sys
     9080832  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2237  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           1  involuntary context switches
  /usr/bin/time -l odoc link odocs/_build/install/default/lib/foo/foo2.odoc -o odocls/_build/install/default/lib/foo/foo2.odocl 
  Starting link
          0.01 real         0.00 user         0.00 sys
     9121792  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2247  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           6  involuntary context switches
  /usr/bin/time -l odoc link odocs/_build/install/default/lib/foo/byte/foo_byte.odoc -o odocls/_build/install/default/lib/foo/byte/foo_byte.odocl 
  Starting link
          0.01 real         0.00 user         0.00 sys
     9084928  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2238  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           3  involuntary context switches
  /usr/bin/time -l odoc link odocs/_build/install/default/lib/foo/.private/foo3.odoc -o odocls/_build/install/default/lib/foo/.private/foo3.odocl 
  Starting link
          0.01 real         0.00 user         0.00 sys
     9084928  maximum resident set size
           0  average shared memory size
           0  average unshared data size
           0  average unshared stack size
        2238  page reclaims
           0  page faults
           0  swaps
           0  block input operations
           0  block output operations
           0  messages sent
           0  messages received
           0  signals received
           0  voluntary context switches
           5  involuntary context switches
  odocmkgen generate --package _build
  Makefile._build.generate:17: warning: overriding recipe for target 'html/_build/Bar/index.html'
  Makefile._build.generate:2: warning: ignoring old recipe for target 'html/_build/Bar/index.html'
  Makefile._build.generate:17: warning: overriding group membership for target 'html/_build/Bar/index.html'
  Makefile._build.generate:20: warning: overriding recipe for target 'html/_build/Foo/index.html'
  Makefile._build.generate:5: warning: ignoring old recipe for target 'html/_build/Foo/index.html'
  Makefile._build.generate:20: warning: overriding group membership for target 'html/_build/Foo/index.html'
  Makefile._build.generate:23: warning: overriding recipe for target 'html/_build/Foo2/index.html'
  Makefile._build.generate:8: warning: ignoring old recipe for target 'html/_build/Foo2/index.html'
  Makefile._build.generate:23: warning: overriding group membership for target 'html/_build/Foo2/index.html'
  Makefile._build.generate:26: warning: overriding recipe for target 'html/_build/Foo3/index.html'
  Makefile._build.generate:11: warning: ignoring old recipe for target 'html/_build/Foo3/index.html'
  Makefile._build.generate:26: warning: overriding group membership for target 'html/_build/Foo3/index.html'
  Makefile._build.generate:29: warning: overriding recipe for target 'html/_build/Foo_byte/index.html'
  Makefile._build.generate:14: warning: ignoring old recipe for target 'html/_build/Foo_byte/index.html'
  Makefile._build.generate:29: warning: overriding group membership for target 'html/_build/Foo_byte/index.html'
  Makefile._build.generate:32: warning: overriding recipe for target 'html/_build/Foo3/index.html'
  Makefile._build.generate:26: warning: ignoring old recipe for target 'html/_build/Foo3/index.html'
  Makefile._build.generate:32: warning: overriding group membership for target 'html/_build/Foo3/index.html'
  Makefile._build.generate:50: warning: overriding recipe for target 'latex/page-_build-module-Bar.tex'
  Makefile._build.generate:35: warning: ignoring old recipe for target 'latex/page-_build-module-Bar.tex'
  Makefile._build.generate:50: warning: overriding group membership for target 'latex/page-_build-module-Bar.tex'
  Makefile._build.generate:53: warning: overriding recipe for target 'latex/page-_build-module-Foo.tex'
  Makefile._build.generate:38: warning: ignoring old recipe for target 'latex/page-_build-module-Foo.tex'
  Makefile._build.generate:53: warning: overriding group membership for target 'latex/page-_build-module-Foo.tex'
  Makefile._build.generate:56: warning: overriding recipe for target 'latex/page-_build-module-Foo2.tex'
  Makefile._build.generate:41: warning: ignoring old recipe for target 'latex/page-_build-module-Foo2.tex'
  Makefile._build.generate:56: warning: overriding group membership for target 'latex/page-_build-module-Foo2.tex'
  Makefile._build.generate:59: warning: overriding recipe for target 'latex/page-_build-module-Foo3.tex'
  Makefile._build.generate:44: warning: ignoring old recipe for target 'latex/page-_build-module-Foo3.tex'
  Makefile._build.generate:59: warning: overriding group membership for target 'latex/page-_build-module-Foo3.tex'
  Makefile._build.generate:62: warning: overriding recipe for target 'latex/page-_build-module-Foo_byte.tex'
  Makefile._build.generate:47: warning: ignoring old recipe for target 'latex/page-_build-module-Foo_byte.tex'
  Makefile._build.generate:62: warning: overriding group membership for target 'latex/page-_build-module-Foo_byte.tex'
  Makefile._build.generate:65: warning: overriding recipe for target 'latex/page-_build-module-Foo3.tex'
  Makefile._build.generate:59: warning: ignoring old recipe for target 'latex/page-_build-module-Foo3.tex'
  Makefile._build.generate:65: warning: overriding group membership for target 'latex/page-_build-module-Foo3.tex'
  Makefile._build.generate:83: warning: overriding recipe for target 'man/_build/Bar.3o'
  Makefile._build.generate:68: warning: ignoring old recipe for target 'man/_build/Bar.3o'
  Makefile._build.generate:83: warning: overriding group membership for target 'man/_build/Bar.3o'
  Makefile._build.generate:86: warning: overriding recipe for target 'man/_build/Foo.3o'
  Makefile._build.generate:71: warning: ignoring old recipe for target 'man/_build/Foo.3o'
  Makefile._build.generate:86: warning: overriding group membership for target 'man/_build/Foo.3o'
  Makefile._build.generate:89: warning: overriding recipe for target 'man/_build/Foo2.3o'
  Makefile._build.generate:74: warning: ignoring old recipe for target 'man/_build/Foo2.3o'
  Makefile._build.generate:89: warning: overriding group membership for target 'man/_build/Foo2.3o'
  Makefile._build.generate:92: warning: overriding recipe for target 'man/_build/Foo3.3o'
  Makefile._build.generate:77: warning: ignoring old recipe for target 'man/_build/Foo3.3o'
  Makefile._build.generate:92: warning: overriding group membership for target 'man/_build/Foo3.3o'
  Makefile._build.generate:95: warning: overriding recipe for target 'man/_build/Foo_byte.3o'
  Makefile._build.generate:80: warning: ignoring old recipe for target 'man/_build/Foo_byte.3o'
  Makefile._build.generate:95: warning: overriding group membership for target 'man/_build/Foo_byte.3o'
  Makefile._build.generate:98: warning: overriding recipe for target 'man/_build/Foo3.3o'
  Makefile._build.generate:92: warning: ignoring old recipe for target 'man/_build/Foo3.3o'
  Makefile._build.generate:98: warning: overriding group membership for target 'man/_build/Foo3.3o'
  gmake: Nothing to be done for 'default'.

