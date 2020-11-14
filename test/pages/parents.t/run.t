Here we are replicating the structure that you'd get by passing '--package' to odoc on the compile command line.
We make a 'package' mld file:

  $ cat package.mld
  {0 Package page}
  

And we'll have a module that we'll put underneath this package page.

  $ cat test.mli 
  type t
  

Compile the module first

  $ ocamlc -c -bin-annot test.mli

Then we need to compile the package mld file, because when you're passing the '--parent' argument specifying the parent odoc file, it must exist.

  $ odoc compile package.mld

This will have produced a file called 'page-package.odoc'. Now we can compile the module odoc file passing that file as parent.

  $ odoc compile test.cmti -I . --parent package

Link and generate the HTML:

  $ for i in *.odoc; do odoc link $i; done
  $ for i in *.odocl; do odoc html-generate $i -o html; done

We should see a directory structure here where the module 'Test' is found underneath the top-level directory 'package'. Also, the contents of the
file 'package.mld' should be written to the file 'package/index.html'.

  $ ls -R html
  package
  
  html/package:
  Test
  index.html
  
  html/package/Test:
  index.html
