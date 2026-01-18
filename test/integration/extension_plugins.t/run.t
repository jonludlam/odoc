Test the extension plugin system.

This tests:
1. Custom tags compile without error (graceful fallback)
2. The 'odoc extensions' command works
3. Custom tags are rendered in HTML output with default handling
4. Support files mechanism works

First, compile the test module with custom tags:

  $ ocamlc -bin-annot -c test_extension.ml

Compile with odoc - custom tags should work without errors:

  $ odoc compile --package test test_extension.cmt

Link the compiled unit:

  $ odoc link -I . test_extension.odoc

Generate HTML output:

  $ odoc html-generate -o html test_extension.odocl

Test the 'odoc extensions' command.
The output depends on what extensions are installed:

  $ odoc extensions | head -1
  Installed extensions:

Check that tag content is preserved in the output.

The custom.note tag should be rendered (either by extension or default):

  $ grep -q "This is a custom note tag" html/test/Test_extension/index.html && echo "custom.note content found"
  custom.note content found

The mytag tags should be rendered:

  $ grep -q "Some custom content here" html/test/Test_extension/index.html && echo "mytag content found"
  mytag content found

The admonition.warning content should be present:

  $ grep -q "This operation may fail" html/test/Test_extension/index.html && echo "admonition content found"
  admonition content found

Test the support-files command works:

  $ odoc support-files -o support
  $ test -d support && echo "support directory created"
  support directory created

  $ test -f support/odoc.css && echo "odoc.css present"
  odoc.css present
