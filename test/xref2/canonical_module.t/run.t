Test that @canonical tags work on compilation units when it is placed in the
top-comment.

The module Test_x is expected to be referenced as Test.X.

  $ compile test_x.mli test_y.ml test.ml

Test_x and Test_y have a 'canonical' field:

  $ odoc_print test_x.odocl | jq -c ".canonical"
  {"Some":{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["38","test_x.cmti"]},"X"]},"key":["43","test_x.cmti"]}}
  $ odoc_print test_y.odocl | jq -c ".canonical"
  {"Some":{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["38","test_y.cmt"]},"Y"]},"key":["41","test_y.cmt"]}}

Test_x is defined as a .mli file and Test_y as a .ml file in order to test the
loader code handling canonical tags.

Submodules *_out have the canonical tag in the declarations and submodules *_in
have it in the top-comment.

Every references should be marked as canonical:

  $ odoc_print test.odocl | jq -c ".content.Module.items | .[] | .Module[1].type_.Alias[0] | select(.)"
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},"key":["51","test.cmt"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X"]}},"key":["71","test.odoc"]}},"key":["76","test.odoc"]}]},"key":["79","test.odoc"]}},"key":["80","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Module":[{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},"key":["51","test.cmt"]},{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["38","test_x.cmti"]},"X"]},"key":["43","test_x.cmti"]}]},"key":["97","test.odoc"]},"Out"]},"key":["98","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_out"]}},"key":["85","test.odoc"]}},"key":["94","test.odoc"]}]},"key":["99","test.odoc"]}},"key":["100","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Module":[{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},"key":["51","test.cmt"]},{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["38","test_x.cmti"]},"X"]},"key":["43","test_x.cmti"]}]},"key":["97","test.odoc"]},"In"]},"key":["112","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_in"]}},"key":["104","test.odoc"]}},"key":["109","test.odoc"]}]},"key":["113","test.odoc"]}},"key":["114","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},"key":["77","test.cmt"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y"]}},"key":["117","test.odoc"]}},"key":["121","test.odoc"]}]},"key":["124","test.odoc"]}},"key":["125","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Module":[{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},"key":["77","test.cmt"]},{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["38","test_y.cmt"]},"Y"]},"key":["41","test_y.cmt"]}]},"key":["144","test.odoc"]},"Out"]},"key":["145","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_out"]}},"key":["131","test.odoc"]}},"key":["141","test.odoc"]}]},"key":["146","test.odoc"]}},"key":["147","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Module":[{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},"key":["77","test.cmt"]},{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["38","test_y.cmt"]},"Y"]},"key":["41","test_y.cmt"]}]},"key":["144","test.odoc"]},"In"]},"key":["159","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_in"]}},"key":["151","test.odoc"]}},"key":["156","test.odoc"]}]},"key":["160","test.odoc"]}},"key":["161","test.odoc"]}
