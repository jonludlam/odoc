Test that @canonical tags work on compilation units when it is placed in the
top-comment.

The module Test_x is expected to be referenced as Test.X.

  $ compile test_x.mli test_y.ml test.ml

Test_x and Test_y have a 'canonical' field:

  $ odoc_print test_x.odocl | jq -c ".canonical"
  {"Some":{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["40","test_x.cmti"]},"X"]},"key":["45","test_x.cmti"]}}
  $ odoc_print test_y.odocl | jq -c ".canonical"
  {"Some":{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["40","test_y.cmt"]},"Y"]},"key":["43","test_y.cmt"]}}

Test_x is defined as a .mli file and Test_y as a .ml file in order to test the
loader code handling canonical tags.

Submodules *_out have the canonical tag in the declarations and submodules *_in
have it in the top-comment.

Every references should be marked as canonical:

  $ odoc_print test.odocl | jq -c ".content.Module.items | .[] | .Module[1].type_.Alias[0] | select(.)"
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},"key":["56","test.cmt"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X"]}},"key":["73","test.odoc"]}},"key":["78","test.odoc"]}]},"key":["81","test.odoc"]}},"key":["82","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Module":[{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},"key":["56","test.cmt"]},{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["40","test_x.cmti"]},"X"]},"key":["45","test_x.cmti"]}]},"key":["99","test.odoc"]},"Out"]},"key":["100","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_out"]}},"key":["87","test.odoc"]}},"key":["96","test.odoc"]}]},"key":["101","test.odoc"]}},"key":["102","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Module":[{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},"key":["56","test.cmt"]},{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["40","test_x.cmti"]},"X"]},"key":["45","test_x.cmti"]}]},"key":["99","test.odoc"]},"In"]},"key":["114","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_in"]}},"key":["106","test.odoc"]}},"key":["111","test.odoc"]}]},"key":["115","test.odoc"]}},"key":["116","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},"key":["82","test.cmt"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y"]}},"key":["119","test.odoc"]}},"key":["123","test.odoc"]}]},"key":["126","test.odoc"]}},"key":["127","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Module":[{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},"key":["82","test.cmt"]},{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["40","test_y.cmt"]},"Y"]},"key":["43","test_y.cmt"]}]},"key":["146","test.odoc"]},"Out"]},"key":["147","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_out"]}},"key":["133","test.odoc"]}},"key":["143","test.odoc"]}]},"key":["148","test.odoc"]}},"key":["149","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Module":[{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},"key":["82","test.cmt"]},{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["40","test_y.cmt"]},"Y"]},"key":["43","test_y.cmt"]}]},"key":["146","test.odoc"]},"In"]},"key":["161","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_in"]}},"key":["153","test.odoc"]}},"key":["158","test.odoc"]}]},"key":["162","test.odoc"]}},"key":["163","test.odoc"]}
