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
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},"key":["80","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X"]}},"key":["75","test.odoc"]}},"key":["77","test.odoc"]}]},"key":["81","test.odoc"]}},"key":["82","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Module":[{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},"key":["80","test.odoc"]},{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["40","test_x.cmti"]},"X"]},"key":["45","test_x.cmti"]}]},"key":["99","test.odoc"]},"Out"]},"key":["100","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_out"]}},"key":["94","test.odoc"]}},"key":["96","test.odoc"]}]},"key":["101","test.odoc"]}},"key":["102","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Module":[{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},"key":["80","test.odoc"]},{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["40","test_x.cmti"]},"X"]},"key":["45","test_x.cmti"]}]},"key":["99","test.odoc"]},"In"]},"key":["114","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_in"]}},"key":["109","test.odoc"]}},"key":["111","test.odoc"]}]},"key":["115","test.odoc"]}},"key":["116","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},"key":["126","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y"]}},"key":["122","test.odoc"]}},"key":["123","test.odoc"]}]},"key":["127","test.odoc"]}},"key":["128","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Module":[{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},"key":["126","test.odoc"]},{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["40","test_y.cmt"]},"Y"]},"key":["43","test_y.cmt"]}]},"key":["147","test.odoc"]},"Out"]},"key":["148","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_out"]}},"key":["142","test.odoc"]}},"key":["144","test.odoc"]}]},"key":["149","test.odoc"]}},"key":["150","test.odoc"]}
  {"v":{"`Resolved":{"v":{"`Canonical":[{"v":{"`Module":[{"v":{"`Canonical":[{"v":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},"key":["126","test.odoc"]},{"v":{"`Dot":[{"v":{"`Root":"Test"},"key":["40","test_y.cmt"]},"Y"]},"key":["43","test_y.cmt"]}]},"key":["147","test.odoc"]},"In"]},"key":["162","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_in"]}},"key":["157","test.odoc"]}},"key":["159","test.odoc"]}]},"key":["163","test.odoc"]}},"key":["164","test.odoc"]}
