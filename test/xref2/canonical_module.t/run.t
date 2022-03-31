Test that @canonical tags work on compilation units when it is placed in the
top-comment.

The module Test_x is expected to be referenced as Test.X.

  $ compile test_x.mli test_y.ml test.ml

Test_x and Test_y have a 'canonical' field:

  $ odoc_print test_x.odocl | jq -c ".canonical"
  {"Some":{"`Dot":[{"`Root":"Test"},"X"]}}
  $ odoc_print test_y.odocl | jq -c ".canonical"
  {"Some":{"`Dot":[{"`Root":"Test"},"Y"]}}

Test_x is defined as a .mli file and Test_y as a .ml file in order to test the
loader code handling canonical tags.

Submodules *_out have the canonical tag in the declarations and submodules *_in
have it in the top-comment.

Every references should be marked as canonical:

  $ odoc_print test.odocl | jq -c ".content.Module.items | .[] | .Module[1].type_.Alias[0] | select(.)"
  {"`Resolved":["1057324280",{"`Canonical":[{"`Resolved":["235455617",{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}}]},{"`Resolved":["861748510",{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X"]}}]}]}]}
  {"`Resolved":["727051156",{"`Canonical":[{"`Resolved":["667810971",{"`Module":[["543999806",{"`Canonical":[{"`Resolved":["235455617",{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}}]},{"`Dot":[{"`Root":"Test"},"X"]}]}],"Out"]}]},{"`Resolved":["871725559",{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_out"]}}]}]}]}
  {"`Resolved":["600251234",{"`Canonical":[{"`Resolved":["190242306",{"`Module":[["543999806",{"`Canonical":[{"`Resolved":["235455617",{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}}]},{"`Dot":[{"`Root":"Test"},"X"]}]}],"In"]}]},{"`Resolved":["879086977",{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_in"]}}]}]}]}
  {"`Resolved":["1031709197",{"`Canonical":[{"`Resolved":["462134970",{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}}]},{"`Resolved":["553889793",{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y"]}}]}]}]}
  {"`Resolved":["264053892",{"`Canonical":[{"`Resolved":["539539241",{"`Module":[["905774272",{"`Canonical":[{"`Resolved":["462134970",{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}}]},{"`Dot":[{"`Root":"Test"},"Y"]}]}],"Out"]}]},{"`Resolved":["811235783",{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_out"]}}]}]}]}
  {"`Resolved":["793502086",{"`Canonical":[{"`Resolved":["716280932",{"`Module":[["905774272",{"`Canonical":[{"`Resolved":["462134970",{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}}]},{"`Dot":[{"`Root":"Test"},"Y"]}]}],"In"]}]},{"`Resolved":["153439974",{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_in"]}}]}]}]}
