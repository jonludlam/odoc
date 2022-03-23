Test that @canonical tags work on compilation units when it is placed in the
top-comment.

The module Test_x is expected to be referenced as Test.X.

  $ compile test_x.mli test_y.ml test.ml
  Starting type_of pass
  Adding (root Test_x).t to env
  Adding (root Test_x).Out to env
  Adding (root Test_x).In to env
  Adding (root Test_x).Out.t to env
  Adding (root Test_x).In.t to env
  Finished type_of pass
  Adding (root Test_x).t to env
  Adding (root Test_x).Out to env
  Adding (root Test_x).In to env
  Adding (root Test_x).Out.t to env
  Adding (root Test_x).In.t to env
  Starting type_of pass
  Adding (root Test_y).t to env
  Adding (root Test_y).Out to env
  Adding (root Test_y).In to env
  Adding (root Test_y).Out.t to env
  Adding (root Test_y).In.t to env
  Finished type_of pass
  Adding (root Test_y).t to env
  Adding (root Test_y).Out to env
  Adding (root Test_y).In to env
  Adding (root Test_y).Out.t to env
  Adding (root Test_y).In.t to env
  Starting type_of pass
  Adding (root Test).X to env
  Adding (root Test).X_out to env
  Adding (root Test).X_in to env
  Adding (root Test).Y to env
  Adding (root Test).Y_out to env
  Adding (root Test).Y_in to env
  Adding (root Test).t to env
  Adding (root Test).u to env
  Finished type_of pass
  Adding (root Test).X to env
  Adding (root Test).X_out to env
  Adding (root Test).X_in to env
  Adding (root Test).Y to env
  Adding (root Test).Y_out to env
  Adding (root Test).Y_in to env
  Adding (root Test).t to env
  Adding (root Test).u to env
  Adding (root Test_x).t to env
  Adding (root Test_x).Out to env
  Adding (root Test_x).In to env
  Adding (root Test_x).Out.t to env
  Adding (root Test_x).In.t to env
  Adding (root Test_y).t to env
  Adding (root Test_y).Out to env
  Adding (root Test_y).In to env
  Adding (root Test_y).Out.t to env
  Adding (root Test_y).In.t to env
  Adding (root Test).X to env
  Adding (root Test).X_out to env
  Adding (root Test).X_in to env
  Adding (root Test).Y to env
  Adding (root Test).Y_out to env
  Adding (root Test).Y_in to env
  Adding (root Test).t to env
  Adding (root Test).u to env
  Adding (root Test).X.t to env
  Adding (root Test).X.Out to env
  Adding (root Test).X.In to env
  Adding (root Test).X.Out.t to env
  Adding (root Test).X.In.t to env
  Adding (root Test).X_out.t to env
  Adding (root Test).X_in.t to env
  Adding (root Test).Y.t to env
  Adding (root Test).Y.Out to env
  Adding (root Test).Y.In to env
  Adding (root Test).Y.Out.t to env
  Adding (root Test).Y.In.t to env
  Adding (root Test).Y_out.t to env
  Adding (root Test).Y_in.t to env

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
  {"`Resolved":{"`Canonical":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X"]}}}]}}
  {"`Resolved":{"`Canonical":[{"`Module":[{"`Canonical":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X"]}}}]},"Out"]},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_out"]}}}]}}
  {"`Resolved":{"`Canonical":[{"`Module":[{"`Canonical":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X"]}}}]},"In"]},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_in"]}}}]}}
  {"`Resolved":{"`Canonical":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y"]}}}]}}
  {"`Resolved":{"`Canonical":[{"`Module":[{"`Canonical":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y"]}}}]},"Out"]},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_out"]}}}]}}
  {"`Resolved":{"`Canonical":[{"`Module":[{"`Canonical":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y"]}}}]},"In"]},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_in"]}}}]}}
