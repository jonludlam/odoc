Test that @canonical tags work on compilation units when it is placed in the
top-comment.

The module Test_x is expected to be referenced as Test.X.

  $ compile test_x.mli test_y.ml test.ml
  Error: Unresolved path doesn't look like unresolve_resolve_module_path: unresolvedroot(Test_x) != identifier((root Test_x), false)
  Error: Unresolved path doesn't look like unresolve_resolve_module_path: unresolvedroot(Test_x) != identifier((root Test_x), false)
  Error: Unresolved path doesn't look like unresolve_resolve_module_path: unresolvedroot(Test_x).Out != identifier((root Test_x), false).Out
  Error: Unresolved path doesn't look like unresolve_resolve_module_path: unresolvedroot(Test_x).In != identifier((root Test_x), false).In
  Error: Unresolved path doesn't look like unresolve_resolve_module_path: unresolvedroot(Test_y) != identifier((root Test_y), false)
  Error: Unresolved path doesn't look like unresolve_resolve_module_path: unresolvedroot(Test_y) != identifier((root Test_y), false)
  Error: Unresolved path doesn't look like unresolve_resolve_module_path: unresolvedroot(Test_y).Out != identifier((root Test_y), false).Out
  Error: Unresolved path doesn't look like unresolve_resolve_module_path: unresolvedroot(Test_y).In != identifier((root Test_y), false).In

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
  {"`Resolved":["946685804",{"`Canonical":[{"`Identifier":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]},"false"]},{"`Resolved":["861748510",{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X"]}}]}]}]}
  {"`Resolved":["220932592",{"`Canonical":[{"`Dot":[{"`Identifier":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]},"false"]},"Out"]},{"`Resolved":["871725559",{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_out"]}}]}]}]}
  {"`Resolved":["664379825",{"`Canonical":[{"`Dot":[{"`Identifier":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]},"false"]},"In"]},{"`Resolved":["879086977",{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_in"]}}]}]}]}
  {"`Resolved":["192583442",{"`Canonical":[{"`Identifier":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]},"false"]},{"`Resolved":["553889793",{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y"]}}]}]}]}
  {"`Resolved":["1023244492",{"`Canonical":[{"`Dot":[{"`Identifier":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]},"false"]},"Out"]},{"`Resolved":["811235783",{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_out"]}}]}]}]}
  {"`Resolved":["39483914",{"`Canonical":[{"`Dot":[{"`Identifier":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]},"false"]},"In"]},{"`Resolved":["153439974",{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_in"]}}]}]}]}
