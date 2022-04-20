In this test we're marking a ModuleType with a canonical path, then referencing
it later in the file. The resulting resolved path should contain a `CanonicalModuleType
constructor where the second element of the tuple is Resolved.

  $ cat test.mli
  (** @canonical Test.Y *)
  module type A = sig
    type t
  end
  
  module type B = sig
    (** The canonical tag is in the top-comment.
        @canonical Test.X *)
  
    type t
  end
  
  module type X = B
  
  module type Y = A
  
  module type Z = A

  $ ocamlc -c -bin-annot test.mli
  $ odoc compile --package x test.cmti
  $ odoc link test.odoc

Every module type aliases and the path they link to:

  $ odoc_print test.odocl | jq -c '.content.Module.items | .[] | select(.ModuleType.expr.Some.Path) | .ModuleType | { "from": .id, "to": .expr.Some.Path.p_path }'
  {"from":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"X"]},"to":{"v":{"`Resolved":{"v":{"`CanonicalModuleType":[{"v":{"`Identifier":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"B"]}},"key":["96","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"X"]}},"key":["57","test.odoc"]}},"key":["71","test.odoc"]}]},"key":["97","test.odoc"]}},"key":["98","test.odoc"]}}
  {"from":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"Y"]},"to":{"v":{"`Resolved":{"v":{"`CanonicalModuleType":[{"v":{"`Identifier":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"A"]}},"key":["102","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"Y"]}},"key":["77","test.odoc"]}},"key":["88","test.odoc"]}]},"key":["103","test.odoc"]}},"key":["104","test.odoc"]}}
  {"from":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"Z"]},"to":{"v":{"`Resolved":{"v":{"`CanonicalModuleType":[{"v":{"`Identifier":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"A"]}},"key":["102","test.odoc"]},{"v":{"`Resolved":{"v":{"`Identifier":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"Y"]}},"key":["77","test.odoc"]}},"key":["88","test.odoc"]}]},"key":["103","test.odoc"]}},"key":["104","test.odoc"]}}

  $ odoc html-generate -o html test.odocl
  $ odoc support-files -o html
