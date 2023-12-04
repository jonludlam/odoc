  $ ocamlc -c -bin-annot a.mli
  $ odoc compile a.cmti
  Here we are!
  Here we are!
  Here we are!
  Here we are!
  File "a.cmti":
  Warning: Failed to compile expansion for include : module type of struct include identifier((root A).{A}2, true) end Unresolved module path r((root A).{A}2) (Find failure)
  File "a.cmti":
  Warning: Failed to compile expansion for include : module type of struct include identifier((root A).{A}2, true) end Unresolved module path r((root A).{A}2) (Find failure)
  File "a.cmti":
  Warning: Failed to compile expansion for include : module type of struct include identifier((root A).{A}1, true) end Unresolved module path r((root A).{A}1) (Find failure)
  File "a.cmti":
  Warning: Failed to compile expansion for include : module type of struct include identifier((root A).{A}1, true) end Unresolved module path r((root A).{A}1) (Find failure)
  $ odoc link a.odoc

