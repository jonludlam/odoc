Test handling of internal tags.
We expect no warning for "good.mli". The code already ensures that either tags
are handled of a warning is emitted.

  $ compile good.mli
  Starting type_of pass
  Adding (root Good).t to env
  Adding (root Good).u to env
  Adding (root Good).M to env
  Adding (root Good).T.t to env
  Handling include in type_of
  Removing (root Good).t from env
  Finished handling include in type_of
  Finished type_of pass
  Adding (root Good).t to env
  Adding (root Good).u to env
  Adding (root Good).M to env
  Adding (root Good).T.t to env
  Adding (root Good).T.t to env
  Handling include of : identifier((root Good).T, false)
  Removing (root Good).t from env
  Removing (root Good).t from env
  Adding (root Good).t to env
  Adding (root Good).t to env
  Adding (root Good).u to env
  Adding (root Good).M to env
  Adding (root Good).T.t to env

We expect warnings to be emitted for each bad tags:

  $ compile bad.mli
  Starting type_of pass
  Finished type_of pass
  File "bad.mli", line 3, characters 4-11:
  Warning: Unexpected tag '@inline' at this location.
  File "bad.mli", line 7, characters 4-19:
  Warning: Unexpected tag '@canonical' at this location.
  File "bad.mli", line 12, characters 4-19:
  Warning: Unexpected tag '@canonical' at this location.
  File "bad.mli", line 17, characters 4-19:
  Warning: Unexpected tag '@canonical' at this location.
  File "bad.mli", line 21, characters 4-11:
  Warning: Unexpected tag '@inline' at this location.
