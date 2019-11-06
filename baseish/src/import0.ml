
include
  (Shadow_stdlib
    : (module type of struct include Shadow_stdlib end
      with module Array := Shadow_stdlib.Array)) [@ocaml.warning "-3"]

