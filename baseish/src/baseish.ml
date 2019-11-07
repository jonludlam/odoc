include (Shadow_stdlib : module type of struct include Shadow_stdlib end
with module Array := Caml.Array) 

open! Import

(** @canonical Baseish.Array *)
module Array = Array

