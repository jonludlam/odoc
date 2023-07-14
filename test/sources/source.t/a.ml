type t = string

let x = 2
let y = x + 1
let z a = if x = 1 || true then x + y else 0

module A = struct
  let b = 1
end
module B = A

let a =
  ignore(A.b);
  let open A in
  b

module type T = sig end
module type U = T

type ext = ..
type ext += Foo

exception Exn

class cls = object end
class cls' = cls
class type ct = object end
