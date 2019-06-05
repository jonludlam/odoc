open Odoc__xref2
open Odoc_xref_test


let test_data = {|

module type A = sig
  module M : sig module type S end
  module N : M.S
end

module B : sig module type S = sig type t end end

module C : A with module M = B

type t = C.N.t

|}

let test_resolve () =
    let _, _, sg = Common.model_of_string test_data in
    let c = Component.Of_Lang.of_signature [] sg in
    let sg' = Resolve.signature Env.empty sg in
    let c' = Component.Of_Lang.of_signature [] sg' in

    Format.(fprintf std_formatter "BEFORE\n======\n%a\n%!" Component.Fmt.signature c);
    Format.(fprintf std_formatter "AFTER \n======\n%a\n%!" Component.Fmt.signature c')

let _ =
    test_resolve ()