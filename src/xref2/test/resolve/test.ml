open Odoc__xref2
open Odoc_xref_test

let name = "Basic resolution"
let description = {|
This test simply attempts to resolve everything within a signature.
It includes a pattern that the previous implementation could not resolve.
|}

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
    let open Format in
    fprintf std_formatter "%s\n%s\n%!" name description;
    fprintf std_formatter "BEFORE\n======\n%a\n%!" Component.Fmt.signature c;
    try
        let sg' = Resolve.signature Env.empty sg in
        let c' = Component.Of_Lang.of_signature [] sg' in

        fprintf std_formatter "AFTER \n======\n%a\n%!" Component.Fmt.signature c'
    with
    | Tools.Lookup_failure (e, p, ty) ->
        let bt = Printexc.get_backtrace () in
        fprintf std_formatter "FAILURE when lookup up a %s: \n%!" ty;
        fprintf std_formatter "Path: %a\n%!" Component.Fmt.path p;
        fprintf std_formatter "Env:\n%!%a" Env.pp e;
        fprintf std_formatter "Backtrace:\n%s\n%!" bt


let _ =
    test_resolve ()