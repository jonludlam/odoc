open Odoc__xref2
open Odoc_xref_test

let input = {|

type t
type u = t

|}


let test_strengthen () =
    let p = Common.root_identifier in
    let _, _, sg = Common.model_of_string input in
    let c = Component.Of_Lang.of_signature [] sg in
    let c' = Strengthen.signature p c in
    Format.(fprintf std_formatter "%a\n%!" Component.Fmt.signature c')

let _ =
    test_strengthen ()