open Odoc__xref2
open Odoc_xref_test

let test_strengthen () =
    let p = Common.root_identifier in 
    let filename = "strengthen.mli.input" in
    let mli = Common.string_of_file filename in
    let _, _, sg = Common.model_of_string mli in
    let c = Component.Of_Lang.of_signature [] sg in
    let c' = Strengthen.signature p c in
    Format.(fprintf std_formatter "%a\n%!" Component.Fmt.signature c')

let _ =
    test_strengthen ()