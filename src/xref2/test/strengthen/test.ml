open Odoc__xref2.Test
open Odoc__alias

let _ = Toploop.set_paths ()

let cmti_of_string s =
    let env = Compmisc.initial_env () in
    let l = Lexing.from_string s in
    let p = Parse.interface l in
    Typemod.type_interface "" env p;;

let root_of_compilation_unit ~package ~hidden ~module_name ~digest =
  let file_representation : Model.Root.Odoc_file.t =
  Model.Root.Odoc_file.create_unit ~force_hidden:hidden module_name in
  {Model.Root.package; file = file_representation; digest}

let dummy_root = 
    root_of_compilation_unit
        ~package:"nopackage"
        ~hidden:false
        ~module_name:"Test"
        ~digest:"nodigest"

let model_of_string str = 
    let cmti = cmti_of_string str in
    Odoc__loader__Cmti.read_interface dummy_root "noname" cmti

let my_compilation_unit ex =
    let id, docs, s = ex () in
    { Model.Lang.Compilation_unit.
      id = id
    ; doc = docs
    ; digest = "nodigest"
    ; imports = []
    ; source = None
    ; interface = true
    ; hidden = false
    ; content = Module s
    ; expansion = None
    }

let string_of_file f =
    let ic = open_in f in
    let buffer = Buffer.create 100 in
    let rec loop () =
        try
            Buffer.add_channel buffer ic 1024;
            loop ()
        with End_of_file ->
            ()
    in loop ();
    close_in ic;
    Buffer.contents buffer

let file_of_string ~filename str =
    let oc = open_out filename in
    Printf.fprintf oc "%s%!" str;
    close_out oc

let list_files path =
    Sys.readdir path |> Array.to_list


let test_strengthen () =
    let p = `Identifier (`Root (dummy_root, Model.Names.UnitName.of_string "Root")) in
    let filename = "strengthen.mli.input" in
    let mli = string_of_file filename in
    let _, _, sg = model_of_string mli in
    let c = Component.Of_Lang.of_signature sg in
    let c' = Component.Strengthen.signature p c in
    Format.(fprintf std_formatter "%a%!" Component.Format.signature c')
    
let _ =
    test_strengthen ()