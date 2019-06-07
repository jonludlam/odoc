open Odoc__alias

(* Example usage of these:

$ dune utop src/xref2/test/lib
utop # open Odoc__xref2;;
utop # open Odoc_xref_test;;
utop # let test_data = "module type M = sig type t end module N : M type u = N.t";;
utop # let id, docs, sg = Common.model_of_string test_data;;
utop # let env = Env.open_signature sg Env.empty;;
utop # let unit = Common.my_compilation_unit id docs sg;
utop # Common.resolve unit
utop # Resolve.signature Env.empty sg

*)

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

let root_identifier = `Identifier (`Root (dummy_root, Model.Names.UnitName.of_string "Root"))

let model_of_string str = 
    let cmti = cmti_of_string str in
    Odoc__loader__Cmti.read_interface dummy_root "noname" cmti


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


module LangUtils = struct

    module Lookup = struct
        let module_from_sig : Model.Lang.Signature.t -> string -> Model.Lang.Module.t =
            fun sg mname ->
                let rec inner = function
                    | Model.Lang.Signature.Module (_, m) :: rest -> begin
                        let id = m.Model.Lang.Module.id in
                        match id with
                        | `Module (_, mname') ->
                            if Model.Names.ModuleName.to_string mname' = mname
                            then m
                            else inner rest
                        | _ -> inner rest
                        end
                    | _::rest -> 
                        Format.fprintf Format.std_formatter "Found somethine else\n%!";                   
                        inner rest
                    | _ -> raise Not_found
                in 
                inner sg
    end

    let sig_of_module : Model.Lang.Module.t -> Model.Lang.Signature.t =
        let open Model.Lang in
        fun x ->
            match x.type_ with
            | Module.ModuleType ModuleType.Signature s -> s
            | _ -> raise Not_found


end

let my_compilation_unit id docs s =
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

let mkenv () =
  Odoc.Env.create ~important_digests:false ~directories:[]

let resolve unit =
  let env = mkenv () in
  let resolve_env = Odoc.Env.build env (`Unit unit) in
  let resolver = Odoc.Env.resolver resolve_env in
  let result = Xref.resolve resolver unit in
  let tbl = Xref.tbl resolver in
  (result,tbl)