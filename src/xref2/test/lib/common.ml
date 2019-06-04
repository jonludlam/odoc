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

let root_identifier = `Identifier (`Root (dummy_root, Model.Names.UnitName.of_string "Root"))

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
