
(* Example usage of these:

$ dune utop src/xref2/test/lib
utop # open Odoc_xref2;;
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
  let file_representation : Odoc_model.Root.Odoc_file.t =
  Odoc_model.Root.Odoc_file.create_unit ~force_hidden:hidden module_name in
  {Odoc_model.Root.package; file = file_representation; digest}

let dummy_root = 
    root_of_compilation_unit
        ~package:"nopackage"
        ~hidden:false
        ~module_name:"Test"
        ~digest:"nodigest"

let root_identifier = `Identifier (`Root (dummy_root, Odoc_model.Names.UnitName.of_string "Root"))

let model_of_string str = 
    let cmti = cmti_of_string str in
    Odoc_loader__Cmti.read_interface dummy_root "noname" cmti


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
        let module_from_sig : Odoc_model.Lang.Signature.t -> string -> Odoc_model.Lang.Module.t =
            fun sg mname ->
                let rec inner = function
                    | Odoc_model.Lang.Signature.Module (_, m) :: rest -> begin
                        let id = m.Odoc_model.Lang.Module.id in
                        match id with
                        | `Module (_, mname') ->
                            if Odoc_model.Names.ModuleName.to_string mname' = mname
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

    let sig_of_module : Odoc_model.Lang.Module.t -> Odoc_model.Lang.Signature.t =
        let open Odoc_model.Lang in
        fun x ->
            match x.type_ with
            | Module.ModuleType ModuleType.Signature s -> s
            | _ -> raise Not_found


end

let my_compilation_unit id docs s =
    { Odoc_model.Lang.Compilation_unit.
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
  Odoc_odoc.Env.create ~important_digests:false ~directories:[]

let resolve unit =
  let env = mkenv () in
  let resolve_env = Odoc_odoc.Env.build env (`Unit unit) in
  let resolver = Odoc_odoc.Env.resolver resolve_env in
  let result = Odoc_xref.resolve resolver unit in
  result


let resolve_from_string s =
    let id, docs, sg = model_of_string s in
    let unit = my_compilation_unit id docs sg in
    resolve unit