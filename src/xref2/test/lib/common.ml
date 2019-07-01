
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

let root = 
    root_of_compilation_unit
        ~package:"nopackage"
        ~hidden:false
        ~module_name:"Test"
        ~digest:"nodigest"

let root_identifier = `Identifier (`Root (root, Odoc_model.Names.UnitName.of_string "Root"))

let root_pp fmt (_ : Odoc_model.Root.t) = Format.fprintf fmt "Common.root"

let model_of_string str = 
    let cmti = cmti_of_string str in
    Odoc_loader__Cmti.read_interface root "Root" cmti

let signature_of_mli_string str =
    let _, _, sg = model_of_string str in
    sg

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

    module Lens = struct
        open Odoc_model

        type ('a, 'b) lens =
            { get : 'a -> 'b
            ; set : 'b -> 'a -> 'a }

        type ('a, 'b) prism =
            { preview : 'a -> 'b option
            ; review : 'b -> 'a }

        let option : ('a option, 'a) prism =
            let preview = function | Some x -> Some x | None -> None in
            let review = function x -> Some x in
            {preview; review}
    
        let compose : ('a, 'b) lens -> ('b, 'c) lens -> ('a, 'c) lens =
            fun x y ->
                let get z = y.get (x.get z) in
                let set a z = x.set (y.set a (x.get z)) z in
                {get; set}

        let compose_prism : ('a, 'b) lens -> ('b, 'c) prism -> ('a, 'c) lens =
            fun x y ->
                let get z = x.get z |> y.preview |> function | Some x -> x | None -> raise Not_found in
                let set a z = x.set (y.review a) z in
                {get; set}

        let (|--) = compose

        let (|-~) = compose_prism

        let get lens x = lens.get x
        let set lens y x = lens.set y x

        let name_of_id = Paths.Identifier.name

        module Signature = struct
            open Lang.Signature
            let module_ : string -> (t, Lang.Module.t) lens = fun name ->
                let module M = Lang.Module in
                let rec get = function
                    | [] -> raise Not_found
                    | (Module (_r,m'))::_xs when name_of_id m'.M.id = name ->
                        m'
                    | _::xs -> get xs
                in
                let set m =
                    let rec inner = function
                        | [] -> raise Not_found
                        | (Module (r, m'))::xs when name_of_id m'.M.id = name ->
                            (Module (r, m)::xs)
                        | x::xs -> x :: inner xs
                    in inner
                in
                { get; set }

            let module_type : string -> (t, Lang.ModuleType.t) lens = fun name ->
                let module MT = Lang.ModuleType in
                let rec get = function
                    | [] -> raise Not_found
                    | (ModuleType m')::_xs when name_of_id m'.MT.id = name ->
                        m'
                    | _::xs -> get xs
                in
                let set m =
                    let rec inner = function
                        | [] -> raise Not_found
                        | (ModuleType m')::xs when name_of_id m'.MT.id = name ->
                            (ModuleType m::xs)
                        | x::xs -> x :: inner xs
                    in inner
                in
                { get; set }

            let type_ : string -> (t, Lang.TypeDecl.t) lens = fun name ->
                let module T = Lang.TypeDecl in
                let rec get = function
                    | [] -> raise Not_found
                    | (Type (_,t'))::_xs when name_of_id t'.T.id = name ->
                        t'
                    | _::xs -> get xs
                in
                let set t =
                    let rec inner = function
                        | [] -> raise Not_found
                        | (Type (r,t'))::xs when name_of_id t'.T.id = name ->
                            (Type (r,t))::xs
                        | x::xs -> x :: inner xs
                    in inner
                in
                { get; set }

            end

            module Module = struct
                open Lang.Module

                let id : (t, Paths.Identifier.Module.t) lens =
                    let get m = m.id in
                    let set id t = {t with id} in
                    {get; set}

                let type_ : (t, decl) lens =
                    let get m = m.type_ in
                    let set type_ m = {m with type_} in
                    {get; set}
                

            end

            module ModuleType = struct
                open Lang.ModuleType

                let expr : (t, expr option) lens =
                    let get mt = mt.expr in
                    let set expr mt = {mt with expr} in
                    {get; set}

                let expr_signature : (expr, Lang.Signature.t) prism =
                    let review x = Signature x in
                    let preview = function | Signature x -> Some x | _ -> None in
                    {review; preview}
            end

            module TypeDecl = struct
                open Lang.TypeDecl

                module Equation = struct
                    open Equation

                    let params : (t, param list) lens =
                        let get t = t.params in
                        let set params t = {t with params} in
                        { get; set }

                    let manifest : (t, Lang.TypeExpr.t option) lens =
                        let get t = t.manifest in
                        let set manifest t = {t with manifest} in
                        { get; set }
                end

                let id : (t, Paths.Identifier.Type.t) lens =
                    let get t = t.id in
                    let set id t = {t with id} in
                    { get; set }
                
                let equation : (t, Lang.TypeDecl.Equation.t) lens =
                    let get t = t.equation in
                    let set equation t = {t with equation} in
                    { get; set }
                
                let representation : (t, Representation.t option) lens =
                    let get t = t.representation in
                    let set representation t = {t with representation} in
                    { get; set }                    
            end
        

    end

    let test =
        let open Lens in
        Signature.type_ "t" |-- TypeDecl.equation |-- TypeDecl.Equation.manifest


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