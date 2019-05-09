(* Playground for new model *)

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

let myexample0 () = model_of_string {|
  type t

  type u = t
|}

let myexample () = model_of_string {|
module X : sig
  type t
  module Y : sig
    type z 
  end
  type u = t
end

module Z : sig
  type t = X.u
end

type x = X.Y.z
|}



module Component = struct
    open Model.Paths
    open Model.Names

    module Ident = struct

        (* For simplicity keep a global counter *)
        let counter = ref 0

        (* Again, for simplicity right now all idents are just strings.
           Later we'll likely convert these to be typed Names *)
        type t = string * int

        let of_identifier : Identifier.t -> t =
            fun i ->
                let n = !counter in
                incr counter;
                match i with
                | `Module (_,s) -> (ModuleName.to_string s, n)
                | `Type (_,s) -> (TypeName.to_string s, n)
                | `ModuleType (_,s) -> (ModuleTypeName.to_string s, n)
                | _ -> failwith "Unhandled"

    end

    module Path = struct 
        (* Again, keep one Path.t rather than typed ones for now *)
        type t = [
            | `Local of Ident.t
            | `Ldot of t * string
            | `Global of Path.t
        ]
    end


    (* The following types are identical in structure to those in
       {!Lang}, which may well be too complex for our use, we'll
       see as we expand on this *)
    module rec Module : sig
      type decl =
        | Alias of Path.t
        | ModuleType of ModuleType.expr

      type t =
        { type_ : decl }
    end = Module 

    and TypeExpr : sig
        type t =
           | Var of string
           | Constr of Path.t * t list 
    end = TypeExpr

    and ModuleType : sig
        type expr =
            | Path of Path.t
            | Signature of Signature.t
        type t = expr option
    end = ModuleType

    and Type : sig
        type t = TypeExpr.t option (* manifest *)
    end = Type

    and Signature : sig
        type item = 
            | Module of Ident.t * Module.t
            | ModuleType of Ident.t * ModuleType.t
            | Type of Ident.t * Type.t
        
        type t = item list
    end = Signature


    module Env = struct
        (* A bunch of association lists. Let's hashtbl them up later *)
        type t =
            { ident_max: int
            ; modules : (Identifier.Module.t * Module.t) list
            ; moduletypes : (Identifier.ModuleType.t * ModuleType.t) list
            ; types : (Identifier.Type.t * Type.t) list }

        (* Handy for extrating transient state *)
        exception MyFailure of Model.Paths.Identifier.t * t

        let empty =
            { ident_max = 0
            ; modules = []
            ; moduletypes = []
            ; types = [] }

        let add_module identifier m env =
            { env with modules = (identifier, m)::env.modules}

        let add_type identifier t env =
            { env with types = (identifier, t)::env.types}

        let lookup_module identifier env =
            try
                List.assoc identifier env.modules
            with _ -> raise (MyFailure ((identifier :> Model.Paths.Identifier.t), env))
        
        let lookup_type identifier env =
            List.assoc identifier env.types

        let ident_of_identifier ident_map identifier =
            List.assoc_opt identifier ident_map

        let rec local_path_of_path : _ -> Model.Paths.Path.t -> Path.t = fun ident_map path ->
            match path with
            | `Resolved (`Identifier i) -> begin
                match ident_of_identifier ident_map i with
                | Some ident -> 
                    `Local ident
                | None ->
                    `Global path
                end 
            | `Dot (path, x) -> `Ldot (local_path_of_path ident_map (path :> Model.Paths.Path.t), x)
            | _ -> failwith "Unhandled in local_path_of_path"

        let rec of_type ident_map ty =
            let open Model.Lang.TypeDecl in
            match ty.equation.Equation.manifest with
            | None -> None
            | Some expr ->
                match expr with
                | Constr (path, _) ->
                    Some (TypeExpr.Constr (local_path_of_path ident_map (path :> Model.Paths.Path.t), []))
                | _ -> failwith "Unhandled in of_type"

        and of_module ident_map m =
            let type_ =
                match m.Model.Lang.Module.type_ with
                | Model.Lang.Module.Alias p ->
                    Module.Alias (local_path_of_path ident_map (p :> Model.Paths.Path.t))
                | Model.Lang.Module.ModuleType s ->
                    Module.ModuleType (of_moduletype ident_map s)
            in
            {Module.type_}

        and of_moduletype _ident_map m =
            match m with
            | Model.Lang.ModuleType.Signature s ->
                let (s,_) = of_signature empty s in
                ModuleType.Signature s
            | _ -> failwith "Unhandled here"

        and of_signature : t -> Model.Lang.Signature.t -> Signature.t * t =
            fun env items ->
                (* First we construct a list of brand new [Ident.t]s 
                   for each item in the signature *)
                let ident_map : (Identifier.t * Ident.t) list = List.map (function
                    | Model.Lang.Signature.Type (_, t) ->
                        let identifier = t.Model.Lang.TypeDecl.id in
                        let id = Ident.of_identifier (identifier :> Identifier.t) in
                        ((identifier :> Identifier.t), id)
                    | Model.Lang.Signature.Module (_, m) ->
                        let identifier = m.Model.Lang.Module.id in 
                        let id = Ident.of_identifier (identifier :> Identifier.t) in
                        ((identifier :> Identifier.t), id)
                    | _ -> failwith "Unhandled type in of_signature (1)") items
                in

                (* Now we construct the Components for each item,
                   converting all paths containing Identifiers pointing at
                   our elements to local paths *)
                List.fold_right2 (fun item (_, id) (items, env) ->
                    match item with
                    |  Model.Lang.Signature.Type (_, t) ->
                        let t' = of_type ident_map t in
                        (Signature.Type (id,t'))::items, add_type t.Model.Lang.TypeDecl.id t' {env with ident_max = env.ident_max + 1}
                    | Model.Lang.Signature.Module (_, m) ->
                        let m' = of_module ident_map m in
                        (Signature.Module (id, m'))::items, add_module m.Model.Lang.Module.id m' {env with ident_max = env.ident_max + 1}
                    | _ -> failwith "Unhandled type in of_signature") items ident_map ([], env)


    end
end


let signature_of_module_type _env m =
    match m with
    | Component.ModuleType.Path _ -> failwith "Unhandled"
    | Component.ModuleType.Signature s -> s

let signature_of_module env m =
    match m.Component.Module.type_ with
    | Component.Module.Alias _ -> failwith "Unhandled"
    | Component.Module.ModuleType expr -> signature_of_module_type env expr


(* When resolving paths, we go down the path until we find an Identifier. Once we've
   got that far we look up the component via the Identifier in the environment and return the
   resolved path for that Identifier and the Module component for it. As we then go up the Path.t
   we look up the Ident.t in the module component, finding a new module and passing
   that up the chain, building the resolved path as we go, until we pop out at the
   top to find the final thing - so far the only thing that can be handled is a
   type. *)
type parent_module_path =
  | Resolved of Model.Paths.Path.Resolved.Module.t * Component.Module.t
  | Unresolved of Model.Paths.Path.Module.t


let rec resolve_parent_module_path : Component.Env.t -> Model.Paths.Path.Module.t -> parent_module_path =
    fun env path ->
        match path with
        | `Resolved p ->
            resolve_resolved_parent_module_path env p
        | `Dot (parent, v) -> begin
            match resolve_parent_module_path env parent with
            | Unresolved pr -> Unresolved(`Dot(pr,v))
            | Resolved (pr, m) ->
                let s = signature_of_module env m in
                let m =
                    List.find_opt
                        (function
                        | Component.Signature.Module ((s,_), _) when s=v -> true
                        | _ -> false) s in
                match m with
                | Some (Component.Signature.Module (_,x)) ->
                    Resolved (`Module(pr, Model.Names.ModuleName.of_string v), x)
                | _ ->
                    Printf.printf "Failed to find '%s'" v;
                    Unresolved path 
            end
        | `Forward _
        | `Root _
        | `Apply _ -> Unresolved path

and resolve_resolved_parent_module_path env p =
    match p with
    | `Identifier ident ->
        Resolved(p, Component.Env.lookup_module ident env)
    | _ -> failwith "Unhandled"

and resolve_type_path : Component.Env.t -> Model.Paths.Path.Type.t -> Model.Paths.Path.Type.t = fun env p ->
    match p with
    | `Dot (parent, v) -> begin
        match resolve_parent_module_path env parent with
        | Resolved (path, m) ->
            let s = signature_of_module env m in
            if List.exists (function | Component.Signature.Type ((s,_), _) when s=v -> true | _ -> false) s
            then `Resolved (`Type (path, Model.Names.TypeName.of_string v))
            else `Dot (parent, v)
        | Unresolved path ->
            `Dot (path, v)
        end
    | `Resolved _ -> p

    
let rec resolve_unit env t =
    let open Model.Lang.Compilation_unit in
    {t with content = resolve_content env t.content}

and resolve_content env =
    let open Model.Lang.Compilation_unit in
    function
    | Module m -> Module (resolve_signature env m)
    | Pack _ -> failwith "Unhandled content"

and resolve_signature : Component.Env.t -> Model.Lang.Signature.t -> _ = fun env s ->
    let open Model.Lang.Signature in
    let (_,env) = Component.Env.of_signature env s in
    let (_, items') = 
        List.fold_right (fun item (env, items) ->
            match item with
            | Module (r, m) ->
                let m' = resolve_module env m in
(*                let env' = update_env env (`Module m') in *)
                (env, (Module (r, m'))::items)
            | Type (r, t) ->
                let t' = resolve_type env t in
(*                let env' = update_env env (`Type t') in*)
                (env, (Type (r, t'))::items)
            | _ -> failwith "Unhandled signature element") s (env, [])
    in items'

and resolve_module : Component.Env.t -> Model.Lang.Module.t -> Model.Lang.Module.t = fun env m ->
    let open Model.Lang.Module in
    match m.type_ with
    | ModuleType expr ->
        {m with type_ = ModuleType (resolve_module_type_expr env expr)}
    | _ -> failwith "Unhandled module"

and resolve_module_type_expr : Component.Env.t -> Model.Lang.ModuleType.expr -> Model.Lang.ModuleType.expr = fun env expr ->
    let open Model.Lang.ModuleType in
    match expr with
    | Signature s -> Signature (resolve_signature env s)
    | _ -> failwith "Unhandled module type expression"

and resolve_type env t =
    let open Model.Lang.TypeDecl in
    match t.equation.manifest with
    | Some texpr ->
        let texpr' = resolve_type_expression env texpr in
        {t with equation = {t.equation with manifest = Some texpr'}}
    | None -> t

and resolve_type_expression env texpr =
    let open Model.Lang.TypeExpr in 
    match texpr with
    | Constr (path, ts) ->
        Constr (resolve_type_path env path, ts)
    | _ -> failwith "Unhandled type expression"



let result () =
    let (_,_,s) = myexample () in
    resolve_signature Component.Env.empty s
