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
module AA : sig
  type 'a a
end

module X : sig
  type 'a t
  module type Y = sig
    type z 
  end
  type 'a u = 'a AA.a
end

module Z : X.Y

type x = Z.z
|}

let myexample2 () = model_of_string {|
module X : sig
  type t
  module type Y = sig
    type z 
  end
  module Z : Y
end

type x = X.Z.z
|}

let myexample3 () = model_of_string {|
module type A = sig
  module M : sig module type S end
  module N : M.S
end

module B : sig module type S = sig type t end end

module C : A with module M = B

type t = C.N.t
|}

let my_compilation_unit () =
    let id, docs, s = myexample3 () in
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


module Component = struct
    open Model.Paths
    open Model.Names

    module Ident = struct

        (* For simplicity keep a global counter *)
        let counter = ref 0

        (* Again, for simplicity right now all idents are just strings.
           Later we'll likely convert these to be typed Names *)
        type t = string * int

        let fresh_int () =
            let n = !counter in
            incr counter;
            n

        let of_identifier : Identifier.t -> t =
            fun i ->
                let n = fresh_int () in 
                match i with
                | `Module (_,s) -> (ModuleName.to_string s, n)
                | `Type (_,s) -> (TypeName.to_string s, n)
                | `ModuleType (_,s) -> (ModuleTypeName.to_string s, n)
                | _ -> failwith "Unhandled"

        let name : t -> string = fst

        let counter : t -> int = snd

        let fmt ppf (id : t) = Format.fprintf ppf "%s/%d" (fst id) (snd id)

        let rename (s, _) = (s, fresh_int ())
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
        type substitution =
            | ModuleEq of Model.Paths.Fragment.Module.t * Module.decl

        type expr =
            | Path of Path.t
            | Signature of Signature.t
            | With of expr * substitution list
    
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

    module Format = struct

        let rec signature ppf sg =
            let open Signature in
            Format.fprintf ppf "@[<v>";
            List.iter (function
                | Module (id,m) ->
                    Format.fprintf ppf
                        "@[<v 2>module %a%a@]@," Ident.fmt id
                        module_ m
                | ModuleType (id,mt) ->
                    Format.fprintf ppf
                        "@[<v 2>module type %a%a@]@," Ident.fmt id
                        module_type mt
                | Type (id,t) ->
                    Format.fprintf ppf
                        "@[<v 2>type %a%a@]@," Ident.fmt id type_ t) sg;
            Format.fprintf ppf "@]";

        and module_ ppf m =
            let open Module in
            match m.type_ with
            | Alias p ->
                Format.fprintf ppf " = %a" path p
            | ModuleType mt ->
                Format.fprintf ppf " : %a" module_type_expr mt
        
        and module_type ppf mt =
            match mt with
            | Some x -> Format.fprintf ppf " = %a" module_type_expr x
            | None -> ()

        and module_type_expr ppf mt =
            let open ModuleType in
            match mt with
            | Path p -> path ppf p
            | Signature sg -> Format.fprintf ppf "sig@,@[<v 2>%a@]end" signature sg
            | With (expr,_subs) -> Format.fprintf ppf "%a with subs" module_type_expr expr

        and type_ ppf t =
            match t with
            | Some expr -> Format.fprintf ppf " = %a" type_expr expr
            | None -> ()

        and type_expr ppf e =
            let open TypeExpr in
            match e with
            | Var x -> Format.fprintf ppf "%s" x
            | Constr (p,_args) -> path ppf p

        and path ppf p =
            match p with
            | `Local ident -> Format.fprintf ppf "%a" Ident.fmt ident
            | `Ldot (p,str) -> Format.fprintf ppf "%a.%s" path p str
            | `Global p -> Format.fprintf ppf "%a" model_path p
        
        and model_path ppf (p : Model.Paths.Path.t) =
            match p with
            | `Resolved rp -> model_resolved_path ppf rp
            | `Root s -> Format.fprintf ppf "%s" s
            | `Forward s -> Format.fprintf ppf "%s" s
            | `Dot (parent,s) -> Format.fprintf ppf "%a.%s" model_path (parent :> Model.Paths.Path.t) s
            | `Apply (func,arg) -> Format.fprintf ppf "%a(%a)" model_path (func :> Model.Paths.Path.t) model_path (arg :> Model.Paths.Path.t)

        and model_resolved_path ppf (p : Model.Paths.Path.Resolved.t) =
            match p with
            | `Identifier id -> Format.fprintf ppf "(%a)" model_identifier id
            | `Module (parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> Model.Paths.Path.Resolved.t) (Model.Names.ModuleName.to_string name)
            | `ModuleType (parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> Model.Paths.Path.Resolved.t) (Model.Names.ModuleTypeName.to_string name)
            | `Type (parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> Model.Paths.Path.Resolved.t) (Model.Names.TypeName.to_string name)
            | _ -> failwith "Unimplemented"
        
        and model_identifier ppf (p : Model.Paths.Identifier.t) =
            match p with
            | `Root (_, unit_name) -> Format.fprintf ppf "%s" (UnitName.to_string unit_name)
            | `Module (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Model.Paths.Identifier.t) (Model.Names.ModuleName.to_string name)
            | `ModuleType (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Model.Paths.Identifier.t) (Model.Names.ModuleTypeName.to_string name)
            | `Type (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Model.Paths.Identifier.t) (Model.Names.TypeName.to_string name)
            | _ -> failwith "Unimplemented"
    end

    module Of_Lang = struct
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

        and of_module_decl ident_map m =
            match m with
            | Model.Lang.Module.Alias p ->
                Module.Alias (local_path_of_path ident_map (p :> Model.Paths.Path.t))
            | Model.Lang.Module.ModuleType s ->
                Module.ModuleType (of_module_type_expr ident_map s)

        and of_module ident_map m =
            let type_ = of_module_decl ident_map m.Model.Lang.Module.type_ in
            {Module.type_}

        and of_module_type_substitution ident_map m =
            match m with
            | Model.Lang.ModuleType.ModuleEq (frag,decl) ->
                ModuleType.ModuleEq (frag, of_module_decl ident_map decl)
            | _ -> failwith "unhandled"
        
        and of_module_type_expr ident_map m =
            match m with
            | Model.Lang.ModuleType.Signature s ->
                let s = of_signature s in
                ModuleType.Signature s
            | Model.Lang.ModuleType.Path p ->
                let p' = local_path_of_path ident_map (p :> Model.Paths.Path.t) in
                ModuleType.Path p'
            | Model.Lang.ModuleType.With (e, subs) ->
                ModuleType.With (of_module_type_expr ident_map e,
                    List.map (of_module_type_substitution ident_map) subs)
            | _ -> failwith "Unhandled here"
        
        and of_module_type ident_map m =
            match m.Model.Lang.ModuleType.expr with
            | None -> None
            | Some m -> Some (of_module_type_expr ident_map m)

        and of_signature : Model.Lang.Signature.t -> Signature.t =
            fun items ->
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
                    | Model.Lang.Signature.ModuleType m ->
                        let identifier = m.Model.Lang.ModuleType.id in
                        let id = Ident.of_identifier (identifier :> Identifier.t) in
                        ((identifier :> Identifier.t), id)
                    | _ -> failwith "Unhandled type in of_signature (1)") items
                in

                (* Now we construct the Components for each item,
                   converting all paths containing Identifiers pointing at
                   our elements to local paths *)
                List.map2 (fun item (_, id) ->
                    match item with
                    |  Model.Lang.Signature.Type (_, t) ->
                        let t' = of_type ident_map t in
                        Signature.Type (id,t')
                    | Model.Lang.Signature.Module (_, m) ->
                        let m' = of_module ident_map m in
                        Signature.Module (id, m')
                    | Model.Lang.Signature.ModuleType m ->
                        let m' = of_module_type ident_map m in
                        Signature.ModuleType (id, m')
                    | _ -> failwith "Unhandled type in of_signature") items ident_map

    end

    module Subst = struct
        type t = {
            map : (Ident.t * [ `Local of Ident.t | `Global of Model.Paths.Path.t ]) list
        }

        let identity = {
            map = []
        }

        let add id subst t =
            { map = (id, subst) :: t.map }

        let rec path : t -> Path.t -> Path.t = fun s p ->
            match p with
            | `Local id -> begin
                match List.assoc_opt id s.map with
                | Some (`Local id') -> `Local id'
                | Some (`Global path) -> `Global path
                | None -> `Local id
                end
            | `Global _ -> p
            | `Ldot (parent,x) -> `Ldot (path s parent, x)

        let rec type_ s t = 
            match t with
            | Some t' -> Some (type_expr s t')
            | None -> None
        
        and type_expr s t =
            let open TypeExpr in
            match t with
            | Var s -> Var s
            | Constr (p, ts) -> Constr (path s p, List.map (type_expr s) ts)

        and module_type s t =
            match t with
            | Some m -> Some (module_type_expr s m)
            | None -> None

        and module_type_expr s t =
            let open ModuleType in
            match t with
            | Path p -> Path (path s p)
            | Signature sg -> Signature (signature s sg)
            | With (_,_) -> failwith "Unahdlalell"

        and module_ s t =
            let open Module in
            let type_ = match t.type_ with
                | Alias p -> Alias (path s p)
                | ModuleType t -> ModuleType (module_type_expr s t)
            in
            { type_ }

        and rename_bound_idents s sg =
            let open Signature in
            function
            | [] -> s, sg
            | Module (id, m) :: rest ->
                let id' = Ident.rename id in
                rename_bound_idents
                    (add id (`Local id') s)
                    (Module (id', m) :: sg)
                    rest
            | ModuleType (id, t) :: rest ->
                let id' = Ident.rename id in
                rename_bound_idents
                    (add id (`Local id') s)
                    (ModuleType (id', t) :: sg)
                    rest
            | Type (id, t) :: rest ->
                let id' = Ident.rename id in
                rename_bound_idents
                    (add id (`Local id') s)
                    (Type (id', t) :: sg)
                    rest

        and signature s sg =
            let open Signature in
            let s, sg = rename_bound_idents s [] sg in
            List.rev_map (function
                | Module (id, m) -> Module (id, module_ s m)
                | ModuleType (id, m) -> ModuleType (id, module_type s m)
                | Type (id, t) -> Type (id, type_ s t)) sg
    end

    module Env = struct
        (* A bunch of association lists. Let's hashtbl them up later *)
        type t =
            { ident_max: int
            ; modules : (Identifier.Module.t * Module.t) list
            ; module_types : (Identifier.ModuleType.t * ModuleType.t) list
            ; types : (Identifier.Type.t * Type.t) list }

        (* Handy for extrating transient state *)
        exception MyFailure of Model.Paths.Identifier.t * t

        let empty =
            { ident_max = 0
            ; modules = []
            ; module_types = []
            ; types = [] }

        let add_module identifier m env =
            { env with modules = (identifier, m)::env.modules}

        let add_type identifier t env =
            { env with types = (identifier, t)::env.types}

        let add_module_type identifier t env =
            { env with module_types = (identifier, t)::env.module_types}

        let lookup_module identifier env =
            try
                List.assoc identifier env.modules
            with _ -> raise (MyFailure ((identifier :> Model.Paths.Identifier.t), env))
        
        let lookup_type identifier env =
            List.assoc identifier env.types

        let lookup_module_type identifier env =
            List.assoc identifier env.module_types
        let open_signature : Model.Lang.Signature.t -> t -> t =
            fun s env ->
                List.fold_left (fun env orig ->
                    match orig with
                    | Model.Lang.Signature.Type (_, t) ->
                        let identifier = (t.id :> Model.Paths.Identifier.t) in
                        let id = Ident.of_identifier identifier in
                        let ty = Of_Lang.of_type [identifier,id] t in
                        add_type t.Model.Lang.TypeDecl.id ty env
                    | Model.Lang.Signature.Module (_, t) ->
                        let identifier = (t.id :> Model.Paths.Identifier.t) in
                        let id = Ident.of_identifier identifier in
                        let ty = Of_Lang.of_module [identifier,id] t in
                        add_module t.Model.Lang.Module.id ty env
                    | Model.Lang.Signature.ModuleType t ->
                        let identifier = (t.id :> Model.Paths.Identifier.t) in
                        let id = Ident.of_identifier identifier in
                        let ty = Of_Lang.of_module_type [identifier,id] t in
                        add_module_type t.Model.Lang.ModuleType.id ty env
                    | _ -> failwith "foo") env s
    end

    let prefix_signature path s =
        let open Signature in
        let sub = List.fold_left (fun map item ->
            match item with
            | Type (ident,_) -> Subst.add ident (`Global (`Resolved (`Type (path, TypeName.of_string (Ident.name ident)) :> Model.Paths.Path.Resolved.t))) map
            | Module (ident, _) -> Subst.add ident (`Global (`Resolved (`Module (path, ModuleName.of_string (Ident.name ident)) :> Model.Paths.Path.Resolved.t))) map
            | ModuleType (ident, _) -> Subst.add ident (`Global (`Resolved (`ModuleType (path, ModuleTypeName.of_string (Ident.name ident)) :> Model.Paths.Path.Resolved.t))) map)
                Subst.identity s in
        let open Signature in
        List.map (function
                | Module (id, m) -> Module (Ident.rename id, Subst.module_ sub m)
                | ModuleType (id, m) -> ModuleType (Ident.rename id, Subst.module_type sub m)
                | Type (id, t) -> Type (Ident.rename id, Subst.type_ sub t)) s

    
    module Strengthen = struct
        let rec signature (prefix : Model.Paths.Path.Resolved.Module.t) sg =
            let open Signature in
            let new_ids = List.map (function
                | Module (id, _) -> (id, Ident.rename id)
                | ModuleType (id, _) -> (id, Ident.rename id)
                | Type (id, _) -> (id, Ident.rename id)) sg in
            let sg' = List.map2 (fun item (_, new_id) ->
                match item with
                | Module (id, m) -> Module (new_id, module_ (`Module (prefix, ModuleName.of_string (Ident.name id))) m)
                | ModuleType (id, m) -> ModuleType (new_id, module_type (`ModuleType (prefix, ModuleTypeName.of_string (Ident.name id))) m)
                | Type (id, m) -> Type (new_id, type_ (`Type (prefix, TypeName.of_string (Ident.name id))) m))
                sg new_ids in
            let subst = List.fold_left (fun subst (old_id, new_id) -> Subst.add old_id (`Local new_id) subst) Subst.identity new_ids in
            Subst.signature subst sg'
        
        and module_ (prefix : Model.Paths.Path.Resolved.Module.t) m =
            match m.Module.type_ with
            | Alias _ -> m
            | ModuleType (Signature sg) -> { type_ = ModuleType (Signature (signature prefix sg)) }
            | ModuleType _ -> m

        and module_type (prefix: Model.Paths.Path.Resolved.ModuleType.t) m =
            match m with
            | None -> Some (ModuleType.Path (`Global ((`Resolved (prefix :> Model.Paths.Path.Resolved.t)))))
            | _ -> m

        and type_ (path: Model.Paths.Path.Resolved.Type.t) t =
            match t with
            | None -> Some (TypeExpr.Constr (`Global (`Resolved (path :> Model.Paths.Path.Resolved.t)), []))
            | _ -> t
    end
        
end

type env = Component.Env.t

let find_module_in_sig s name =
    let m =
        List.find_opt
            (function
            | Component.Signature.Module ((s,_), _) when s=name -> true
            | _ -> false) s
    in
    match m with
    | Some (Component.Signature.Module (_,x)) -> x
    | _ ->
        Printf.printf "Failed to find '%s'" name;
        failwith "Failed to find component"

let find_module_type_in_sig s name =
    let m =
        List.find_opt
            (function
            | Component.Signature.ModuleType ((s,_), _) when s=name -> true
            | _ -> false) s
    in
    match m with
    | Some (Component.Signature.ModuleType (_,x)) -> x
    | _ ->
        Printf.printf "Failed to find '%s'" name;
        failwith "Failed to find component"

let find_type_in_sig s name =
    let m =
        List.find_opt
            (function
            | Component.Signature.Type ((s,_), _) when s=name -> true
            | _ -> false) s
    in
    match m with
    | Some (Component.Signature.Type (_,x)) -> x
    | _ ->
        Printf.printf "Failed to find '%s'" name;
        failwith "Failed to find component"

type module_lookup_result =
    Model.Paths.Path.Resolved.Module.t * Component.Module.t

type module_type_lookup_result =
    Model.Paths.Path.Resolved.ModuleType.t * Component.ModuleType.t

type type_lookup_result =
    Model.Paths.Path.Resolved.Type.t * Component.Type.t

let rec lookup_module_from_model_path : env -> Model.Paths.Path.Module.t -> (module_lookup_result, Model.Paths.Path.Module.t) Result.result = fun env p ->
    match p with
    | `Resolved p -> Ok (lookup_module_from_resolved_model_path env p)
    | `Dot (m, x) -> begin
        match lookup_module_from_model_path env m with
        | Error p -> Error (`Dot (p, x))
        | Ok (p', m) -> 
            let s = signature_of_module env m |> Component.prefix_signature p' in
            let m' = find_module_in_sig s x in
            Ok (`Module (p', Model.Names.ModuleName.of_string x), m')
        end
    | _ -> Error p

and lookup_module_from_resolved_model_path : env -> Model.Paths.Path.Resolved.Module.t -> module_lookup_result = fun env p ->
    match p with
    | `Identifier i ->
        let m = Component.Env.lookup_module i env in
        (p, m)
    | `Hidden m ->
        let (p, m) = lookup_module_from_resolved_model_path env m in
        (`Hidden p, m)
    | `Module (p, name) ->
        let (p', m) = lookup_module_from_resolved_model_path env p in
        let sg = signature_of_module env m |> Component.prefix_signature p in
        let m' = find_module_in_sig sg (Model.Names.ModuleName.to_string name) in
        (`Module (p', name), m')
    | _ ->
        let b = Buffer.create 1024 in
        let fmt = Format.formatter_of_buffer b in
        Format.fprintf fmt "Failed to lookup path: %a@." Component.Format.model_resolved_path (p :> Model.Paths.Path.Resolved.t);
        failwith (Buffer.contents b)

and lookup_module_from_path : env -> Component.Path.t -> (module_lookup_result, Model.Paths.Path.Module.t) Result.result = fun env p ->
    match p with
    | `Local id -> failwith (Printf.sprintf "oh no %s" (Component.Ident.name id))
    | `Ldot (parent, id) -> begin
        match lookup_module_from_path env parent with
        | Ok (p, m) ->
            let sg = signature_of_module env m |> Component.prefix_signature p in
            let m' = find_module_in_sig sg id in
            Ok (`Module (p, Model.Names.ModuleName.of_string id), m')
        | Error p ->
            Error (`Dot (p, id))
        end
    | `Global ((
              `Root _
            | `Forward _
            | `Dot (#Model.Paths.Path.Module.t, _)
            | `Resolved (#Model.Paths_types.Resolved_path.module_no_id)
            | `Resolved (`Identifier (#Model.Paths_types.Identifier.module_))
            | `Apply (_,_)) as p) ->
        lookup_module_from_model_path env p
    | _ -> failwith "bad lookup"



and lookup_module_type_from_model_path : env -> Model.Paths.Path.ModuleType.t -> (module_type_lookup_result, Model.Paths.Path.ModuleType.t) Result.result = fun env p ->
    match p with
    | `Resolved p -> Ok (lookup_module_type_from_resolved_model_path env p)
    | `Dot (m, x) -> begin
        match lookup_module_from_model_path env m with
        | Error p -> Error (`Dot (p, x))
        | Ok (p', m) ->
            let s = signature_of_module env m in
            let m' = find_module_type_in_sig s x in
            Ok (`ModuleType (p', Model.Names.ModuleTypeName.of_string x), m')
        end

and lookup_module_type_from_resolved_model_path : env -> Model.Paths.Path.Resolved.ModuleType.t -> module_type_lookup_result = fun env p ->
    match p with
    | `Identifier i ->
        let m = Component.Env.lookup_module_type i env in
        (p, m)
    | `ModuleType (parent, name) ->
        let (p', m) = lookup_module_from_resolved_model_path env parent in
        let sg = signature_of_module env m in
        let mt = find_module_type_in_sig sg name in
        (`ModuleType (p', name), mt)

and lookup_module_type_from_path : env -> Component.Path.t -> (module_type_lookup_result, Model.Paths.Path.ModuleType.t) Result.result = fun env p ->
    match p with
    | `Local id -> failwith (Printf.sprintf "oh no %s" (Component.Ident.name id))
    | `Ldot (parent, id) -> begin
        match lookup_module_from_path env parent with
        | Ok (p, m) ->
            let sg = signature_of_module env m in
            let m' = find_module_type_in_sig sg id in
            Ok (`ModuleType (p, Model.Names.ModuleName.of_string id), m')
        | Error p ->
            Error (`Dot (p, id))
        end
    | `Global ((
              `Resolved (#Model.Paths_types.Resolved_path.module_type_no_id)
            | `Resolved (`Identifier (#Model.Paths_types.Identifier.module_type))
            | `Dot (_,_)) as p) ->
        lookup_module_type_from_model_path env p
    | _ -> failwith "bad lookup"

and lookup_type_from_model_path : env -> Model.Paths.Path.Type.t -> (type_lookup_result, Model.Paths.Path.Type.t) Result.result = fun env p ->
    match p with
    | `Resolved p -> Ok (lookup_type_from_resolved_model_path env p)
    | `Dot (m, x) -> begin
        match lookup_module_from_model_path env m with
        | Error p -> Error (`Dot (p, x))
        | Ok (p', m) ->
            let s = signature_of_module env m in
            let m' = find_type_in_sig s x in
            Ok (`Type (p', Model.Names.TypeName.of_string x), m')
        end

and lookup_type_from_resolved_model_path : env -> Model.Paths.Path.Resolved.Type.t -> type_lookup_result = fun env p ->
    match p with
    | `Identifier (`Type _ as i) ->
        let m = Component.Env.lookup_type i env in
        (p, m)
    | `Type (parent, name) ->
        let (p', m) = lookup_module_from_resolved_model_path env parent in
        let sg = signature_of_module env m in
        let t = find_type_in_sig sg name in
        (`Type (p', name), t)
    | _ -> failwith "Unhandled"

and lookup_type_from_path : env -> Component.Path.t -> (type_lookup_result, Model.Paths.Path.Type.t) Result.result = fun env p ->
    match p with
    | `Local id -> failwith (Printf.sprintf "oh no %s" (Component.Ident.name id))
    | `Ldot (parent, id) -> begin
        match lookup_module_from_path env parent with
        | Ok (p, m) ->
            let sg = signature_of_module env m in
            let t' = find_type_in_sig sg id in
            Ok (`Type (p, Model.Names.ModuleName.of_string id), t')
        | Error p ->
            Error (`Dot (p, id))
        end
    | `Global ((
              `Resolved (#Model.Paths_types.Resolved_path.type_no_id)
            | `Resolved (`Identifier (#Model.Paths_types.Identifier.type_))
            | `Dot (_,_)) as p) ->
        lookup_type_from_model_path env p
    | _ -> failwith "bad lookup"


and fragmap_signature : env -> Model.Paths.Fragment.Resolved.Signature.t -> (Component.Signature.t -> Component.Signature.t) -> Component.Signature.t -> Component.Signature.t =
    fun env frag fn sg ->
        match frag with
        | `Root -> fn sg
        | `Module (parent, name) ->
            fragmap_signature env parent (fun s ->
                List.map (function
                    | Component.Signature.Module ((id, _) as id', m) when id=name ->
                        let sg = signature_of_module env m in
                        let sg' = fn sg in
                        Component.Signature.Module (id',Component.Module.{type_=ModuleType (Component.ModuleType.Signature sg')})
                    | x -> x) s) sg
        | _ -> failwith "foo"

and fragmap_module : env -> Model.Paths.Fragment.Resolved.Module.t -> (Component.Module.t -> Component.Module.t) -> Component.Signature.t -> Component.Signature.t =
    fun env frag fn sg ->
        match frag with
        | `Module (parent, name) ->
            fragmap_signature env parent (fun s ->
                List.map (function 
                | Component.Signature.Module ((id, _) as id', m) when id=name ->
                    let m' = fn m in
                    Component.Signature.Module (id', m')
                | x -> x) s) sg
        | `Subst (_, x) -> fragmap_module env x fn sg
        | `SubstAlias (_, x) -> fragmap_module env x fn sg


(*        fragmap_signature env (frag : Model.Paths.Fragment.Resolved.Module.t :> Model.Paths.Fragment.Resolved.Signature.t) fn sg*)

and fragmap_unresolved_signature env frag fn sg =
    match frag with
    | `Dot (parent, name) ->
        fragmap_unresolved_signature env parent (fun s ->
            List.map (function
                | Component.Signature.Module ((id, _) as id', m) when id=name ->
                    let sg = signature_of_module env m in
                    let sg' = fn sg in
                    Component.Signature.Module (id', Component.Module.{type_=ModuleType (Component.ModuleType.Signature sg')})
                | x -> x) s) sg
    | `Resolved x ->
        fragmap_signature env x fn sg

and fragmap_unresolved_module env frag fn sg =
    match frag with
    | `Dot (parent, name) ->
        fragmap_unresolved_signature env parent (fun s ->
            List.map (function
                | Component.Signature.Module ((id, _) as id', m) when id=name ->
                    let m' = fn m in
                    Component.Signature.Module (id', m')
                | x -> x) s) sg
    | `Resolved x ->
        fragmap_module env x fn sg

and signature_of_module_type_expr : env -> Component.ModuleType.expr -> Component.Signature.t = fun env m ->
    match m with
    | Component.ModuleType.Path p -> begin
        match lookup_module_type_from_path env p with
        | Ok (_p, mt) ->
            let sg = signature_of_module_type env mt in
(*            Component.Strengthen.signature _p sg *)
            sg
        | Error _p ->
            failwith "Couldn't find signature"
        end
    | Component.ModuleType.Signature s -> s
    | Component.ModuleType.With (s, subs) ->
        let sg = signature_of_module_type_expr env s in
        List.fold_left (fun sg sub ->
            match sub with
            | Component.ModuleType.ModuleEq (frag, Alias path) -> begin
                match lookup_module_from_path env path with
                | Ok (_p, m) ->
                    fragmap_unresolved_module env frag (fun _ -> m) sg
                | Error _ ->
                    failwith "Failed to lookup substitute module"
                end
            | ModuleEq (frag, ModuleType expr) ->

                fragmap_unresolved_module env frag (fun _ -> Component.Module.{type_ = ModuleType expr}) sg
            ) sg subs

and signature_of_module_type : env -> Component.ModuleType.t -> Component.Signature.t = fun env m ->
    match m with
    | None -> failwith "oh no"
    | Some expr -> signature_of_module_type_expr env expr

and signature_of_module env m =
    match m.Component.Module.type_ with
    | Component.Module.Alias _ -> failwith "Unhandled"
    | Component.Module.ModuleType expr -> signature_of_module_type_expr env expr




(* When resolving paths, we go down the path until we find an Identifier. Once we've
   got that far we look up the component via the Identifier in the environment and return the
   resolved path for that Identifier and the Module component for it. As we then go up the Path.t
   we look up the Ident.t in the module component, finding a new module and passing
   that up the chain, building the resolved path as we go, until we pop out at the
   top to find the final thing - so far the only thing that can be handled is a
   type. *)

and resolve_type_path : env -> Model.Paths.Path.Type.t -> Model.Paths.Path.Type.t = fun env p ->
    match lookup_type_from_model_path env p with
    | Ok (p', _) -> `Resolved p'
    | Error p -> p

and resolve_module_type_path : env -> Model.Paths.Path.ModuleType.t -> Model.Paths.Path.ModuleType.t = fun env p ->
    match lookup_module_type_from_model_path env p with
    | Ok (p', _) -> `Resolved p'
    | Error p -> p
    
let rec resolve_unit env t =
    let open Model.Lang.Compilation_unit in
    {t with content = resolve_content env t.content}

and resolve_content env =
    let open Model.Lang.Compilation_unit in
    function
    | Module m -> Module (resolve_signature env m)
    | Pack _ -> failwith "Unhandled content"

and resolve_signature : env -> Model.Lang.Signature.t -> _ = fun env s ->
    let open Model.Lang.Signature in
    let env = Component.Env.open_signature s env in
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
            | ModuleType mt ->
                let mt' = resolve_module_type env mt in
                (env, (ModuleType mt')::items)
            | _ -> failwith "Unhandled signature element") s (env, [])
    in items'

and resolve_module : env -> Model.Lang.Module.t -> Model.Lang.Module.t = fun env m ->
    let open Model.Lang.Module in
    match m.type_ with
    | ModuleType expr ->
        {m with type_ = ModuleType (resolve_module_type_expr env expr)}
    | _ -> failwith "Unhandled module"

and resolve_module_type : env -> Model.Lang.ModuleType.t -> Model.Lang.ModuleType.t = fun env m ->
    let open Model.Lang.ModuleType in
    let expr' = match m.expr with | None -> None | Some expr -> Some (resolve_module_type_expr env expr) in
    {m with expr = expr'}

and resolve_module_type_expr : env -> Model.Lang.ModuleType.expr -> Model.Lang.ModuleType.expr = fun env expr ->
    let open Model.Lang.ModuleType in
    match expr with
    | Signature s -> Signature (resolve_signature env s)
    | Path p -> Path (resolve_module_type_path env p)
    | With (expr, subs) ->
        With (resolve_module_type_expr env expr,
            List.map (function
                | ModuleEq (frag, eqn) -> ModuleEq (frag, eqn)
                | TypeEq (frag, eqn) -> TypeEq (frag, eqn)
                | x -> x) subs)
    | _ -> failwith "Unhandled module type expression"

and resolve_type env t =
    let open Model.Lang.TypeDecl in
    match t.equation.manifest with
    | Some texpr ->
        let texpr' = resolve_type_expression env texpr in
        {t with equation = {t.equation with manifest = Some texpr'}}
    | None -> t

and resolve_type_expression : env -> _ -> _ = fun env texpr ->
    let open Model.Lang.TypeExpr in 
    match texpr with
    | Constr (path, ts) -> begin
        match lookup_type_from_model_path env path with
        | Ok (p, _) ->  Constr (`Resolved p, ts)
        | Error p -> Constr (p, ts)
        end
    | _ -> failwith "Unhandled type expression"

and resolve_compilation_unit : env -> Odoc.Compilation_unit.t -> Odoc.Compilation_unit.t = fun env c ->
    let open Model.Lang.Compilation_unit in
    let content' = 
        match c.content with
        | Module s -> Module (resolve_signature env s)
        | Pack _ -> failwith "Unhandled"
    in
    { c with
      content = content' }

let mkenv () =
  Odoc.Env.create ~important_digests:false ~directories:[]

let resolve unit =
  let env = mkenv () in
  let resolve_env = Odoc.Env.build env (`Unit unit) in
  let resolver = Odoc.Env.resolver resolve_env in
  let result = Xref.resolve resolver unit in
  let tbl = Xref.tbl resolver in
  (result,tbl)


let result () =
    let u = my_compilation_unit () in
    let x = resolve_compilation_unit Component.Env.empty u in
(*    let y = resolve u in 
    (x = fst y)*)
    x
