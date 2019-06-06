(* The following types are identical in structure to those in
    {!Lang}, which may well be too complex for our use, we'll
    see as we expand on this *)
module rec Module : sig
    type decl =
    | Alias of Cpath.t
    | ModuleType of ModuleType.expr

    type t =
      { id : Ident.t
      ; type_ : decl }
end = Module 

and TypeExpr : sig
    type t =
        | Var of string
        | Constr of Cpath.t * t list 
end = TypeExpr

and ModuleType : sig
    type substitution =
        | ModuleEq of Model.Paths.Fragment.Module.t * Module.decl

    type expr =
        | Path of Cpath.t
        | Signature of Signature.t
        | With of expr * substitution list

    type t =
      { id : Ident.t
      ; expr : expr option }
end = ModuleType

and Type : sig
    type t =
      { id : Ident.t
      ; manifest : TypeExpr.t option }
end = Type

and Signature : sig
    type item = 
        | Module of Module.t
        | ModuleType of ModuleType.t
        | Type of Type.t
    
    type t = item list
end = Signature



module Fmt = struct

    let string_of fmt c =
        let b = Buffer.create 100 in
        Format.fprintf (Format.formatter_of_buffer b) "%a%!" fmt c;
        Buffer.contents b

    let rec signature ppf sg =
        let open Signature in
        Format.fprintf ppf "@[<v>";
        List.iter (function
            | Module m ->
                Format.fprintf ppf
                    "@[<v 2>module %a@]@,"
                    module_ m
            | ModuleType mt ->
                Format.fprintf ppf
                    "@[<v 2>module type %a@]@,"
                    module_type mt
            | Type t ->
                Format.fprintf ppf
                    "@[<v 2>type %a@]@," type_ t) sg;
        Format.fprintf ppf "@]"

    and module_decl ppf d =
        let open Module in
        match d with
        | Alias p ->
            Format.fprintf ppf "= %a" path p
        | ModuleType mt ->
            Format.fprintf ppf ": %a" module_type_expr mt

    and module_ ppf m =
        Format.fprintf ppf "%a %a" Ident.fmt m.id module_decl m.type_

    and module_type ppf mt =
        match mt.expr with
        | Some x -> Format.fprintf ppf "%a = %a" Ident.fmt mt.id module_type_expr x
        | None -> Format.fprintf ppf "%a" Ident.fmt mt.id

    and module_type_expr ppf mt =
        let open ModuleType in
        match mt with
        | Path p -> path ppf p
        | Signature sg -> Format.fprintf ppf "sig@,@[<v 2>%a@]end" signature sg
        | With (expr,subs) -> Format.fprintf ppf "%a with [%a]" module_type_expr expr substitution_list subs

    and type_ ppf t =
        match t.manifest with
        | Some expr -> Format.fprintf ppf "%a = %a" Ident.fmt t.id type_expr expr
        | None -> Format.fprintf ppf "%a" Ident.fmt t.id

    and substitution ppf t =
        let open ModuleType in
        match t with
        | ModuleEq (frag, decl) ->
            Format.fprintf ppf "%a -> %a" model_fragment (frag :> Model.Paths.Fragment.t) module_decl decl
    
    and substitution_list ppf l =
        match l with
        | sub :: (_ :: _) as subs -> Format.fprintf ppf "%a; %a" substitution sub substitution_list subs
        | sub :: [] -> Format.fprintf ppf "%a" substitution sub
        | [] -> ()

    and type_expr ppf e =
        let open TypeExpr in
        match e with
        | Var x -> Format.fprintf ppf "%s" x
        | Constr (p,_args) -> path ppf p

    and path ppf p =
        match p with
        | `Local ident -> Format.fprintf ppf "%a" Ident.fmt ident
        | `Ldot (p,str) -> Format.fprintf ppf "%a.%s" path p str
        | `Global p -> Format.fprintf ppf "[%a]" model_path p

    and model_path ppf (p : Model.Paths.Path.t) =
        match p with
        | `Resolved rp -> model_resolved_path ppf rp
        | `Root s -> Format.fprintf ppf "*%s" s
        | `Forward s -> Format.fprintf ppf "*%s" s
        | `Dot (parent,s) -> Format.fprintf ppf "*%a.%s" model_path (parent :> Model.Paths.Path.t) s
        | `Apply (func,arg) -> Format.fprintf ppf "*%a(%a)" model_path (func :> Model.Paths.Path.t) model_path (arg :> Model.Paths.Path.t)

    and model_resolved_path ppf (p : Model.Paths.Path.Resolved.t) =
        match p with
        | `Identifier id -> Format.fprintf ppf "(%a)" model_identifier id
        | `Module (parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> Model.Paths.Path.Resolved.t) (Model.Names.ModuleName.to_string name)
        | `ModuleType (parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> Model.Paths.Path.Resolved.t) (Model.Names.ModuleTypeName.to_string name)
        | `Type (parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> Model.Paths.Path.Resolved.t) (Model.Names.TypeName.to_string name)
        | _ -> failwith "Unimplemented"
    
    and model_identifier ppf (p : Model.Paths.Identifier.t) =
        match p with
        | `Root (_, unit_name) -> Format.fprintf ppf "%s" (Model.Names.UnitName.to_string unit_name)
        | `Module (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Model.Paths.Identifier.t) (Model.Names.ModuleName.to_string name)
        | `ModuleType (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Model.Paths.Identifier.t) (Model.Names.ModuleTypeName.to_string name)
        | `Type (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Model.Paths.Identifier.t) (Model.Names.TypeName.to_string name)
        | _ -> failwith "Unimplemented"

    and model_fragment ppf (f : Model.Paths.Fragment.t) =
        match f with
        | `Resolved rf -> model_resolved_fragment ppf rf
        | `Dot (sg, d) -> Format.fprintf ppf "*%a.%s" model_fragment (sg :> Model.Paths.Fragment.t) d
    
    and model_resolved_fragment ppf (f : Model.Paths.Fragment.Resolved.t) =
        match f with
        | `Root -> ()
        | `Module (sg, m) -> Format.fprintf ppf "%a.%s" model_resolved_fragment (sg :> Model.Paths.Fragment.Resolved.t) m
        | _ -> failwith "Unimplmented"

end

module Of_Lang = struct
    let ident_of_identifier ident_map identifier =
        List.assoc_opt identifier ident_map

    let rec local_path_of_path : _ -> Model.Paths.Path.t -> Cpath.t = fun ident_map path ->
        match path with
        | `Resolved (`Identifier i) -> begin
            match ident_of_identifier ident_map i with
            | Some ident -> 
                `Local ident
            | None ->
                `Global path
            end 
        | `Resolved _ ->
            `Global path
        | `Dot (path', x) ->
            `Ldot (local_path_of_path ident_map (path' :> Model.Paths.Path.t), x)
        | _ -> failwith "Unhandled in local_path_of_path"

    let rec of_type ident_map id ty =
        let open Model.Lang.TypeDecl in
        let manifest =
            match ty.equation.Equation.manifest with
            | None -> None
            | Some expr ->
                match expr with
                | Constr (path, _) ->
                    Some (TypeExpr.Constr (local_path_of_path ident_map (path :> Model.Paths.Path.t), []))
                | _ -> failwith "Unhandled in of_type"
        in
        { Type.id; manifest }
    and of_module_decl ident_map m =
        match m with
        | Model.Lang.Module.Alias p ->
            Module.Alias (local_path_of_path ident_map (p :> Model.Paths.Path.t))
        | Model.Lang.Module.ModuleType s ->
            Module.ModuleType (of_module_type_expr ident_map s)

    and of_module ident_map id m =
        let type_ = of_module_decl ident_map m.Model.Lang.Module.type_ in
        {Module.id; type_}

    and of_module_type_substitution ident_map m =
        match m with
        | Model.Lang.ModuleType.ModuleEq (frag,decl) ->
            ModuleType.ModuleEq (frag, of_module_decl ident_map decl)
        | _ -> failwith "unhandled"
    
    and of_module_type_expr ident_map m =
        match m with
        | Model.Lang.ModuleType.Signature s ->
            let s = of_signature ident_map s in
            ModuleType.Signature s
        | Model.Lang.ModuleType.Path p ->
            let p' = local_path_of_path ident_map (p :> Model.Paths.Path.t) in
            ModuleType.Path p'
        | Model.Lang.ModuleType.With (e, subs) ->
            ModuleType.With (of_module_type_expr ident_map e,
                List.map (of_module_type_substitution ident_map) subs)
        | _ -> failwith "Unhandled here"
    
    and of_module_type ident_map id m =
        let expr =
            match m.Model.Lang.ModuleType.expr with
            | None -> None
            | Some m -> Some (of_module_type_expr ident_map m)
        in
        {ModuleType.id; expr}

    and of_signature : _ -> Model.Lang.Signature.t -> Signature.t =
        let open Model.Paths in
        fun ident_map items ->
            (* First we construct a list of brand new [Ident.t]s 
                for each item in the signature *)
            let ident_map_new : (Identifier.t * Ident.t) list = List.map (function
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

            let ident_map = ident_map_new @ ident_map in
            (* Now we construct the Components for each item,
                converting all paths containing Identifiers pointing at
                our elements to local paths *)
            List.map2 (fun item (_, id) ->
                match item with
                |  Model.Lang.Signature.Type (_, t) ->
                    let t' = of_type ident_map id t in
                    Signature.Type t'
                | Model.Lang.Signature.Module (_, m) ->
                    let m' = of_module ident_map id m in
                    Signature.Module m'
                | Model.Lang.Signature.ModuleType m ->
                    let m' = of_module_type ident_map id m in
                    Signature.ModuleType m'
                | _ -> failwith "Unhandled type in of_signature") items ident_map_new

end

module Find = struct

exception Find_failure of Signature.t * string * string

let fail sg name ty = raise (Find_failure (sg, name, ty))

let module_in_sig s name =
    let rec inner = function
      | (Signature.Module m)::_ when (Ident.name m.id)=name -> m
      | _::rest -> inner rest
      | [] -> fail s name "module"
    in inner s

let module_type_in_sig s name =
    let rec inner = function
      | (Signature.ModuleType m)::_ when (Ident.name m.id)=name -> m
      | _::rest -> inner rest
      | [] -> fail s name "module"
    in inner s

let type_in_sig s name =
    let rec inner = function
      | (Signature.Type m)::_ when (Ident.name m.id)=name -> m
      | _::rest -> inner rest
      | [] -> fail s name "module"
    in inner s

end
