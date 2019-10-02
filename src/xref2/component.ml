(* The following types are identical in structure to those in
    {!Lang}, which may well be too complex for our use, we'll
    see as we expand on this *)

module Comment = Odoc_model.Comment

module Delayed = struct
    type 'a t = {
        mutable v : 'a option;
        get : unit -> 'a;
    }

    let get : 'a t -> 'a = fun x ->
        match x.v with
        | Some x -> x
        | None ->
            let v = x.get () in
            x.v <- Some v;
            v

    let put : (unit -> 'a) -> 'a t = fun f ->
        { v = None
        ; get = f }
end

module Opt = struct
    let map f = function
      | Some x -> Some (f x)
      | None -> None
end

module rec Module : sig
    type decl =
    | Alias of Cpath.t
    | ModuleType of ModuleType.expr

    type t =
      { doc : Comment.docs
      ; type_ : decl 
      ; canonical : (Cpath.t * Odoc_model.Paths.Reference.Module.t) option
      ; hidden : bool
      ; display_type : decl option
      }
end = Module

and TypeExpr : sig

    module Polymorphic_variant : sig
        type kind = Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind
    
        module Constructor :
        sig
          type t = {
            name : string;
            constant : bool;
            arguments : TypeExpr.t list;
            doc : Comment.docs;
          }
        end
    
        type element =
          | Type of TypeExpr.t
          | Constructor of Constructor.t
    
        type t =
          { kind: kind;
            elements: element list;}
    end

    module Object : sig

        type method_ =
          { name: string;
            type_: TypeExpr.t; }
    
        type field =
          | Method of method_
          | Inherit of TypeExpr.t
    
        type t =
          { fields: field list;
            open_ : bool; }
    
      end
    
      module Package : sig
    
        type substitution = Odoc_model.Paths.Fragment.Type.t * TypeExpr.t
    
        type t =
          { path: Cpath.t;
            substitutions: substitution list; }
    
      end
    
    type label = Odoc_model.Lang.TypeExpr.label
    type t =
        | Var of string
        | Any
        | Alias of t * string
        | Arrow of label option * t * t
        | Tuple of t list
        | Constr of Cpath.t * t list
        | Polymorphic_variant of TypeExpr.Polymorphic_variant.t
        | Object of TypeExpr.Object.t
        | Class of Cpath.t * t list
        | Poly of string list * t
        | Package of TypeExpr.Package.t

end = TypeExpr

and Extension : sig
    module Constructor : sig

        type t =
          { id: Ident.t;
            doc: Comment.docs;
            args: TypeDecl.Constructor.argument;
            res: TypeExpr.t option; }
    
      end
    
      type t =
        { type_path: Cpath.t;
          doc: Comment.docs;
          type_params: TypeDecl.param list;
          private_: bool;
          constructors: Constructor.t list; }
    
end = Extension
and Exception : sig

    type t = 
        { doc : Comment.docs
        ; args : TypeDecl.Constructor.argument
        ; res : TypeExpr.t option }

end = Exception

and FunctorArgument : sig
    type t =
        { id : Ident.t
        ; expr : ModuleType.expr }
end = FunctorArgument

and ModuleType : sig
    type substitution =
        | ModuleEq of Odoc_model.Paths.Fragment.Module.t * Module.decl
        | ModuleSubst of Odoc_model.Paths.Fragment.Module.t * Cpath.t
        | TypeEq of Odoc_model.Paths.Fragment.Type.t * TypeDecl.Equation.t
        | TypeSubst of Odoc_model.Paths.Fragment.Type.t * TypeDecl.Equation.t

    type expr =
        | Path of Cpath.t
        | Signature of Signature.t
        | With of expr * substitution list
        | Functor of FunctorArgument.t option * expr
        | TypeOf of Module.decl

    type t =
      { doc : Comment.docs
      ; expr : expr option }
end = ModuleType


and TypeDecl : sig

    module Field : sig
        type t =
            { id : Ident.t
            ; doc : Comment.docs
            ; mutable_ : bool
            ; type_ : TypeExpr.t }
    end

    module Constructor : sig
        type argument =
            | Tuple of TypeExpr.t list
            | Record of Field.t list
        
        type t = 
            { id : Ident.t
            ; doc : Comment.docs
            ; args : argument
            ; res: TypeExpr.t option }
    end

    module Representation : sig
        type t =
            | Variant of Constructor.t list
            | Record of Field.t list
            | Extensible
    end

    type param = Odoc_model.Lang.TypeDecl.param

    module Equation : sig
        type t =
            { params: param list
            ; private_ : bool
            ; manifest : TypeExpr.t option
            ; constraints : (TypeExpr.t * TypeExpr.t) list}
    end

    type t =
      { doc : Comment.docs
      ; equation : Equation.t
      ; representation : Representation.t option }
end = TypeDecl


and Value : sig

    type t =
      { doc: Comment.docs
      ; type_: TypeExpr.t }
  
end = Value

and Signature : sig
    type recursive = Odoc_model.Lang.Signature.recursive

    type item =
        | Module of Ident.t * recursive * (Module.t Delayed.t)
        | ModuleType of Ident.t * ModuleType.t
        | Type of Ident.t * recursive * TypeDecl.t
        | Exception of Ident.t * Exception.t
        | TypExt of Extension.t
        | Value of Ident.t * Value.t
        | External of Ident.t * External.t
        | Class of Ident.t * recursive * Class.t
        | ClassType of Ident.t * recursive * ClassType.t
        | Include of Ident.t * Include.t
        | Comment of Comment.docs_or_stop

    (* When doing destructive substitution we keep track of the items that have been removed,
       and the path they've been substituted with *)
    type removed_item =
        | RModule of Ident.t * Cpath.resolved option
        | RType of Ident.t * Cpath.resolved option

    type t =
        { items: item list
        ; removed: removed_item list }
end = Signature

and Include : sig
    type t =
        { doc: Comment.docs
        ; decl: Module.decl }
end = Include

and External : sig
    type t =
        { doc : Comment.docs
        ; type_ : TypeExpr.t
        ; primitives : string list }

end = External

and Class : sig
    type decl =
        | ClassType of ClassType.expr
        | Arrow of TypeExpr.label option * TypeExpr.t * decl
    
    type t =
        { doc : Comment.docs
        ; virtual_ : bool
        ; params: TypeDecl.param list
        ; type_ : decl }
end = Class

and ClassType : sig
    type expr =
        | Constr of Cpath.t * TypeExpr.t list
        | Signature of ClassSignature.t
    
    type t = 
        { doc : Comment.docs
        ; virtual_ : bool
        ; params: TypeDecl.param list
        ; expr: expr }
end = ClassType

and ClassSignature : sig
    type item =
        | Method of Method.t
        | InstanceVariable of InstanceVariable.t
        | Constraint of TypeExpr.t * TypeExpr.t
        | Inherit of ClassType.expr
        | Comment of Comment.docs_or_stop
    
    type t =
        { self: TypeExpr.t option
        ; items: item list }
end = ClassSignature

and Method : sig
    type t =
        { id : Ident.t
        ; doc : Comment.docs
        ; private_: bool
        ; virtual_: bool
        ; type_ : TypeExpr.t }
end = Method

and InstanceVariable : sig
    type t =
        { id: Ident.t
        ; doc: Comment.docs
        ; mutable_ : bool
        ; virtual_ : bool
        ; type_ : TypeExpr.t }
end = InstanceVariable


  

module Element = struct
    open Odoc_model.Paths
    type module_ = [ `Module of Identifier.Module.t * Module.t ]
    type module_type = [ `ModuleType of Identifier.ModuleType.t * ModuleType.t ]
    type type_ = [ `Type of Identifier.Type.t * TypeDecl.t ]
    type value = [ `Value of Identifier.Value.t * Value.t ]
    type label = [ `Label of Identifier.Label.t ]
    type signature = [ module_ | module_type ]
    type any = [ signature | value | type_ | label ]
end



module Fmt = struct
    let option formatter fmt x =
        match x with
        | Some x' -> Format.fprintf fmt "%a" formatter x'
        | None -> ()

    let string_of fmt c =
        let b = Buffer.create 100 in
        Format.fprintf (Format.formatter_of_buffer b) "%a%!" fmt c;
        Buffer.contents b

    let rec signature ppf sg =
        let open Signature in
        Format.fprintf ppf "@[<v>";
        List.iter (function
            | Module (id,_,m) ->
                Format.fprintf ppf
                    "@[<v 2>module %a %a@]@,"
                    Ident.fmt id
                    module_ (Delayed.get m)
            | ModuleType (id, mt) ->
                Format.fprintf ppf
                    "@[<v 2>module type %a %a@]@,"
                    Ident.fmt id
                    module_type mt
            | Type (id, _,t) ->
                Format.fprintf ppf
                    "@[<v 2>type %a %a@]@," Ident.fmt id type_decl t
            | Exception (id, e) ->
                Format.fprintf ppf
                    "@[<v 2>exception %a %a@]@," Ident.fmt id exception_ e
            | TypExt e ->
                Format.fprintf ppf
                    "@[<v 2>type_extension %a@]@," extension e
            | Value (id, v) ->
                Format.fprintf ppf
                    "@[<v 2>val %a %a@]@," Ident.fmt id value v
            | External (id, e) ->
                Format.fprintf ppf
                    "@[<v 2>external %a %a@]@," Ident.fmt id external_ e
            | Class (id, _,c) ->
                Format.fprintf ppf
                    "@[<v 2>class %a %a@]@," Ident.fmt id class_ c
            | ClassType (id,_,c) ->
                Format.fprintf ppf
                    "@[<v 2>class type %a %a@]@," Ident.fmt id class_type c
            | Include (id, i) ->
                Format.fprintf ppf
                    "@[<v 2>include %a %a@]@," Ident.fmt id include_ i
            | Comment _c ->
                ()
            ) sg.items;
        Format.fprintf ppf "@]"

    and external_ ppf _ =
        Format.fprintf ppf "<todo>"
    
    and class_ ppf _c =
        Format.fprintf ppf "<todo>"
    
    and class_type ppf _c =
        Format.fprintf ppf "<todo>"
    
    and include_ ppf _i =
        Format.fprintf ppf "<todo>"

    and value ppf v =
        let open Value in
        Format.fprintf ppf ": %a" type_expr v.type_

    and module_decl ppf d =
        let open Module in
        match d with
        | Alias p ->
            Format.fprintf ppf "= %a" path p
        | ModuleType mt ->
            Format.fprintf ppf ": %a" module_type_expr mt

    and module_ ppf m =
        Format.fprintf ppf "%a" module_decl m.type_

    and module_type ppf mt =
        match mt.expr with
        | Some x -> Format.fprintf ppf "= %a" module_type_expr x
        | None -> ()

    and module_type_expr ppf mt =
        let open ModuleType in
        match mt with
        | Path p -> path ppf p
        | Signature sg -> Format.fprintf ppf "sig@,@[<v 2>%a@]end" signature sg
        | With (expr,subs) -> Format.fprintf ppf "%a with [%a]" module_type_expr expr substitution_list subs
        | Functor (arg, res) -> Format.fprintf ppf "(%a) -> %a" functor_argument_opt arg module_type_expr res
        | TypeOf decl -> Format.fprintf ppf "module type of %a" module_decl decl

    and functor_argument_opt ppf x =
        match x with
        | None -> ()
        | Some x -> Format.fprintf ppf "%a" functor_argument x

    and functor_argument ppf x =
        Format.fprintf ppf "%a : %a" Ident.fmt x.FunctorArgument.id module_type_expr x.FunctorArgument.expr

    and type_decl ppf t =
        match TypeDecl.(t.equation.Equation.manifest) with
        | Some x -> Format.fprintf ppf "= %a" type_expr x
        | None -> ()

    and type_equation ppf t =
        match t.TypeDecl.Equation.manifest with
        | None -> ()
        | Some m -> Format.fprintf ppf " = %a" type_expr m

    and type_equation2 ppf t =
        match t.TypeDecl.Equation.manifest with
        | None -> ()
        | Some m -> Format.fprintf ppf " = %a" type_expr m

    and exception_ _ppf _e =
        ()

    and extension ppf e =
        Format.fprintf ppf "%a" path e.Extension.type_path

    and substitution ppf t =
        let open ModuleType in
        match t with
        | ModuleEq (frag, decl) ->
            Format.fprintf ppf "%a = %a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) module_decl decl
        | ModuleSubst (frag, mpath) ->
            Format.fprintf ppf "%a := %a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) path mpath
        | TypeEq (frag, decl) ->
            Format.fprintf ppf "%a%a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) type_equation decl
        | TypeSubst (frag, decl) ->
            Format.fprintf ppf "%a%a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) type_equation2 decl


    and substitution_list ppf l =
        match l with
        | sub :: (_ :: _) as subs -> Format.fprintf ppf "%a; %a" substitution sub substitution_list subs
        | sub :: [] -> Format.fprintf ppf "%a" substitution sub
        | [] -> ()

    and type_expr_list ppf l =
        match l with
        | t :: (_ :: _) as ts -> Format.fprintf ppf "%a * %a" type_expr t type_expr_list ts
        | t :: [] -> Format.fprintf ppf "%a" type_expr t
        | [] -> ()

    and type_object ppf _o =
        Format.fprintf ppf "(object)"
    
    and type_class ppf (_x,_y) =
        Format.fprintf ppf "(class)"
    
    and type_package ppf _p =
        Format.fprintf ppf "(package)"

    and type_expr ppf e =
        let open TypeExpr in
        match e with
        | Var x -> Format.fprintf ppf "%s" x
        | Any -> Format.fprintf ppf "_"
        | Alias (x,y) -> Format.fprintf ppf "(alias %a %s)" type_expr x y
        | Arrow (_l, t1, t2) -> Format.fprintf ppf "%a -> %a" type_expr t1 type_expr t2
        | Tuple ts -> Format.fprintf ppf "(%a)" type_expr_list ts
        | Constr (p,_args) -> path ppf p
        | Polymorphic_variant _poly -> Format.fprintf ppf "(poly_var)"
        | Object x -> type_object ppf x
        | Class (x,y) -> type_class ppf (x,y)
        | Poly (_ss,_t) -> Format.fprintf ppf "(poly)" 
        | Package x -> type_package ppf x
    
    and resolved_path : Format.formatter -> Cpath.resolved -> unit = fun ppf p ->
        match p with
        | `Local ident -> Format.fprintf ppf "%a" Ident.fmt ident
        | `Apply (p1, p2) -> Format.fprintf ppf "%a(%a)" resolved_path p1 path p2
        | `Identifier p -> Format.fprintf ppf "global(%a)" model_identifier p
        | `Substituted p -> Format.fprintf ppf "substituted(%a)" resolved_path p
        | `Module (p, m) -> Format.fprintf ppf "%a.%s" resolved_path p (Odoc_model.Names.ModuleName.to_string m)
        | `ModuleType (p, mt) -> Format.fprintf ppf "%a.%s" resolved_path p (Odoc_model.Names.ModuleTypeName.to_string mt)
        | `Type (p, t) -> Format.fprintf ppf "%a.%s" resolved_path p (Odoc_model.Names.TypeName.to_string t)
        | `Alias (p1, p2) -> Format.fprintf ppf "(alias %a -> %a)" resolved_path p1 resolved_path p2
        | `Subst (p1, p2) -> Format.fprintf ppf "(subst %a -> %a)" resolved_path p1 resolved_path p2
        | `SubstAlias (p1, p2) -> Format.fprintf ppf "(substalias %a -> %a)" resolved_path p1 resolved_path p2
        | `Hidden p1 -> Format.fprintf ppf "(hidden %a)" resolved_path p1
        | `Canonical (p1, p2) -> Format.fprintf ppf "(canonical %a -> %a)" resolved_path p1 path p2
        | `Class (p, t) -> Format.fprintf ppf "%a.%s" resolved_path p (Odoc_model.Names.ClassName.to_string t)
        | `ClassType (p, t) -> Format.fprintf ppf "%a.%s" resolved_path p (Odoc_model.Names.ClassTypeName.to_string t)
        

    and path ppf p =
        match p with
        | `Resolved p -> Format.fprintf ppf "%a" resolved_path p
        | `Dot (p,str) -> Format.fprintf ppf "%a.%s" path p str
        | `Apply (p1, p2) -> Format.fprintf ppf "%a(%a)" path p1 path p2
        | `Substituted p -> Format.fprintf ppf "substituted(%a)" path p
        | `Forward s -> Format.fprintf ppf "forward(%s)" s
        | `Root r -> Format.fprintf ppf "%s" r

    and model_path : Format.formatter -> Odoc_model.Paths.Path.t -> unit =
        fun ppf (p : Odoc_model.Paths.Path.t) ->
        match p with
        | `Resolved rp -> Format.fprintf ppf "resolved(%a)" model_resolved_path rp
        | `Root s -> Format.fprintf ppf "*%s" s
        | `Forward s -> Format.fprintf ppf "*%s" s
        | `Dot (parent,s) -> Format.fprintf ppf "*%a.%s" model_path (parent :> Odoc_model.Paths.Path.t) s
        | `Apply (func,arg) -> Format.fprintf ppf "*%a(%a)" model_path (func :> Odoc_model.Paths.Path.t) model_path (arg :> Odoc_model.Paths.Path.t)

    and model_resolved_path ppf (p : Odoc_model.Paths.Path.Resolved.t) =
        let open Odoc_model.Paths.Path.Resolved in
        match p with
        | `Identifier id -> Format.fprintf ppf "(%a)" model_identifier id
        | `Module (parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> t) (Odoc_model.Names.ModuleName.to_string name)
        | `ModuleType (parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> t) (Odoc_model.Names.ModuleTypeName.to_string name)
        | `Type (parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> t) (Odoc_model.Names.TypeName.to_string name)
        | `Alias (path, realpath) -> Format.fprintf ppf "alias(%a,%a)" model_resolved_path (path :> t) model_resolved_path (realpath :> t)
        | `Subst (modty, m) -> Format.fprintf ppf "subst(%a,%a)" model_resolved_path (modty :> t) model_resolved_path (m :> t)
        | `Apply (funct, arg) -> Format.fprintf ppf "%a(%a)" model_resolved_path (funct :> t) model_path (arg :> Odoc_model.Paths.Path.t)
        | `Canonical (p1,p2) -> Format.fprintf ppf "canonical(%a,%a)" model_resolved_path (p1 :> t) model_path (p2 :> Odoc_model.Paths.Path.t)
        | `Hidden(p) -> Format.fprintf ppf "hidden(%a)" model_resolved_path (p :> t)
        | `SubstAlias(_,_) -> Format.fprintf ppf "UNIMPLEMENTED substalias in model_resolved_path"
        | `Class(parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> t) (Odoc_model.Names.ClassName.to_string name)
        | `ClassType(parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> t) (Odoc_model.Names.ClassTypeName.to_string name)

    and model_identifier ppf (p : Odoc_model.Paths.Identifier.t) =
        match p with
        | `Root (_, unit_name) -> Format.fprintf ppf "%s" (Odoc_model.Names.UnitName.to_string unit_name)
        | `Module (`Root _, name) -> Format.fprintf ppf "%s" (Odoc_model.Names.ModuleName.to_string name)
        | `ModuleType (`Root _, name) -> Format.fprintf ppf "%s" (Odoc_model.Names.ModuleTypeName.to_string name)
        | `Type (`Root _, name) -> Format.fprintf ppf "%s" (Odoc_model.Names.TypeName.to_string name)
        | `Module (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Odoc_model.Paths.Identifier.t) (Odoc_model.Names.ModuleName.to_string name)
        | `ModuleType (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Odoc_model.Paths.Identifier.t) (Odoc_model.Names.ModuleTypeName.to_string name)
        | `Type (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Odoc_model.Paths.Identifier.t) (Odoc_model.Names.TypeName.to_string name)
        | `Parameter (parent, name) -> Format.fprintf ppf "(param %a %s)" model_identifier (parent :> Odoc_model.Paths.Identifier.t) (Odoc_model.Names.ParameterName.to_string name)
        | `Result parent -> Format.fprintf ppf "%a.result" model_identifier (parent :> Odoc_model.Paths.Identifier.t)
        | `CoreType name -> Format.fprintf ppf "%s" (Odoc_model.Names.TypeName.to_string name)
        | _ -> Format.fprintf ppf "UNIMPLEMENTED model_identifier"

    and model_fragment ppf (f : Odoc_model.Paths.Fragment.t) =
        match f with
        | `Resolved rf -> model_resolved_fragment ppf rf
        | `Dot (sg, d) -> Format.fprintf ppf "*%a.%s" model_fragment (sg :> Odoc_model.Paths.Fragment.t) d

    and model_resolved_fragment ppf (f : Odoc_model.Paths.Fragment.Resolved.t) =
        let open Odoc_model.Paths.Fragment.Resolved in
        match f with
        | `Root -> ()
        | `Module (sg, m) -> Format.fprintf ppf "%a.%s" model_resolved_fragment (sg :> t) (Odoc_model.Names.ModuleName.to_string m)
        | `Type (sg, t) -> Format.fprintf ppf "%a.%s" model_resolved_fragment (sg :> t) (Odoc_model.Names.TypeName.to_string t)
        | `Subst (path, m) -> Format.fprintf ppf "(%a subst -> %a)" model_resolved_path (path :> Odoc_model.Paths.Path.Resolved.t) model_resolved_fragment (m :> t)
        | `SubstAlias (_,_) -> Format.fprintf ppf "UNIMPLEMENTED subst alias!?"
        | `Class (sg, c) -> Format.fprintf ppf "%a.%s" model_resolved_fragment (sg :> t) (Odoc_model.Names.ClassName.to_string c)
        | `ClassType (sg, c) -> Format.fprintf ppf "%a.%s" model_resolved_fragment (sg :> t) (Odoc_model.Names.ClassTypeName.to_string c)
end

module LocalIdents = struct

end


module Of_Lang = struct
    let ident_of_identifier ident_map identifier =
        List.assoc_opt identifier ident_map

    let option conv ident_map x =
        match x with
        | None -> None
        | Some x' -> Some (conv ident_map x')

    let rec local_resolved_path_of_resolved_path : _ -> Odoc_model.Paths.Path.Resolved.t -> Cpath.resolved = fun ident_map path ->
        let recurse p = local_resolved_path_of_resolved_path ident_map (p :> Odoc_model.Paths.Path.Resolved.t) in
        match path with
        | `Identifier i -> begin
            match ident_of_identifier ident_map i with
            | Some ident ->
                `Local ident
            | None ->
                `Identifier i
            end
        | `Module (p, name) ->
            `Module (recurse p, name)
        | `ModuleType (p, name) ->
            `ModuleType (recurse p, name)
        | `Type (p, name) ->
            `Type (recurse p, name)
        | `Apply (p1, p2) ->
            `Apply (recurse p1, local_path_of_path ident_map (p2 :> Odoc_model.Paths.Path.t))
        | `Alias (p1, p2) ->
            `Alias (recurse p1, recurse p2)
        | `Subst (p1, p2) ->
            `Subst (recurse p1, recurse p2)
        | `SubstAlias (p1, p2) ->
            `SubstAlias (recurse p1, recurse p2)
        | `Canonical (p1, p2) ->
            `Canonical (recurse p1, local_path_of_path ident_map (p2 :> Odoc_model.Paths.Path.t))
        | `Hidden p1 ->
            `Hidden (recurse p1)
        | _ -> failwith "local_resolved_path_of_resolved_path"

    and local_path_of_path : _ -> Odoc_model.Paths.Path.t -> Cpath.t = fun ident_map path ->
        match path with
        | `Resolved r ->
            `Resolved (local_resolved_path_of_resolved_path ident_map r)
        | `Dot (path', x) ->
            `Dot (local_path_of_path ident_map (path' :> Odoc_model.Paths.Path.t), x)
        | `Apply (p1, p2) ->
            `Apply (local_path_of_path ident_map (p1 :> Odoc_model.Paths.Path.t),
                    (local_path_of_path ident_map (p2 :> Odoc_model.Paths.Path.t)))
        | `Forward str ->
            `Forward str
        | `Root str ->
            `Root str

    let rec type_decl ident_map ty =
        let open Odoc_model.Lang.TypeDecl in
        { TypeDecl.doc = ty.doc
        ; equation = type_equation ident_map ty.equation }

    and type_equation ident_map teq =
        let open Odoc_model.Lang.TypeDecl.Equation in
        { TypeDecl.Equation.params = teq.params
        ; private_ = teq.private_
        ; manifest = option type_expression ident_map teq.manifest
        ; constraints = List.map (fun (x, y) -> (type_expression ident_map x, type_expression ident_map y)) teq.constraints }

    and type_expr_polyvar ident_map v =
        let open Odoc_model.Lang.TypeExpr.Polymorphic_variant in
        let map_element = function
        | Type expr -> TypeExpr.Polymorphic_variant.Type (type_expression ident_map expr)
        | Constructor c ->
            Constructor TypeExpr.Polymorphic_variant.Constructor.{
                name = c.name;
                constant = c.constant;
                arguments = List.map (type_expression ident_map) c.arguments;
                doc = c.doc
            }
        in
        { TypeExpr.Polymorphic_variant.kind = v.kind
        ; elements = List.map map_element v.elements}

    and type_object ident_map o =
        let open Odoc_model.Lang.TypeExpr.Object in
        let map_field = function
        | Method m -> TypeExpr.(Object.Method {Object.name=m.name; type_ = type_expression ident_map m.type_})
        | Inherit i -> Inherit (type_expression ident_map i)
        in
        { TypeExpr.Object.open_ = o.open_
        ; fields = List.map map_field o.fields}

    and type_package ident_map pkg =
        let open Odoc_model.Lang.TypeExpr.Package in
        { TypeExpr.Package.path = local_path_of_path ident_map (pkg.path :> Odoc_model.Paths.Path.t)
        ; substitutions = List.map (fun (x,y) -> (x, type_expression ident_map y)) pkg.substitutions}

    and type_expression ident_map expr =
        let open Odoc_model.Lang.TypeExpr in
        match expr with
        | Var s -> TypeExpr.Var s
        | Any -> Any
        | Constr (path, _) -> Constr (local_path_of_path ident_map (path :> Odoc_model.Paths.Path.t), [])
        | Arrow (lbl, t1, t2) -> Arrow (lbl, type_expression ident_map t1, type_expression ident_map t2)
        | Tuple ts -> Tuple (List.map (type_expression ident_map) ts)
        | Polymorphic_variant v -> Polymorphic_variant (type_expr_polyvar ident_map v)
        | Poly (s,ts) -> Poly (s, type_expression ident_map ts)
        | Alias (t, s) -> Alias (type_expression ident_map t, s) 
        | Class _ -> failwith "Unhandled in of_type_equation: Class"
        | Object o -> Object (type_object ident_map o)
        | Package p -> Package (type_package ident_map p)

    and module_decl ident_map m =
        match m with
        | Odoc_model.Lang.Module.Alias p ->
            Module.Alias (local_path_of_path ident_map (p :> Odoc_model.Paths.Path.t))
        | Odoc_model.Lang.Module.ModuleType s ->
            Module.ModuleType (module_type_expr ident_map s)

    and canonical ident_map ( canonical : (Odoc_model.Paths.Path.Module.t * Odoc_model.Paths.Reference.Module.t) option) =
        match canonical with
        | Some (path, r) -> Some (local_path_of_path ident_map (path :> Odoc_model.Paths.Path.t), r)
        | None -> None

    and module_ ident_map m =
        let type_ = module_decl ident_map m.Odoc_model.Lang.Module.type_ in
        let canonical = canonical ident_map m.Odoc_model.Lang.Module.canonical in
        let display_type = Opt.map (module_decl ident_map) m.Odoc_model.Lang.Module.display_type in
        {Module.doc = m.doc; type_; canonical; hidden=m.hidden; display_type}

    and module_type_substitution ident_map m =
        let open Odoc_model.Lang.ModuleType in
        match m with
        | ModuleEq (frag, decl) ->
            ModuleType.ModuleEq (frag, module_decl ident_map decl)
        | ModuleSubst (frag, path) ->
            ModuleType.ModuleSubst (frag, local_path_of_path ident_map (path :> Odoc_model.Paths.Path.t))
        | TypeEq (frag, eqn) ->
            ModuleType.TypeEq (frag, type_equation ident_map eqn)
        | TypeSubst (frag, eqn) ->
            ModuleType.TypeSubst (frag, type_equation ident_map eqn)

    and functor_argument ident_map id a =
        let expr' = module_type_expr ident_map a.Odoc_model.Lang.FunctorArgument.expr in
        { FunctorArgument.id
        ; expr = expr' }

    and module_type_expr ident_map m =
        match m with
        | Odoc_model.Lang.ModuleType.Signature s ->
            let s = signature ident_map s in
            ModuleType.Signature s
        | Odoc_model.Lang.ModuleType.Path p ->
            let p' = local_path_of_path ident_map (p :> Odoc_model.Paths.Path.t) in
            ModuleType.Path p'
        | Odoc_model.Lang.ModuleType.With (e, subs) ->
            ModuleType.With (module_type_expr ident_map e,
                List.map (module_type_substitution ident_map) subs)
        | Odoc_model.Lang.ModuleType.Functor (Some arg, expr) ->
            let open Odoc_model.Paths in
            let identifier = arg.Odoc_model.Lang.FunctorArgument.id in
            let id = Ident.of_identifier (identifier :> Identifier.t) in
            let ident_map' = ((identifier :> Identifier.t), id) :: ident_map in
            let arg' = functor_argument ident_map' id arg in
            let expr' = module_type_expr ident_map' expr in
            ModuleType.Functor (Some arg', expr')
        | Odoc_model.Lang.ModuleType.Functor (None, expr) ->
            let expr' = module_type_expr ident_map expr in
            ModuleType.Functor (None, expr')
        | Odoc_model.Lang.ModuleType.TypeOf decl ->
            let decl' = module_decl ident_map decl in
            ModuleType.TypeOf decl'


    and module_type ident_map m =
        let expr = Opt.map (module_type_expr ident_map) m.Odoc_model.Lang.ModuleType.expr in
        {ModuleType.doc = m.doc; expr}

    and value ident_map id v =
        let type_ = type_expression ident_map v.Odoc_model.Lang.Value.type_ in
        { Value.id
        ; type_ 
        ; doc = v.doc }

    and signature : _ -> Odoc_model.Lang.Signature.t -> Signature.t =
        let open Odoc_model.Paths in
        fun ident_map items ->
            (* First we construct a list of brand new [Ident.t]s
                for each item in the signature *)
            let ident_map_new : (Identifier.t * Ident.t) list =
                let open Odoc_model.Lang.Signature in
                List.fold_left (fun acc item ->
                    match item with
                    | Type (_, t) ->
                        let identifier = t.Odoc_model.Lang.TypeDecl.id in
                        let id = Ident.of_identifier (identifier :> Identifier.t) in
                        ((identifier :> Identifier.t), id)::acc
                    | Module (_, m) ->
                        let identifier = m.Odoc_model.Lang.Module.id in
                        let id = Ident.of_identifier (identifier :> Identifier.t) in
                        ((identifier :> Identifier.t), id)::acc
                    | ModuleType m ->
                        let identifier = m.Odoc_model.Lang.ModuleType.id in
                        let id = Ident.of_identifier (identifier :> Identifier.t) in
                        ((identifier :> Identifier.t), id)::acc
                    | TypExt _ ->
                        acc
                    | Exception e ->
                        let identifier = e.Odoc_model.Lang.Exception.id in
                        let id = Ident.of_identifier (identifier :> Identifier.t) in
                        ((identifier :> Identifier.t), id)::acc
                    | Value v ->
                        let identifier = v.Odoc_model.Lang.Value.id in
                        let id = Ident.of_identifier (identifier :> Identifier.t) in
                        ((identifier :> Identifier.t), id)::acc
                    | External e ->
                        let identifier = e.Odoc_model.Lang.External.id in
                        let id = Ident.of_identifier (identifier :> Identifier.t) in
                        ((identifier :> Identifier.t), id)::acc
                    | Class (_,c) ->
                        let identifier = c.id in
                        let id = Ident.of_identifier (identifier :> Identifier.t) in
                        ((identifier :> Identifier.t), id)::acc
                    | ClassType (_,c) ->
                        let identifier = c.Odoc_model.Lang.ClassType.id in
                        let id = Ident.of_identifier (identifier :> Identifier.t) in
                        ((identifier :> Identifier.t), id)::acc
                    | Include _ ->
                        acc
                    | Comment _ ->
                        acc) [] items
            in

            let ident_map = ident_map_new @ ident_map in
            (* Now we construct the Components for each item,
                converting all paths containing Identifiers pointing at
                our elements to local paths *)
            let items = List.map (
                let open Odoc_model.Lang.Signature in
                function
                |  Type (r, t) ->
                    let id = List.assoc (t.id :> Identifier.t) ident_map in 
                    let t' = type_decl ident_map t in
                    Signature.Type (id, r, t')
                | Module (r, m) ->
                    let id = List.assoc (m.id :> Identifier.t) ident_map in 
                    let m' = Delayed.put (fun () -> module_ ident_map m) in
                    Signature.Module (id,r,m')
                | ModuleType m ->
                    let id = List.assoc (m.id :> Identifier.t) ident_map in 
                    let m' = module_type ident_map m in
                    Signature.ModuleType (id,m')
                | Value v ->
                    let id = List.assoc (v.id :> Identifier.t) ident_map in
                    let v' = value ident_map id v in
                    Signature.Value v'
                | Comment c ->
                    Signature.Comment c
                | TypExt _ ->
                    failwith "Unhandled typext in of_signature"
                | Exception _ ->
                    failwith "Unhandled exception in of_signature"
                | External _ ->
                    failwith "Unhandled external in of_signature"
                | Class (_,_) ->
                    failwith "Unhandled class in of_signature"
                | ClassType (_,_) ->
                    failwith "Unhandled classtype in of_signature"
                | Include _ ->
                    failwith "Unhandled include in of_signature"
                ) items
            in
            { items; removed=[] }

end

module Find = struct

exception Find_failure of Signature.t * string * string

let fail sg name ty = raise (Find_failure (sg, name, ty))

type 'a found =
    | Found of 'a
    | Replaced of Cpath.resolved

let careful_module_in_sig s name =
    let rec inner_removed = function
      | Signature.RModule (id, Some p) :: _ when Ident.name id = name -> Replaced p
      | _::rest -> inner_removed rest
      | [] -> fail s name "module" in
    let rec inner = function
      | Signature.Module (id,_,m) :: _ when Ident.name id = name -> Found (Delayed.get m)
      | _::rest -> inner rest
      | [] -> inner_removed s.removed
    in inner s.items

let careful_type_in_sig s name =
    let rec inner_removed = function
      | Signature.RType (id, Some p) :: _ when Ident.name id = name -> Replaced p
      | _::rest -> inner_removed rest
      | [] -> fail s name "type" in
    let rec inner = function
      | Signature.Type (id,_,m) :: _ when Ident.name id = name -> Found m
      | _::rest -> inner rest
      | [] -> inner_removed s.removed
    in inner s.items

let module_in_sig s name =
    match careful_module_in_sig s name with
    | Found m -> m
    | Replaced _ -> fail s name "module"

let module_type_in_sig s name =
    let rec inner = function
        | Signature.ModuleType (id,m) :: _ when Ident.name id = name -> m
        | _::rest -> inner rest
        | [] -> fail s name "module type"
    in inner s.items

let type_in_sig s name =
    match careful_type_in_sig s name with
    | Found t -> t
    | Replaced _ -> fail s name "type"
end

