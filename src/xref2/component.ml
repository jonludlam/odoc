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
        | Include of Include.t
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
        { parent: Odoc_model.Paths.Identifier.Signature.t
        ; doc: Comment.docs
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
            | Include (i) ->
                Format.fprintf ppf
                    "@[<v 2>include %a@]@," include_ i
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
        | `Constructor (ty,x) -> Format.fprintf ppf "%a.%s" model_identifier (ty :> Odoc_model.Paths.Identifier.t) x
        | `Value (parent,name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Odoc_model.Paths.Identifier.t) name
        | `CoreException name -> Format.fprintf ppf "%s" name
        | `Class (sg, name) -> Format.fprintf ppf "%a.%s" model_identifier (sg :> Odoc_model.Paths.Identifier.t) name
        | `ClassType (sg, name) -> Format.fprintf ppf "%a.%s" model_identifier (sg :> Odoc_model.Paths.Identifier.t) name
        | `InstanceVariable (sg, name) -> Format.fprintf ppf "%a.%s" model_identifier (sg :> Odoc_model.Paths.Identifier.t) name
        | `Method (sg, name) -> Format.fprintf ppf "%a.%s" model_identifier (sg :> Odoc_model.Paths.Identifier.t) name
        | `Label (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Odoc_model.Paths.Identifier.t) name
        | `Field (ty, name) -> Format.fprintf ppf "%a.%s" model_identifier (ty :> Odoc_model.Paths.Identifier.t) name
        | `Exception (p, name) -> Format.fprintf ppf "%a.%s" model_identifier (p :> Odoc_model.Paths.Identifier.t) name
        | `Extension (p, name) -> Format.fprintf ppf "%a.%s" model_identifier (p :> Odoc_model.Paths.Identifier.t) name
        | `Page (_, name) -> Format.fprintf ppf "%s" name

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
    open Odoc_model

    type t =
        { modules : Paths.Identifier.Module.t list
        ; module_types : Paths.Identifier.ModuleType.t list
        ; fields : Paths.Identifier.Field.t list
        ; constructors : Paths.Identifier.Constructor.t list
        ; types : Paths.Identifier.Type.t list
        ; extensions : Paths.Identifier.Extension.t list
        ; exceptions : Paths.Identifier.Exception.t list
        ; values : Paths.Identifier.Value.t list
        ; classes : Paths.Identifier.Class.t list
        ; class_types : Paths.Identifier.ClassType.t list
        ; methods : Paths.Identifier.Method.t list
        ; instance_variables : Paths.Identifier.InstanceVariable.t list
    }

    let empty =
        { modules = []
        ; module_types = []
        ; fields = []
        ; constructors = []
        ; types = []
        ; extensions = []
        ; exceptions = []
        ; values = []
        ; classes = []
        ; class_types = []
        ; methods = []
        ; instance_variables = []
        }

    open Lang

    let opt conv opt ids =
        match opt with
        | Some x -> conv x ids
        | None -> ids

    let rec module_ m ids =
        { ids with modules = m.Module.id :: ids.modules }

    and module_type m ids =
        { ids with module_types = m.ModuleType.id :: ids.module_types }

    and type_decl t ids =
        let ids = opt type_decl_representation t.TypeDecl.representation ids in
        { ids with types = t.TypeDecl.id :: ids.types }

    and type_decl_representation r ids =
        match r with
        | TypeDecl.Representation.Extensible -> ids
        | Record fs -> List.fold_right type_decl_field fs ids
        | Variant cs -> List.fold_right type_decl_constructor cs ids

    and type_decl_field f ids =
        { ids with fields = f.TypeDecl.Field.id :: ids.fields }

    and type_decl_constructor c ids =
        { ids with constructors = c.TypeDecl.Constructor.id :: ids.constructors }

    and extension e ids =
        List.fold_right extension_constructor e.Extension.constructors ids
    
    and extension_constructor c ids =
        { ids with extensions = c.Extension.Constructor.id :: ids.extensions }
    
    and exception_ e ids =
        { ids with exceptions = e.Exception.id :: ids.exceptions }
    
    and value_ v ids =
        { ids with values = v.Value.id :: ids.values }
    
    and external_ e ids =
        { ids with values = e.External.id :: ids.values }
    
    and class_ _c ids = ids

    and class_type _c ids = ids

    and signature s ids =
        List.fold_right (fun c ids ->
            match c with
            | Signature.Module (_,m) -> module_ m ids
            | ModuleType m -> module_type m ids
            | Type (_,t) -> type_decl t ids
            | TypExt ext -> extension ext ids
            | Exception e -> exception_ e ids
            | Value v -> value_ v ids
            | External e -> external_ e ids
            | Class (_,c) -> class_ c ids
            | ClassType (_,c) -> class_type c ids
            | Include _ -> ids
            | Comment _ -> ids
            ) s ids
end


let core_type_ids =
    let open Odoc_model.Predefined in
    List.map (fun id -> (id, Ident.of_identifier (id :> Odoc_model.Paths.Identifier.t))) [
        bool_identifier; int_identifier; char_identifier; bytes_identifier;
        string_identifier; float_identifier; unit_identifier; exn_identifier;
        array_identifier; list_identifier; option_identifier; int32_identifier;
        int64_identifier; nativeint_identifier; lazy_t_identifier; extension_constructor_identifier;
        floatarray_identifier;
    ]

let core_constructors =
    let open Odoc_model.Predefined in
    List.map (fun id -> (id, Ident.of_identifier (id :> Odoc_model.Paths.Identifier.t))) [
        false_identifier; true_identifier; void_identifier; nil_identifier;
        cons_identifier; none_identifier; some_identifier
    ]

let core_exceptions =
    let open Odoc_model.Predefined in
    List.map (fun id -> (id, Ident.of_identifier (id :> Odoc_model.Paths.Identifier.t))) [
        match_failure_identifier; assert_failure_identifier; invalid_argument_identifier;
        failure_identifier; not_found_identifier; out_of_memory_identifier;
        stack_overflow_identifier; sys_error_identifier; end_of_file_identifier;
        division_by_zero_identifier; sys_blocked_io_identifier; undefined_recursive_module_identifier        
    ]


module Of_Lang = struct
    open Odoc_model

    type map =
        { modules : (Paths.Identifier.Module.t * Ident.t) list
        ; module_types : (Paths.Identifier.ModuleType.t * Ident.t) list
        ; fields : (Paths.Identifier.Field.t * Ident.t) list
        ; constructors : (Paths.Identifier.Constructor.t * Ident.t) list
        ; types : (Paths.Identifier.Type.t * Ident.t) list
        ; path_types : (Paths_types.Identifier.path_type * Ident.t) list
        ; path_class_types : (Paths_types.Identifier.path_class_type * Ident.t) list
        ; extensions : (Paths.Identifier.Extension.t * Ident.t) list
        ; exceptions : (Paths.Identifier.Exception.t * Ident.t) list
        ; values : (Paths.Identifier.Value.t * Ident.t) list
        ; classes : (Paths.Identifier.Class.t * Ident.t) list
        ; class_types : (Paths.Identifier.ClassType.t * Ident.t) list
        ; methods : (Paths.Identifier.Method.t * Ident.t) list
        ; instance_variables : (Paths.Identifier.InstanceVariable.t * Ident.t) list }

    let empty = 
        { modules = []
        ; module_types = []
        ; fields = []
        ; constructors = core_constructors
        ; types = core_type_ids
        ; path_types = []
        ; path_class_types = []
        ; extensions = []
        ; exceptions = core_exceptions
        ; values = []
        ; classes = []
        ; class_types = []
        ; methods = []
        ; instance_variables = []
        }

    let map_of_idents ids map =
        let open Paths_types.Identifier in
        let types = List.fold_left (fun acc i -> (i, Ident.of_identifier (i :> any)) :: acc) map.types ids.LocalIdents.types in
        let classes = List.fold_left (fun acc i -> (i, Ident.of_identifier (i :> any)) :: acc) map.classes ids.LocalIdents.classes in
        let class_types = List.fold_left (fun acc i -> (i, Ident.of_identifier (i :> any)) :: acc) map.class_types ids.LocalIdents.class_types in
        { modules = List.fold_left (fun acc i -> (i, Ident.of_identifier (i :> any)) :: acc) map.modules ids.LocalIdents.modules
        ; module_types = List.fold_left (fun acc i -> (i, Ident.of_identifier (i :> any)) :: acc) map.module_types ids.LocalIdents.module_types
        ; fields = List.fold_left (fun acc i -> (i, Ident.of_identifier (i :> any)) :: acc) map.fields ids.LocalIdents.fields
        ; constructors = List.fold_left (fun acc i -> (i, Ident.of_identifier (i :> any)) :: acc) map.constructors ids.LocalIdents.constructors
        ; types
        ; extensions = List.fold_left (fun acc i -> (i, Ident.of_identifier (i :> any)) :: acc) map.extensions ids.LocalIdents.extensions
        ; exceptions = List.fold_left (fun acc i -> (i, Ident.of_identifier (i :> any)) :: acc) map.exceptions ids.LocalIdents.exceptions
        ; values = List.fold_left (fun acc i -> (i, Ident.of_identifier (i :> any)) :: acc) map.values ids.LocalIdents.values
        ; classes
        ; class_types
        ; methods = List.fold_left (fun acc i -> (i, Ident.of_identifier (i :> any)) :: acc) map.methods ids.LocalIdents.methods 
        ; instance_variables = List.fold_left (fun acc i -> (i, Ident.of_identifier (i :> any)) :: acc) map.instance_variables ids.LocalIdents.instance_variables
        ; path_types =
            (types :> (path_type * Ident.t) list) @
            (classes :> (path_type * Ident.t) list) @
            (class_types :> (path_type * Ident.t) list) @ map.path_types
        ; path_class_types =
            (classes :> (path_class_type * Ident.t) list) @
            (class_types :> (path_class_type * Ident.t) list) @ map.path_class_types
        }

    let ident_of_identifier ident_map identifier =
        List.assoc_opt identifier ident_map

    let option conv ident_map x =
        match x with
        | None -> None
        | Some x' -> Some (conv ident_map x')

    let optvalue o ~default =
        match o with
        | None -> default
        | Some x -> x
    
    let optmap o f =
        match o with
        | Some x -> Some (f x)
        | None -> None
    
    let identifier map i =
        optvalue
            (optmap (ident_of_identifier map i)
            (fun x -> `Local x))
            ~default:(`Identifier (i :> Paths.Identifier.t))
    
    let rec resolved_module_path : _ -> Odoc_model.Paths.Path.Resolved.Module.t -> Cpath.resolved = fun ident_map p ->
        let recurse p = resolved_module_path ident_map p in
        match p with
        | `Identifier i -> identifier ident_map.modules i
        | `Module (p, name) -> `Module (recurse p, name)
        | `Apply (p1, p2) -> `Apply (recurse p1, module_path ident_map p2)
        | `Alias (p1, p2) -> `Alias (recurse p1, recurse p2)
        | `Subst (p1, p2) -> `Subst (resolved_module_type_path ident_map p1, recurse p2)
        | `SubstAlias (p1, p2) -> `SubstAlias (recurse p1, recurse p2)
        | `Canonical (p1, p2) -> `Canonical (recurse p1, module_path ident_map p2)
        | `Hidden p1 -> `Hidden (recurse p1)

    and resolved_module_type_path : _ -> Odoc_model.Paths.Path.Resolved.ModuleType.t -> Cpath.resolved = fun ident_map p ->
        match p with
        | `Identifier i -> identifier ident_map.module_types i
        | `ModuleType (p, name) -> `ModuleType (resolved_module_path ident_map p, name)

    and resolved_type_path : _ -> Odoc_model.Paths.Path.Resolved.Type.t -> Cpath.resolved = fun ident_map p ->
        match p with
        | `Identifier i -> identifier ident_map.path_types i
        | `Type (p, name) -> `Type (resolved_module_path ident_map p, name)
        | `Class (p, name) -> `Class (resolved_module_path ident_map p, name)
        | `ClassType (p, name) -> `ClassType (resolved_module_path ident_map p, name)

    and resolved_class_type_path : _ -> Odoc_model.Paths.Path.Resolved.ClassType.t -> Cpath.resolved = fun ident_map p ->
        match p with
        | `Identifier i -> identifier ident_map.path_class_types i
        | `Class (p, name) -> `Class (resolved_module_path ident_map p, name)
        | `ClassType (p, name) -> `ClassType (resolved_module_path ident_map p, name)

    and module_path : _ -> Odoc_model.Paths.Path.Module.t -> Cpath.t = fun ident_map p ->
        match p with
        | `Resolved r -> `Resolved (resolved_module_path ident_map r)
        | `Dot (path', x) -> `Dot (module_path ident_map path', x)
        | `Apply (p1, p2) -> `Apply (module_path ident_map p1, module_path ident_map p2)
        | `Forward str -> `Forward str
        | `Root str -> `Root str
    
    and module_type_path : _ -> Odoc_model.Paths.Path.ModuleType.t -> Cpath.t = fun ident_map p ->
        match p with
        | `Resolved r -> `Resolved (resolved_module_type_path ident_map r)
        | `Dot (path', x) -> `Dot (module_path ident_map path', x)

    and type_path : _ -> Odoc_model.Paths.Path.Type.t -> Cpath.t = fun ident_map p ->
        match p with
        | `Resolved r -> `Resolved (resolved_type_path ident_map r)
        | `Dot (path', x) -> `Dot (module_path ident_map path', x)
    
    and class_type_path : _ -> Odoc_model.Paths.Path.ClassType.t -> Cpath.t = fun ident_map p ->
        match p with
        | `Resolved r -> `Resolved (resolved_class_type_path ident_map r)
        | `Dot (path', x) -> `Dot (module_path ident_map path', x)

    let rec type_decl ident_map ty =
        let open Odoc_model.Lang.TypeDecl in
        { TypeDecl.doc = ty.doc
        ; equation = type_equation ident_map ty.equation
        ; representation = Opt.map (type_decl_representation ident_map) ty.representation }

    and type_decl_representation ident_map r =
        let open Odoc_model.Lang.TypeDecl.Representation in
        match r with
        | Variant cs -> TypeDecl.Representation.Variant (List.map (type_decl_constructor ident_map) cs)
        | Record fs -> Record (List.map (type_decl_field ident_map) fs)
        | Extensible -> Extensible
    
    and type_decl_constructor ident_map t =
        let open Odoc_model.Lang.TypeDecl.Constructor in
        let id = try
            List.assoc t.id ident_map.constructors
            with Not_found -> failwith (Format.asprintf "nah! %a" Fmt.model_identifier (t.id :> Paths.Identifier.t)) in
        let args = type_decl_constructor_argument ident_map t.args in
        let res = Opt.map (type_expression ident_map) t.res in
        { TypeDecl.Constructor.id = id
        ; doc = t.doc
        ; args
        ; res }

    and type_decl_constructor_argument ident_map a =
        let open Odoc_model.Lang.TypeDecl.Constructor in 
        match a with
        | Tuple ts -> TypeDecl.Constructor.Tuple (List.map (type_expression ident_map) ts)
        | Record fs -> Record (List.map (type_decl_field ident_map) fs)
    
    and type_decl_field ident_map f =
        let open Odoc_model.Lang.TypeDecl.Field in
        let id = List.assoc f.id ident_map.fields in
        let type_ = type_expression ident_map f.type_ in
        { TypeDecl.Field.id = id
        ; doc = f.doc
        ; mutable_ = f.mutable_
        ; type_ }

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
        { TypeExpr.Package.path = module_type_path ident_map pkg.path
        ; substitutions = List.map (fun (x,y) -> (x, type_expression ident_map y)) pkg.substitutions}

    and type_expression ident_map expr =
        let open Odoc_model.Lang.TypeExpr in
        match expr with
        | Var s -> TypeExpr.Var s
        | Any -> Any
        | Constr (p, _) -> Constr (type_path ident_map p, [])
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
            Module.Alias (module_path ident_map p)
        | Odoc_model.Lang.Module.ModuleType s ->
            Module.ModuleType (module_type_expr ident_map s)

    and canonical ident_map ( canonical : (Odoc_model.Paths.Path.Module.t * Odoc_model.Paths.Reference.Module.t) option) =
        match canonical with
        | Some (p, r) -> Some (module_path ident_map p, r)
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
        | ModuleSubst (frag, p) ->
            ModuleType.ModuleSubst (frag, module_path ident_map p)
        | TypeEq (frag, eqn) ->
            ModuleType.TypeEq (frag, type_equation ident_map eqn)
        | TypeSubst (frag, eqn) ->
            ModuleType.TypeSubst (frag, type_equation ident_map eqn)

    and functor_argument ident_map id a =
        let expr' = module_type_expr ident_map a.Odoc_model.Lang.FunctorArgument.expr in
        { FunctorArgument.id
        ; expr = expr' }

    and extension ident_map e =
        let open Odoc_model.Lang.Extension in
        let type_path = type_path ident_map e.type_path in
        let constructors = List.map (extension_constructor ident_map) e.constructors in
        { Extension.type_path = type_path
        ; doc = e.doc
        ; type_params = e.type_params
        ; private_ = e.private_
        ; constructors }

    and extension_constructor ident_map c =
        let open Odoc_model.Lang.Extension.Constructor in
        let id = List.assoc c.id ident_map.extensions in
        let args = type_decl_constructor_argument ident_map c.args in
        let res = Opt.map (type_expression ident_map) c.res in
        { Extension.Constructor.id = id
        ; doc = c.doc
        ; args
        ; res }
    
    and exception_ ident_map e =
        let open Odoc_model.Lang.Exception in
        let args = type_decl_constructor_argument ident_map e.args in
        let res = Opt.map (type_expression ident_map) e.res in
        { Exception.doc = e.doc
        ; args
        ; res }

    and module_type_expr ident_map m =
        match m with
        | Odoc_model.Lang.ModuleType.Signature s ->
            let s = signature ident_map s in
            ModuleType.Signature s
        | Odoc_model.Lang.ModuleType.Path p ->
            let p' = module_type_path ident_map p in
            ModuleType.Path p'
        | Odoc_model.Lang.ModuleType.With (e, subs) ->
            ModuleType.With (module_type_expr ident_map e,
                List.map (module_type_substitution ident_map) subs)
        | Odoc_model.Lang.ModuleType.Functor (Some arg, expr) ->
            let open Odoc_model.Paths in
            let identifier = arg.Odoc_model.Lang.FunctorArgument.id in
            let id = Ident.of_identifier (identifier :> Identifier.t) in
            let ident_map' = {ident_map with modules = (identifier, id) :: ident_map.modules } in
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

    and value ident_map v =
        let type_ = type_expression ident_map v.Odoc_model.Lang.Value.type_ in
        { Value.type_ 
        ; doc = v.doc }

    and external_ ident_map e =
        let open Odoc_model.Lang.External in
        let type_ = type_expression ident_map e.type_ in 
        { External.doc = e.doc
        ; type_
        ; primitives = e.primitives }

    and include_ ident_map i =
        let open Odoc_model.Lang.Include in
        let decl = module_decl ident_map i.decl in
        { Include.parent = i.parent
        ; doc = i.doc
        ; decl }

    and signature : _ -> Odoc_model.Lang.Signature.t -> Signature.t =
        fun ident_map items ->
            (* First we construct a list of brand new [Ident.t]s
                for each item in the signature *)

            let ident_map = map_of_idents (LocalIdents.signature items LocalIdents.empty) ident_map in
            (* Now we construct the Components for each item,
                converting all paths containing Identifiers pointing at
                our elements to local paths *)
            let items = List.map (
                let open Odoc_model.Lang.Signature in
                function
                |  Type (r, t) ->
                    let id = List.assoc t.id ident_map.types in 
                    let t' = type_decl ident_map t in
                    Signature.Type (id, r, t')
                | Module (r, m) ->
                    let id = List.assoc m.id ident_map.modules in 
                    let m' = Delayed.put (fun () -> module_ ident_map m) in
                    Signature.Module (id,r,m')
                | ModuleType m ->
                    let id = List.assoc m.id ident_map.module_types in 
                    let m' = module_type ident_map m in
                    Signature.ModuleType (id,m')
                | Value v ->
                    let id = List.assoc v.id ident_map.values in
                    let v' = value ident_map v in
                    Signature.Value (id, v')
                | Comment c ->
                    Signature.Comment c
                | TypExt e ->
                    TypExt (extension ident_map e)
                | Exception e ->
                    let id = List.assoc e.id ident_map.exceptions in
                    Exception (id, exception_ ident_map e)
                | External e ->
                    let id = List.assoc e.id ident_map.values in
                    External (id, external_ ident_map e)
                | Class (_,_) ->
                    failwith "Unhandled class in of_signature"
                | ClassType (_,_) ->
                    failwith "Unhandled classtype in of_signature"
                | Include _i ->
                    failwith "Unhandled classtype in of_signature"
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

