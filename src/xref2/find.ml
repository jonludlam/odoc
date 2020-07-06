open Component

type module_ = [ `M of Module.t ]

type module_type = [ `MT of ModuleType.t ]

type datatype = [ `T of TypeDecl.t ]

type class_ = [ `C of Class.t | `CT of ClassType.t ]

type value = [ `E of External.t | `V of Value.t ]

type label = [ `L of Ident.label ]

type exception_ = [ `Exn of Exception.t ]

type extension = [ `Ext of Extension.t * Extension.Constructor.t ]

type substitution = [ `Msub of ModuleSubstitution.t | `Tsub of TypeDecl.t ]

type signature = [ module_ | module_type ]

type type_ = [ datatype | class_ ]

type label_parent = [ signature | type_ ]

type constructor = [ `Cs of TypeDecl.Constructor.t ]

type field = [ `Fd of TypeDecl.Field.t ]

type any_in_type = [ constructor | field ]

type any_in_type_in_sig =
  [ `In_type of Odoc_model.Names.TypeName.t * TypeDecl.t * any_in_type ]

type any_in_sig =
  [ label_parent
  | value
  | label
  | exception_
  | extension
  | substitution
  | any_in_type_in_sig ]

type instance_variable = [ `Mv of InstanceVariable.t ]

type method_ = [ `Mm of Method.t ]

type any_in_class_sig = [ instance_variable | method_ ]

module N = Ident.Name

let rec filter_map f = function
  | hd :: tl -> ( match f hd with Some _ as x -> x | None -> filter_map f tl )
  | [] -> None

let find_in_sig sg f =
  let rec inner f = function
    | Signature.Include i :: tl -> (
        match inner f i.Include.expansion_.items with
        | Some _ as x -> x
        | None -> inner f tl )
    | hd :: tl -> ( match f hd with Some _ as x -> x | None -> inner f tl )
    | [] -> None
  in
  inner f sg.Signature.items

let module_in_sig sg name =
  find_in_sig sg (function
    | Signature.Module (id, _, m) when N.module_ id = name ->
        Some (`M (Delayed.get m))
    | _ -> None)

let type_in_sig sg name =
  find_in_sig sg (function
    | Signature.Type (id, _, m) when N.type_ id = name ->
        Some (`T (Delayed.get m))
    | Class (id, _, c) when N.class_ id = name -> Some (`C c)
    | ClassType (id, _, c) when N.class_type id = name -> Some (`CT c)
    | _ -> None)

type careful_module = [ module_ | `M_removed of Cpath.Resolved.module_ ]

type careful_type = [ type_ | `T_removed of TypeExpr.t ]

let careful_module_in_sig sg name =
  let removed_module = function
    | Signature.RModule (id, p) when N.module_ id = name ->
        Some (`M_removed p)
    | _ -> None
  in
  match module_in_sig sg name with
  | Some _ as x -> x
  | None -> filter_map removed_module sg.Signature.removed

let careful_type_in_sig sg name =
  let removed_type = function
    | Signature.RType (id, p) when N.type_ id = name -> Some (`T_removed p)
    | _ -> None
  in
  match type_in_sig sg name with
  | Some _ as x -> x
  | None -> filter_map removed_type sg.Signature.removed

let datatype_in_sig sg name =
  find_in_sig sg (function
    | Signature.Type (id, _, t) when N.type_ id = name ->
        Some (`T (Component.Delayed.get t))
    | _ -> None)

let class_in_sig sg name =
  find_in_sig sg (function
    | Signature.Class (id, _, c) when N.class_ id = name -> Some (`C c)
    | Signature.ClassType (id, _, c) when N.class_type id = name -> Some (`CT c)
    | _ -> None)

let typename_of_typeid (`LType (n, _) | `LCoreType n) = n

let any_in_type (typ : TypeDecl.t) name =
  let rec find_cons = function
    | ({ TypeDecl.Constructor.name = name'; _ } as cons) :: _ when name' = name
      ->
        Some (`Cs cons)
    | _ :: tl -> find_cons tl
    | [] -> None
  in
  let rec find_field = function
    | ({ TypeDecl.Field.name = name'; _ } as field) :: _ when name' = name ->
        Some (`Fd field)
    | _ :: tl -> find_field tl
    | [] -> None
  in
  match typ.representation with
  | Some (Variant cons) -> find_cons cons
  | Some (Record fields) -> find_field fields
  | Some Extensible | None -> None

let any_in_typext (typext : Extension.t) name =
  let rec inner = function
    | ({ Extension.Constructor.name = name'; _ } as cons) :: _ when name' = name
      ->
        Some (`Ext (typext, cons))
    | _ :: tl -> inner tl
    | [] -> None
  in
  inner typext.constructors

let any_in_comment d name =
  let rec inner xs =
    match xs with
    | elt :: rest -> (
        match elt.Odoc_model.Location_.value with
        | `Heading (_, label, _) when Ident.Name.label label = name ->
            Some (`L label)
        | _ -> inner rest )
    | [] -> None
  in
  inner d

let any_in_sig (s : Signature.t) name =
  let rec inner = function
    | Signature.Module (id, _, m) :: _ when N.module_ id = name ->
        Some (`M (Delayed.get m))
    | ModuleSubstitution (id, ms) :: _ when N.module_ id = name ->
        Some (`Msub ms)
    | ModuleType (id, mt) :: _ when N.module_type id = name ->
        Some (`MT (Delayed.get mt))
    | Type (id, _, t) :: _ when N.type_ id = name -> Some (`T (Delayed.get t))
    | TypeSubstitution (id, ts) :: _ when N.type_ id = name -> Some (`Tsub ts)
    | Exception (id, exc) :: _ when N.exception_ id = name -> Some (`Exn exc)
    | Value (id, v) :: _ when N.value id = name -> Some (`V (Delayed.get v))
    | External (id, vex) :: _ when N.value id = name -> Some (`E vex)
    | Class (id, _, c) :: _ when N.class_ id = name -> Some (`C c)
    | ClassType (id, _, ct) :: _ when N.class_type id = name -> Some (`CT ct)
    | Include inc :: tl -> (
        match inner inc.Include.expansion_.items with
        | Some _ as found -> found
        | None -> inner tl )
    | Type (id, _, t) :: tl -> (
        let typ = Delayed.get t in
        match any_in_type typ name with
        | Some r -> Some (`In_type (typename_of_typeid id, typ, r))
        | None -> inner tl )
    | TypExt typext :: tl -> (
        match any_in_typext typext name with
        | Some _ as found -> found
        | None -> inner tl )
    | Comment (`Docs d) :: tl -> (
        match any_in_comment d name with
        | Some _ as found -> found
        | None -> inner tl )
    | _ :: tl -> inner tl
    | [] -> None
  in
  inner s.items

let signature_in_sig sg name =
  find_in_sig sg (function
    | Signature.Module (id, _, m) when N.module_ id = name ->
        Some (`M (Delayed.get m))
    | ModuleType (id, mt) when N.module_type id = name ->
        Some (`MT (Delayed.get mt))
    | _ -> None)

let module_type_in_sig sg name =
  find_in_sig sg (function
    | Signature.ModuleType (id, m) when N.module_type id = name ->
        Some (`MT (Delayed.get m))
    | _ -> None)

let value_in_sig sg name =
  find_in_sig sg (function
    | Signature.Value (id, m) when N.value id = name ->
        Some (`V (Delayed.get m))
    | External (id, e) when N.value id = name -> Some (`E e)
    | _ -> None)

let label_in_sig sg name =
  find_in_sig sg (function
    | Signature.Comment (`Docs d) -> any_in_comment d name
    | _ -> None)

let exception_in_sig sg name =
  find_in_sig sg (function
    | Signature.Exception (id, e) when N.exception_ id = name -> Some (`Exn e)
    | _ -> None)

let extension_in_sig sg name =
  let rec inner t = function
    | ec :: _ when ec.Extension.Constructor.name = name -> Some (`Ext (t, ec))
    | _ :: tl -> inner t tl
    | [] -> None
  in
  find_in_sig sg (function
    | Signature.TypExt t -> inner t t.Extension.constructors
    | _ -> None)

let label_parent_in_sig sg name =
  let module N = Ident.Name in
  find_in_sig sg (function
    | Signature.Module (id, _, m) when N.module_ id = name ->
        Some (`M (Component.Delayed.get m))
    | ModuleType (id, mt) when N.module_type id = name ->
        Some (`MT (Component.Delayed.get mt))
    | Type (id, _, t) when N.type_ id = name ->
        Some (`T (Component.Delayed.get t))
    | Class (id, _, c) when N.class_ id = name -> Some (`C c)
    | ClassType (id, _, c) when N.class_type id = name -> Some (`CT c)
    | _ -> None)

let any_in_type_in_sig sg name =
  find_in_sig sg (function
    | Signature.Type (id, _, t) -> (
        match any_in_type (Component.Delayed.get t) name with
        | Some x -> Some (typename_of_typeid id, x)
        | None -> None )
    | _ -> None)

let find_in_class_signature cs f =
  let rec inner = function
    | ClassSignature.Inherit ct_expr :: tl -> (
        match inner_inherit ct_expr with Some _ as x -> x | None -> inner tl )
    | it :: tl -> ( match f it with Some _ as x -> x | None -> inner tl )
    | [] -> None
  and inner_inherit = function
    | Constr _ -> None
    | Signature cs -> inner cs.items
  in
  inner cs.ClassSignature.items

let any_in_class_signature cs name =
  find_in_class_signature cs (function
    | ClassSignature.Method (id, m) when Ident.Name.method_ id = name ->
        Some (`Mm m)
    | InstanceVariable (id, iv) when Ident.Name.instance_variable id = name ->
        Some (`Mv iv)
    | _ -> None)

let method_in_class_signature cs name =
  find_in_class_signature cs (function
    | ClassSignature.Method (id, m) when Ident.Name.method_ id = name ->
        Some (`Mm m)
    | _ -> None)

let instance_variable_in_class_signature cs name =
  find_in_class_signature cs (function
    | ClassSignature.InstanceVariable (id, iv)
      when Ident.Name.instance_variable id = name ->
        Some (`Mv iv)
    | _ -> None)
