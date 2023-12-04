let parenthesise name =
  match name with
  | "asr" | "land" | "lor" | "lsl" | "lsr" | "lxor" | "mod" -> "(" ^ name ^ ")"
  | _ ->
      if String.length name > 0 then
        match name.[0] with
        | 'a' .. 'z'
        | '\223' .. '\246'
        | '\248' .. '\255'
        | '_'
        | 'A' .. 'Z'
        | '\192' .. '\214'
        | '\216' .. '\222' ->
            name
        | _ -> "(" ^ name ^ ")"
      else name

let contains_double_underscore s =
  let len = String.length s in
  let rec aux i =
    if i > len - 2 then false
    else if s.[i] = '_' && s.[i + 1] = '_' then true
    else aux (i + 1)
  in
  aux 0

module type Name = sig
  type t

  val to_string : t -> string

  val to_string_unsafe : t -> string

  val make_std : string -> t

  val of_ident : Ident.t -> t

  val hidden_of_string : string -> t

  val hidden_of_ident : Ident.t -> t

  val shadowed_of_string : string -> t

  val shadowed_of_ident : Ident.t -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val fmt : Format.formatter -> t -> unit

  val is_hidden : t -> bool
end

let internal_counter = ref 0

module Name : Name = struct
  type t = Hidden of string | Shadowed of string * int | Std of string

  let to_string = function
    | Std s -> parenthesise s
    | Hidden s -> Printf.sprintf "%s" s
    | Shadowed (s, i) -> Printf.sprintf "{%s}%d" s i

  let to_string_unsafe = function
    | Std s -> s
    | Hidden s -> s
    | Shadowed (s, _i) -> s

  let make_std s = Std s

  let of_ident id = make_std (Ident.name id)

  let hidden_of_string id = Hidden id

  let hidden_of_ident id = hidden_of_string (Ident.name id)

  let shadowed_of_string id =
    incr internal_counter;
    Shadowed (id, !internal_counter)

  let shadowed_of_ident id = shadowed_of_string (Ident.name id)

  let equal (x : t) (y : t) = x = y

  let compare = compare

  let fmt ppf x = Format.fprintf ppf "%s" (to_string x)

  let is_hidden = function
    | Std s -> contains_double_underscore s
    | Hidden _ -> true
    | Shadowed _ -> true
end

module type SimpleName = sig
  type t

  val to_string : t -> string

  val make_std : string -> t

  val of_ident : Ident.t -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val fmt : Format.formatter -> t -> unit

  val is_hidden : t -> bool
end

module SimpleName : SimpleName = struct
  type t = string

  let to_string s = s

  let make_std s = s

  let of_ident id = make_std (Ident.name id)

  let equal (x : t) (y : t) = x = y

  let compare x y = String.compare x y

  let fmt ppf t = Format.pp_print_string ppf (to_string t)

  let is_hidden s = contains_double_underscore s
end

module ModuleName = Name
module ModuleTypeName = Name
module TypeName = Name
module ConstructorName = SimpleName
module FieldName = SimpleName
module ExtensionName = SimpleName
module ExceptionName = SimpleName
module ValueName = Name
module ClassName = Name
module ClassTypeName = Name
module MethodName = SimpleName
module InstanceVariableName = SimpleName
module LabelName = SimpleName
module PageName = SimpleName
module DefName = SimpleName
module LocalName = SimpleName
