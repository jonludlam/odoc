
(* Identifiers *)

type sig_ident =
  [ `Module of sig_ident * string
  | `ModuleType of sig_ident * string
  | `Root of string ]

type mod_ident =
  [ `Module of sig_ident * string
  | `Root of string ]

type modty_ident =
  [ `ModuleType of sig_ident * string ]

type type_ident =
  [ `Type of sig_ident * string ]

type ('a, 'b) ident =
  [ `Local of 'a | `Global of 'b ]

  
type mod_id = (unit, mod_ident) ident
type modty_id = (unit, modty_ident) ident
type type_id = (unit, type_ident) ident

(* paths *)
type ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) mod_path =
  [ `Resolved of ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) r_mod_path
  | `Identifier of 'mod_id
  | `Dot of (('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) mod_path) * string
  | `Apply of (('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) mod_path) * (('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) mod_path) ]

and ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) r_mod_path =
  [ `Identifier of 'mod_id
  | `Module of 'parent * string
  | `External of 'mod_extern
  | `Alias of ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) r_mod_path * ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) mod_path
  | `Apply of (('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) r_mod_path) * (('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) r_mod_path)
  | `Subst of (('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) r_modty_path) * ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) r_mod_path]

and ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) modty_path =
  [ `Resolved of ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) r_modty_path
  | `Identifier of 'modty_id
  | `Dot of ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) mod_path * string ]

and ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) r_modty_path =
  [ `Identifier of 'modty_id
  | `External of 'modty_extern
  | `ModuleType of 'parent * string ]

and ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent, 'ty_id, 'ty_extern) type_path =
  [ `Resolved of ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent, 'ty_id, 'ty_extern) r_type_path
  | `Identifier of 'ty_id
  | `Dot of ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent) mod_path * string ]

and ('mod_id, 'mod_extern, 'modty_id, 'modty_extern, 'parent, 'ty_id, 'ty_extern) r_type_path =
  [ `Identifier of 'ty_id
  | `External of 'ty_extern
  | `Type of 'parent * string ]



(* global paths *)
type gmod_path = (mod_id, unit, modty_id, unit, gmod_path) mod_path
type gr_mod_path = (mod_id, unit, modty_id, unit, gmod_path) r_mod_path
type gmodty_path = (mod_id, unit, modty_id, unit, gmod_path) modty_path
type gr_modty_path = (mod_id, unit, modty_id, unit, gmod_path) r_modty_path
type gtype_path = (mod_id, unit, modty_id, unit, gmod_path, type_id, unit) type_path
type gr_type_path = (mod_id, unit, modty_id, unit, gmod_path, type_id, unit) r_type_path

(* local paths *)

(* local idents *)

type lsig_ident =
  [ `LRoot of string * int
  | `LModule of string * int
  | `LModuleType of string * int ]

type lmod_ident =
  [ `LRoot of string * int
  | `LModule of string * int ]

type lmodty_ident =
  [ `LModuleType of string * int ]

type ltype_ident =
  [ `LType of string * int ]


type lmod_id = (lmod_ident, mod_ident) ident
type lmodty_id = (lmodty_ident, modty_ident) ident
type ltype_id = (ltype_ident, type_ident) ident

type parent = [ `Module of lmod_path | `ModuleType of lmodty_path | `FragmentRoot ]

and lmod_path = (lmod_id, gmod_path, lmodty_id, gmodty_path, parent) mod_path
and lr_mod_path = (lmod_id, gmod_path, lmodty_id, gmodty_path, parent) r_mod_path
and lmodty_path = (lmod_id, gmod_path, lmodty_id, gmodty_path, parent) modty_path
and lr_modty_path = (lmod_id, gmod_path, lmodty_id, gmodty_path, parent) r_modty_path
and ltype_path = (lmod_id, gmod_path, lmodty_id, gmodty_path, parent, ltype_id, gtype_path) type_path
and lr_type_path = (lmod_id, gmod_path, lmodty_id, gmodty_path, parent, ltype_id, gtype_path) r_type_path



type env = {
  e_mods : (mod_ident * unit) list;
  e_modtys : (modty_ident * unit) list;
  e_tys : (type_ident * unit list);
}

let rec resolve_module : type a b c d e f. env -> ((a,mod_ident) ident,b,(c,modty_ident) ident,d,e) mod_path -> ((a,mod_ident) ident,b,(c, modty_ident) ident,d,e) r_mod_path = fun env p ->
  match p with
  | `Identifier (`Global id) -> (
    match List.assoc_opt id env.e_mods with
    | Some () -> `Identifier (`Global id)
    | None -> failwith "Bad")
  | `
