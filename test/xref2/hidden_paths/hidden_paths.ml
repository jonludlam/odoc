(* Each case gives the model's answer and the component path's answer for the
   same path. *)

open Odoc_model
open Odoc_model.Names
module Id = Paths.Identifier
module P = Paths.Path

let root = Id.Mk.root (None, ModuleName.make_std "R")

let m = Id.Mk.module_ (root, ModuleName.make_std "M")

let h = Id.Mk.module_ (root, ModuleName.make_std "H__x")

let s = Id.Mk.module_type (root, ModuleTypeName.make_std "S")

let s_h = Id.Mk.module_type (root, ModuleTypeName.hidden_of_string "S_h")

let c_h = Id.Mk.class_ (root, TypeName.hidden_of_string "c_h")

let rm : P.Resolved.Module.t = `Identifier m

let rh : P.Resolved.Module.t = `Identifier h

let pm : P.Resolved.parent = `Module rm

let case name model cpath =
  Printf.printf "%-34s model=%-5b cpath=%-5b%s\n" name model cpath
    (if model = cpath then "" else "  DIFFERENT")

let resolved_module ?(weak = false) name (p : P.Resolved.Module.t) =
  case name
    (P.Resolved.Module.is_hidden ~weak_canonical_test:weak p)
    (Odoc_xref2.Cpath.is_resolved_module_hidden ~weak_canonical_test:weak
       (p :> Odoc_xref2.Cpath.Resolved.module_))

let resolved_module_type name (p : P.Resolved.ModuleType.t) =
  case name
    (P.Resolved.is_hidden (p :> P.Resolved.t))
    (Odoc_xref2.Cpath.is_resolved_module_type_hidden
       (p :> Odoc_xref2.Cpath.Resolved.module_type))

let resolved_type name (p : P.Resolved.Type.t) =
  case name
    (P.Resolved.is_hidden (p :> P.Resolved.t))
    (Odoc_xref2.Cpath.is_resolved_type_hidden
       (p :> Odoc_xref2.Cpath.Resolved.type_))

let module_ name (p : P.Module.t) =
  case name
    (P.is_hidden (p :> P.t))
    (Odoc_xref2.Cpath.is_module_hidden (p :> Odoc_xref2.Cpath.module_))

let module_type name (p : P.ModuleType.t) =
  case name
    (P.is_hidden (p :> P.t))
    (Odoc_xref2.Cpath.is_module_type_hidden (p :> Odoc_xref2.Cpath.module_type))

let type_ name (p : P.Type.t) =
  case name
    (P.is_hidden (p :> P.t))
    (Odoc_xref2.Cpath.is_type_hidden (p :> Odoc_xref2.Cpath.type_))

let class_type name (p : P.ClassType.t) =
  case name
    (P.is_hidden (p :> P.t))
    (Odoc_xref2.Cpath.is_class_type_hidden (p :> Odoc_xref2.Cpath.class_type))

let () =
  print_endline "Resolved paths:";
  resolved_module "Alias (hidden, visible unresolved)"
    (`Alias (rh, `Identifier (m, false), None));
  resolved_module "Apply (visible, hidden)" (`Apply (rm, rh));
  resolved_module_type "ModuleType (visible, hidden name)"
    (`ModuleType (pm, ModuleTypeName.hidden_of_string "S_h"));
  resolved_module_type "AliasModuleType (hidden, visible)"
    (`AliasModuleType (`Identifier s_h, `Identifier s));
  resolved_module_type "OpaqueModuleType (hidden)"
    (`OpaqueModuleType (`Identifier s_h));
  resolved_type "Class (visible, hidden name)"
    (`Class (pm, TypeName.hidden_of_string "c_h"));
  resolved_type "Identifier (hidden class)" (`Identifier c_h);
  resolved_module ~weak:true "Subst, weak canonical test"
    (`Subst
       ( `ModuleType
           ( `Module (`Canonical (rh, `Root (ModuleName.make_std "C"))),
             ModuleTypeName.make_std "T" ),
         rm ));
  print_endline "Unresolved paths:";
  module_ "Identifier (hidden name)" (`Identifier (h, false));
  module_ "Root (hidden name)" (`Root (ModuleName.make_std "H__x"));
  module_ "Dot (visible, hidden name)"
    (`Dot (`Identifier (m, false), ModuleName.make_std "H__x"));
  module_ "Apply (visible, hidden)"
    (`Apply (`Identifier (m, false), `Identifier (h, false)));
  module_type "DotMT (visible, hidden name)"
    (`DotMT (`Identifier (m, false), ModuleTypeName.hidden_of_string "S_h"));
  type_ "DotT (visible, hidden name)"
    (`DotT (`Identifier (m, false), TypeName.hidden_of_string "t_h"));
  class_type "Identifier (hidden class)" (`Identifier (c_h, false))
