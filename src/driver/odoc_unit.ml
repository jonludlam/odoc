(* The link-time reference scope of a package: the page trees ([-P]) and the
   module trees ([-L]) that its units may refer to. Every unit of a package is
   linked with the same scope. Paths are absolute. *)
type scope = {
  page_roots : (string * Fpath.t) list;
  lib_roots : (string * Fpath.t) list;
}

(* A package's index and the sidebar derived from it, both binary, and the
   directory of the package's HTML, where the search database and the JSON
   sidebar go. *)
type index = {
  index_file : Fpath.t;
  sidebar_file : Fpath.t;
  html_dir : Fpath.t;
}

type +'a t = {
  parent_id : Odoc.Id.t;
  input_file : Fpath.t;
  input_copy : Fpath.t option;
      (* Used to stash cmtis from virtual libraries into the odoc dir for voodoo mode.
         See https://github.com/ocaml/odoc/pull/1309 *)
  odoc_file : Fpath.t;
  odocl_file : Fpath.t;
  enable_warnings : bool;
  to_output : bool;
  kind : 'a;
}

type intf_extra = {
  hidden : bool;
  hash : string;
  deps : (string * Digest.t) list;
}

and intf = [ `Intf of intf_extra ]

type impl_extra = { src_id : Odoc.Id.t; src_path : Fpath.t }
type impl = [ `Impl of impl_extra ]

type mld = [ `Mld ]
type md = [ `Md ]
type asset = [ `Asset ]

type all_kinds = [ impl | intf | mld | asset | md ]
type any = all_kinds t

(* The units of a library, and those of a package's documentation. *)
type module_unit = [ intf | impl ] t
type page = [ mld | md | asset ] t

(* A library: its modules and their implementations, the libraries of this
   build that must be compiled before it, and the [-I] search path its units
   are compiled and linked with (the directories of its dependency cone). *)
type lib = {
  lib_name : string;
  requires : string list;
  includes : Fpath.t list;
  units : module_unit list;
}

(* A package, the unit of building: its libraries, its pages (including the
   generated landing pages), the reference scope they all link with, and the
   index they are gathered in. *)
type pkg = {
  pkgname : string option;
  scope : scope;
  index : index option;
  libs : lib list;
  pages : page list;
}

let all_units pkg =
  (pkg.pages :> any list)
  @ List.concat_map (fun l -> (l.units :> any list)) pkg.libs

(* What an earlier run left behind, for the packages being built to refer to:
   for each library, the package providing it and the directory of its [.odoc]
   files; for each package, its doc directory. Paths are relative to the odoc
   directory. *)
module Prebuilt = struct
  type t = {
    libs : (string * Fpath.t) Util.StringMap.t;
    pkgs : Fpath.t Util.StringMap.t;
  }

  let empty = { libs = Util.StringMap.empty; pkgs = Util.StringMap.empty }
end

let pkg_dir : Packages.t -> Fpath.t = fun pkg -> pkg.pkg_dir
let doc_dir : Packages.t -> Fpath.t = fun pkg -> pkg.doc_dir
let lib_dir (pkg : Packages.t) (lib : Packages.libty) =
  match lib.id_override with
  | Some id -> Fpath.v id
  | None -> Fpath.(doc_dir pkg / lib.Packages.lib_name)
let lib_obj_dir (pkg : Packages.t) (lib : Packages.libty) =
  Fpath.(pkg_dir pkg // lib.Packages.rel_dir)
let src_dir pkg = Fpath.(doc_dir pkg / "src")
let src_lib_dir (pkg : Packages.t) (lib : Packages.libty) =
  match lib.id_override with
  | Some id -> Fpath.v id
  | None -> Fpath.(src_dir pkg / lib.Packages.lib_name)

let output_root (u : _ t) =
  let dir = Fpath.parent u.odoc_file in
  match Odoc.Id.to_string u.parent_id with
  | "" -> dir
  | id ->
      let rec up d = function 0 -> d | n -> up (Fpath.parent d) (n - 1) in
      up dir (List.length (String.split_on_char '/' id))

type dirs = {
  odoc_dir : Fpath.t;
  odocl_dir : Fpath.t;
  index_dir : Fpath.t;
  mld_dir : Fpath.t;
}
