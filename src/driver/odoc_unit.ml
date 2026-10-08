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

let hash (u : intf t) =
  let (`Intf { hash; _ }) = u.kind in
  hash

let deps (u : intf t) =
  let (`Intf { deps; _ }) = u.kind in
  List.map snd deps

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
   build that must be compiled before it, its dependency cone, which its units
   are compiled and linked with as [-L], and the page the driver writes to list
   its modules. *)
type lib = {
  lib_name : string;
  pkgname : string option;
  requires : lib list;
  includes : (string * Fpath.t) list;
  units : module_unit list;
  page : mld t option;
}

let include_dirs lib = List.map snd lib.includes

let link_libs scope lib =
  scope.lib_roots
  @ List.filter
      (fun (name, _) -> not (List.mem_assoc name scope.lib_roots))
      lib.includes

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

let is_hidden (u : any) =
  match u.kind with `Intf { hidden; _ } -> hidden | _ -> false

let is_output (u : any) = u.to_output && not (is_hidden u)

let lib_units lib =
  (Option.to_list lib.page :> any list) @ (lib.units :> any list)

let all_units pkg = (pkg.pages :> any list) @ List.concat_map lib_units pkg.libs

let index_inputs pkg =
  all_units pkg
  |> List.filter_map (fun u -> if is_output u then Some u.odocl_file else None)
  |> List.sort_uniq Fpath.compare

let scope_pkgs pkgs =
  let by_name =
    List.fold_left
      (fun acc p ->
        match p.pkgname with
        | Some name -> Util.StringMap.add name p acc
        | None -> acc)
      Util.StringMap.empty pkgs
  in
  fun pkg ->
    List.filter_map
      (fun (name, _) -> Util.StringMap.find_opt name by_name)
      pkg.scope.page_roots

let link_units pkg =
  List.map (fun u -> (pkg.scope.lib_roots, u)) (pkg.pages :> any list)
  @ List.concat_map
      (fun lib ->
        let libs = link_libs pkg.scope lib in
        List.map (fun u -> (libs, u)) (lib_units lib))
      pkg.libs

let pkg_dir : Packages.t -> Fpath.t = fun pkg -> pkg.pkg_dir
let doc_dir : Packages.t -> Fpath.t = fun pkg -> pkg.doc_dir
let lib_dir (pkg : Packages.t) (lib : Packages.libty) =
  Fpath.(doc_dir pkg / lib.Packages.lib_name)

(* Where the [.odoc] files go, relative to the odoc directory, which mirrors
   the switch: a library's modules beside where its objects are installed
   ([lib/<findlib dir>]), a package's pages below [doc/<pkg>], the layout of
   their identifiers ([doc_dir]) reproduced there. *)
let lib_obj_dir (lib : Packages.libty) = lib.Packages.rel_dir
let pages_dir (pkg : Packages.t) = Fpath.(v "doc" / pkg.name)
let page_obj_dir (pkg : Packages.t) rel_dir =
  match Fpath.relativize ~root:(doc_dir pkg) rel_dir with
  | Some rel -> Fpath.(pages_dir pkg // rel |> normalize)
  | None -> Fpath.(pages_dir pkg // rel_dir |> normalize)
let src_dir pkg = Fpath.(doc_dir pkg / "src")
let src_lib_dir (pkg : Packages.t) (lib : Packages.libty) =
  Fpath.(src_dir pkg / lib.Packages.lib_name)

type dirs = {
  odoc_dir : Fpath.t;
  odocl_dir : Fpath.t;
  index_dir : Fpath.t;
  mld_dir : Fpath.t;
}
