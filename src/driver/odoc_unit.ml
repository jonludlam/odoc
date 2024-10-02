type pkg_args = {
  pages : (string * Fpath.t) list;
  libs : (string * Fpath.t) list;
}

type index = {
  pkg_args : pkg_args;
  output_file : Fpath.t;
  json : bool;
  search_dir : Fpath.t;
}

type 'a unit = {
  parent_id : Odoc.Id.t;
  odoc_dir : Fpath.t;
  input_file : Fpath.t;
  output_dir : Fpath.t;
  odoc_file : Fpath.t;
  odocl_file : Fpath.t;
  pkg_args : pkg_args;
  pkgname : string;
  include_dirs : Fpath.t list;
  index : index option;
  kind : 'a;
}

type intf_extra = { hidden : bool; hash : string; deps : intf unit list }
and intf = [ `Intf of intf_extra ]

type impl_extra = { src_id : Odoc.Id.t; src_path : Fpath.t }
type impl = [ `Impl of impl_extra ]

type mld = [ `Mld ]

type asset = [ `Asset ]

type t = [ impl | intf | mld | asset ] unit

let of_packages ~output_dir ~linked_dir ~index_dir ~extra_libs_paths
    (pkgs : Packages.t list) : t list =
  let linked_dir =
    match linked_dir with None -> output_dir | Some dir -> dir
  in
  let index_dir = match index_dir with None -> output_dir | Some dir -> dir in

  (* This isn't a hashtable, but a table of hashes! Yay! *)
  let hashtable, lib_dirs =
    let open Packages in
    let h = Util.StringMap.empty in
    let lds = extra_libs_paths in
    List.fold_left
      (fun (h, lds) pkg ->
        List.fold_left
          (fun (h, lds) lib ->
            let h' =
              List.fold_left
                (fun h mod_ ->
                  Util.StringMap.add mod_.m_intf.mif_hash (pkg, lib, mod_) h)
                h lib.modules
            in
            let lds' =
              Util.StringMap.add lib.lib_name
                Fpath.(
                  output_dir // pkg.Packages.pkg_dir / "lib" / lib.lib_name)
                lds
            in
            (h', lds'))
          (h, lds) pkg.libraries)
      (h, lds) pkgs
  in
  (* This one is a hashtable *)
  let cache = Hashtbl.create 10 in
  let pkg_args_of pkg lib_deps : pkg_args =
    let pages =
      [
        (pkg.Packages.name, Fpath.(output_dir // pkg.Packages.pkg_dir / "doc"));
      ]
    in
    let libs =
      List.filter_map
        (fun lib_name ->
          match Util.StringMap.find_opt lib_name lib_dirs with
          | Some x -> Some (lib_name, x)
          | None -> None)
        (Util.StringSet.to_list lib_deps)
    in
    { pages; libs }
  in
  let index_of pkg =
    let pkg_args =
      pkg_args_of pkg
        (List.fold_left
           (fun acc l -> Util.StringSet.add l.Packages.lib_name acc)
           Util.StringSet.empty pkg.libraries)
    in
    let output_file = Fpath.(index_dir / pkg.name / Odoc.index_filename) in
    { pkg_args; output_file; json = false; search_dir = pkg.pkg_dir }
  in
  let make_unit ~name ~kind ~rel_dir ~input_file ~pkg ~include_dirs ~lib_deps :
      _ unit =
    let ( // ) = Fpath.( // ) in
    let ( / ) = Fpath.( / ) in
    let pkg_args = pkg_args_of pkg lib_deps in
    let odoc_dir = output_dir // rel_dir in
    let parent_id = rel_dir |> Odoc.Id.of_fpath in
    let odoc_file = odoc_dir / (String.uncapitalize_ascii name ^ ".odoc") in
    (* odoc will uncapitalise the output filename *)
    let odocl_file =
      linked_dir // rel_dir / (String.uncapitalize_ascii name ^ ".odocl")
    in
    {
      output_dir;
      pkgname = pkg.Packages.name;
      pkg_args;
      parent_id;
      odoc_dir;
      input_file;
      odoc_file;
      odocl_file;
      include_dirs;
      kind;
      index = Some (index_of pkg);
    }
  in
  let rec build_deps deps =
    List.filter_map
      (fun (_name, hash) ->
        match Util.StringMap.find_opt hash hashtable with
        | None -> None
        | Some (pkg, lib, mod_) ->
            let lib_deps = Util.StringSet.add lib.lib_name lib.lib_deps in
            let result =
              of_intf mod_.m_hidden pkg lib.lib_name lib_deps mod_.m_intf
            in
            Hashtbl.add cache mod_.m_intf.mif_hash result;
            Some result)
      deps
  and of_intf hidden pkg libname lib_deps (intf : Packages.intf) : intf unit =
    match Hashtbl.find_opt cache intf.mif_hash with
    | Some unit -> unit
    | None ->
        let open Fpath in
        let rel_dir = pkg.Packages.pkg_dir / "lib" / libname in
        let include_dirs, kind =
          let deps = build_deps intf.mif_deps in
          let include_dirs = List.map (fun u -> u.odoc_dir) deps in
          let kind = `Intf { hidden; hash = intf.mif_hash; deps } in
          (include_dirs, kind)
        in
        if not @@ Util.StringSet.mem libname lib_deps then failwith "ERrorrr";
        let name = intf.mif_path |> Fpath.rem_ext |> Fpath.basename in
        make_unit ~name ~kind ~rel_dir ~input_file:intf.mif_path ~pkg
          ~include_dirs ~lib_deps
  in
  let of_impl pkg libname lib_deps (impl : Packages.impl) : impl unit option =
    let open Fpath in
    match impl.mip_src_info with
    | None -> None
    | Some { src_path } ->
        let rel_dir = pkg.Packages.pkg_dir / "lib" / libname in
        let include_dirs =
          let deps = build_deps impl.mip_deps in
          List.map (fun u -> u.odoc_dir) deps
        in
        let kind =
          let src_name = Fpath.filename src_path in
          let src_id =
            Fpath.(pkg.pkg_dir / "src" / libname / src_name) |> Odoc.Id.of_fpath
          in
          `Impl { src_id; src_path }
        in
        let name =
          impl.mip_path |> Fpath.rem_ext |> Fpath.basename
          |> String.uncapitalize_ascii |> ( ^ ) "impl-"
        in
        let unit =
          make_unit ~name ~kind ~rel_dir ~input_file:impl.mip_path ~pkg
            ~include_dirs ~lib_deps
        in
        Some unit
  in

  let of_module pkg libname lib_deps (m : Packages.modulety) :
      [ impl | intf ] unit list =
    let i :> [ impl | intf ] unit =
      of_intf m.m_hidden pkg libname lib_deps m.m_intf
    in
    let m :> [ impl | intf ] unit list =
      Option.bind m.m_impl (of_impl pkg libname lib_deps) |> Option.to_list
    in
    i :: m
  in
  let of_lib pkg (lib : Packages.libty) : [ impl | intf ] unit list =
    let lib_deps = Util.StringSet.add lib.lib_name lib.lib_deps in
    List.concat_map (of_module pkg lib.lib_name lib_deps) lib.modules
  in
  let of_mld pkg (mld : Packages.mld) : mld unit list =
    let open Fpath in
    let { Packages.mld_path; mld_rel_path } = mld in
    let rel_dir =
      pkg.Packages.pkg_dir / "doc" // Fpath.parent mld_rel_path
      |> Fpath.normalize
    in
    let include_dirs =
      List.map
        (fun (lib : Packages.libty) ->
          Fpath.(output_dir // pkg.pkg_dir / "lib" / lib.lib_name))
        pkg.libraries
    in
    let include_dirs = (output_dir // rel_dir) :: include_dirs in
    let kind = `Mld in
    let name = mld_path |> Fpath.rem_ext |> Fpath.basename |> ( ^ ) "page-" in
    let unit =
      make_unit ~name ~kind ~rel_dir ~input_file:mld_path ~pkg ~include_dirs
        ~lib_deps:Util.StringSet.empty
    in
    [ unit ]
  in
  let of_asset pkg (asset : Packages.asset) : asset unit list =
    let open Fpath in
    let { Packages.asset_path; asset_rel_path } = asset in
    let rel_dir =
      pkg.Packages.pkg_dir / "doc" // Fpath.parent asset_rel_path
      |> Fpath.normalize
    in
    let include_dirs = [] in
    let kind = `Asset in
    let unit =
      let name = asset_path |> Fpath.basename |> ( ^ ) "asset-" in
      make_unit ~name ~kind ~rel_dir ~input_file:asset_path ~pkg ~include_dirs
        ~lib_deps:Util.StringSet.empty
    in
    [ unit ]
  in
  let of_package (pkg : Packages.t) : t list =
    let lib_units :> t list list = List.map (of_lib pkg) pkg.libraries in
    let mld_units :> t list list = List.map (of_mld pkg) pkg.mlds in
    let asset_units :> t list list = List.map (of_asset pkg) pkg.assets in
    List.concat (lib_units @ mld_units @ asset_units)
  in
  List.concat_map of_package pkgs
