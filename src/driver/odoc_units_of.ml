open Odoc_unit

type indices_style =
  | Voodoo
  | Normal of { toplevel_content : string option }
  | Automatic

let packages ~dirs ~extra_paths ~remap ~indices_style (pkgs : Packages.t list) :
    pkg list =
  let { odoc_dir; odocl_dir; index_dir; mld_dir = _ } = dirs in

  let extra_libs_paths = extra_paths.Voodoo.libs in
  let extra_libs_of_pkg = extra_paths.Voodoo.libs_of_pkg in
  let extra_pkg_paths = extra_paths.Voodoo.pkgs in

  (* Where each library's [.odoc] files are: its object directory, mirrored
     below the odoc dir (see [Odoc_unit.lib_obj_dir]). This is what [-L] and
     [-I] point at. *)
  let lib_dirs =
    let open Packages in
    let lds = extra_libs_paths in
    List.fold_left
      (fun lds pkg ->
        List.fold_left
          (fun lds lib ->
            let lib_dir = lib_obj_dir pkg lib in
            let lds' = Util.StringMap.add lib.lib_name lib_dir lds in
            lds')
          lds pkg.libraries)
      lds pkgs
  in
  let pkg_paths =
    List.fold_left
      (fun acc pkg -> Util.StringMap.add pkg.Packages.name (doc_dir pkg) acc)
      extra_pkg_paths pkgs
  in

  let libs_of_pkg =
    let libs_of_pkg pkg =
      List.map (fun lib -> lib.Packages.lib_name) pkg.Packages.libraries
    in
    List.fold_left
      (fun acc pkg ->
        Util.StringMap.add pkg.Packages.name (libs_of_pkg pkg) acc)
      extra_libs_of_pkg pkgs
  in

  let dash_p pkgname path = (pkgname, Fpath.(odoc_dir // path)) in

  let dash_l lib_name =
    match Util.StringMap.find_opt lib_name lib_dirs with
    | Some dir -> [ (lib_name, Fpath.(odoc_dir // dir)) ]
    | None ->
        Logs.debug (fun m -> m "Library %s not found" lib_name);
        []
  in

  (* The reference scope of a package, shared by all its units. [-P] is the
     package's own pages plus the packages named in its [odoc-config.sexp]; [-L]
     is its own libraries, their dependencies, and the libraries named in the
     config file, directly or through a named package. *)
  let scope_of (pkg : Packages.t) : scope =
    let { Global_config.deps = { packages = cfg_pkgs; libraries = cfg_libs } } =
      pkg.config
    in
    let pages =
      dash_p pkg.name (doc_dir pkg)
      :: List.filter_map
           (fun pkgname ->
             match Util.StringMap.find_opt pkgname pkg_paths with
             | None ->
                 Logs.debug (fun m -> m "Package '%s' not found" pkgname);
                 None
             | Some path -> Some (dash_p pkgname path))
           cfg_pkgs
    in
    let lib_set =
      List.fold_left
        (fun acc (lib : Packages.libty) ->
          Util.StringSet.add lib.lib_name
            (Util.StringSet.union lib.lib_deps acc))
        (Util.StringSet.of_list cfg_libs)
        pkg.libraries
    in
    let lib_set =
      List.fold_left
        (fun acc pkgname ->
          match Util.StringMap.find_opt pkgname libs_of_pkg with
          | Some libs -> List.fold_left (Fun.flip Util.StringSet.add) acc libs
          | None -> acc)
        lib_set cfg_pkgs
    in
    let libs = List.concat_map dash_l (Util.StringSet.elements lib_set) in
    { pages; libs }
  in

  (* The [-I] search path of a library: the directories of its dependencies
     and its own. *)
  let includes_of (lib : Packages.libty) =
    Util.StringSet.add lib.lib_name lib.lib_deps
    |> Util.StringSet.elements |> List.concat_map dash_l |> List.map snd
    |> List.sort_uniq Fpath.compare
  in

  let index_of (pkg : Packages.t) =
    let output_file = Fpath.(index_dir / pkg.name / Odoc.index_filename) in
    let pkg_dir = doc_dir pkg in
    let sidebar =
      let output_file = Fpath.(index_dir / pkg.name / Odoc.sidebar_filename) in
      { output_file; json = false; pkg_dir }
    in
    {
      output_file;
      json = false;
      search_dir = doc_dir pkg;
      sidebar = Some sidebar;
    }
  in

  (* [rel_dir] is the unit's parent id, which fixes its identifier and URL.
     [obj_dir] is where its [.odoc]/[.odocl] files go; for modules this is the
     library's mirrored object directory, for pages it is [rel_dir]. *)
  let make_unit ~name ~kind ~rel_dir ~obj_dir ~input_file ~enable_warnings
      ~to_output ~stash_input : _ t =
    let to_output = to_output || not remap in
    (* If we haven't got active remapping, we output everything *)
    let ( // ) = Fpath.( // ) in
    let ( / ) = Fpath.( / ) in
    let parent_id = rel_dir |> Odoc.Id.of_fpath in
    let odoc_file =
      odoc_dir // obj_dir / (String.uncapitalize_ascii name ^ ".odoc")
    in
    (* odoc will uncapitalise the output filename *)
    let odocl_file =
      odocl_dir // obj_dir / (String.uncapitalize_ascii name ^ ".odocl")
    in
    let input_copy =
      if stash_input then
        Some (odoc_dir // obj_dir / (String.uncapitalize_ascii name ^ ".cmti"))
      else None
    in
    {
      parent_id;
      input_file;
      input_copy;
      odoc_file;
      odocl_file;
      kind;
      to_output;
      enable_warnings;
    }
  in

  let of_intf hidden (pkg : Packages.t) (lib : Packages.libty)
      (intf : Packages.intf) : intf t =
    let rel_dir = lib_dir pkg lib in
    let kind =
      let deps = intf.mif_deps in
      let kind = `Intf { hidden; hash = intf.mif_hash; deps } in
      kind
    in
    let name = intf.mif_path |> Fpath.rem_ext |> Fpath.basename in
    let stash_input = lib.archive_name = None in
    make_unit ~name ~kind ~rel_dir ~obj_dir:(lib_obj_dir pkg lib)
      ~input_file:intf.mif_path ~enable_warnings:pkg.selected
      ~to_output:pkg.selected ~stash_input
  in
  let of_impl (pkg : Packages.t) lib (impl : Packages.impl) : impl t option =
    match impl.mip_src_info with
    | None -> None
    | Some { src_path } ->
        let rel_dir = lib_dir pkg lib in
        let kind =
          let src_name = Fpath.filename src_path in
          let src_id =
            Fpath.(src_lib_dir pkg lib / src_name) |> Odoc.Id.of_fpath
          in
          `Impl { src_id; src_path }
        in
        let name =
          impl.mip_path |> Fpath.rem_ext |> Fpath.basename
          |> String.uncapitalize_ascii |> ( ^ ) "impl-"
        in
        let unit =
          make_unit ~name ~kind ~rel_dir ~obj_dir:(lib_obj_dir pkg lib)
            ~input_file:impl.mip_path ~enable_warnings:false
            ~to_output:pkg.selected ~stash_input:false
        in
        Some unit
  in

  let of_module pkg (lib : Packages.libty) (m : Packages.modulety) : any list =
    let i :> any = of_intf m.m_hidden pkg lib m.m_intf in
    let m :> any list =
      Option.bind m.m_impl (of_impl pkg lib) |> Option.to_list
    in
    i :: m
  in
  (* A library's units, and its landing page (which is a page of the package). *)
  let of_lib (pkg : Packages.t) (lib : Packages.libty) :
      Odoc_unit.lib * any list =
    let units = List.concat_map (of_module pkg lib) lib.modules in
    let landing_page :> any list =
      if remap && not pkg.selected then []
      else [ Landing_pages.library ~dirs ~pkg lib ]
    in
    ( { lib_name = lib.lib_name; includes = includes_of lib; units },
      landing_page )
  in
  let of_mld (pkg : Packages.t) (mld : Packages.mld) : mld t list =
    let open Fpath in
    let { Packages.mld_path; mld_rel_path } = mld in
    let rel_dir = doc_dir pkg // Fpath.parent mld_rel_path |> Fpath.normalize in
    let kind = `Mld in
    let name = mld_path |> Fpath.rem_ext |> Fpath.basename |> ( ^ ) "page-" in
    let unit =
      make_unit ~name ~kind ~rel_dir ~obj_dir:rel_dir ~input_file:mld_path
        ~enable_warnings:pkg.selected ~to_output:pkg.selected ~stash_input:false
    in
    [ unit ]
  in
  let of_md (pkg : Packages.t) (md : Packages.md) : md t list =
    let ext = Fpath.get_ext md.md_path in
    match ext with
    | ".md" ->
        let open Fpath in
        let { Packages.md_path; md_rel_path } = md in
        let rel_dir =
          doc_dir pkg // Fpath.parent md_rel_path |> Fpath.normalize
        in
        let kind = `Md in
        let name =
          md_path |> Fpath.rem_ext |> Fpath.basename |> ( ^ ) "page-"
        in
        let unit =
          make_unit ~name ~kind ~rel_dir ~obj_dir:rel_dir ~input_file:md_path
            ~enable_warnings:pkg.selected ~to_output:pkg.selected
            ~stash_input:false
        in
        [ unit ]
    | _ ->
        Logs.debug (fun m ->
            m "Skipping non-markdown doc file %a" Fpath.pp md.md_path);
        []
  in
  let of_asset (pkg : Packages.t) (asset : Packages.asset) : asset t list =
    let open Fpath in
    let { Packages.asset_path; asset_rel_path } = asset in
    let rel_dir =
      doc_dir pkg // Fpath.parent asset_rel_path |> Fpath.normalize
    in
    let kind = `Asset in
    let unit =
      let name = asset_path |> Fpath.basename |> ( ^ ) "asset-" in
      make_unit ~name ~kind ~rel_dir ~obj_dir:rel_dir ~input_file:asset_path
        ~enable_warnings:false ~to_output:true ~stash_input:false
    in
    [ unit ]
  in

  let of_package (pkg : Packages.t) : Odoc_unit.pkg =
    let libs, landing_pages =
      List.split (List.map (of_lib pkg) pkg.libraries)
    in
    let mld_units :> any list list = List.map (of_mld pkg) pkg.mlds in
    let asset_units :> any list list = List.map (of_asset pkg) pkg.assets in
    let md_units :> any list list = List.map (of_md pkg) pkg.other_docs in
    let pkg_index () :> any list =
      let has_index_page =
        List.exists
          (fun mld ->
            Fpath.equal
              (Fpath.normalize mld.Packages.mld_rel_path)
              (Fpath.normalize (Fpath.v "./index.mld")))
          pkg.mlds
      in
      if has_index_page || (remap && not pkg.selected) then []
      else [ Landing_pages.package ~dirs ~pkg ]
    in
    let src_index () :> any list =
      if remap && not pkg.selected then []
      else if
        (* Some library has a module which has an implementation which has a source *)
        List.exists
          (fun lib ->
            List.exists
              (fun m ->
                match m.Packages.m_impl with
                | Some { mip_src_info = Some _; _ } -> true
                | _ -> false)
              lib.Packages.modules)
          pkg.libraries
      then [ Landing_pages.src ~dirs ~pkg ]
      else []
    in
    let std_pages =
      List.concat (mld_units @ asset_units @ md_units @ landing_pages)
    in
    let pages =
      match indices_style with
      | Automatic when pkg.name = Monorepo_style.monorepo_pkg_name ->
          let others :> any list = Landing_pages.make_custom dirs pkg in
          others @ std_pages
      | Normal _ | Voodoo | Automatic -> pkg_index () @ src_index () @ std_pages
    in
    {
      pkgname = Some pkg.name;
      scope = scope_of pkg;
      index = Some (index_of pkg);
      libs;
      pages;
    }
  in
  (* The top-level index belongs to no package; its scope is every package and
     library we know of. *)
  let toplevel (page : mld t) : Odoc_unit.pkg =
    let scope =
      {
        pages =
          Util.StringMap.fold (fun p d acc -> dash_p p d :: acc) pkg_paths [];
        libs = Util.StringMap.fold (fun l _ acc -> dash_l l @ acc) lib_dirs [];
      }
    in
    {
      pkgname = None;
      scope;
      index = None;
      libs = [];
      pages = [ (page :> any) ];
    }
  in
  match indices_style with
  | Normal { toplevel_content = None } ->
      let gen_indices = Landing_pages.package_list ~dirs ~remap pkgs in
      toplevel gen_indices :: List.map of_package pkgs
  | Normal { toplevel_content = Some content } ->
      let content ppf = Format.fprintf ppf "%s" content in
      let index =
        Landing_pages.make_index ~dirs
          ~rel_dir:Fpath.(v "./")
          ~enable_warnings:true ~content
      in
      toplevel index :: List.map of_package pkgs
  | Voodoo | Automatic -> List.map of_package pkgs
