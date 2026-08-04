open Odoc_unit

type indices_style =
  | Voodoo
  | Normal of { toplevel_content : string option }
  | Automatic

let packages ~dirs ~extra_paths ~remap ~indices_style (pkgs : Packages.t list) :
    any list =
  let { odoc_dir; odocl_dir; index_dir; mld_dir = _ } = dirs in

  let extra_libs_paths = extra_paths.Voodoo.libs in
  let extra_libs_of_pkg = extra_paths.Voodoo.libs_of_pkg in
  let extra_pkg_paths = extra_paths.Voodoo.pkgs in

  let lib_dirs =
    let open Packages in
    let lds = extra_libs_paths in
    List.fold_left
      (fun lds pkg ->
        List.fold_left
          (fun lds lib ->
            let lib_dir = lib_dir pkg lib in
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

  let dash_p pkgname path = (pkgname, path) in

  let dash_l lib_name =
    match Util.StringMap.find_opt lib_name lib_dirs with
    | Some dir -> [ (lib_name, dir) ]
    | None ->
        Logs.debug (fun m -> m "Library %s not found" lib_name);
        []
  in
  (* Inverse of [libs_of_pkg]: which package provides a given library. *)
  let pkg_of_lib =
    Util.StringMap.fold
      (fun pkgname libs acc ->
        List.fold_left
          (fun acc lib -> Util.StringMap.add lib pkgname acc)
          acc libs)
      libs_of_pkg Util.StringMap.empty
  in

  (* The library graph: each library's declared dependencies. The libraries
     being built now are authoritative; the rest come from the markers earlier
     runs left behind, which is the only way voodoo mode -- holding just one
     package's META files -- learns about libraries outside its own package. *)
  let lib_deps_of =
    List.fold_left
      (fun acc pkg ->
        List.fold_left
          (fun acc (lib : Packages.libty) ->
            Util.StringMap.add lib.lib_name lib.lib_deps acc)
          acc pkg.Packages.libraries)
      extra_paths.Voodoo.lib_deps pkgs
  in

  (* Transitive closure over [lib_deps_of]. The entry is memoised /before/
     recursing: a cycle then yields a slightly small closure rather than
     looping. The build graph cannot contain one -- mutually dependent libraries
     could not have been compiled -- but the graph assembled here is not one
     coherent installation, since markers from different versions of a package
     can coexist in a warm odoc dir. *)
  let lib_closure =
    let cache = Hashtbl.create 64 in
    let rec close lib_name =
      match Hashtbl.find_opt cache lib_name with
      | Some deps -> deps
      | None ->
          Hashtbl.add cache lib_name Util.StringSet.empty;
          let direct =
            Option.value ~default:Util.StringSet.empty
              (Util.StringMap.find_opt lib_name lib_deps_of)
          in
          let deps =
            Util.StringSet.fold
              (fun dep acc ->
                Util.StringSet.union (Util.StringSet.add dep (close dep)) acc)
              direct Util.StringSet.empty
          in
          Hashtbl.replace cache lib_name deps;
          deps
    in
    close
  in

  (* The search path ([-I]) shared by a library's units: the directories holding
     the odoc files of the library itself and of its dependency closure -- the
     same directories the compiler was given when it built the library.

     This is deeper than the reference scope below, and deliberately so: it
     resolves each unit's imports, their expansions and its source links, which
     can reach past the directly-declared dependencies. It is kept per library
     rather than shared across the package so that alternative implementations
     of a virtual library never appear on one search path: each publishes a
     module of the same name with the same interface digest, and odoc resolves
     such an import to whichever it finds first. *)
  let includes_of_lib =
    let cache = Hashtbl.create 16 in
    fun lib_name ->
      match Hashtbl.find_opt cache lib_name with
      | Some dirs -> dirs
      | None ->
          let dirs =
            Util.StringSet.fold
              (fun l acc ->
                match Util.StringMap.find_opt l lib_dirs with
                | Some dir -> Fpath.Set.add Fpath.(odoc_dir // dir) acc
                | None ->
                    Logs.debug (fun m ->
                        m "No directory known for library %s, needed by %s" l
                          lib_name);
                    acc)
              (Util.StringSet.add lib_name (lib_closure lib_name))
              Fpath.Set.empty
          in
          Hashtbl.add cache lib_name dirs;
          dirs
  in

  (* Link arguments ([-L]/[-P]) are computed per package, so every unit in
     a package gets the same set. [-L] is the directly-declared library
     dependencies of all the package's libraries (which therefore includes the
     package's own libraries), plus any libraries named in [odoc-config.sexp]
     (directly, or via a named package). We do not take the transitive closure:
     dependencies the META files omit are recovered earlier by digest
     ([Packages.fix_missing_deps]). [-P] is every package that provides one of
     those libraries, plus the package itself and any package named in
     [odoc-config.sexp]. The search path ([-I]) is computed per library
     instead, and is deeper: see [includes_of_lib] above. *)
  let link_args_of =
    let cache = Hashtbl.create 10 in
    fun pkg : Pkg_args.t ->
      match Hashtbl.find_opt cache pkg.Packages.name with
      | Some res -> res
      | None ->
          let {
            Global_config.deps = { packages = cfg_pkgs; libraries = cfg_libs };
          } =
            pkg.Packages.config
          in
          let libs_from_pkg =
            List.fold_left
              (fun acc (lib : Packages.libty) ->
                Util.StringSet.add lib.lib_name
                  (Util.StringSet.union lib.lib_deps acc))
              Util.StringSet.empty pkg.Packages.libraries
          in
          let libs_of_cfg_pkgs =
            List.fold_left
              (fun acc pkgname ->
                match Util.StringMap.find_opt pkgname libs_of_pkg with
                | Some libs ->
                    List.fold_left
                      (fun acc l -> Util.StringSet.add l acc)
                      acc libs
                | None -> acc)
              Util.StringSet.empty cfg_pkgs
          in
          let lib_set =
            Util.StringSet.union libs_from_pkg
              (Util.StringSet.union
                 (Util.StringSet.of_list cfg_libs)
                 libs_of_cfg_pkgs)
          in
          let libs = List.concat_map dash_l (Util.StringSet.to_list lib_set) in
          let pkg_set =
            Util.StringSet.fold
              (fun libname acc ->
                match Util.StringMap.find_opt libname pkg_of_lib with
                | Some p -> Util.StringSet.add p acc
                | None -> acc)
              lib_set
              (Util.StringSet.add pkg.Packages.name
                 (Util.StringSet.of_list cfg_pkgs))
          in
          let pages =
            Util.StringSet.to_list pkg_set
            |> List.filter_map (fun pkgname ->
                   match Util.StringMap.find_opt pkgname pkg_paths with
                   | Some path -> Some (dash_p pkgname path)
                   | None -> None)
          in
          let result = Pkg_args.v ~pages ~libs ~odoc_dir in
          Hashtbl.add cache pkg.Packages.name result;
          result
  in

  let index_of pkg =
    let roots = [ Fpath.( // ) odocl_dir (doc_dir pkg) ] in
    let output_file = Fpath.(index_dir / pkg.name / Odoc.index_filename) in
    let pkg_dir = doc_dir pkg in
    let sidebar =
      let output_file = Fpath.(index_dir / pkg.name / Odoc.sidebar_filename) in
      { output_file; json = false; pkg_dir }
    in
    {
      roots;
      output_file;
      json = false;
      search_dir = doc_dir pkg;
      sidebar = Some sidebar;
    }
  in

  let make_unit ~name ~kind ~rel_dir ~input_file ~pkg ~lib_name ~deps ~includes
      ~enable_warnings ~to_output ~input_copy : _ t =
    let to_output = to_output || not remap in
    (* If we haven't got active remapping, we output everything *)
    let ( // ) = Fpath.( // ) in
    let ( / ) = Fpath.( / ) in
    let pkg_args = link_args_of pkg in
    let parent_id = rel_dir |> Odoc.Id.of_fpath in
    let odoc_file =
      odoc_dir // rel_dir / (String.uncapitalize_ascii name ^ ".odoc")
    in
    (* odoc will uncapitalise the output filename *)
    let odocl_file =
      odocl_dir // rel_dir / (String.uncapitalize_ascii name ^ ".odocl")
    in
    {
      output_dir = odoc_dir;
      pkgname = Some pkg.Packages.name;
      pkg_args;
      lib_name;
      deps;
      includes;
      parent_id;
      input_file;
      input_copy;
      odoc_file;
      odocl_file;
      kind;
      to_output;
      enable_warnings;
      index = Some (index_of pkg);
    }
  in

  let of_intf hidden pkg (lib : Packages.libty) (intf : Packages.intf) : intf t
      =
    let rel_dir = lib_dir pkg lib in
    let kind = `Intf { hidden; hash = intf.mif_hash } in
    let name = intf_unit_name intf in
    (* A virtual library has no archive; stash its interface so that
       implementations compiled by later runs can be pointed at it. *)
    let input_copy =
      match lib.archive_name with
      | Some _ -> None
      | None -> Some Fpath.(odoc_dir // rel_dir / stash_basename intf)
    in
    make_unit ~name ~kind ~rel_dir ~input_file:intf.mif_path ~pkg
      ~lib_name:lib.lib_name ~deps:intf.mif_deps
      ~includes:(includes_of_lib lib.lib_name)
      ~enable_warnings:pkg.selected ~to_output:pkg.selected ~input_copy
  in
  let of_impl pkg (lib : Packages.libty) (impl : Packages.impl) : impl t option
      =
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
          make_unit ~name ~kind ~rel_dir ~input_file:impl.mip_path ~pkg
            ~lib_name:lib.lib_name ~deps:impl.mip_deps
            ~includes:(includes_of_lib lib.lib_name)
            ~enable_warnings:false ~to_output:pkg.selected ~input_copy:None
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
  let of_lib pkg (lib : Packages.libty) =
    let index = index_of pkg in
    let units = List.concat_map (of_module pkg lib) lib.modules in
    if remap && not pkg.selected then units
    else
      let landing_page :> any = Landing_pages.library ~dirs ~pkg ~index lib in
      landing_page :: units
  in
  let of_mld pkg (mld : Packages.mld) : mld t list =
    let open Fpath in
    let { Packages.mld_path; mld_rel_path } = mld in
    let rel_dir = doc_dir pkg // Fpath.parent mld_rel_path |> Fpath.normalize in
    let kind = `Mld in
    let name = mld_path |> Fpath.rem_ext |> Fpath.basename |> ( ^ ) "page-" in
    let unit =
      make_unit ~name ~kind ~rel_dir ~input_file:mld_path ~pkg ~lib_name:""
        ~deps:[] ~includes:Fpath.Set.empty ~enable_warnings:pkg.selected
        ~to_output:pkg.selected ~input_copy:None
    in
    [ unit ]
  in
  let of_md pkg (md : Packages.md) : md t list =
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
          make_unit ~name ~kind ~rel_dir ~input_file:md_path ~pkg ~lib_name:""
            ~deps:[] ~includes:Fpath.Set.empty ~enable_warnings:pkg.selected
            ~to_output:pkg.selected ~input_copy:None
        in
        [ unit ]
    | _ ->
        Logs.debug (fun m ->
            m "Skipping non-markdown doc file %a" Fpath.pp md.md_path);
        []
  in
  let of_asset pkg (asset : Packages.asset) : asset t list =
    let open Fpath in
    let { Packages.asset_path; asset_rel_path } = asset in
    let rel_dir =
      doc_dir pkg // Fpath.parent asset_rel_path |> Fpath.normalize
    in
    let kind = `Asset in
    let unit =
      let name = asset_path |> Fpath.basename |> ( ^ ) "asset-" in
      make_unit ~name ~kind ~rel_dir ~input_file:asset_path ~pkg ~lib_name:""
        ~deps:[] ~includes:Fpath.Set.empty ~enable_warnings:false
        ~to_output:true ~input_copy:None
    in
    [ unit ]
  in

  let of_package (pkg : Packages.t) : any list =
    let lib_units :> any list list = List.map (of_lib pkg) pkg.libraries in
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
      else
        let index = index_of pkg in
        [ Landing_pages.package ~dirs ~pkg ~index ]
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
      then
        let index = index_of pkg in
        [ Landing_pages.src ~dirs ~pkg ~index ]
      else []
    in
    let std_units = mld_units @ asset_units @ md_units @ lib_units in
    match indices_style with
    | Automatic when pkg.name = Monorepo_style.monorepo_pkg_name ->
        let others :> any list =
          Landing_pages.make_custom dirs index_of
            (List.find
               (fun p -> p.Packages.name = Monorepo_style.monorepo_pkg_name)
               pkgs)
        in
        others @ List.concat std_units
    | Normal _ | Voodoo | Automatic ->
        List.concat (pkg_index () :: src_index () :: std_units)
  in
  match indices_style with
  | Normal { toplevel_content = None } ->
      let gen_indices :> any = Landing_pages.package_list ~dirs ~remap pkgs in
      gen_indices :: List.concat_map of_package pkgs
  | Normal { toplevel_content = Some content } ->
      let content ppf = Format.fprintf ppf "%s" content in
      let libs =
        List.concat_map
          (fun pkg -> List.map (fun lib -> (pkg, lib)) pkg.Packages.libraries)
          pkgs
      in
      let index :> any =
        Landing_pages.make_index ~dirs
          ~rel_dir:Fpath.(v "./")
          ~libs ~pkgs ~enable_warnings:true ~content ~index:None
      in
      index :: List.concat_map of_package pkgs
  | Voodoo | Automatic -> List.concat_map of_package pkgs
