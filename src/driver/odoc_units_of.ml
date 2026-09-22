open Odoc_unit

type indices_style =
  | Voodoo
  | Normal of { toplevel_content : string option }
  | Automatic

(* Everything known about libraries and packages: those being built and those
   an earlier run built. Paths are absolute. *)
type known = {
  lib_dir : Fpath.t Util.StringMap.t;
      (** library -> directory of its [.odoc] files *)
  lib_pkg : string Util.StringMap.t;  (** library -> package providing it *)
  pkg_dir : Fpath.t Util.StringMap.t;  (** package -> its doc directory *)
  building : Util.StringSet.t;  (** the libraries being built *)
  requires : string -> Util.StringSet.t;  (** direct META requires *)
}

let known ~odoc_dir ~(prebuilt : Prebuilt.t) (pkgs : Packages.t list) =
  let abs p = Fpath.(odoc_dir // p) in
  let lib_dir, lib_pkg =
    Util.StringMap.fold
      (fun lib (pkg, dir) (dirs, pkgs) ->
        (Util.StringMap.add lib (abs dir) dirs, Util.StringMap.add lib pkg pkgs))
      prebuilt.libs
      (Util.StringMap.empty, Util.StringMap.empty)
  in
  let lib_dir, lib_pkg, building, own_requires =
    List.fold_left
      (fun acc (pkg : Packages.t) ->
        List.fold_left
          (fun (dirs, pkgs, building, reqs) (lib : Packages.libty) ->
            ( Util.StringMap.add lib.lib_name (abs (lib_obj_dir pkg lib)) dirs,
              Util.StringMap.add lib.lib_name pkg.name pkgs,
              Util.StringSet.add lib.lib_name building,
              Util.StringMap.add lib.lib_name lib.lib_deps reqs ))
          acc pkg.libraries)
      (lib_dir, lib_pkg, Util.StringSet.empty, Util.StringMap.empty)
      pkgs
  in
  let pkg_dir =
    List.fold_left
      (fun acc (pkg : Packages.t) ->
        Util.StringMap.add pkg.name (abs (doc_dir pkg)) acc)
      (Util.StringMap.map abs prebuilt.pkgs)
      pkgs
  in
  (* The requires of a library being built are known; for any other -- one an
     earlier run built, or one providing no modules, which has no libty -- ask
     findlib, the libraries being installed in the switch in every mode. *)
  let requires =
    let cache = Hashtbl.create 100 in
    fun lib ->
      match Util.StringMap.find_opt lib own_requires with
      | Some deps -> deps
      | None -> (
          match Hashtbl.find_opt cache lib with
          | Some deps -> deps
          | None ->
              let deps =
                match Ocamlfind.direct_deps lib with
                | Ok deps -> deps
                | Error (`Msg msg) ->
                    Logs.debug (fun m ->
                        m "No META dependencies for library '%s': %s" lib msg);
                    Util.StringSet.empty
              in
              Hashtbl.add cache lib deps;
              deps)
  in
  { lib_dir; lib_pkg; pkg_dir; building; requires }

(* What unit construction needs to know: the world of libraries and packages,
   where the output goes, and whether unselected packages are remapped. *)
type ctx = { known : known; dirs : dirs; remap : bool }

(* The libraries a list of requires actually names. A library that provides no
   modules of its own -- [num] forwarding to [num.core], [threads.posix] to
   [threads] -- is only an alias for its own requires, which stand in for it.
   Libraries we know nothing about are dropped. *)
let resolve known names =
  let rec go seen acc = function
    | [] -> acc
    | l :: todo when Util.StringSet.mem l seen -> go seen acc todo
    | l :: todo ->
        let seen = Util.StringSet.add l seen in
        if Util.StringMap.mem l known.lib_dir then
          go seen (Util.StringSet.add l acc) todo
        else go seen acc (Util.StringSet.elements (known.requires l) @ todo)
  in
  go Util.StringSet.empty Util.StringSet.empty (Util.StringSet.elements names)

(* The dependency cone of a library: itself and, transitively, what it
   requires. This is what the compiler saw when building it. *)
let cone known lib =
  let rec go acc = function
    | [] -> acc
    | l :: todo when Util.StringSet.mem l acc -> go acc todo
    | l :: todo ->
        let deps = resolve known (known.requires l) in
        go (Util.StringSet.add l acc) (Util.StringSet.elements deps @ todo)
  in
  go Util.StringSet.empty [ lib ]

let dirs_of known libs =
  Util.StringSet.fold
    (fun l acc ->
      match Util.StringMap.find_opt l known.lib_dir with
      | Some d -> Fpath.Set.add d acc
      | None -> acc)
    libs Fpath.Set.empty
  |> Fpath.Set.elements

(* The reference scope of a package, shared by all its units (see the
   "reference scope" section of driver.mld). [-L] is its own libraries, their
   direct META requires -- not the transitive closure -- and the libraries
   named in its [odoc-config.sexp], directly or through a named package. [-P]
   is the package itself, the packages providing those libraries, and the
   packages named in the config file. *)
let scope_of known (pkg : Packages.t) : scope =
  let { Global_config.deps = { packages = cfg_pkgs; libraries = cfg_libs } } =
    pkg.config
  in
  let libs_of_pkg p =
    Util.StringMap.fold
      (fun lib p' acc -> if p = p' then Util.StringSet.add lib acc else acc)
      known.lib_pkg Util.StringSet.empty
  in
  let named =
    List.fold_left
      (fun acc (lib : Packages.libty) ->
        Util.StringSet.add lib.lib_name (Util.StringSet.union lib.lib_deps acc))
      (Util.StringSet.of_list cfg_libs)
      pkg.libraries
  in
  let named =
    List.fold_left
      (fun acc p -> Util.StringSet.union (libs_of_pkg p) acc)
      named cfg_pkgs
  in
  let libs = resolve known named in
  let pkgs =
    Util.StringSet.fold
      (fun lib acc ->
        match Util.StringMap.find_opt lib known.lib_pkg with
        | Some p -> Util.StringSet.add p acc
        | None -> acc)
      libs
      (Util.StringSet.add pkg.name (Util.StringSet.of_list cfg_pkgs))
  in
  let with_dirs table names =
    Util.StringSet.fold
      (fun name acc ->
        match Util.StringMap.find_opt name table with
        | Some dir -> (name, dir) :: acc
        | None ->
            Logs.debug (fun m -> m "'%s' not found" name);
            acc)
      names []
  in
  {
    page_roots = with_dirs known.pkg_dir pkgs;
    lib_roots = with_dirs known.lib_dir libs;
  }

let index_of ~dirs (pkg : Packages.t) : index =
  {
    index_file = Fpath.(dirs.index_dir / pkg.name / Odoc.index_filename);
    sidebar_file = Fpath.(dirs.index_dir / pkg.name / Odoc.sidebar_filename);
    html_dir = doc_dir pkg;
  }

(* [rel_dir] is the unit's parent id, which fixes its identifier and URL.
   [obj_dir] is where its [.odoc]/[.odocl] files go; for modules this is the
   library's mirrored object directory, for pages it is [rel_dir]. *)
let make_unit ctx ~name ~kind ~rel_dir ~obj_dir ~input_file ~enable_warnings
    ~to_output ~stash_input : _ t =
  let to_output = to_output || not ctx.remap in
  (* If we haven't got active remapping, we output everything *)
  let ( // ) = Fpath.( // ) in
  let ( / ) = Fpath.( / ) in
  let name = String.uncapitalize_ascii name in
  (* odoc uncapitalises the output filename *)
  let odoc_file = ctx.dirs.odoc_dir // obj_dir / (name ^ ".odoc") in
  let odocl_file = ctx.dirs.odocl_dir // obj_dir / (name ^ ".odocl") in
  let input_copy =
    if stash_input then Some (ctx.dirs.odoc_dir // obj_dir / (name ^ ".cmti"))
    else None
  in
  {
    parent_id = Odoc.Id.of_fpath rel_dir;
    input_file;
    input_copy;
    odoc_file;
    odocl_file;
    kind;
    to_output;
    enable_warnings;
  }

let of_intf ctx (pkg : Packages.t) (lib : Packages.libty)
    (m : Packages.modulety) : intf t =
  let intf = m.m_intf in
  let kind =
    `Intf { hidden = m.m_hidden; hash = intf.mif_hash; deps = intf.mif_deps }
  in
  let name = intf.mif_path |> Fpath.rem_ext |> Fpath.basename in
  make_unit ctx ~name ~kind ~rel_dir:(lib_dir pkg lib)
    ~obj_dir:(lib_obj_dir pkg lib) ~input_file:intf.mif_path
    ~enable_warnings:pkg.selected ~to_output:pkg.selected
    ~stash_input:(lib.archive_name = None)

let of_impl ctx (pkg : Packages.t) lib (impl : Packages.impl) : impl t option =
  match impl.mip_src_info with
  | None -> None
  | Some { src_path } ->
      let src_id =
        Fpath.(src_lib_dir pkg lib / filename src_path) |> Odoc.Id.of_fpath
      in
      let name = "impl-" ^ (impl.mip_path |> Fpath.rem_ext |> Fpath.basename) in
      Some
        (make_unit ctx ~name
           ~kind:(`Impl { src_id; src_path })
           ~rel_dir:(lib_dir pkg lib) ~obj_dir:(lib_obj_dir pkg lib)
           ~input_file:impl.mip_path ~enable_warnings:false
           ~to_output:pkg.selected ~stash_input:false)

let of_lib ctx (pkg : Packages.t) (lib : Packages.libty) : Odoc_unit.lib =
  let units =
    List.concat_map
      (fun (m : Packages.modulety) ->
        let i = of_intf ctx pkg lib m in
        let impl = Option.bind m.m_impl (of_impl ctx pkg lib) in
        (i :> module_unit) :: (Option.to_list impl :> module_unit list))
      lib.modules
  in
  let requires =
    resolve ctx.known lib.lib_deps
    |> Util.StringSet.inter ctx.known.building
    |> Util.StringSet.remove lib.lib_name
    |> Util.StringSet.elements
  in
  let includes = dirs_of ctx.known (cone ctx.known lib.lib_name) in
  { lib_name = lib.lib_name; requires; includes; units }

(* A page of the package's documentation: the parent id follows its path below
   the doc directory, and so does the file. *)
let of_doc ctx (pkg : Packages.t) ~kind ~name ~rel_path ~file ~enable_warnings
    ~to_output =
  let rel_dir = Fpath.(doc_dir pkg // parent rel_path |> normalize) in
  make_unit ctx ~name ~kind ~rel_dir ~obj_dir:rel_dir ~input_file:file
    ~enable_warnings ~to_output ~stash_input:false

let of_mld ctx (pkg : Packages.t) (mld : Packages.mld) : mld t =
  of_doc ctx pkg ~kind:`Mld
    ~name:("page-" ^ (mld.mld_path |> Fpath.rem_ext |> Fpath.basename))
    ~rel_path:mld.mld_rel_path ~file:mld.mld_path ~enable_warnings:pkg.selected
    ~to_output:pkg.selected

let of_md ctx (pkg : Packages.t) (md : Packages.md) : md t option =
  if Fpath.has_ext ".md" md.md_path then
    Some
      (of_doc ctx pkg ~kind:`Md
         ~name:("page-" ^ (md.md_path |> Fpath.rem_ext |> Fpath.basename))
         ~rel_path:md.md_rel_path ~file:md.md_path ~enable_warnings:pkg.selected
         ~to_output:pkg.selected)
  else (
    Logs.debug (fun m ->
        m "Skipping non-markdown doc file %a" Fpath.pp md.md_path);
    None)

let of_asset ctx (pkg : Packages.t) (asset : Packages.asset) : asset t =
  of_doc ctx pkg ~kind:`Asset
    ~name:("asset-" ^ Fpath.basename asset.asset_path)
    ~rel_path:asset.asset_rel_path ~file:asset.asset_path ~enable_warnings:false
    ~to_output:true

(* The landing pages the driver writes for a package: one for the package
   (unless it ships its own index.mld), one per library, one for the sources.
   In monorepo mode, one per directory instead. *)
let landing_pages ctx ~indices_style (pkg : Packages.t) : mld t list =
  if ctx.remap && not pkg.selected then []
  else
    match indices_style with
    | Automatic when pkg.name = Monorepo_style.monorepo_pkg_name ->
        Landing_pages.make_custom ctx.dirs pkg
        @ List.map (Landing_pages.library ~dirs:ctx.dirs ~pkg) pkg.libraries
    | Normal _ | Voodoo | Automatic ->
        let has_index_page =
          List.exists
            (fun (mld : Packages.mld) ->
              Fpath.equal
                (Fpath.normalize mld.mld_rel_path)
                (Fpath.v "index.mld"))
            pkg.mlds
        in
        let has_sources =
          List.exists
            (fun (lib : Packages.libty) ->
              List.exists
                (fun (m : Packages.modulety) ->
                  match m.m_impl with
                  | Some { mip_src_info = Some _; _ } -> true
                  | _ -> false)
                lib.modules)
            pkg.libraries
        in
        (if has_index_page then []
         else [ Landing_pages.package ~dirs:ctx.dirs ~pkg ])
        @ (if has_sources then [ Landing_pages.src ~dirs:ctx.dirs ~pkg ] else [])
        @ List.map (Landing_pages.library ~dirs:ctx.dirs ~pkg) pkg.libraries

let of_package ctx ~indices_style (pkg : Packages.t) : Odoc_unit.pkg =
  let pages =
    (landing_pages ctx ~indices_style pkg :> page list)
    @ (List.map (of_mld ctx pkg) pkg.mlds :> page list)
    @ (List.filter_map (of_md ctx pkg) pkg.other_docs :> page list)
    @ (List.map (of_asset ctx pkg) pkg.assets :> page list)
  in
  {
    pkgname = Some pkg.name;
    scope = scope_of ctx.known pkg;
    index = Some (index_of ~dirs:ctx.dirs pkg);
    libs = List.map (of_lib ctx pkg) pkg.libraries;
    pages;
  }

(* The top-level index belongs to no package; its scope is every package and
   library we know of. *)
let toplevel known (page : mld t) : Odoc_unit.pkg =
  let scope =
    {
      page_roots = Util.StringMap.bindings known.pkg_dir;
      lib_roots = Util.StringMap.bindings known.lib_dir;
    }
  in
  { pkgname = None; scope; index = None; libs = []; pages = [ (page :> page) ] }

let packages ~dirs ~prebuilt ~remap ~indices_style (pkgs : Packages.t list) :
    pkg list =
  let known = known ~odoc_dir:dirs.odoc_dir ~prebuilt pkgs in
  let ctx = { known; dirs; remap } in
  let built = List.map (of_package ctx ~indices_style) pkgs in
  match indices_style with
  | Normal { toplevel_content = None } ->
      built @ [ toplevel known (Landing_pages.package_list ~dirs ~remap pkgs) ]
  | Normal { toplevel_content = Some content } ->
      let content ppf = Format.fprintf ppf "%s" content in
      let page =
        Landing_pages.make_index ~dirs ~rel_dir:(Fpath.v "./")
          ~enable_warnings:true ~content
      in
      built @ [ toplevel known page ]
  | Voodoo | Automatic -> built
