open Odoc_unit

type indices_style = Voodoo | Normal of { toplevel_content : string option }

(* Everything known about libraries and packages: those being built, and any
   other through findlib -- the libraries being installed in the switch in
   every mode -- and the layout convention (see [Odoc_unit.lib_obj_dir]).
   Paths are absolute. *)
type known = {
  odoc_root : Fpath.t;  (** the odoc directory *)
  packages : Util.StringSet.t;  (** the packages being built *)
  building : Util.StringSet.t;  (** the libraries being built *)
  lib_dir : string -> Fpath.t option;
      (** library -> directory of its [.odoc] files *)
  lib_pkg : string -> string option;  (** library -> package providing it *)
  requires : string -> Util.StringSet.t;  (** direct META requires *)
}

let memo f =
  let cache = Hashtbl.create 100 in
  fun x ->
    match Hashtbl.find_opt cache x with
    | Some y -> y
    | None ->
        let y = f x in
        Hashtbl.add cache x y;
        y

let known ~odoc_dir (pkgs : Packages.t list) =
  let own =
    List.fold_left
      (fun acc (pkg : Packages.t) ->
        List.fold_left
          (fun acc (lib : Packages.libty) ->
            Util.StringMap.add lib.lib_name (pkg, lib) acc)
          acc pkg.libraries)
      Util.StringMap.empty pkgs
  in
  let building =
    Util.StringMap.fold
      (fun l _ acc -> Util.StringSet.add l acc)
      own Util.StringSet.empty
  in
  let packages =
    List.fold_left
      (fun acc (p : Packages.t) -> Util.StringSet.add p.name acc)
      Util.StringSet.empty pkgs
  in
  let roots = lazy (Opam.install_roots ()) in
  let lib_dir =
    memo (fun lib ->
        match Util.StringMap.find_opt lib own with
        | Some (_, l) -> Some Fpath.(odoc_dir // lib_obj_dir l)
        | None -> (
            match Ocamlfind.get_dir lib with
            | Ok dir ->
                Some
                  Fpath.(
                    odoc_dir
                    // Packages.Lib.rel_dir ~roots:(Lazy.force roots) dir)
            | Error _ -> None))
  in
  (* Findlib does not know which opam package installed a library; opam's
     record of installed files does (and ocaml-docs-ci keeps it for every
     package in a build's stack). Only consulted for libraries outside this
     run. *)
  let installed_by = lazy (snd (Opam.pkg_to_dir_map ())) in
  let lib_pkg =
    memo (fun lib ->
        match Util.StringMap.find_opt lib own with
        | Some (pkg, _) -> Some pkg.name
        | None -> (
            match Ocamlfind.get_dir lib with
            | Error _ -> None
            | Ok dir ->
                Option.map
                  (fun (p : Opam.package) -> p.name)
                  (Fpath.Map.find_opt dir (Lazy.force installed_by))))
  in
  let requires =
    memo (fun lib ->
        match Util.StringMap.find_opt lib own with
        | Some (_, l) -> l.lib_deps
        | None -> (
            match Ocamlfind.direct_deps lib with
            | Ok deps -> deps
            | Error (`Msg msg) ->
                Logs.debug (fun m ->
                    m "No META dependencies for library '%s': %s" lib msg);
                (* Whatever else it needs, it was compiled against this. *)
                Util.StringSet.singleton "stdlib"))
  in
  { odoc_root = odoc_dir; packages; building; lib_dir; lib_pkg; requires }

(* Where a package's pages are, by convention. *)
let pkg_pages_dir known name = Fpath.(known.odoc_root / "doc" / name)

(* What unit construction needs to know: the world of libraries and packages,
   where the output goes, and whether unselected packages are remapped. *)
type ctx = {
  known : known;
  dirs : dirs;
  remap : bool;
  selected : Util.StringSet.t;
}

(* The packages the run was asked for, rather than pulled in as
   dependencies. Their warnings are reported and their pages rendered. *)
let selected ctx (pkg : Packages.t) = Util.StringSet.mem pkg.name ctx.selected

(* Walk [names] and, transitively, the requires of those that do not satisfy
   [keep], collecting those that do. A library the caller does not keep is
   thereby stood in for by its requires. That is how an alias is followed:
   [num] forwards to [num.core], [threads.posix] to [threads]. *)
let close known ~keep names =
  let rec go seen acc = function
    | [] -> acc
    | l :: todo when Util.StringSet.mem l seen -> go seen acc todo
    | l :: todo ->
        let seen = Util.StringSet.add l seen in
        if keep l then go seen (Util.StringSet.add l acc) todo
        else go seen acc (Util.StringSet.elements (known.requires l) @ todo)
  in
  go Util.StringSet.empty Util.StringSet.empty (Util.StringSet.elements names)

(* The libraries a list of requires actually names, among those we know the
   directory of. *)
let resolve known names =
  close known ~keep:(fun l -> Option.is_some (known.lib_dir l)) names

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
      match known.lib_dir l with Some d -> Fpath.Set.add d acc | None -> acc)
    libs Fpath.Set.empty
  |> Fpath.Set.elements

(* The same, keeping each library's name: [odoc compile] is given these with
   [-L] and writes the names into the units, so that a later [odoc link]
   resolving into a unit knows which libraries it could see. *)
let named_dirs_of known libs =
  Util.StringSet.fold
    (fun l acc ->
      match known.lib_dir l with Some d -> (l, d) :: acc | None -> acc)
    libs []
  |> List.sort_uniq compare

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
    Util.StringSet.filter (fun lib -> known.lib_pkg lib = Some p) known.building
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
  (* A library is worth naming as a root if it is being built in this run or
     was built before, that is if its directory exists already. Otherwise it
     stands for the libraries it requires: an alias such as [threads.posix]
     for an in-run [threads], or an optional dependency that is not
     documented at all, which then contributes nothing. *)
  let built_or_building ~building name dir =
    Util.StringSet.mem name building || Bos.OS.Dir.exists dir = Ok true
  in
  let libs =
    close known
      ~keep:(fun l ->
        match known.lib_dir l with
        | Some dir -> built_or_building ~building:known.building l dir
        | None -> false)
      named
  in
  let pkgs =
    Util.StringSet.fold
      (fun lib acc ->
        match known.lib_pkg lib with
        | Some p -> Util.StringSet.add p acc
        | None -> acc)
      libs
      (Util.StringSet.add pkg.name (Util.StringSet.of_list cfg_pkgs))
  in
  let lib_roots =
    Util.StringSet.fold
      (fun lib acc ->
        match known.lib_dir lib with
        | Some dir -> (lib, dir) :: acc
        | None -> acc)
      libs []
  in
  let page_roots =
    Util.StringSet.fold
      (fun p acc ->
        let dir = pkg_pages_dir known p in
        if built_or_building ~building:known.packages p dir then (p, dir) :: acc
        else acc)
      pkgs []
  in
  { page_roots; lib_roots }

(* The driver writes no pages of its own for a package it only remaps. *)
let writes_pages ctx (pkg : Packages.t) = (not ctx.remap) || selected ctx pkg

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
    ~obj_dir:(lib_obj_dir lib) ~input_file:intf.mif_path
    ~enable_warnings:(selected ctx pkg) ~to_output:(selected ctx pkg)
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
           ~rel_dir:(lib_dir pkg lib) ~obj_dir:(lib_obj_dir lib)
           ~input_file:impl.mip_path ~enable_warnings:false
           ~to_output:(selected ctx pkg) ~stash_input:false)

let of_lib ctx (pkg : Packages.t) (lib : Packages.libty) ~requires :
    Odoc_unit.lib =
  let units =
    List.concat_map
      (fun (m : Packages.modulety) ->
        let i = of_intf ctx pkg lib m in
        let impl = Option.bind m.m_impl (of_impl ctx pkg lib) in
        (i :> module_unit) :: (Option.to_list impl :> module_unit list))
      lib.modules
  in
  let includes = named_dirs_of ctx.known (cone ctx.known lib.lib_name) in
  let page =
    if writes_pages ctx pkg then
      Some (Landing_pages.library ~dirs:ctx.dirs ~pkg lib)
    else None
  in
  {
    lib_name = lib.lib_name;
    pkgname = Some pkg.name;
    requires;
    includes;
    units;
    page;
  }

(* Every library of the run, each built after the libraries it requires, so
   that a library holds the libraries themselves rather than their names. The
   requires of a library outside the run stand in for it, aliases in
   particular. Requiring is acyclic, since the compiler could not have built
   a cycle, but a [META] file is data on disk: a library already under way is
   skipped rather than followed again. *)
let libs_of ctx (pkgs : Packages.t list) =
  (* A library name is a findlib name, so one package provides it. Two that
     claim the same name are a broken switch: keep the first, as findlib
     does, and say so. *)
  let sources =
    List.fold_left
      (fun acc (pkg : Packages.t) ->
        List.fold_left
          (fun acc (lib : Packages.libty) ->
            match Util.StringMap.find_opt lib.lib_name acc with
            | Some ((other : Packages.t), _) ->
                Logs.warn (fun m ->
                    m "Library '%s' is provided by both '%s' and '%s'"
                      lib.lib_name other.name pkg.name);
                acc
            | None -> Util.StringMap.add lib.lib_name (pkg, lib) acc)
          acc pkg.libraries)
      Util.StringMap.empty pkgs
  in
  let needs (lib : Packages.libty) =
    close ctx.known
      ~keep:(fun l -> Util.StringSet.mem l ctx.known.building)
      lib.lib_deps
    |> Util.StringSet.remove lib.lib_name
    |> Util.StringSet.elements
  in
  let rec build ~under_way built name =
    if Util.StringMap.mem name built || Util.StringSet.mem name under_way then
      built
    else
      match Util.StringMap.find_opt name sources with
      | None -> built
      | Some (pkg, lib) ->
          let under_way = Util.StringSet.add name under_way in
          let names = needs lib in
          let built = List.fold_left (build ~under_way) built names in
          let requires =
            List.filter_map (fun n -> Util.StringMap.find_opt n built) names
          in
          Util.StringMap.add name (of_lib ctx pkg lib ~requires) built
  in
  Util.StringMap.fold
    (fun name _ built -> build ~under_way:Util.StringSet.empty built name)
    sources Util.StringMap.empty

(* A page of the package's documentation: the parent id follows its path below
   the doc directory, and so does the file. *)
let of_doc ctx (pkg : Packages.t) ~kind ~name ~rel_path ~file ~enable_warnings
    ~to_output =
  let rel_dir = Fpath.(doc_dir pkg // parent rel_path |> normalize) in
  make_unit ctx ~name ~kind ~rel_dir ~obj_dir:(page_obj_dir pkg rel_dir)
    ~input_file:file ~enable_warnings ~to_output ~stash_input:false

let of_mld ctx (pkg : Packages.t) (mld : Packages.mld) : mld t =
  of_doc ctx pkg ~kind:`Mld
    ~name:("page-" ^ (mld.mld_path |> Fpath.rem_ext |> Fpath.basename))
    ~rel_path:mld.mld_rel_path ~file:mld.mld_path
    ~enable_warnings:(selected ctx pkg) ~to_output:(selected ctx pkg)

let of_md ctx (pkg : Packages.t) (md : Packages.md) : md t option =
  if Fpath.has_ext ".md" md.md_path then
    Some
      (of_doc ctx pkg ~kind:`Md
         ~name:("page-" ^ (md.md_path |> Fpath.rem_ext |> Fpath.basename))
         ~rel_path:md.md_rel_path ~file:md.md_path
         ~enable_warnings:(selected ctx pkg) ~to_output:(selected ctx pkg))
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
   (unless it ships its own index.mld) and one for the sources. The page of a
   library is made with the library, in [of_lib], because it is linked with the
   library's search path. *)
let landing_pages ctx (pkg : Packages.t) : mld t list =
  if not (writes_pages ctx pkg) then []
  else
    let has_index_page =
      List.exists
        (fun (mld : Packages.mld) ->
          Fpath.equal (Fpath.normalize mld.mld_rel_path) (Fpath.v "index.mld"))
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
    @ if has_sources then [ Landing_pages.src ~dirs:ctx.dirs ~pkg ] else []

let of_package ctx libs (pkg : Packages.t) : Odoc_unit.pkg =
  let pages =
    (landing_pages ctx pkg :> page list)
    @ (List.map (of_mld ctx pkg) pkg.mlds :> page list)
    @ (List.filter_map (of_md ctx pkg) pkg.other_docs :> page list)
    @ (List.map (of_asset ctx pkg) pkg.assets :> page list)
  in
  {
    pkgname = Some pkg.name;
    scope = scope_of ctx.known pkg;
    index = Some (index_of ~dirs:ctx.dirs pkg);
    libs =
      List.filter_map
        (fun (lib : Packages.libty) ->
          Util.StringMap.find_opt lib.lib_name libs)
        pkg.libraries;
    pages;
  }

(* The top-level index belongs to no package; its scope is every package and
   library we know of. *)
let toplevel known (pkgs : Packages.t list) (page : mld t) : Odoc_unit.pkg =
  let scope =
    {
      page_roots =
        List.map
          (fun (p : Packages.t) -> (p.name, pkg_pages_dir known p.name))
          pkgs;
      lib_roots =
        Util.StringSet.fold
          (fun lib acc ->
            match known.lib_dir lib with
            | Some d -> (lib, d) :: acc
            | None -> acc)
          known.building [];
    }
  in
  { pkgname = None; scope; index = None; libs = []; pages = [ (page :> page) ] }

let packages ~dirs ~remap ~selected ~indices_style (pkgs : Packages.t list) :
    pkg list =
  let known = known ~odoc_dir:dirs.odoc_dir pkgs in
  let ctx = { known; dirs; remap; selected } in
  let libs = libs_of ctx pkgs in
  let built = List.map (of_package ctx libs) pkgs in
  match indices_style with
  | Normal { toplevel_content = None } ->
      built
      @ [
          toplevel known pkgs
            (Landing_pages.package_list ~dirs ~remap ~selected pkgs);
        ]
  | Normal { toplevel_content = Some content } ->
      let content ppf = Format.fprintf ppf "%s" content in
      let page =
        Landing_pages.make_index ~dirs ~rel_dir:(Fpath.v "./")
          ~obj_dir:(Fpath.v "./") ~enable_warnings:true ~content
      in
      built @ [ toplevel known pkgs page ]
  | Voodoo -> built
