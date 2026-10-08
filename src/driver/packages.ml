(* Packages *)

type dep = string * Digest.t

type intf = { mif_hash : string; mif_path : Fpath.t; mif_deps : dep list }

let pp_intf fmt (i : intf) =
  Format.fprintf fmt "@[<hov>{@,mif_hash: %s;@,mif_path: %a;@,mif_deps: %a@,}@]"
    i.mif_hash Fpath.pp i.mif_path
    (Fmt.Dump.list (Fmt.Dump.pair Fmt.string Fmt.string))
    i.mif_deps

type src_info = { src_path : Fpath.t }

let pp_src_info fmt i =
  Format.fprintf fmt "@[<hov>{@,src_path: %a@,}@]" Fpath.pp i.src_path

type impl = { mip_path : Fpath.t; mip_src_info : src_info option }

let pp_impl fmt i =
  Format.fprintf fmt "@[<hov>{@,mip_path: %a;@,mip_src_info: %a@,}@]" Fpath.pp
    i.mip_path
    (Fmt.Dump.option pp_src_info)
    i.mip_src_info

type modulety = {
  m_name : string;
  m_intf : intf;
  m_impl : impl option;
  m_hidden : bool;
}

let pp_modulety fmt i =
  Format.fprintf fmt
    "@[<hov>{@,m_name: %s;@,m_intf: %a;@,m_impl: %a;@,m_hidden: %b@,}@]"
    i.m_name pp_intf i.m_intf (Fmt.Dump.option pp_impl) i.m_impl i.m_hidden

type mld = { mld_path : Fpath.t; mld_rel_path : Fpath.t }

type md = { md_path : Fpath.t; md_rel_path : Fpath.t }

let pp_mld fmt m =
  Format.fprintf fmt "@[<hov>{@,mld_path: %a;@,mld_rel_path: %a@,}@]" Fpath.pp
    m.mld_path Fpath.pp m.mld_rel_path

let pp_md fmt m =
  Format.fprintf fmt "@[<hov>{@,md_path: %a;@,md_rel_path: %a@,}@]" Fpath.pp
    m.md_path Fpath.pp m.md_rel_path

type asset = { asset_path : Fpath.t; asset_rel_path : Fpath.t }

let pp_asset fmt m =
  Format.fprintf fmt "@[<hov>{@,asset_path: %a;@,asset_rel_path: %a@,}@]"
    Fpath.pp m.asset_path Fpath.pp m.asset_rel_path

type libty = {
  lib_name : string;
  dir : Fpath.t;
  rel_dir : Fpath.t;
  archive_name : string option;
  lib_deps : Util.StringSet.t;
  modules : modulety list;
}

let pp_libty fmt l =
  Format.fprintf fmt
    "@[<hov>{@,\
     lib_name: %s;@,\
     dir: %a;@,\
     rel_dir: %a;@,\
     archive_name: %a;@,\
     lib_deps: %a;@,\
     modules: %a@,\
     }@]"
    l.lib_name Fpath.pp l.dir Fpath.pp l.rel_dir
    (Fmt.Dump.option Fmt.string)
    l.archive_name
    (Fmt.list ~sep:Fmt.comma Fmt.string)
    (Util.StringSet.elements l.lib_deps)
    (Fmt.Dump.list pp_modulety)
    l.modules

type t = {
  name : string;
  version : string;
  libraries : libty list;
  mlds : mld list;
  assets : asset list;
  other_docs : md list;
  pkg_dir : Fpath.t;
  doc_dir : Fpath.t;
  config : Global_config.t;
}

(* The documentation of a package this run does not render is on ocaml.org,
   so a link into it is rewritten to point there. Every library of the
   package goes to the same page as the package itself. *)
let remaps t =
  let local_pkg_path = Fpath.to_string (Fpath.to_dir_path t.pkg_dir) in
  let pkg_path =
    Printf.sprintf "https://ocaml.org/p/%s/%s/doc/" t.name t.version
  in
  let lib_paths =
    List.map
      (fun (lib : libty) ->
        (Printf.sprintf "%s%s/" local_pkg_path lib.lib_name, pkg_path))
      t.libraries
  in
  (local_pkg_path, pkg_path) :: lib_paths

let pp fmt t =
  Format.fprintf fmt
    "@[<hov>{@,\
     name: %s;@,\
     version: %s;@,\
     libraries: %a;@,\
     mlds: %a;@,\
     assets: %a;@,\
     other_docs: %a;@,\
     pkg_dir: %a@,\
     }@]"
    t.name t.version (Fmt.Dump.list pp_libty) t.libraries (Fmt.Dump.list pp_mld)
    t.mlds (Fmt.Dump.list pp_asset) t.assets (Fmt.Dump.list pp_md) t.other_docs
    Fpath.pp t.pkg_dir

let maybe_prepend_top top_dir dir =
  match top_dir with None -> dir | Some d -> Fpath.(d // dir)

let pkg_dir top_dir pkg_name = maybe_prepend_top top_dir Fpath.(v pkg_name)

module Module = struct
  type t = modulety

  let pp ppf (t : t) =
    Fmt.pf ppf "name: %s@.intf: %a@.impl: %a@.hidden: %b@." t.m_name Fpath.pp
      t.m_intf.mif_path (Fmt.option pp_impl) t.m_impl t.m_hidden

  let is_hidden name = Astring.String.is_infix ~affix:"__" name

  let vs dir modules =
    let mk m_name =
      let exists ext =
        let p =
          Fpath.(dir // add_ext ext (v (String.uncapitalize_ascii m_name)))
        in
        let upperP =
          Fpath.(dir // add_ext ext (v (String.capitalize_ascii m_name)))
        in
        Logs.debug (fun m ->
            m "Checking %a (then %a)" Fpath.pp p Fpath.pp upperP);
        match Bos.OS.File.exists p with
        | Ok true -> Some p
        | _ -> (
            match Bos.OS.File.exists upperP with
            | Ok true -> Some upperP
            | _ -> None)
      in
      let mk_intf mif_path =
        match Odoc.compile_deps mif_path with
        | Ok { digest; deps } ->
            { mif_hash = digest; mif_path; mif_deps = deps }
        | Error _ -> failwith "bad deps"
      in
      let mk_impl mip_path =
        let mip_src_info =
          match Ocamlobjinfo.get_source mip_path [ dir ] with
          | None ->
              Logs.debug (fun m -> m "No source found for module %s" m_name);
              None
          | Some src_path ->
              Logs.debug (fun m ->
                  m "Found source file %a for %s" Fpath.pp src_path m_name);
              Some { src_path }
        in
        { mip_src_info; mip_path }
      in
      let state = (exists "cmt", exists "cmti") in

      let m_hidden = is_hidden m_name in
      try
        let r (m_intf, m_impl) = Some { m_name; m_intf; m_impl; m_hidden } in
        match state with
        | Some cmt, Some cmti -> r (mk_intf cmti, Some (mk_impl cmt))
        | Some cmt, None -> r (mk_intf cmt, Some (mk_impl cmt))
        | None, Some cmti -> r (mk_intf cmti, None)
        | None, None ->
            Logs.info (fun m -> m "No files for module: %s" m_name);
            None
      with _ ->
        Logs.err (fun m -> m "Error processing module %s. Ignoring." m_name);
        None
    in

    Eio.Fiber.List.filter_map mk modules
end

module Lib = struct
  (* The object directory of a library -- where its [cmi]/[cmt] files live --
     relative to the first of [roots] that contains it. The [.odoc] files of
     the library are written to the same relative path below the odoc
     directory, so that the directory structure of compiled documentation
     mirrors that of the compiled objects. Libraries installed in a single
     directory (e.g. the [compiler-libs.*] family) therefore share an odoc
     directory too, and the [-L] of any one of them finds every one of them,
     just as the compiler's [-I] does. *)
  let rel_dir ~roots dir =
    let dir = Fpath.normalize dir in
    match List.find_map (fun root -> Fpath.rem_prefix root dir) roots with
    | Some rel -> rel
    | None -> (
        if Fpath.is_rel dir then dir
        else
          (* Not below any root: drop the leading '/' and use the path as is. *)
          match Fpath.segs dir with
          | "" :: segs -> Fpath.v (String.concat "/" segs)
          | _ -> dir)

  (* What a library requires, as its META declared it. A library found
     without one, which is how the compiler's own libraries are found in a
     switch with no ocamlfind, declares nothing; it is still compiled against
     the standard library, and saying so is what lets its modules resolve the
     signatures they are constrained by. *)
  let lib_deps_of all_lib_deps lib_name =
    match Util.StringMap.find_opt lib_name all_lib_deps with
    | Some deps -> Util.StringSet.add "stdlib" deps
    | None -> Util.StringSet.singleton "stdlib"

  let handle_virtual_lib ~roots ~dir ~lib_name ~all_lib_deps =
    let modules =
      match
        Bos.OS.Dir.fold_contents
          (fun p acc ->
            if Fpath.has_ext "cmti" p then
              let m_name = Fpath.rem_ext p |> Fpath.basename in
              m_name :: acc
            else acc)
          [] dir
      with
      | Ok x -> x
      | Error (`Msg e) ->
          Logs.err (fun m -> m "Error reading dir %a: %s" Fpath.pp dir e);
          []
    in
    let modules = Module.vs dir modules in
    let lib_deps = lib_deps_of all_lib_deps lib_name in
    let rel_dir = rel_dir ~roots dir in
    [ { lib_name; archive_name = None; modules; lib_deps; dir; rel_dir } ]

  let v ~roots ~libname_of_archive ~pkg_name ~dir ~all_lib_deps ~cmi_only_libs =
    Logs.debug (fun m ->
        m "Classifying dir %a for package %s" Fpath.pp dir pkg_name);
    let results = Odoc.classify [ dir ] in
    let rel_dir = rel_dir ~roots dir in
    match List.length results with
    | 0 -> (
        match List.assoc_opt dir cmi_only_libs with
        | None -> []
        | Some lib_name ->
            handle_virtual_lib ~roots ~dir ~lib_name ~all_lib_deps)
    | _ ->
        Logs.debug (fun m -> m "Got %d lines" (List.length results));
        let of_libraries =
          List.filter_map
            (fun (archive_name, modules) ->
              match
                Fpath.Map.find Fpath.(dir / archive_name) libname_of_archive
              with
              | Some lib_name -> Some (lib_name, archive_name, modules)
              | None ->
                  Logs.info (fun m ->
                      m "No entry for '%a' in libname_of_archive" Fpath.pp
                        Fpath.(dir / archive_name));
                  Logs.info (fun m ->
                      m "Unable to determine library of archive %s: Ignoring."
                        archive_name);
                  None)
            results
        in
        (* An archive may bundle another's modules: ocamloptcomp holds all of
           ocamlmiddleend's. [odoc classify] says so, reporting the module
           under both, but a module is documented once, so each is given to
           the largest library that holds it. The bundling archive is the one
           that ships as a library people can depend on -- findlib declares
           compiler-libs.optcomp and has no name at all for the middle end --
           and the smaller archive is a component of it. Without a rule the
           two libraries write the same file and whichever ran last decides,
           which lost every page of the loser. *)
        let owner =
          List.fold_left
            (fun acc (lib_name, _, modules) ->
              let size = List.length modules in
              List.fold_left
                (fun acc m ->
                  match Util.StringMap.find_opt m acc with
                  | Some (_, best) when best >= size -> acc
                  | _ -> Util.StringMap.add m (lib_name, size) acc)
                acc modules)
            Util.StringMap.empty of_libraries
        in
        List.filter_map
          (fun (lib_name, archive_name, modules) ->
            let modules =
              List.filter
                (fun m ->
                  match Util.StringMap.find_opt m owner with
                  | Some (owner, _) -> owner = lib_name
                  | None -> true)
                modules
            in
            match modules with
            | [] ->
                Logs.info (fun m ->
                    m
                      "Every module of library %s is also in a larger library \
                       in the same directory, which is the one that ships; it \
                       has nothing of its own to document"
                      lib_name);
                None
            | _ ->
                let modules = Module.vs dir modules in
                let lib_deps = lib_deps_of all_lib_deps lib_name in
                Some
                  {
                    lib_name;
                    archive_name = Some archive_name;
                    modules;
                    lib_deps;
                    dir;
                    rel_dir;
                  })
          of_libraries

  let pp ppf t =
    Fmt.pf ppf "archive: %a modules: [@[<hov 2>@,%a@]@,]"
      Fmt.(option string)
      t.archive_name
      Fmt.(list ~sep:sp Module.pp)
      t.modules
end

(* Construct the list of mlds and assets from a package name and its list of pages *)
let mk_mlds docs =
  List.fold_left
    (fun (mlds, assets, others) (doc : Opam.doc_file) ->
      match doc.kind with
      | `Mld ->
          ( { mld_path = doc.file; mld_rel_path = doc.rel_path } :: mlds,
            assets,
            others )
      | `Asset ->
          ( mlds,
            { asset_path = doc.file; asset_rel_path = doc.rel_path } :: assets,
            others )
      | `Other ->
          ( mlds,
            assets,
            { md_path = doc.file; md_rel_path = doc.rel_path } :: others ))
    ([], [], []) docs

let of_packages ~packages_dir packages =
  Logs.app (fun m -> m "Deciding which packages to build...");
  let deps =
    if packages = [] then Opam.all_opam_packages () else Opam.deps packages
  in

  let Ocamlfind.Db.{ libname_of_archive; cmi_only_libs; all_lib_deps; _ } =
    Ocamlfind.Db.create ()
  in

  let opam_map, _opam_rmap = Opam.pkg_to_dir_map () in
  let roots = Opam.install_roots () in

  let ps =
    List.filter_map
      (fun pkg ->
        match
          List.find_opt
            (fun (pkg', _) -> pkg.Opam.name = pkg'.Opam.name)
            opam_map
        with
        | None ->
            Logs.warn (fun m ->
                m "Didn't find package %a in opam_map" Opam.pp pkg);
            None
        | x -> x)
      deps
  in

  let orig =
    List.filter_map
      (fun pkg ->
        List.find_opt (fun (pkg', _) -> pkg = pkg'.Opam.name) opam_map)
      packages
  in

  let all = orig @ ps in
  let all =
    List.sort_uniq
      (fun (a, _) (b, _) -> String.compare a.Opam.name b.Opam.name)
      all
  in

  Logs.app (fun m -> m "Performing module-level dependency analysis...");

  let packages =
    List.map
      (fun (pkg, files) ->
        let libraries =
          List.fold_left
            (fun acc dir ->
              Lib.v ~roots ~libname_of_archive ~pkg_name:pkg.Opam.name ~dir
                ~all_lib_deps ~cmi_only_libs
              @ acc)
            []
            (files.Opam.libs |> Fpath.Set.to_list)
        in
        let pkg_dir = pkg_dir packages_dir pkg.name in
        let config =
          match files.odoc_config with
          | None -> Global_config.empty
          | Some f -> Global_config.load f
        in
        let mlds, assets, _ = mk_mlds files.docs in
        {
          name = pkg.name;
          version = pkg.version;
          libraries;
          mlds;
          assets;
          other_docs = [];
          pkg_dir;
          doc_dir = pkg_dir;
          config;
        })
      all
  in
  Logs.debug (fun m -> m "Packages: %a" Fmt.Dump.(list pp) packages);
  packages

(* A virtual library's implementations ship [.cmt] files only. Their
   interface is the virtual library's [.cmti], which has the same digest, so
   that is the interface they are given. *)
let remap_virtual all =
  let cmtis =
    List.fold_left
      (fun acc pkg ->
        List.fold_left
          (fun acc lib ->
            List.fold_left
              (fun acc m ->
                if Fpath.has_ext "cmti" m.m_intf.mif_path then
                  Util.StringMap.add_to_list m.m_intf.mif_hash m.m_intf acc
                else acc)
              acc lib.modules)
          acc pkg.libraries)
      Util.StringMap.empty all
  in
  let cmti_of hash =
    match Util.StringMap.find_opt hash cmtis with
    | Some [ x ] -> Some x
    | _ -> None
  in
  let remap m =
    if Fpath.has_ext "cmt" m.m_intf.mif_path then
      match cmti_of m.m_intf.mif_hash with
      | Some m_intf -> { m with m_intf }
      | None -> m
    else m
  in
  List.map
    (fun pkg ->
      {
        pkg with
        libraries =
          List.map
            (fun lib -> { lib with modules = List.map remap lib.modules })
            pkg.libraries;
      })
    all
