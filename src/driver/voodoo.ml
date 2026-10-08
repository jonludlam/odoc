(* Voodoo *)

type pkg = {
  name : string;
  version : string;
  universe : string;
  blessed : bool;
  files : Fpath.t list;
}

(* Where voodoo-prep put the packages' files. *)
let prep = Fpath.v "prep"

let top_dir pkg =
  if pkg.blessed then Fpath.(v "p" / pkg.name / pkg.version)
  else Fpath.(v "u" / pkg.universe / pkg.name / pkg.version)

(* Use output from Voodoo Prep as input *)

(* Given a directory containing for example [a.cma] and [b.cma], this
   function returns a Fpath.Map.t mapping [dir/a.cma -> a] and [dir/b.cma -> b] *)
let libname_of_archives_of_dir dir =
  let files_res = Bos.OS.Dir.contents dir in
  match files_res with
  | Error _ -> Fpath.Map.empty
  | Ok files ->
      List.fold_left
        (fun acc file ->
          let base = Fpath.basename file in
          if Astring.String.is_suffix ~affix:".cma" base then
            let libname = String.sub base 0 (String.length base - 4) in
            Fpath.Map.add Fpath.(dir / libname) libname acc
          else acc)
        Fpath.Map.empty files

let of_voodoo pkg =
  let pkg_path =
    Fpath.(prep / "universes" / pkg.universe / pkg.name / pkg.version)
  in
  let metas =
    List.filter_map
      (fun p ->
        if Fpath.filename p = "META" then
          Some (Library_names.process_meta_file Fpath.(pkg_path // p))
        else None)
      pkg.files
  in

  (* a map from libname to the set of dependencies of that library *)
  let (all_lib_deps, cmi_only_libs) :
      Util.StringSet.t Util.StringMap.t * (Fpath.t * string) list =
    List.fold_left
      (fun (d, c) (m : Library_names.t) ->
        let d' =
          List.fold_left
            (fun acc lib ->
              Util.StringMap.add lib.Library_names.name
                (Util.StringSet.of_list ("stdlib" :: lib.Library_names.deps))
                acc)
            d m.libraries
        in
        let c' =
          List.fold_left
            (fun acc (lib : Library_names.library) ->
              match (lib.archive_name, lib.dir) with
              | None, Some dir ->
                  Logs.debug (fun m -> m "Found cmi_only_lib in dir: %s" dir);
                  (Fpath.(m.meta_dir / dir), lib.name) :: acc
              | None, None -> acc
              | Some _, _ -> acc)
            c m.libraries
        in
        (d', c'))
      (Util.StringMap.empty, []) metas
  in

  (* [all_lib_deps] holds the directly-declared META dependencies of each
     library, as the reference scope wants; the closure, the cone, is taken
     in [Odoc_units_of]. *)
  let ss_pp fmt ss = Format.fprintf fmt "[%d]" (Util.StringSet.cardinal ss) in
  Logs.debug (fun m ->
      m "all_lib_deps: %a\n%!"
        Fmt.(list ~sep:comma (pair ~sep:comma string ss_pp))
        (Util.StringMap.bindings all_lib_deps));

  let docs = Opam.classify_docs pkg_path (Some pkg.name) pkg.files in
  let mlds, assets, other_docs = Packages.mk_mlds docs in

  let config =
    let config_file =
      Fpath.(pkg_path / "doc" / pkg.name / "odoc-config.sexp")
    in
    match Bos.OS.File.read config_file with
    | Error (`Msg msg) ->
        Logs.debug (fun m ->
            m "No config file found: %a\n%s\n%!" Fpath.pp config_file msg);
        Global_config.empty
    | Ok s ->
        Logs.debug (fun m -> m "Config file: %a\n%!" Fpath.pp config_file);
        Global_config.parse s
  in

  Logs.debug (fun m ->
      m "Config.packages: %s\n%!" (String.concat ", " config.deps.packages));
  let meta_libraries : Packages.libty list =
    List.concat_map
      (fun m ->
        let libname_of_archive = Library_names.libname_of_archive m in
        Fpath.Map.iter
          (fun k v -> Logs.debug (fun m -> m "%a,%s" Fpath.pp k v))
          libname_of_archive;
        List.concat_map
          (fun directory ->
            Logs.debug (fun m ->
                m "Processing directory: %a" Fpath.pp directory);
            Packages.Lib.v ~roots:[ pkg_path ] ~libname_of_archive
              ~pkg_name:pkg.name ~dir:directory ~all_lib_deps ~cmi_only_libs)
          (Fpath.Set.to_list (Library_names.directories m)))
      metas
  in

  (* Check the main package lib directory even if there's no meta file *)
  let non_meta_libraries =
    let libdirs_without_meta =
      List.filter
        (fun p ->
          match Fpath.segs p with
          | "lib" :: _ :: _
            when Sys.is_directory Fpath.(pkg_path // p |> to_string) ->
              not
                (List.exists
                   (fun lib ->
                     Fpath.equal
                       Fpath.(to_dir_path lib.Packages.dir)
                       Fpath.(to_dir_path (pkg_path // p)))
                   meta_libraries)
          | _ -> false)
        pkg.files
    in

    Logs.debug (fun m ->
        m "libdirs_without_meta: %a\n%!"
          Fmt.(list ~sep:comma Fpath.pp)
          (List.map (fun p -> Fpath.(pkg_path // p)) libdirs_without_meta));

    Logs.debug (fun m ->
        m "lib dirs: %a\n%!"
          Fmt.(list ~sep:comma Fpath.pp)
          (List.map (fun (lib : Packages.libty) -> lib.dir) meta_libraries));

    List.map
      (fun libdir ->
        let libname_of_archive =
          libname_of_archives_of_dir Fpath.(pkg_path // libdir)
        in
        Logs.debug (fun m ->
            m "Processing directory without META: %a" Fpath.pp libdir);
        Packages.Lib.v ~roots:[ pkg_path ] ~libname_of_archive
          ~pkg_name:pkg.name
          ~dir:Fpath.(pkg_path // libdir)
          ~all_lib_deps ~cmi_only_libs:[])
      libdirs_without_meta
    |> List.flatten
  in
  let libraries = meta_libraries @ non_meta_libraries in
  let pkg_dir = top_dir pkg in
  let doc_dir = Fpath.(pkg_dir / "doc") in
  let result =
    {
      Packages.name = pkg.name;
      version = pkg.version;
      libraries;
      mlds;
      assets;
      other_docs;
      pkg_dir;
      doc_dir;
      config;
    }
  in
  result

let find_pkg pkg_name ~blessed =
  let contents =
    Bos.OS.Dir.fold_contents ~dotfiles:true (fun p acc -> p :: acc) [] prep
  in
  match contents with
  | Error _ -> None
  | Ok c -> (
      let sorted = List.sort (fun p1 p2 -> Fpath.compare p1 p2) c in
      let last, packages =
        List.fold_left
          (fun (cur_opt, acc) file ->
            match Fpath.segs file with
            | "prep" :: "universes" :: u :: p :: v :: (_ :: _ as rest)
              when p = pkg_name -> (
                let file = Fpath.v (Astring.String.concat ~sep:"/" rest) in
                match cur_opt with
                | Some cur
                  when cur.name = p && cur.version = v && cur.universe = u ->
                    (Some { cur with files = file :: cur.files }, acc)
                | _ ->
                    ( Some
                        {
                          name = p;
                          version = v;
                          universe = u;
                          blessed;
                          files = [ file ];
                        },
                      cur_opt :: acc ))
            | _ -> (cur_opt, acc))
          (None, []) sorted
      in
      let packages = List.filter_map (fun x -> x) (last :: packages) in
      match packages with
      | [ package ] -> Some package
      | [] ->
          Logs.err (fun m -> m "No package found for %s" pkg_name);
          None
      | _ ->
          Logs.err (fun m -> m "Multiple packages found for %s" pkg_name);
          None)

let occurrence_file_of_pkg pkg =
  let top_dir = top_dir pkg in
  Fpath.(top_dir / "occurrences-all.odoc-occurrences")
