(* Voodoo *)

let ( >>= ) = Result.bind

type pkg = {
  name : string;
  version : string;
  universe : string;
  blessed : bool;
  files : Fpath.t list;
}

let prep_path = ref "prep"

let top_dir pkg =
  if pkg.blessed then Fpath.(v "p" / pkg.name / pkg.version)
  else Fpath.(v "u" / pkg.universe / pkg.name / pkg.version)

(* Use output from Voodoo Prep as input *)

let find_universe_and_version pkg_name =
  Bos.OS.Dir.contents Fpath.(v !prep_path / "universes") >>= fun universes ->
  let universe =
    match
      List.find_opt
        (fun u ->
          match Bos.OS.Dir.exists Fpath.(u / pkg_name) with
          | Ok b -> b
          | Error _ -> false)
        universes
    with
    | Some u -> Ok u
    | None -> Error (`Msg (Format.sprintf "Failed to find package %s" pkg_name))
  in
  universe >>= fun u ->
  Bos.OS.Dir.contents ~rel:true Fpath.(u / pkg_name) >>= fun version ->
  match (Fpath.segs u, version) with
  | _ :: _ :: u :: _, [ version ] -> Ok (u, Fpath.to_string version)
  | _ -> Error (`Msg (Format.sprintf "Failed to find package %s" pkg_name))

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

let metas_of_pkg pkg =
  List.filter
    (fun p ->
      let filename = Fpath.filename p in
      filename = "META")
    pkg.files

let of_voodoo pkg =
  let metas = metas_of_pkg pkg in

  let pkg_path =
    Fpath.(v "prep" / "universes" / pkg.universe / pkg.name / pkg.version)
  in

  (* a map from libname to the set of dependencies of that library *)
  let (all_lib_deps, cmi_only_libs) :
      Util.StringSet.t Util.StringMap.t * (Fpath.t * string) list =
    List.fold_left
      (fun (d, c) meta ->
        let full_meta_path = Fpath.(pkg_path // meta) in
        let m = Library_names.process_meta_file full_meta_path in
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

  (* [all_lib_deps] holds the directly-declared META dependencies of each library
     in this package. Libraries outside it are described by no META file we have;
     their dependencies are read back from the markers earlier runs left behind
     (see [extra_paths]), and the two halves are combined into a single graph in
     [Odoc_units_of]. *)
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
    metas
    |> List.filter_map (fun meta_file ->
           let full_meta_path = Fpath.(pkg_path // meta_file) in
           let m = Library_names.process_meta_file full_meta_path in
           let libname_of_archive = Library_names.libname_of_archive m in
           Fpath.Map.iter
             (fun k v -> Logs.debug (fun m -> m "%a,%s\n%!" Fpath.pp k v))
             libname_of_archive;

           let directories = Library_names.directories m in
           Some
             (List.concat_map
                (fun directory ->
                  Logs.debug (fun m ->
                      m "Processing directory: %a\n%!" Fpath.pp directory);
                  Packages.Lib.v ~libname_of_archive ~pkg_name:pkg.name
                    ~dir:directory ~cmtidir:None ~all_lib_deps ~cmi_only_libs
                    ~id_override:None)
                Fpath.(Set.to_list directories)))
    |> List.flatten
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
        Packages.Lib.v ~libname_of_archive ~pkg_name:pkg.name
          ~dir:Fpath.(pkg_path // libdir)
          ~cmtidir:None ~all_lib_deps ~cmi_only_libs:[] ~id_override:None)
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
      selected = true;
      remaps = [];
      other_docs;
      pkg_dir;
      doc_dir;
      config;
    }
  in
  (result, all_lib_deps)

let pp ppf v =
  Format.fprintf ppf "n: %s v: %s u: %s [\n" v.name v.version v.universe;
  List.iter (fun fp -> Format.fprintf ppf "%a\n" Fpath.pp fp) v.files;
  Format.fprintf ppf "]\n%!"

let () = ignore pp

let find_pkg pkg_name ~blessed =
  let contents =
    Bos.OS.Dir.fold_contents ~dotfiles:true
      (fun p acc -> p :: acc)
      []
      Fpath.(v !prep_path)
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

type extra_paths = {
  pkgs : Fpath.t Util.StringMap.t;
  libs : Fpath.t Util.StringMap.t;
  libs_of_pkg : string list Util.StringMap.t;
  lib_deps : Util.StringSet.t Util.StringMap.t;
  virtual_cmtis : (string * Fpath.t) list Util.StringMap.t;
}

let empty_extra_paths =
  {
    pkgs = Util.StringMap.empty;
    libs = Util.StringMap.empty;
    libs_of_pkg = Util.StringMap.empty;
    lib_deps = Util.StringMap.empty;
    virtual_cmtis = Util.StringMap.empty;
  }

(* The package marker carries the library graph of the package that wrote it:
   every library its META files declare, including the archive-less wrapper
   packages that never become libraries of their own and so have no library
   marker. Together, the markers of all previously-compiled packages give the
   graph beyond the package being built now. *)
let read_pkg_marker ~abs_path acc =
  match Bos.OS.File.read abs_path with
  | Error (`Msg msg) ->
      Logs.debug (fun m ->
          m "Failed to read package marker %a: %s" Fpath.pp abs_path msg);
      acc
  | Ok contents -> (
      match Marker.Pkg.of_string contents with
      | None ->
          Logs.debug (fun m ->
              m "Ignoring package marker %a: not of the current format" Fpath.pp
                abs_path);
          acc
      | Some { Marker.Pkg.libraries; pkg_name = _ } ->
          let lib_deps =
            List.fold_left
              (fun acc (lib_name, requires) ->
                Util.StringMap.add lib_name
                  (Util.StringSet.of_list requires)
                  acc)
              acc.lib_deps libraries
          in
          { acc with lib_deps })

(* Fold the contents of one library marker into the accumulator. A marker we
   cannot parse still tells us that the directory holds a library, which is all
   the marker used to convey; we just learn nothing more about it. *)
let read_lib_marker ~abs_path ~rel_dir acc =
  match Bos.OS.File.read abs_path with
  | Error (`Msg msg) ->
      Logs.debug (fun m ->
          m "Failed to read library marker %a: %s" Fpath.pp abs_path msg);
      acc
  | Ok contents -> (
      match Marker.Lib.of_string contents with
      | None ->
          Logs.debug (fun m ->
              m "Ignoring library marker %a: not of the current format" Fpath.pp
                abs_path);
          acc
      | Some { Marker.Lib.modules; lib_name = _; pkg_name = _ } ->
          let virtual_cmtis =
            List.fold_left
              (fun acc (m : Marker.Lib.module_info) ->
                match m.stashed_cmti with
                | None -> acc
                | Some cmti ->
                    Util.StringMap.update m.digest
                      (fun prev ->
                        Some
                          ((m.m_name, Fpath.(rel_dir / cmti))
                          :: Option.value ~default:[] prev))
                      acc)
              acc.virtual_cmtis modules
          in
          { acc with virtual_cmtis })

let extra_paths compile_dir =
  let contents =
    Bos.OS.Dir.fold_contents ~dotfiles:true
      (fun p acc -> p :: acc)
      [] compile_dir
  in
  let add_lib ~abs_path ~rel_dir ~pkg ~libname acc =
    Logs.debug (fun m -> m "Found lib marker: %a" Fpath.pp rel_dir);
    let acc =
      {
        acc with
        libs = Util.StringMap.add libname rel_dir acc.libs;
        libs_of_pkg =
          Util.StringMap.update pkg
            (function
              | None -> Some [ libname ] | Some l -> Some (libname :: l))
            acc.libs_of_pkg;
      }
    in
    read_lib_marker ~abs_path ~rel_dir acc
  in
  let add_pkg ~abs_path ~rel_dir ~pkg acc =
    Logs.debug (fun m -> m "Found pkg marker: %a" Fpath.pp rel_dir);
    let acc = { acc with pkgs = Util.StringMap.add pkg rel_dir acc.pkgs } in
    read_pkg_marker ~abs_path acc
  in
  match contents with
  | Error _ -> empty_extra_paths
  | Ok c ->
      List.fold_left
        (fun acc abs_path ->
          let path = Fpath.rem_prefix compile_dir abs_path |> Option.get in
          let rel_dir = Fpath.parent path in
          match Fpath.segs path with
          | [ "p"; pkg; _version; "doc"; libname; l ]
          | [ "u"; _; pkg; _version; "doc"; libname; l ]
            when l = Marker.Lib.filename ->
              add_lib ~abs_path ~rel_dir ~pkg ~libname acc
          | [ "p"; pkg; _version; "doc"; l ]
          | [ "u"; _; pkg; _version; "doc"; l ]
            when l = Marker.Pkg.filename ->
              add_pkg ~abs_path ~rel_dir ~pkg acc
          | _ -> acc)
        empty_extra_paths c

let write_markers odoc_dir ~lib_graph pkgs =
  let write file str =
    match Bos.OS.File.write file str with
    | Ok () -> ()
    | Error (`Msg msg) -> Logs.err (fun m -> m "Failed to write marker: %s" msg)
  in
  let libraries =
    Util.StringMap.bindings lib_graph
    |> List.map (fun (name, deps) -> (name, Util.StringSet.elements deps))
  in
  List.iter
    (fun (pkg : Packages.t) ->
      let pkg_path = Odoc_unit.doc_dir pkg in
      write
        Fpath.(odoc_dir // pkg_path / Marker.Pkg.filename)
        (Marker.Pkg.to_string { pkg_name = pkg.name; libraries });
      List.iter
        (fun (lib : Packages.libty) ->
          let lib_dir = Odoc_unit.lib_dir pkg lib in
          write
            Fpath.(odoc_dir // lib_dir / Marker.Lib.filename)
            (Marker.Lib.to_string (Marker.Lib.of_lib ~pkg_name:pkg.name lib)))
        pkg.libraries)
    pkgs
