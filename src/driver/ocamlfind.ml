(* Findlib reads a configuration file that ocamlfind installs, and it need not
   be there. ocaml-docs-ci documents each package in a switch holding only
   that package's dependency closure, and a closure of dune-only packages
   contains no ocamlfind, so loading the configuration fails. Load it once,
   remember whether it was there, and let every query below answer "not known"
   rather than raise. *)
let available =
  let state = ref None in
  fun () ->
    match !state with
    | Some known -> known
    | None ->
        let known =
          try
            Findlib.init ();
            true
          with e ->
            Logs.debug (fun m ->
                m "No findlib configuration, so findlib knows no library: %s"
                  (Printexc.to_string e));
            false
        in
        state := Some known;
        known

(* What findlib would have answered, reconstructed from what is on disk, for a
   switch that has no ocamlfind in it. Each installed package still ships the
   META that says where its libraries are and what they require, and the
   compiler's own libraries, which have no META of their own, sit below the
   directory [ocamlc -where] prints. Built once, and only if it is needed. *)
let by_hand =
  lazy
    (let tbl = Hashtbl.create 100 in
     let add name dir deps =
       if not (Hashtbl.mem tbl name) then Hashtbl.add tbl name (dir, deps)
     in
     let dirs_under dir =
       match Bos.OS.Dir.contents dir with Ok cs -> cs | Error _ -> []
     in
     (match
        Bos.OS.Cmd.(run_out Bos.Cmd.(v "ocamlc" % "-where") |> to_string)
      with
     | Error _ -> ()
     | Ok where ->
         let where = Fpath.(v (String.trim where) |> to_dir_path) in
         add "stdlib" where [];
         List.iter
           (fun d ->
             match Bos.OS.Dir.exists d with
             | Ok true -> add (Fpath.basename d) (Fpath.to_dir_path d) []
             | _ -> ())
           (dirs_under where));
     List.iter
       (fun d ->
         let meta = Fpath.(d / "META") in
         match Bos.OS.File.exists meta with
         | Ok true ->
             let { Library_names.meta_dir; libraries } =
               Library_names.process_meta_file meta
             in
             List.iter
               (fun (l : Library_names.library) ->
                 let dir =
                   match l.dir with
                   | None | Some "" -> Fpath.to_dir_path meta_dir
                   | Some sub -> Fpath.(meta_dir // v sub |> to_dir_path)
                 in
                 add l.name dir l.deps)
               libraries
         | _ -> ())
       (dirs_under Fpath.(v (Opam.prefix ()) / "lib"));
     Logs.debug (fun m ->
         m "findlib is not configured; %d libraries found on disk"
           (Hashtbl.length tbl));
     tbl)

(* A library of the compiler distribution has no META, so its sub-libraries,
   [compiler-libs.common] and [threads.posix] among them, are named after a
   directory that holds them all. *)
let by_hand_find name =
  let tbl = Lazy.force by_hand in
  match Hashtbl.find_opt tbl name with
  | Some x -> Some x
  | None -> (
      match String.index_opt name '.' with
      | None -> None
      | Some i -> Hashtbl.find_opt tbl (String.sub name 0 i))

let all () =
  if available () then Fl_package_base.list_packages ()
  else Hashtbl.fold (fun name _ acc -> name :: acc) (Lazy.force by_hand) []

(* Where a library's files are. Without findlib, fall back to [lib/<name>]
   under the switch prefix, which is where opam and dune put a library whose
   name has no dots. A directory that is not there is never returned, so the
   caller sees the same "not known" it sees for a library findlib has never
   heard of, which is routine for an optional dependency named in a META
   [requires]. *)
let get_dir lib =
  let not_found = Error (`Msg "Error getting directory") in
  if available () then (
    try
      Fl_package_base.query lib |> fun x ->
      Ok Fpath.(v x.package_dir |> to_dir_path)
    with e ->
      Logs.debug (fun m ->
          m "No findlib directory for '%s': %s" lib (Printexc.to_string e));
      not_found)
  else
    match by_hand_find lib with
    | Some (dir, _) -> Ok dir
    | None ->
        Logs.debug (fun m ->
            m "No directory for library '%s', and findlib is not configured" lib);
        not_found

let archives pkg =
  match pkg with
  | "stdlib" -> [ "stdlib.cma"; "stdlib.cmxa" ]
  | _ when not (available ()) -> []
  | _ -> (
      match Fl_package_base.query pkg with
      | exception e ->
          Logs.debug (fun m ->
              m "No findlib archives for '%s': %s" pkg (Printexc.to_string e));
          []
      | package ->
          let get_1 preds =
            try
              [
                Fl_metascanner.lookup "archive" preds
                  package.Fl_package_base.package_defs;
              ]
            with _ -> []
          in
          get_1 [ "native" ] @ get_1 [ "byte" ]
          @ get_1 [ "native"; "ppx_driver" ]
          @ get_1 [ "byte"; "ppx_driver" ]
          |> List.filter (fun x -> String.length x > 0)
          |> List.sort_uniq String.compare)

let sub_libraries top =
  let packages = all () in
  List.fold_left
    (fun acc lib ->
      let package = String.split_on_char '.' lib |> List.hd in
      if package = top then Util.StringSet.add lib acc else acc)
    Util.StringSet.empty packages

(* Returns deep dependencies for the given package *)
let rec dep =
  let memo = ref Util.StringMap.empty in
  fun pkg ->
    try Util.StringMap.find pkg !memo
    with Not_found -> (
      try
        if not (available ()) then failwith "findlib is not configured";
        let deps = Fl_package_base.requires ~preds:[ "ppx_driver" ] pkg in
        let result =
          List.fold_left
            (fun acc x ->
              match dep x with
              | Ok dep_deps -> Util.StringSet.(union acc (add x dep_deps))
              | Error _ -> acc)
            Util.StringSet.empty deps
        in
        memo := Util.StringMap.add pkg (Ok result) !memo;
        Ok result
      with e ->
        let result = Error (`Msg (Printexc.to_string e)) in
        memo := Util.StringMap.add pkg result !memo;
        result)

let deps pkgs =
  let results = List.map dep pkgs in
  Ok
    (List.fold_left Util.StringSet.union
       (Util.StringSet.singleton "stdlib")
       (List.map (Result.value ~default:Util.StringSet.empty) results))

(* The libraries a library requires directly: its META [requires] field. The
   field is read as written rather than resolved, because
   [Fl_package_base.requires] fails outright when an optional dependency such
   as [faraday-async] is not installed, which would lose the library's other
   dependencies too.

   It is read under the [ppx_driver] predicate, as the compiler does when
   building against the library. Without it, the [requires(-ppx_driver)]
   stanzas of ppx libraries would add the ppx runner ([ppx_deriving], say) to
   the dependencies of a library that merely offers a rewriter; those stanzas
   are for programs using the rewriter, not for the library's modules. *)
let direct_deps pkg =
  if not (available ()) then
    match by_hand_find pkg with
    | Some (_, deps) ->
        Ok (Util.StringSet.add "stdlib" (Util.StringSet.of_list deps))
    | None -> Error (`Msg "findlib is not configured")
  else
    try
      let package = Fl_package_base.query pkg in
      let requires =
        try
          Fl_metascanner.lookup "requires" [ "ppx_driver" ] package.package_defs
        with Not_found -> ""
      in
      Ok
        (Util.StringSet.add "stdlib"
           (Util.StringSet.of_list (Fl_split.in_words requires)))
    with e -> Error (`Msg (Printexc.to_string e))

module Db = struct
  type t = {
    all_libs : Util.StringSet.t;
    all_lib_deps : Util.StringSet.t Util.StringMap.t;
    lib_dirs_and_archives : (string * Fpath.t * Util.StringSet.t) list;
    archives_by_dir : Util.StringSet.t Fpath.map;
    libname_of_archive : string Fpath.map;
    cmi_only_libs : (Fpath.t * string) list;
  }

  let create libs =
    let _ = Opam.prefix () in
    let libs = Util.StringSet.to_seq libs |> List.of_seq in

    (* First, find the complete set of libraries - that is, including all of
       the dependencies of the libraries supplied on the commandline *)
    let all_libs_deps =
      match deps libs with
      | Error (`Msg msg) ->
          Logs.err (fun m -> m "Error finding dependencies: %s" msg);
          Util.StringSet.empty
      | Ok libs -> Util.StringSet.add "stdlib" libs
    in

    let all_libs_set =
      Util.StringSet.union all_libs_deps (Util.StringSet.of_list libs)
    in
    let all_libs = Util.StringSet.elements all_libs_set in

    (* The directly-declared dependencies of each library. We deliberately keep
       these un-closed: -L/-P are computed from the direct dependencies, and
       the closure needed for -I is taken later ([Odoc_units_of]). *)
    let all_lib_deps =
      List.fold_right
        (fun lib_name acc ->
          match direct_deps lib_name with
          | Ok deps -> Util.StringMap.add lib_name deps acc
          | Error (`Msg msg) ->
              Logs.err (fun m ->
                  m
                    "Error finding dependencies of library '%s' through \
                     ocamlfind: %s"
                    lib_name msg);
              acc)
        all_libs Util.StringMap.empty
    in

    (* We also need to find, for each library, the library directory and
       the list of archives for that library *)
    let lib_dirs_and_archives =
      List.filter_map
        (fun lib ->
          match get_dir lib with
          | Error _ ->
              Logs.err (fun m -> m "No dir for library %s" lib);
              None
          | Ok p ->
              let archives = archives lib in
              let archives =
                List.map
                  (fun x ->
                    try Filename.chop_extension x
                    with e ->
                      Logs.err (fun m -> m "Can't chop extension from %s" x);
                      raise e)
                  archives
              in
              let archives = Util.StringSet.(of_list archives) in
              Some (lib, p, archives))
        all_libs
    in

    (* An individual directory may contain multiple libraries, each with
       zero or more archives. We need to know which directories contain
       which archives *)
    let archives_by_dir =
      List.fold_left
        (fun set (_lib, p, archives) ->
          Fpath.Map.update p
            (function
              | Some set -> Some (Util.StringSet.union set archives)
              | None -> Some archives)
            set)
        Fpath.Map.empty lib_dirs_and_archives
    in

    (* Compute the mapping between full path of an archive to the
       name of the libary *)
    let libname_of_archive =
      List.fold_left
        (fun map (lib, dir, archives) ->
          match Util.StringSet.elements archives with
          | [] -> map
          | [ archive ] ->
              Fpath.Map.update
                Fpath.(dir / archive)
                (function
                  | None -> Some lib
                  | Some x ->
                      Logs.info (fun m ->
                          m
                            "Multiple libraries for archive %s: %s and %s. \
                             Arbitrarily picking the latter."
                            archive x lib);
                      Some lib)
                map
          | xs ->
              Logs.err (fun m ->
                  m "multiple archives detected: [%a]"
                    Fmt.(list ~sep:sp string)
                    xs);
              assert false)
        Fpath.Map.empty lib_dirs_and_archives
    in

    (* We also need to know about libraries that have no archives at all
       (these are virtual libraries usually) *)
    let cmi_only_libs =
      List.fold_left
        (fun map (lib, dir, archives) ->
          match Util.StringSet.elements archives with
          | [] -> (dir, lib) :: map
          | _ -> map)
        [] lib_dirs_and_archives
    in
    {
      all_libs = all_libs_set;
      all_lib_deps;
      lib_dirs_and_archives;
      archives_by_dir;
      libname_of_archive;
      cmi_only_libs;
    }
end
