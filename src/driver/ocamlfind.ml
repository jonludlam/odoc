(* Findlib reads a configuration file that ocamlfind installs, and it need not
   be there. ocaml-docs-ci documents each package in a switch holding only
   that package's dependency closure, and a closure of dune-only packages
   contains no ocamlfind, so loading the configuration fails. Load it once,
   remember whether it was there, and let every query below answer "not known"
   rather than raise. *)
let available =
  let known =
    lazy
      (try
         Findlib.init ();
         true
       with e ->
         Logs.debug (fun m ->
             m "No findlib configuration, so findlib knows no library: %s"
               (Printexc.to_string e));
         false)
  in
  fun () -> Lazy.force known

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
    all_lib_deps : Util.StringSet.t Util.StringMap.t;
    libname_of_archive : string Fpath.map;
    cmi_only_libs : (Fpath.t * string) list;
  }

  let create () =
    let all_libs =
      Util.StringSet.(elements (add "stdlib" (of_list (all ()))))
    in

    (* The directly-declared dependencies of each library. We deliberately keep
       these un-closed: the scope is computed from the direct dependencies,
       and the closure, the cone, is taken later ([Odoc_units_of]). *)
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

    (* For each library, its directory and its archives. *)
    let lib_dirs_and_archives =
      List.filter_map
        (fun lib ->
          match get_dir lib with
          | Error _ ->
              Logs.err (fun m -> m "No dir for library %s" lib);
              None
          | Ok p ->
              let archives =
                List.map
                  (fun x ->
                    try Filename.chop_extension x
                    with e ->
                      Logs.err (fun m -> m "Can't chop extension from %s" x);
                      raise e)
                  (archives lib)
              in
              Some (lib, p, Util.StringSet.of_list archives))
        all_libs
    in

    (* The library each archive, given by its full path, belongs to. *)
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

    (* Libraries with no archive at all, virtual libraries usually. *)
    let cmi_only_libs =
      List.filter_map
        (fun (lib, dir, archives) ->
          if Util.StringSet.is_empty archives then Some (dir, lib) else None)
        lib_dirs_and_archives
      |> List.rev
    in
    { all_lib_deps; libname_of_archive; cmi_only_libs }
end
