(* Construct the tree from the JS sources *)


let libmap_path = Fpath.v "libmap.sexp"

let read_lib_map file =
  let ic = open_in (Fpath.to_string file) in
  let lines = Util.lines_of_channel ic in
  close_in ic;
  let sexp = Sexplib.Sexp.of_string (String.concat "\n" lines) in
  match sexp with
  | Sexplib.Sexp.List ls ->
    List.filter_map (function | Sexplib.Sexp.List [ Sexplib.Sexp.Atom lib; Sexplib.Sexp.Atom path ] -> Some (lib, Fpath.v path) | _ -> None) ls
  | _ -> []

let rec read_lib hash map libname =
  let path = List.assoc libname map in
  let files = Bos.OS.Dir.fold_contents (fun p acc ->
    p::acc) ~traverse:`None
    [] path in
  match files with
  | Error _ ->
    Format.eprintf "No files found in dir: %a\n%!" Fpath.pp path;
    []
  | Ok files ->
    Format.eprintf "Found files: [%a]" (Fmt.list Fpath.pp) files;
    let deps_file_opt = List.find_opt (fun p -> Fpath.has_ext "libdeps" p) files in
    let deps =
      match deps_file_opt with
      | None ->
        Format.eprintf "XXX no deps file\n%!";
        []
      | Some f ->
        let ic = open_in (Fpath.to_string f) in
        let deps = Util.lines_of_channel ic in
        close_in ic;
        let deps = String.split_on_char ' ' (String.concat " " deps) in
        deps
    in
    let _ =
      List.iter (fun dep -> 
        if Hashtbl.mem hash libname then () else
        let _ = read_lib hash map dep in
        ()) deps in
    let modules = Packages.Lib.v Fpath.(v "docs" // path) Util.StringMap.empty libname path in
    Hashtbl.replace hash libname modules;
    modules


let of_js libs =
  let map = read_lib_map libmap_path in
  List.iter (fun (x,y) -> Format.eprintf "%s %a\n%!" x Fpath.pp y) map;
  let hashtbl = Hashtbl.create 100 in
  List.iter (fun lib -> ignore (read_lib hashtbl map lib)) libs;
  let libs = Hashtbl.fold (fun _ m acc ->  m:: acc) hashtbl [] |> List.flatten in
  let package = Packages.{
    name = "package";
    version = "0.0.0";
    libraries = libs;
    mlds = [];
    mld_odoc_dir = Fpath.v "mld";
    other_docs = Fpath.Set.empty;
  } in
  Util.StringMap.singleton "package" package

  