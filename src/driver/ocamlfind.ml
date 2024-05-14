module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let package_to_dir_map () =
  Findlib.init ();
  let packages = Fl_package_base.list_packages () in
  List.map
    (fun pkg_name ->
      let dir = (Fl_package_base.query pkg_name).package_dir in
      (pkg_name, dir))
    packages

let get_dir lib =
  try
    Findlib.init ();
    Fl_package_base.query lib |> fun x ->
    Ok Fpath.(v x.package_dir |> to_dir_path)
  with e ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);
    Error (`Msg "Error getting directory")

let top_libraries () =
  Findlib.init ();
  let packages = Fl_package_base.list_packages () in
  List.fold_left
    (fun acc lib ->
      let package = String.split_on_char '.' lib |> List.hd in
      StringSet.add package acc)
    StringSet.empty packages

let archives pkg =
  Findlib.init ();
  let package = Fl_package_base.query pkg in
  let get_1 preds =
    try
      [
        Fl_metascanner.lookup "archive" preds
          package.Fl_package_base.package_defs;
      ]
    with _ -> []
  in
  match pkg with
  | "stdlib" -> [ "stdlib.cma"; "stdlib.cmxa" ]
  | _ ->
      get_1 [ "native" ] @ get_1 [ "byte" ]
      |> List.filter (fun x -> String.length x > 0)

let sub_libraries top =
  Findlib.init ();
  let packages = Fl_package_base.list_packages () in
  List.fold_left
    (fun acc lib ->
      let package = String.split_on_char '.' lib |> List.hd in
      if package = top then StringSet.add lib acc else acc)
    StringSet.empty packages
  |> StringSet.elements

let dir_to_package_map () =
  let package_to_dir = package_to_dir_map () in
  List.fold_left
    (fun map (pkg_name, dir) ->
      StringMap.update dir
        (function None -> Some [ pkg_name ] | Some l -> Some (pkg_name :: l))
        map)
    StringMap.empty package_to_dir

let deps pkgs =
  try
    let packages = Fl_package_base.requires_deeply ~preds:[] pkgs in
    Ok packages
  with e -> Error (`Msg (Printexc.to_string e))