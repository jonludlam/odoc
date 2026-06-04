open Odoc_utils
open ResultMonad
open Odoc_model

module H = Odoc_model.Paths.Identifier.Hashtbl.Any
module Id = Odoc_model.Paths.Identifier

let parse_input_file input =
  Fs.File.read input >>= fun content ->
  Ok (List.rev_map Fs.File.of_string (Odoc_utils.String.split_lines content))

let parse_input_files input =
  List.fold_left
    (fun acc file ->
      acc >>= fun acc ->
      parse_input_file file >>= fun files -> Ok (files :: acc))
    (Ok []) input
  >>= fun files -> Ok (List.concat files)

let absolute_normalization p =
  let p =
    if Path.is_relative p then Path.append (Sys.getcwd ()) p else p
  in
  Path.normalize p

let build_hierarchies ~warnings_options ~occurrences ~roots ~inputs_in_file
    ~odocls =
  let handle_warnings f =
    let res = Error.catch_warnings f in
    Error.handle_warnings ~warnings_options res |> Result.join
  in
  handle_warnings @@ fun () ->
  parse_input_files inputs_in_file >>= fun files ->
  let files = List.rev_append odocls files in
  let occurrences =
    match occurrences with
    | None -> None
    | Some occurrences -> Some (Occurrences.read_occurrences occurrences)
  in
  let all_files =
    roots
    |> List.fold_left
         (fun set include_rec ->
           Fs.Directory.fold_files_rec ~ext:"odocl"
             (fun files file ->
               Path.Set.add (absolute_normalization file) files)
             set include_rec)
         Path.Set.empty
    |> fun set -> Path.Set.fold (fun a l -> a :: l) set []
  in
  (* let () = List.iter (Format.printf "%a\n" Path.pp) all_files in *)
  let root_groups =
    (* We group the files we have found by root.

       Some files may belong to multiple roots. In this case, we associate the
       file to the root that is the deepest in the hierarchy.
    *)
    let roots = List.map Fs.Directory.to_string roots in
    let roots = List.map absolute_normalization roots in
    (* Add an index to keep the original order *)
    let roots = List.mapi (fun i c -> (i, c)) roots in
    let roots =
      (* Make sure that we treat first the "deepest" one *)
      List.sort
        (fun (_, p1) (_, p2) ->
          if Path.is_prefix ~root:p1 p2 then 1 else -1)
        roots
    in
    let groups, _ =
      List.fold_left
        (fun (acc, remaining_files) (i, root) ->
          let root_files, remaining_files =
            List.partition (Path.is_prefix ~root) remaining_files
          in
          ((i, root_files) :: acc, remaining_files))
        ([], all_files) roots
    in
    let root_groups =
      List.sort (fun (i, _) (j, _) -> compare i j) groups |> List.map snd
    in
    (* Files given without [--root] are grouped together *)
    match files with
    | _ :: _ -> files :: root_groups
    | [] -> root_groups
  in
  let hierarchies =
    (* For each group, we create a hierarchy. *)
    let hierarchy_of_group g =
      let pages, modules, implementations =
        let read (pages, modules, impls) f =
          match Odoc_file.load f with
          | Ok { content = Page_content p; _ } -> (p :: pages, modules, impls)
          | Ok { content = Unit_content m; _ } -> (pages, m :: modules, impls)
          | Ok { content = Impl_content i; _ } -> (pages, modules, i :: impls)
          | _ -> (pages, modules, impls)
        in
        List.fold_left read ([], [], []) g
      in
      Odoc_index.Skeleton_of.lang ~pages ~modules ~implementations
    in
    List.map hierarchy_of_group root_groups
  in
  Ok (hierarchies, occurrences)

let compile ~output ~warnings_options ~occurrences ~roots ~inputs_in_file
    ~odocls =
  build_hierarchies ~warnings_options ~occurrences ~roots ~inputs_in_file
    ~odocls
  >>= fun (hierarchies, _occurrences) ->
  Ok (Odoc_file.save_index output hierarchies)
