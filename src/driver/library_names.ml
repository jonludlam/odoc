open Bos

(* The libraries a META file defines, read with findlib's parser rather than
   through a findlib configuration. *)

type library = {
  name : string;
  archive_name : string option;
  dir : string option;
  deps : string list;
}

type t = { meta_dir : Fpath.t; libraries : library list }

let library_of_pkg_defs ~library_name pkg_defs =
  let archive_filename =
    (* Try the plain [byte]/[native] archives first, then the [ppx_driver]
       variants. ppx derivers such as [ppxlib.traverse] and [ppxlib.metaquot]
       declare their archive only under the [ppx_driver] predicate; without
       this they'd be dropped here and later re-discovered by the no-META
       fallback, which names a library after its [.cma] file (e.g.
       [ppxlib_traverse] instead of [ppxlib.traverse]). Mirrors
       [Ocamlfind.archives]. *)
    let lookup preds =
      try Some (Fl_metascanner.lookup "archive" preds pkg_defs) with _ -> None
    in
    List.find_map lookup
      [
        [ "byte" ];
        [ "native" ];
        [ "byte"; "ppx_driver" ];
        [ "native"; "ppx_driver" ];
      ]
  in

  let deps =
    try
      let deps_str = Fl_metascanner.lookup "requires" [] pkg_defs in
      (* Space-separated library names. *)
      Astring.String.fields ~empty:false deps_str
    with _ -> []
  in

  let dir =
    List.find_opt (fun d -> d.Fl_metascanner.def_var = "directory") pkg_defs
  in
  let dir = Option.map (fun d -> d.Fl_metascanner.def_value) dir in
  let archive_name =
    Option.bind archive_filename (fun a ->
        let file_name_len = String.length a in
        if file_name_len > 0 then Some (Filename.chop_extension a) else None)
  in
  { name = library_name; archive_name; dir; deps }

let process_meta_file file =
  Logs.debug (fun m -> m "Reading %a" Fpath.pp file);
  let meta_dir = Fpath.parent file in
  let meta =
    OS.File.with_ic file (fun ic () -> Fl_metascanner.parse ic) ()
    |> Result.get_ok
  in
  let base_library_name =
    if Fpath.basename file = "META" then Fpath.parent file |> Fpath.basename
    else Fpath.get_ext file
  in
  (* A library, and the sub-libraries its [package] stanzas define. *)
  let rec libraries name (pkg_expr : Fl_metascanner.pkg_expr) =
    library_of_pkg_defs ~library_name:name pkg_expr.pkg_defs
    :: List.concat_map
         (fun (sub, e) -> libraries (name ^ "." ^ sub) e)
         pkg_expr.pkg_children
  in
  let is_not_private (lib : library) =
    not (List.mem "__private__" (String.split_on_char '.' lib.name))
  in
  {
    meta_dir;
    libraries = List.filter is_not_private (libraries base_library_name meta);
  }

let dir { meta_dir; _ } lib =
  match lib.dir with
  | None | Some "" -> meta_dir
  | Some sub -> Fpath.(meta_dir // v sub)

let libname_of_archive t =
  List.fold_left
    (fun acc (x : library) ->
      match x.archive_name with
      | None -> acc
      | Some archive_name ->
          Fpath.Map.update
            Fpath.(dir t x / archive_name)
            (function
              | None -> Some x.name
              | Some y ->
                  Logs.err (fun m ->
                      m "Multiple libraries for archive %s: %s and %s."
                        archive_name x.name y);
                  Some y)
            acc)
    Fpath.Map.empty t.libraries

let directories t =
  List.fold_left
    (fun acc lib ->
      match lib.dir with
      | None | Some "" -> Fpath.Set.add t.meta_dir acc
      | Some _ -> (
          let dir = dir t lib in
          (* A META may name a directory that is not installed. topkg points
             at a ../topkg-care directory that belongs to another package; and
             a package built without an optional dependency still declares the
             sub-library it did not build, as fmt declares fmt.cli when
             cmdliner was absent. Either way there is nothing to document, but
             say so: a library silently missing from the output is hard to
             account for later. *)
          match OS.Dir.exists dir with
          | Ok true -> Fpath.Set.add dir acc
          | _ ->
              Logs.info (fun m ->
                  m
                    "Library %s says its files are in %a, which does not \
                     exist, so it will not be documented"
                    lib.name Fpath.pp dir);
              acc))
    Fpath.Set.empty t.libraries
