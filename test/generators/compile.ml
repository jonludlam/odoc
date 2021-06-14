let die x =
  Format.ksprintf
    (fun s ->
      prerr_endline s;
      exit 1)
    x

let odoc = Bos.Cmd.(v "odoc")

let path f = Filename.quote (Fpath.to_string f)

let cmt_filename f =
  match Fpath.get_ext f with
  | ".ml" -> Fpath.set_ext ".cmt" f
  | ".mli" -> Fpath.set_ext ".cmti" f
  | _ -> die "Don't know what to do with %s" (path f)

let odoc_base_filename f =
  let open Fpath in
  if get_ext f = ".mld" then add_seg (parent f) ("page-" ^ basename f) else f

let odoc_filename f = Fpath.set_ext ".odoc" (odoc_base_filename f)

let odocl_filename f = Fpath.set_ext ".odocl" (odoc_base_filename f)

let run cmd =
  Bos.OS.Cmd.(run_out cmd |> to_lines) |> function
  | Ok l -> l
  | Error (`Msg m) -> die "%s" m

let targets ty file =
  let cmd =
    Bos.Cmd.(
      odoc
      % Printf.sprintf "%s-targets" ty
      % Fpath.to_string file % "-o" % "." % "--extra-suffix" % "gen")
  in
  let cmd = if ty = "html" then Bos.Cmd.(cmd % "--flat") else cmd in
  run cmd

let ocaml_compile =
  let ocamlc = Bos.Cmd.(v "ocamlc" % "-bin-annot" % "-c") in
  fun f ->
    let _lines = run Bos.Cmd.(ocamlc % Fpath.to_string f) in
    ()

let odoc_compile =
  let odoc = Bos.Cmd.(v "odoc" % "compile" % "--package" % "test") in
  fun f ->
    let _lines = run Bos.Cmd.(odoc % Fpath.to_string f) in
    ()

let odoc_link =
  let odoc = Bos.Cmd.(v "odoc" % "link" % "-I" % ".") in
  fun f ->
    let _lines = run Bos.Cmd.(odoc % Fpath.to_string f) in
    ()

let odoc_generate ty test =
  let cmd = Bos.Cmd.(v "odoc" % Printf.sprintf "%s-generate" ty % "-o" % ".") in
  let cmd = if test then Bos.Cmd.(cmd % "--extra-suffix" % "gen") else cmd in
  let cmd =
    if ty = "html" then Bos.Cmd.(cmd % "--flat" % "--indent") else cmd
  in
  fun f ->
    let _lines = run Bos.Cmd.(cmd % Fpath.to_string f) in
    ()

let run test input_files =
  let mld_files, ocaml_files =
    List.partition (fun f -> Fpath.get_ext f = ".mld") input_files
  in
  let cmt_files =
    (* Side effect: error on unknown extension *)
    List.map cmt_filename ocaml_files
  in
  let odoc_files = List.map odoc_filename input_files in
  let odocl_files = List.map odocl_filename input_files in
  List.iter ocaml_compile ocaml_files;
  List.iter odoc_compile (mld_files @ cmt_files);
  List.iter odoc_link odoc_files;
  let files =
    List.fold_left
      (fun acc f ->
        targets "html" f @ targets "man" f @ targets "latex" f @ acc)
      [] odoc_files
  in
  let _ = Sys.command "rm -f *.cmi *.cmo *.cmt*" in
  List.iter (fun x -> Format.printf "expecting: %s\n%!" x) files;
  List.iter (odoc_generate "html" test) odocl_files;
  if Sys.command "odoc support-files -o ." <> 0 then
    die "Failed to generate support-files";
  let dune_file =
    let open Sexplib.Sexp in
    List
      [
        Atom "rule";
        List
          (match files with
          | [] -> die "No targets!"
          | [ file ] -> [ Atom "target"; Atom file ]
          | files -> Atom "targets" :: List.map (fun x -> Atom x) files);
        List
          (Atom "deps"
           :: List.map (fun x -> Atom (Fpath.to_string x)) input_files);
        List
          [
            Atom "action";
            List
              (Atom "run"
               ::
               Atom "compile"
               ::
               Atom "--test"
               :: List.map (fun x -> Atom (Fpath.to_string x)) input_files);
          ];
      ]
  in
  let diffs =
    List.map
      (fun gen_file ->
        let file = Fpath.rem_ext (Fpath.v gen_file) |> Fpath.to_string in
        let open Sexplib.Sexp in
        List
          [
            Atom "rule";
            List [ Atom "alias"; Atom "runtest" ];
            List
              [ Atom "action"; List [ Atom "diff"; Atom file; Atom gen_file ] ];
          ])
      files
  in
  Format.printf "%s\n%!" (Sexplib.Sexp.to_string_hum dune_file);
  List.iter
    (fun sexp -> Format.printf "%s\n%!" (Sexplib.Sexp.to_string_hum sexp))
    diffs

let _ =
  try
    let test, files =
      if Sys.argv.(1) = "--test" then
        (true, List.tl @@ List.tl @@ Array.to_list Sys.argv)
      else (false, List.tl @@ Array.to_list Sys.argv)
    in
    run test (List.map Fpath.v files)
  with _ -> die "Usage: %s [--test] file1 file2 ..." Sys.argv.(0)
