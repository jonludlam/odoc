let die s =
  prerr_endline s;
  exit 1

let html_target_rule path =
  [
    "odoc-html";
    "generate";
    "--indent";
    "--flat";
    "--extra-suffix";
    "gen";
    "-o";
    ".";
    Gen_rules_lib.Dune.arg_dep path;
  ]

let html_targets_cmd path =
  [ "odoc-html"; "targets"; "-o"; "."; Gen_rules_lib.Dune.arg_dep path ]

let latex_target_rule path =
  [
    "odoc";
    "latex-generate";
    "-o";
    ".";
    "--extra-suffix";
    "gen";
    Gen_rules_lib.Dune.arg_dep path;
  ]

let latex_targets_cmd path =
  [ "odoc"; "latex-targets"; "-o"; "."; Gen_rules_lib.Dune.arg_dep path ]

let man_target_rule path =
  [
    "odoc";
    "man-generate";
    "-o";
    ".";
    "--extra-suffix";
    "gen";
    Gen_rules_lib.Dune.arg_dep path;
  ]

let man_targets_cmd path =
  [ "odoc"; "man-targets"; "-o"; "."; Gen_rules_lib.Dune.arg_dep path ]

let markdown_target_rule path =
  [
    "odoc";
    "markdown-generate";
    "-o";
    ".";
    "--extra-suffix";
    "gen";
    Gen_rules_lib.Dune.arg_dep path;
  ]

let markdown_targets_cmd path =
  [ "odoc"; "markdown-targets"; "-o"; "."; Gen_rules_lib.Dune.arg_dep path ]

(** Returns filenames, not paths. *)
let read_files_from_dir dir =
  let arr = Sys.readdir (Fpath.to_string dir) in
  Array.sort String.compare arr;
  Array.to_list arr

let constraints =
  let open Gen_rules_lib in
  [
    ("stop_dead_link_doc.mli", Min "4.04");
    ("bugs_post_406.mli", Min "4.06");
    ("ocamlary.mli", Min "4.14");
    ("recent.mli", Min "5.4");
    ("labels.mli", Min "4.09");
    ("recent_impl.ml", Min "4.09");
    ("bugs_pre_410.ml", Max "4.09");
    ("module_type_subst.mli", Min "4.13");
    ("class_comments.mli", Min "4.08");
    ("functor_ml.ml", Min "4.14");
    ("ocaml_55.mli", Min "5.5");
  ]

let test_cases_dir = Fpath.v "cases"

(** Make a test cases or return the empty list if the given file should be
    ignored. Might abort the program with an error. *)
let make_test_case case_name =
  let input = Fpath.( / ) test_cases_dir case_name in
  let mk odoc_prefix cmt_suffix =
    let base_out_path = Fpath.v (odoc_prefix ^ case_name) in
    let cmt =
      match cmt_suffix with
      | Some suf -> Some (Fpath.set_ext suf base_out_path)
      | None -> None
    in
    let odoc = Fpath.set_ext ".odoc" base_out_path in
    let odocl = Fpath.set_ext ".odocl" base_out_path in
    let enabledif =
      try Some (List.assoc case_name constraints)
      with Not_found -> Some (Min "4.04")
    in
    { Gen_rules_lib.input; cmt; odoc; odocl; enabledif }
  in
  match Fpath.get_ext input with
  | ".ml" -> [ mk "" (Some ".cmt") ]
  | ".mli" -> [ mk "" (Some ".cmti") ]
  | ".mld" -> [ mk "page-" None ]
  (* Dune creates directories starting with a dot, which result in an empty
     extension with Fpath. Also, there's [.ocamlformat]. *)
  | "" -> []
  | ext ->
      die
        (Format.asprintf
           "Don't know what to do with %a because of unrecognized %s extension."
           Fpath.pp input ext)

let read_test_cases () =
  read_files_from_dir test_cases_dir |> List.map make_test_case |> List.concat

let () =
  let cases = read_test_cases () in
  let stanzas =
    Gen_rules_lib.gen_rule
      [
        (html_target_rule, Fpath.v "html", Some "--flat", "odoc-html", html_targets_cmd);
        (latex_target_rule, Fpath.v "latex", None, "odoc", latex_targets_cmd);
        (man_target_rule, Fpath.v "man", None, "odoc", man_targets_cmd);
        (markdown_target_rule, Fpath.v "markdown", None, "odoc", markdown_targets_cmd);
      ]
      cases
  in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
