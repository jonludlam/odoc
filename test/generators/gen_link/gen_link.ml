type sexp = Sexplib0.Sexp.t = Atom of string | List of sexp list

let cu_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom Fpath.(basename target_path) ];
      List [ Atom "deps"; Atom (Fpath.to_string dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "ocamlc";
              Atom "-c";
              Atom "-bin-annot";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let odoc_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom (Fpath.basename target_path) ];
      List [ Atom "deps"; Atom (Fpath.basename dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "compile";
              Atom "--pkg";
              Atom "test";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let odocl_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom (Fpath.basename target_path) ];
      List [ Atom "deps"; Atom (Fpath.basename dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "link";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let mld_odoc_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom (Fpath.basename target_path) ];
      List [ Atom "deps"; Atom (Fpath.to_string dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "compile";
              Atom "--pkg";
              Atom "test";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let mld_odocl_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom (Fpath.to_string target_path) ];
      List [ Atom "deps"; Atom (Fpath.to_string dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "link";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let set_odocl_ext = Fpath.set_ext ".odocl"

let set_odoc_ext = Fpath.set_ext ".odoc"

let file_rule path ext =
  let cm_file = Fpath.set_ext ext path in
  let odoc_file = set_odoc_ext path in
  let odocl_file = set_odocl_ext path in
  [
    cu_target_rule path cm_file;
    odoc_target_rule cm_file odoc_file;
    odocl_target_rule odoc_file odocl_file;
  ]

let mld_file_rule path =
  let path' = Fpath.(v ("page-" ^ basename path)) in
  let odoc_file = set_odoc_ext path' in
  let odocl_file = set_odocl_ext path' in
  [
    mld_odoc_target_rule path odoc_file;
    mld_odocl_target_rule odoc_file odocl_file;
  ]

let die s =
  prerr_endline s;
  exit 1

let path' () f = Filename.quote (Fpath.to_string f)

let ext' () f = Filename.quote (Fpath.get_ext f)

let cases = ref (Fpath.v "cases")

let is_dot_ocamlformat p = Fpath.filename p = ".ocamlformat"

let gen_rule_for_source_file path =
  let ext = Fpath.get_ext path in
  match ext with
  | ".ml" -> file_rule path ".cmt"
  | ".mli" -> file_rule path ".cmti"
  | ".mld" -> mld_file_rule path
  | _ ->
      die
        (Printf.sprintf
           "Don't know what to do with %a because of unrecognized %a extension."
           path' path ext' path)

let html = ref (Fpath.v "html")

let latex = ref (Fpath.v "latex")

let man = ref (Fpath.v "man")

let backends = [ html; latex; man ]

let odocls backend paths =
  paths
  |> List.map (fun p ->
         let path = Fpath.relativize ~root:!backend p in
         match path with
         | Some p -> Printf.sprintf "%%{dep:%s}" (Fpath.to_string p)
         | None -> assert false)
  |> List.map (fun p -> Atom p)

let gen_backend_diff_rule paths =
  List.map
    (fun b ->
      let backend = Fpath.to_string !b in

      List
        [
          Atom "subdir";
          Atom backend;
          List
            [
              Atom "rule";
              List
                [
                  Atom "with-stdout-to";
                  Atom (backend ^ ".dune.inc.gen");
                  List
                    [
                      Atom "pipe-stdout";
                      List
                        (Atom "run"
                         ::
                         Atom ("gen_" ^ backend ^ "/gen_" ^ backend ^ ".exe")
                         :: odocls b paths);
                      List [ Atom "run"; Atom "dune"; Atom "format-dune-file" ];
                    ];
                ];
            ];
        ])
    backends

let diff_rules =
  List.map
    (fun b ->
      let backend = Fpath.to_string !b in

      List
        [
          Atom "rule";
          List [ Atom "alias"; Atom "runtest" ];
          List
            [
              Atom "action";
              List
                [
                  Atom "diff";
                  Atom (backend ^ "/" ^ backend ^ ".dune.inc");
                  Atom (backend ^ "/" ^ backend ^ ".dune.inc.gen");
                ];
            ];
        ])
    backends

let gen_backend_rule paths =
  [ gen_backend_diff_rule paths; diff_rules ] |> List.flatten

let read_file_from_dir dir =
  let filenames =
    let arr = Sys.readdir dir in
    Array.sort String.compare arr;
    Array.to_list arr
  in
  let dir = Fpath.v dir in
  List.map (Fpath.( / ) dir) filenames

let gen_rule paths =
  let paths' =
    List.map
      (fun p ->
        let path = Fpath.relativize ~root:!cases p in
        match path with
        | Some p ->
            if Fpath.get_ext p = ".mld" then
              set_odocl_ext Fpath.(parent p / ("page-" ^ filename p))
            else set_odocl_ext Fpath.(parent p / filename p)
        | None -> assert false)
      paths
  in
  List.flatten
    [
      List.(flatten (map gen_rule_for_source_file paths));
      gen_backend_rule paths';
    ]

let () =
  let paths = read_file_from_dir (Fpath.filename !cases) in
  let paths = List.filter (fun p -> not (is_dot_ocamlformat p)) paths in
  let stanzas = gen_rule paths in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
