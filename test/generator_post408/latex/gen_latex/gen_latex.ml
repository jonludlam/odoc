let latex_target_rule odocl targets : Gen_backend_post408.sexp =
  List
    [
      Atom "rule";
      List
        [
          Atom "action";
          List
            (Atom "progn"
             ::
             List
               [
                 Atom "run";
                 Atom "odoc";
                 Atom "latex-generate";
                 Atom "-o";
                 Atom "latex.gen";
                 Atom ("%{dep:" ^ Fpath.to_string odocl ^ "}");
               ]
             :: Gen_backend_post408.gen_targets targets);
        ];
      List
        [
          Atom "enabled_if";
          List [ Atom "<"; Atom "%{ocaml_version}"; Atom "4.08" ];
        ];
    ]

let () =
  let stanzas =
    Gen_backend_post408.gen_backend_rules "latex" latex_target_rule Gen_backend_post408.files
  in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
