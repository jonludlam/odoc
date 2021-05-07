let man_target_rule odocl targets : Gen_backend.sexp =
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
                 Atom "man-generate";
                 Atom "-o";
                 Atom "man.gen";
                 Atom ("%{dep:" ^ Fpath.to_string odocl ^ "}");
               ]
             :: Gen_backend.gen_targets targets);
        ];
    ]

let () =
  let stanzas =
    Gen_backend.gen_backend_rules "man" man_target_rule Gen_backend.files
  in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
