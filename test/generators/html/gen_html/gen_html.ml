(* let enabled_if_version ver : Gen_backend.sexp list =
  [ List [ Atom ">="; Atom "%{ocaml_version}"; Atom ver ] ]

let enabled_if path =
  let path = Fpath.basename path in
  match path with
  | "labels.odocl" | "recent.odocl" | "recent_impl.odocl" ->
      Some (enabled_if_version "4.08.0")
  | _ -> None *)

let html_target_rule odocl targets : Gen_backend.sexp =
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
                 Atom "html-generate";
                 Atom "--indent";
                 Atom "-o";
                 Atom "html.gen";
                 Atom ("%{dep:" ^ Fpath.to_string odocl ^ "}");
               ]
             :: Gen_backend.gen_targets targets);
        ];
    ]

let () =
  let stanzas =
    Gen_backend.gen_backend_rules "html" html_target_rule Gen_backend.files
  in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas