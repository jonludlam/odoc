open Odoc__odoc

let resolve_and_substitute ~env ~output input_file read_file =
  let filename = Fs.File.to_string input_file in
  match read_file ~filename with
  | Error e -> failwith (Model.Error.to_string e)
  | Ok unit ->
    Printf.printf "INITIAL\n=======\n\n";
    let oc = open_out "initial.sexp" in
    let sexp = Print.Lang.sexp_of_compilation_unit_t unit in
    output_string oc (Sexplib.Sexp.to_string_hum sexp);
    close_out oc;
    let unit = Xref.Lookup.lookup unit in
    Printf.printf "POST INITIAL LOOKUP\n";
    Printf.printf "===================\n\n";
    let oc = open_out "post_lookup.sexp" in
    let sexp = Print.Lang.sexp_of_compilation_unit_t unit in
    output_string oc (Sexplib.Sexp.to_string_hum sexp);
    close_out oc;

    let sexp = Print.Lang.sexp_of_compilation_unit_t unit in
    Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum sexp);
    if not unit.Model.Lang.Compilation_unit.interface then (
      Printf.eprintf "WARNING: not processing the \"interface\" file.%s\n%!"
        (if not (Filename.check_suffix filename "cmt") then "" (* ? *)
         else
          Printf.sprintf
            " Using %S while you should use the .cmti file" filename)
    );
    let resolve_env = Env.build env (`Unit unit) in
    let resolved = Xref.resolve (Env.resolver resolve_env) unit in
    let oc = open_out "post_resolve.sexp" in
    let sexp = Print.Lang.sexp_of_compilation_unit_t resolved in
    output_string oc (Sexplib.Sexp.to_string_hum sexp);
    close_out oc;

    (* [expand unit] fetches [unit] from [env] to get the expansion of local, previously
       defined, elements. We'd rather it got back the resolved bit so we rebuild an
       environment with the resolved unit.
       Note that this is bad and once rewritten expand should not fetch the unit it is
       working on. *)
    let expand_env = Env.build env (`Unit resolved) in
    let expanded = Xref.expand (Env.expander expand_env) resolved in
    let oc = open_out "post_expand.sexp" in
    let sexp = Print.Lang.sexp_of_compilation_unit_t expanded in
    output_string oc (Sexplib.Sexp.to_string_hum sexp);
    close_out oc;

    ignore output;
    ignore expanded;
    ()

let root_of_compilation_unit ~package ~hidden ~module_name ~digest =
  let file_representation : Model.Root.Odoc_file.t =
    Model.Root.Odoc_file.create_unit ~force_hidden:hidden module_name in
  {Model.Root.package; file = file_representation; digest}

let cmti ~env ~package ~hidden ~output input =
  let make_root = root_of_compilation_unit ~package ~hidden in
  let read_file = Loader.read_cmti ~make_root in
  resolve_and_substitute ~env ~output input read_file

let cmt ~env ~package ~hidden ~output input =
  let make_root = root_of_compilation_unit ~package ~hidden in
  let read_file = Loader.read_cmt ~make_root in
  resolve_and_substitute ~env ~output input read_file

let cmi ~env ~package ~hidden ~output input =
  let make_root = root_of_compilation_unit ~package ~hidden in
  let read_file = Loader.read_cmi ~make_root in
  resolve_and_substitute ~env ~output input read_file

let _ =
    let input = Sys.argv.(1) in
    let directories = [] in
    let env =
      Env.create ~important_digests:false ~directories
    in
    let input = Fs.File.of_string input in
    cmti ~env ~package:"pkg" ~hidden:false ~output:() input