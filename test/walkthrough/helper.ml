let walkthrough_dir = "_build/default/test/walkthrough/"
let walkthrough1 = walkthrough_dir ^ ".walkthrough.objs/walkthrough__Test1.cmti"

let root_of_compilation_unit ~package ~hidden ~module_name ~digest =
  let file_representation : Model.Root.Odoc_file.t =
    Model.Root.Odoc_file.create_unit ~force_hidden:hidden module_name in
  {Model.Root.package; file = file_representation; digest}

let read_cmti input =
  let make_root = root_of_compilation_unit ~package:"walkthrough" ~hidden:false in
  Loader.read_cmti ~make_root ~filename:input

let mkenv () =
  Odoc.Env.create ~important_digests:false ~directories:[]
 
let resolve unit =
  let env = mkenv () in
  let resolve_env = Odoc.Env.build env (`Unit unit) in
  Xref.resolve (Odoc.Env.resolver resolve_env) unit

let unit_printer fmt unit = 
  Format.fprintf fmt "%s" (Odoc__print__Print.Lang.sexp_of_compilation_unit_t unit |> Sexplib.Sexp.to_string_hum)
