let walkthrough_dir = "_build/default/test/walkthrough/"
let walkthrough1 = walkthrough_dir ^ ".walkthrough.objs/byte/walkthrough__Test1.cmti"

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
  let resolver = Odoc.Env.resolver resolve_env in
  let result = Xref.resolve resolver unit in
  let tbl = Xref.tbl resolver in
  (result,tbl)

let unit_printer fmt unit = 
  Format.fprintf fmt "%s" (Odoc__print__Print.Lang.sexp_of_compilation_unit_t unit |> Sexplib.Sexp.to_string_hum)


let test = 
  let open Odoc__model in
  let open Lang in
  let root = {Root.package = "walkthrough";
              file =
                Root.Odoc_file.Compilation_unit
                  {Root.Odoc_file.name = "Walkthrough__Test1";
                   hidden = true};
              digest = "abcde"}
  in
  let id = `Root (root, "Walkthrough__Test1") in 
  Compilation_unit.{
    id = id;
    doc = [];
    digest = "abcde";
    imports = Compilation_unit.Import.[
        Unresolved ("CamlinternalFormatBasics", Some "34567");
        Unresolved ("Odoc__alias", Some "67890");
        Unresolved ("Stdlib", Some "23456");
        Unresolved ("Walkthrough", Some "87654")];
    source =
      Some Compilation_unit.Source.{
          file = "test/walkthrough/test1.mli";
          build_dir = "/Users/jon/devel/odoc/_build/default";
          digest = "12345"};
    interface = true;
    hidden = true;
    content =
      Compilation_unit.Module
        [Signature.Module
           (Signature.Ordinary,
            {Module.id = `Module (id, "M");
             doc = [];
             type_ =
               Module.ModuleType
                 (ModuleType.Signature
                    [Signature.ModuleType
                       {ModuleType.id = `ModuleType (`Module (id, "M"), "S");
                        doc = [];
                        expr =
                          Some
                            (ModuleType.Signature
                               [Signature.Type
                                  (Signature.Ordinary,
                                   {TypeDecl.id =
                                      `Type (`ModuleType (`Module (id, "M"), "S"),"t");
                                    doc = [];
                                    equation =
                                      {TypeDecl.Equation.params = [];
                                       private_ = false; manifest = None; constraints = []};
                                    representation = None})]);
                        expansion = Some Module.AlreadyASig};
                     Signature.Module
                       (Signature.Ordinary,
                        {Module.id = `Module (`Module (id,"M"), "T");
                         doc = [];
                         type_ =
                           Module.ModuleType
                             (ModuleType.Path (`Resolved (`Identifier (`ModuleType (`Module (id, "M"), "S")))));
                         canonical = None; hidden = false; display_type = None;
                         expansion = None})]);
             canonical = None; hidden = false; display_type = None;
             expansion = Some Module.AlreadyASig});
         Signature.Type (Signature.Ordinary,
                         {TypeDecl.id = `Type (id,"v");
                          doc = [];
                          equation = TypeDecl.Equation.{
                              params = []; private_ = false;
                              manifest = Some TypeExpr.(
                                  Constr (`Dot (`Dot (`Resolved (`Identifier (`Module (id, "M"))), "T"), "t"), []));
                              constraints = []};
                          representation = None})];
    expansion = None}