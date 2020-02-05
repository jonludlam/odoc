# Ocamlary

```ocaml require-package=odoc
(* Prelude *)

#require "odoc.model";;
#require "odoc.xref2";;
#require "odoc.loader";;
#require "odoc.odoc";;

(* Avoid ellipsis *)
#print_depth 1000;;
#print_length 99999;;

open Odoc_model
open Odoc_xref2

let make_root ~module_name ~digest =
  let file = Root.Odoc_file.create_unit ~force_hidden:false module_name in
  { Root.package = ""; file; digest }

let compile_unit unit =
  let builder = Odoc_odoc.Env.create ~important_digests:false ~directories:[] ~open_modules:[] in
  let env = Odoc_odoc.Env.build builder (`Unit unit) in
  Compile.compile env unit

(* Shorten output of ['a with_location] values *)
let with_location_printer print_value pp (v : 'a Location_.with_location) = print_value pp v.value;;
#install_printer with_location_printer;;

(* Shorten output of [Root.t] and avoid printing [digest] *)
let root_printer pp (_ : Root.t) = Format.fprintf pp "<root>";;
#install_printer root_printer;;
```

## Cmti

```ocaml
let compiled = Odoc_loader.read_cmti ~make_root ~filename:".ocamlary.objs/byte/ocamlary.cmti"
let compiled =
  match compiled with
  | Ok c -> c
  | Error _ -> assert false
```

```ocaml
# compiled.Lang.Compilation_unit.content;;
- : Lang.Compilation_unit.content =
Odoc_model.Lang.Compilation_unit.Module
 [Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "You"; `Space; `Word "may"; `Space; `Word "find"; `Space;
          `Word "more"; `Space; `Word "information"; `Space; `Word "about";
          `Space; `Word "this"; `Space; `Word "HTML"; `Space;
          `Word "documentation"; `Space; `Word "renderer"; `Space;
          `Word "at"; `Space;
          `Link
            ("https://github.com/dsheets/ocamlary",
             [`Word "github.com/dsheets/ocamlary"]);
          `Word "."]]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "This"; `Space; `Word "is"; `Space; `Word "some"; `Space;
          `Word "verbatim"; `Space; `Word "text:"; `Space];
       `Verbatim "verbatim"]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "This"; `Space; `Word "is"; `Space; `Word "some"; `Space;
          `Word "verbatim"; `Space; `Word "text:"; `Space];
       `Verbatim "[][df[]]}}"]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "Here"; `Space; `Word "is"; `Space; `Word "some"; `Space;
          `Word "raw"; `Space; `Word "LaTeX:"; `Space;
          `Code_span " $e^{i\\pi} = -1$ "]]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "Here"; `Space; `Word "is"; `Space; `Word "an"; `Space;
          `Word "index"; `Space; `Word "table"; `Space; `Word "of"; `Space;
          `Code_span "Empty"; `Space; `Word "modules:"; `Space];
       `Modules [`Root ("Empty", `TUnknown); `Root ("EmptyAlias", `TUnknown)]]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "Here"; `Space; `Word "is"; `Space; `Word "a"; `Space;
          `Word "table"; `Space; `Word "of"; `Space; `Word "links"; `Space;
          `Word "to"; `Space; `Word "indexes:"; `Space;
          `Reference (`Root ("indexlist", `TUnknown), [])]]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "Here"; `Space; `Word "is"; `Space; `Word "some"; `Space;
          `Word "superscript:"; `Space; `Word "x";
          `Styled (`Superscript, [`Word "2"])]]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "Here"; `Space; `Word "is"; `Space; `Word "some"; `Space;
          `Word "subscript:"; `Space; `Word "x";
          `Styled (`Subscript, [`Word "0"])]]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "Here"; `Space; `Word "are"; `Space; `Word "some"; `Space;
          `Word "escaped"; `Space; `Word "brackets:"; `Space; `Word "{";
          `Space; `Word "["; `Space; `Word "@"; `Space; `Word "]"; `Space;
          `Word "}"]]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "Here"; `Space; `Word "is"; `Space; `Word "some"; `Space;
          `Styled (`Emphasis, [`Word "emphasis"]); `Space;
          `Code_span "followed by code"; `Word "."]]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "An"; `Space; `Word "unassociated"; `Space; `Word "comment"]]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Title, `Label (`Root (<root>, "Ocamlary"), "level-0"),
          [`Word "Level"; `Space; `Word "0"])]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Section, `Label (`Root (<root>, "Ocamlary"), "level-1"),
          [`Word "Level"; `Space; `Word "1"])]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subsection, `Label (`Root (<root>, "Ocamlary"), "level-2"),
          [`Word "Level"; `Space; `Word "2"])]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subsubsection, `Label (`Root (<root>, "Ocamlary"), "level-3"),
          [`Word "Level"; `Space; `Word "3"])]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Paragraph, `Label (`Root (<root>, "Ocamlary"), "level-4"),
          [`Word "Level"; `Space; `Word "4"])]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subparagraph, `Label (`Root (<root>, "Ocamlary"), "level-5"),
          [`Word "Level"; `Space; `Word "5"])]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subparagraph, `Label (`Root (<root>, "Ocamlary"), "level-6"),
          [`Word "Level"; `Space; `Word "6"])]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subparagraph, `Label (`Root (<root>, "Ocamlary"), "level-7"),
          [`Word "Level"; `Space; `Word "7"])]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subparagraph, `Label (`Root (<root>, "Ocamlary"), "level-8"),
          [`Word "Level"; `Space; `Word "8"])]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subparagraph, `Label (`Root (<root>, "Ocamlary"), "level-9"),
          [`Word "Level"; `Space; `Word "9"])]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subsubsection,
          `Label (`Root (<root>, "Ocamlary"), "basic-module-stuff"),
          [`Word "Basic"; `Space; `Word "module"; `Space; `Word "stuff"])]);
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Empty");
    doc =
     [`Paragraph
        [`Word "A"; `Space; `Word "plain,"; `Space; `Word "empty"; `Space;
         `Word "module"];
      `Paragraph
        [`Word "This"; `Space; `Word "module"; `Space; `Word "has"; `Space;
         `Word "a"; `Space; `Word "signature"; `Space; `Word "without";
         `Space; `Word "any"; `Space; `Word "members."]];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature []);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "Empty");
    doc =
     [`Paragraph
        [`Word "An"; `Space; `Word "ambiguous,"; `Space; `Word "misnamed";
         `Space; `Word "module"; `Space; `Word "type"]];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`ModuleType (`Root (<root>, "Ocamlary"), "Empty"), "t");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None})]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "MissingComment");
    doc =
     [`Paragraph
        [`Word "An"; `Space; `Word "ambiguous,"; `Space; `Word "misnamed";
         `Space; `Word "module"; `Space; `Word "type"]];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type
              (`ModuleType (`Root (<root>, "Ocamlary"), "MissingComment"),
               "t");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None})]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subparagraph, `Label (`Root (<root>, "Ocamlary"), "s9000"),
          [`Word "Level"; `Space; `Word "9000"])]);
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "EmptyAlias");
    doc =
     [`Paragraph
        [`Word "A"; `Space; `Word "plain"; `Space; `Word "module"; `Space;
         `Word "alias"; `Space; `Word "of"; `Space; `Code_span "Empty"]];
    type_ =
     Odoc_model.Lang.Module.Alias
      (`Resolved
         (`Identifier (`Module (`Root (<root>, "Ocamlary"), "Empty"))));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subsubsection, `Label (`Root (<root>, "Ocamlary"), "emptySig"),
          [`Word "EmptySig"])]);
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "EmptySig");
    doc =
     [`Paragraph
        [`Word "A"; `Space; `Word "plain,"; `Space; `Word "empty"; `Space;
         `Word "module"; `Space; `Word "signature"]];
    expr = Some (Odoc_model.Lang.ModuleType.Signature []);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "EmptySigAlias");
    doc =
     [`Paragraph
        [`Word "A"; `Space; `Word "plain,"; `Space; `Word "empty"; `Space;
         `Word "module"; `Space; `Word "signature"; `Space; `Word "alias";
         `Space; `Word "of"; `Space];
      `Code_block "EmptySig"; `Paragraph [`Word "(preformatted)."]];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Path
        (`Resolved
           (`Identifier
              (`ModuleType (`Root (<root>, "Ocamlary"), "EmptySig")))));
    expansion = None};
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "ModuleWithSignature");
    doc =
     [`Paragraph
        [`Word "A"; `Space; `Word "plain"; `Space; `Word "module"; `Space;
         `Word "of"; `Space; `Word "a"; `Space; `Word "signature"; `Space;
         `Word "of"; `Space; `Reference (`Root ("EmptySig", `TUnknown), []);
         `Space; `Word "(reference)"]];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Path
        (`Resolved
           (`Identifier
              (`ModuleType (`Root (<root>, "Ocamlary"), "EmptySig")))));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "ModuleWithSignatureAlias");
    doc =
     [`Paragraph
        [`Word "A"; `Space; `Word "plain"; `Space; `Word "module"; `Space;
         `Word "with"; `Space; `Word "an"; `Space; `Word "alias"; `Space;
         `Word "signature"];
      `Tag
        (`Deprecated
           [`Paragraph
              [`Word "I"; `Space; `Word "don't"; `Space; `Word "like";
               `Space; `Word "this"; `Space; `Word "element"; `Space;
               `Word "any"; `Space; `Word "more."]])];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Path
        (`Resolved
           (`Identifier
              (`ModuleType (`Root (<root>, "Ocamlary"), "EmptySigAlias")))));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "One");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "One"), "one");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None})]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "SigForMod");
    doc =
     [`Paragraph
        [`Word "There's"; `Space; `Word "a"; `Space; `Word "signature";
         `Space; `Word "in"; `Space; `Word "a"; `Space; `Word "module";
         `Space; `Word "in"; `Space; `Word "this"; `Space;
         `Word "signature."]];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`ModuleType (`Root (<root>, "Ocamlary"), "SigForMod"),
               "Inner");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.ModuleType
                 {Odoc_model.Lang.ModuleType.id =
                   `ModuleType
                     (`Module
                        (`ModuleType
                           (`Root (<root>, "Ocamlary"), "SigForMod"),
                         "Inner"),
                      "Empty");
                  doc = [];
                  expr = Some (Odoc_model.Lang.ModuleType.Signature []);
                  expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
           canonical = None; hidden = false; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig})]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "SuperSig");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType
              (`ModuleType (`Root (<root>, "Ocamlary"), "SuperSig"),
               "SubSigA");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Comment
                 (`Docs
                    [`Heading
                       (`Subsubsection,
                        `Label
                          (`ModuleType
                             (`ModuleType
                                (`Root (<root>, "Ocamlary"), "SuperSig"),
                              "SubSigA"),
                           "subSig"),
                        [`Word "A"; `Space; `Word "Labeled"; `Space;
                         `Word "Section"; `Space; `Word "Header"; `Space;
                         `Word "Inside"; `Space; `Word "of"; `Space;
                         `Word "a"; `Space; `Word "Signature"])]);
                Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`ModuleType (`Root (<root>, "Ocamlary"), "SuperSig"),
                         "SubSigA"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`ModuleType
                        (`ModuleType (`Root (<root>, "Ocamlary"), "SuperSig"),
                         "SubSigA"),
                      "SubSigAMod");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.ModuleType
                    (Odoc_model.Lang.ModuleType.Signature
                      [Odoc_model.Lang.Signature.Type
                        (Odoc_model.Lang.Signature.Ordinary,
                        {Odoc_model.Lang.TypeDecl.id =
                          `Type
                            (`Module
                               (`ModuleType
                                  (`ModuleType
                                     (`Root (<root>, "Ocamlary"), "SuperSig"),
                                   "SubSigA"),
                                "SubSigAMod"),
                             "sub_sig_a_mod");
                         doc = [];
                         equation =
                          {Odoc_model.Lang.TypeDecl.Equation.params = [];
                           private_ = false; manifest = None;
                           constraints = []};
                         representation = None})]);
                  canonical = None; hidden = false; display_type = None;
                  expansion = Some Odoc_model.Lang.Module.AlreadyASig})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig};
         Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType
              (`ModuleType (`Root (<root>, "Ocamlary"), "SuperSig"),
               "SubSigB");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Comment
                 (`Docs
                    [`Heading
                       (`Subsubsection,
                        `Label
                          (`ModuleType
                             (`ModuleType
                                (`Root (<root>, "Ocamlary"), "SuperSig"),
                              "SubSigB"),
                           "subSig"),
                        [`Word "Another"; `Space; `Word "Labeled"; `Space;
                         `Word "Section"; `Space; `Word "Header"; `Space;
                         `Word "Inside"; `Space; `Word "of"; `Space;
                         `Word "a"; `Space; `Word "Signature"])]);
                Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`ModuleType (`Root (<root>, "Ocamlary"), "SuperSig"),
                         "SubSigB"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig};
         Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType
              (`ModuleType (`Root (<root>, "Ocamlary"), "SuperSig"),
               "EmptySig");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`ModuleType (`Root (<root>, "Ocamlary"), "SuperSig"),
                         "EmptySig"),
                      "not_actually_empty");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig};
         Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType
              (`ModuleType (`Root (<root>, "Ocamlary"), "SuperSig"), "One");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`ModuleType (`Root (<root>, "Ocamlary"), "SuperSig"),
                         "One"),
                      "two");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig};
         Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType
              (`ModuleType (`Root (<root>, "Ocamlary"), "SuperSig"),
               "SuperSig");
           doc = []; expr = Some (Odoc_model.Lang.ModuleType.Signature []);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "For"; `Space; `Word "a"; `Space; `Word "good"; `Space;
          `Word "time,"; `Space; `Word "see"; `Space;
          `Reference
            (`Dot (`Dot (`Root ("SuperSig", `TUnknown), "SubSigA"), "subSig"),
             []);
          `Space; `Word "or"; `Space;
          `Reference
            (`Dot (`Dot (`Root ("SuperSig", `TUnknown), "SubSigB"), "subSig"),
             []);
          `Space; `Word "or"; `Space;
          `Reference (`Dot (`Root ("SuperSig", `TUnknown), "EmptySig"), []);
          `Word "."; `Space; `Word "Section"; `Space;
          `Reference (`Root ("s9000", `TUnknown), []); `Space; `Word "is";
          `Space; `Word "also"; `Space; `Word "interesting."; `Space;
          `Reference (`Root ("EmptySig", `TUnknown), []); `Space; `Word "is";
          `Space; `Word "a"; `Space; `Word "general"; `Space;
          `Word "reference"; `Space; `Word "but"; `Space;
          `Reference (`Root ("emptySig", `TLabel), []); `Space; `Word "is";
          `Space; `Word "the"; `Space; `Word "section"; `Space; `Word "and";
          `Space; `Reference (`Root ("EmptySig", `TModuleType), []); `Space;
          `Word "is"; `Space; `Word "the"; `Space; `Word "module"; `Space;
          `Word "signature."]]);
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "Buffer");
    doc =
     [`Paragraph [`Reference (`Dot (`Root ("Buffer", `TUnknown), "t"), [])]];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Value
          {Odoc_model.Lang.Value.id =
            `Value (`Module (`Root (<root>, "Ocamlary"), "Buffer"), "f");
           doc = [];
           type_ =
            Odoc_model.Lang.TypeExpr.Arrow (None,
             Odoc_model.Lang.TypeExpr.Constr
              (`Dot (`Dot (`Root "Stdlib", "Buffer"), "t"), []),
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType "unit")), []))}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "Some"; `Space; `Word "text"; `Space; `Word "before"; `Space;
          `Word "exception"; `Space; `Word "title."; `Space];
       `Heading
         (`Subsubsection,
          `Label (`Root (<root>, "Ocamlary"), "basic-exception-stuff"),
          [`Word "Basic"; `Space; `Word "exception"; `Space; `Word "stuff"]);
       `Paragraph
         [`Word "After"; `Space; `Word "exception"; `Space; `Word "title."]]);
  Odoc_model.Lang.Signature.Exception
   {Odoc_model.Lang.Exception.id =
     `Exception (`Root (<root>, "Ocamlary"), "Kaboom");
    doc =
     [`Paragraph
        [`Word "Unary"; `Space; `Word "exception"; `Space;
         `Word "constructor"]];
    args =
     Odoc_model.Lang.TypeDecl.Constructor.Tuple
      [Odoc_model.Lang.TypeExpr.Constr
        (`Resolved (`Identifier (`CoreType "unit")), [])];
    res = None};
  Odoc_model.Lang.Signature.Exception
   {Odoc_model.Lang.Exception.id =
     `Exception (`Root (<root>, "Ocamlary"), "Kablam");
    doc =
     [`Paragraph
        [`Word "Binary"; `Space; `Word "exception"; `Space;
         `Word "constructor"]];
    args =
     Odoc_model.Lang.TypeDecl.Constructor.Tuple
      [Odoc_model.Lang.TypeExpr.Constr
        (`Resolved (`Identifier (`CoreType "unit")), []);
       Odoc_model.Lang.TypeExpr.Constr
        (`Resolved (`Identifier (`CoreType "unit")), [])];
    res = None};
  Odoc_model.Lang.Signature.Exception
   {Odoc_model.Lang.Exception.id =
     `Exception (`Root (<root>, "Ocamlary"), "Kapow");
    doc =
     [`Paragraph
        [`Word "Unary"; `Space; `Word "exception"; `Space;
         `Word "constructor"; `Space; `Word "over"; `Space; `Word "binary";
         `Space; `Word "tuple"]];
    args =
     Odoc_model.Lang.TypeDecl.Constructor.Tuple
      [Odoc_model.Lang.TypeExpr.Tuple
        [Odoc_model.Lang.TypeExpr.Constr
          (`Resolved (`Identifier (`CoreType "unit")), []);
         Odoc_model.Lang.TypeExpr.Constr
          (`Resolved (`Identifier (`CoreType "unit")), [])]];
    res = None};
  Odoc_model.Lang.Signature.Exception
   {Odoc_model.Lang.Exception.id =
     `Exception (`Root (<root>, "Ocamlary"), "EmptySig");
    doc =
     [`Paragraph
        [`Reference (`Root ("EmptySig", `TUnknown), []); `Space; `Word "is";
         `Space; `Word "general"; `Space; `Word "but"; `Space;
         `Reference (`Root ("EmptySig", `TModuleType), []); `Space;
         `Word "is"; `Space; `Word "a"; `Space; `Word "module"; `Space;
         `Word "and"; `Space;
         `Reference (`Root ("EmptySig", `TException), []); `Space;
         `Word "is"; `Space; `Word "this"; `Space; `Word "exception."]];
    args = Odoc_model.Lang.TypeDecl.Constructor.Tuple []; res = None};
  Odoc_model.Lang.Signature.Exception
   {Odoc_model.Lang.Exception.id =
     `Exception (`Root (<root>, "Ocamlary"), "EmptySigAlias");
    doc =
     [`Paragraph
        [`Reference (`Root ("EmptySigAlias", `TException), []); `Space;
         `Word "is"; `Space; `Word "this"; `Space; `Word "exception."]];
    args = Odoc_model.Lang.TypeDecl.Constructor.Tuple []; res = None};
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subsubsection,
          `Label
            (`Root (<root>, "Ocamlary"),
             "basic-type-and-value-stuff-with-advanced-doc-comments"),
          [`Word "Basic"; `Space; `Word "type"; `Space; `Word "and"; `Space;
           `Word "value"; `Space; `Word "stuff"; `Space; `Word "with";
           `Space; `Word "advanced"; `Space; `Word "doc"; `Space;
           `Word "comments"])]);
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "a_function");
    doc =
     [`Paragraph
        [`Reference (`Root ("a_function", `TUnknown), []); `Space;
         `Word "is"; `Space; `Word "general"; `Space; `Word "but"; `Space;
         `Reference (`Root ("a_function", `TType), []); `Space; `Word "is";
         `Space; `Word "this"; `Space; `Word "type"; `Space; `Word "and";
         `Space; `Reference (`Root ("a_function", `TValue), []); `Space;
         `Word "is"; `Space; `Word "the"; `Space; `Word "value"; `Space;
         `Word "below."]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None);
        (Odoc_model.Lang.TypeDecl.Var "b", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Arrow (None,
          Odoc_model.Lang.TypeExpr.Var "a", Odoc_model.Lang.TypeExpr.Var "b"));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id =
     `Value (`Root (<root>, "Ocamlary"), "a_function");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "is"; `Space; `Code_span "a_function";
         `Space; `Word "with"; `Space; `Word "param"; `Space; `Word "and";
         `Space; `Word "return"; `Space; `Word "type."];
      `Tag
        (`Param
           ("x",
            [`Paragraph
               [`Word "the"; `Space; `Code_span "x"; `Space;
                `Word "coordinate"]]));
      `Tag
        (`Return
           [`Paragraph
              [`Word "the"; `Space; `Code_span "y"; `Space;
               `Word "coordinate"]])];
    type_ =
     Odoc_model.Lang.TypeExpr.Arrow
      (Some (Odoc_model.Lang.TypeExpr.Label "x"),
      Odoc_model.Lang.TypeExpr.Constr
       (`Resolved (`Identifier (`CoreType "int")), []),
      Odoc_model.Lang.TypeExpr.Constr
       (`Resolved (`Identifier (`CoreType "int")), []))};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id =
     `Value (`Root (<root>, "Ocamlary"), "fun_fun_fun");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved
         (`Identifier (`Type (`Root (<root>, "Ocamlary"), "a_function"))),
      [Odoc_model.Lang.TypeExpr.Constr
        (`Resolved
           (`Identifier (`Type (`Root (<root>, "Ocamlary"), "a_function"))),
        [Odoc_model.Lang.TypeExpr.Constr
          (`Resolved (`Identifier (`CoreType "int")), []);
         Odoc_model.Lang.TypeExpr.Constr
          (`Resolved (`Identifier (`CoreType "int")), [])]);
       Odoc_model.Lang.TypeExpr.Constr
        (`Resolved
           (`Identifier (`Type (`Root (<root>, "Ocamlary"), "a_function"))),
        [Odoc_model.Lang.TypeExpr.Constr
          (`Resolved (`Identifier (`CoreType "unit")), []);
         Odoc_model.Lang.TypeExpr.Constr
          (`Resolved (`Identifier (`CoreType "unit")), [])])])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id =
     `Value (`Root (<root>, "Ocamlary"), "fun_maybe");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Arrow
      (Some (Odoc_model.Lang.TypeExpr.Optional "yes"),
      Odoc_model.Lang.TypeExpr.Constr
       (`Resolved (`Identifier (`CoreType "unit")), []),
      Odoc_model.Lang.TypeExpr.Arrow (None,
       Odoc_model.Lang.TypeExpr.Constr
        (`Resolved (`Identifier (`CoreType "unit")), []),
       Odoc_model.Lang.TypeExpr.Constr
        (`Resolved (`Identifier (`CoreType "int")), [])))};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id =
     `Value (`Root (<root>, "Ocamlary"), "not_found");
    doc =
     [`Tag
        (`Raise
           ("Not_found",
            [`Paragraph
               [`Word "That's"; `Space; `Word "all"; `Space; `Word "it";
                `Space; `Word "does"]]))];
    type_ =
     Odoc_model.Lang.TypeExpr.Arrow (None,
      Odoc_model.Lang.TypeExpr.Constr
       (`Resolved (`Identifier (`CoreType "unit")), []),
      Odoc_model.Lang.TypeExpr.Constr
       (`Resolved (`Identifier (`CoreType "unit")), []))};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id =
     `Value (`Root (<root>, "Ocamlary"), "ocaml_org");
    doc =
     [`Tag
        (`See
           (`Url, "http://ocaml.org/",
            [`Paragraph
               [`Word "The"; `Space; `Word "OCaml"; `Space; `Word "Web";
                `Space; `Word "site"]]))];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "string")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id =
     `Value (`Root (<root>, "Ocamlary"), "some_file");
    doc =
     [`Tag
        (`See
           (`File, "some_file",
            [`Paragraph
               [`Word "The"; `Space; `Word "file"; `Space; `Word "called";
                `Space; `Code_span "some_file"]]))];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "string")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id =
     `Value (`Root (<root>, "Ocamlary"), "some_doc");
    doc =
     [`Tag
        (`See
           (`Document, "some_doc",
            [`Paragraph
               [`Word "The"; `Space; `Word "document"; `Space;
                `Word "called"; `Space; `Code_span "some_doc"]]))];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "string")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id =
     `Value (`Root (<root>, "Ocamlary"), "since_mesozoic");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "value"; `Space; `Word "was"; `Space;
         `Word "introduced"; `Space; `Word "in"; `Space; `Word "the"; `Space;
         `Word "Mesozoic"; `Space; `Word "era."];
      `Tag (`Since "mesozoic")];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id =
     `Value (`Root (<root>, "Ocamlary"), "changing");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "value"; `Space; `Word "has"; `Space;
         `Word "had"; `Space; `Word "changes"; `Space; `Word "in"; `Space;
         `Word "1.0.0,"; `Space; `Word "1.1.0,"; `Space; `Word "and"; `Space;
         `Word "1.2.0."];
      `Tag
        (`Before
           ("1.0.0", [`Paragraph [`Word "before"; `Space; `Word "1.0.0"]]));
      `Tag
        (`Before
           ("1.1.0", [`Paragraph [`Word "before"; `Space; `Word "1.1.0"]]));
      `Tag (`Version "1.2.0")];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id =
     `Value (`Root (<root>, "Ocamlary"), "with_foo");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "value"; `Space; `Word "has"; `Space;
         `Word "a"; `Space; `Word "custom"; `Space; `Word "tag"; `Space;
         `Code_span "foo"; `Word "."; `Space; `Word "@foo"; `Space;
         `Word "the"; `Space; `Word "body"; `Space; `Word "of"; `Space;
         `Word "the"; `Space; `Word "custom"; `Space; `Code_span "foo";
         `Space; `Word "tag"]];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subsubsection,
          `Label (`Root (<root>, "Ocamlary"), "some-operators"),
          [`Word "Some"; `Space; `Word "Operators"])]);
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(~-)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(!)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(@)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "($)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(%)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(^)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(&)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(*)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(-)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(+)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(<)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(>)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(-?)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(/)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(-|)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(:=)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(=)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "(land)");
    doc = [];
    type_ =
     Odoc_model.Lang.TypeExpr.Constr
      (`Resolved (`Identifier (`CoreType "unit")), [])};
  Odoc_model.Lang.Signature.Comment `Stop;
  Odoc_model.Lang.Signature.Comment
   (`Docs [`Paragraph [`Word "I'm"; `Space; `Word "hidden"]]);
  Odoc_model.Lang.Signature.Comment `Stop;
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subsubsection,
          `Label (`Root (<root>, "Ocamlary"), "advanced-module-stuff"),
          [`Word "Advanced"; `Space; `Word "Module"; `Space; `Word "Stuff"])]);
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "CollectionModule");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "CollectionModule"; `Word "."]];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type
              (`Module (`Root (<root>, "Ocamlary"), "CollectionModule"),
               "collection");
           doc =
            [`Paragraph
               [`Word "This"; `Space; `Word "comment"; `Space; `Word "is";
                `Space; `Word "for"; `Space; `Code_span "collection";
                `Word "."]];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type
              (`Module (`Root (<root>, "Ocamlary"), "CollectionModule"),
               "element");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "CollectionModule"),
               "InnerModuleA");
           doc =
            [`Paragraph
               [`Word "This"; `Space; `Word "comment"; `Space; `Word "is";
                `Space; `Word "for"; `Space; `Code_span "InnerModuleA";
                `Word "."]];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "CollectionModule"),
                         "InnerModuleA"),
                      "t");
                  doc =
                   [`Paragraph
                      [`Word "This"; `Space; `Word "comment"; `Space;
                       `Word "is"; `Space; `Word "for"; `Space;
                       `Code_span "t"; `Word "."]];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false;
                    manifest =
                     Some
                      (Odoc_model.Lang.TypeExpr.Constr
                        (`Resolved
                           (`Identifier
                              (`Type
                                 (`Module
                                    (`Root (<root>, "Ocamlary"),
                                     "CollectionModule"),
                                  "collection"))),
                        []));
                    constraints = []};
                  representation = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "CollectionModule"),
                         "InnerModuleA"),
                      "InnerModuleA'");
                  doc =
                   [`Paragraph
                      [`Word "This"; `Space; `Word "comment"; `Space;
                       `Word "is"; `Space; `Word "for"; `Space;
                       `Code_span "InnerModuleA'"; `Word "."]];
                  type_ =
                   Odoc_model.Lang.Module.ModuleType
                    (Odoc_model.Lang.ModuleType.Signature
                      [Odoc_model.Lang.Signature.Type
                        (Odoc_model.Lang.Signature.Ordinary,
                        {Odoc_model.Lang.TypeDecl.id =
                          `Type
                            (`Module
                               (`Module
                                  (`Module
                                     (`Root (<root>, "Ocamlary"),
                                      "CollectionModule"),
                                   "InnerModuleA"),
                                "InnerModuleA'"),
                             "t");
                         doc =
                          [`Paragraph
                             [`Word "This"; `Space; `Word "comment"; `Space;
                              `Word "is"; `Space; `Word "for"; `Space;
                              `Code_span "t"; `Word "."]];
                         equation =
                          {Odoc_model.Lang.TypeDecl.Equation.params = [];
                           private_ = false;
                           manifest =
                            Some
                             (Odoc_model.Lang.TypeExpr.Constr
                               (`Resolved
                                  (`Identifier
                                     (`Type
                                        (`Root (<root>, "Ocamlary"),
                                         "a_function"))),
                               [Odoc_model.Lang.TypeExpr.Constr
                                 (`Resolved (`Identifier (`CoreType "unit")),
                                 []);
                                Odoc_model.Lang.TypeExpr.Constr
                                 (`Resolved (`Identifier (`CoreType "unit")),
                                 [])]));
                           constraints = []};
                         representation = None})]);
                  canonical = None; hidden = false; display_type = None;
                  expansion = Some Odoc_model.Lang.Module.AlreadyASig});
                Odoc_model.Lang.Signature.ModuleType
                 {Odoc_model.Lang.ModuleType.id =
                   `ModuleType
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "CollectionModule"),
                         "InnerModuleA"),
                      "InnerModuleTypeA'");
                  doc =
                   [`Paragraph
                      [`Word "This"; `Space; `Word "comment"; `Space;
                       `Word "is"; `Space; `Word "for"; `Space;
                       `Code_span "InnerModuleTypeA'"; `Word "."]];
                  expr =
                   Some
                    (Odoc_model.Lang.ModuleType.Signature
                      [Odoc_model.Lang.Signature.Type
                        (Odoc_model.Lang.Signature.Ordinary,
                        {Odoc_model.Lang.TypeDecl.id =
                          `Type
                            (`ModuleType
                               (`Module
                                  (`Module
                                     (`Root (<root>, "Ocamlary"),
                                      "CollectionModule"),
                                   "InnerModuleA"),
                                "InnerModuleTypeA'"),
                             "t");
                         doc =
                          [`Paragraph
                             [`Word "This"; `Space; `Word "comment"; `Space;
                              `Word "is"; `Space; `Word "for"; `Space;
                              `Code_span "t"; `Word "."]];
                         equation =
                          {Odoc_model.Lang.TypeDecl.Equation.params = [];
                           private_ = false;
                           manifest =
                            Some
                             (Odoc_model.Lang.TypeExpr.Constr
                               (`Dot
                                  (`Resolved
                                     (`Identifier
                                        (`Module
                                           (`Module
                                              (`Module
                                                 (`Root (<root>, "Ocamlary"),
                                                  "CollectionModule"),
                                               "InnerModuleA"),
                                            "InnerModuleA'"))),
                                   "t"),
                               []));
                           constraints = []};
                         representation = None})]);
                  expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
           canonical = None; hidden = false; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType
              (`Module (`Root (<root>, "Ocamlary"), "CollectionModule"),
               "InnerModuleTypeA");
           doc =
            [`Paragraph
               [`Word "This"; `Space; `Word "comment"; `Space; `Word "is";
                `Space; `Word "for"; `Space; `Code_span "InnerModuleTypeA";
                `Word "."]];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Path
               (`Dot
                  (`Resolved
                     (`Identifier
                        (`Module
                           (`Module
                              (`Root (<root>, "Ocamlary"),
                               "CollectionModule"),
                            "InnerModuleA"))),
                   "InnerModuleTypeA'")));
           expansion = None}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "COLLECTION");
    doc =
     [`Paragraph [`Word "module"; `Space; `Word "type"; `Space; `Word "of"]];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.TypeOf
        (Odoc_model.Lang.Module.Alias
          (`Resolved
             (`Identifier
                (`Module (`Root (<root>, "Ocamlary"), "CollectionModule"))))));
    expansion = None};
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "Recollection");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Functor
        (Some
          {Odoc_model.Lang.FunctorArgument.id =
            `Parameter
              (`Module (`Root (<root>, "Ocamlary"), "Recollection"), "C");
           expr =
            Odoc_model.Lang.ModuleType.Path
             (`Resolved
                (`Identifier
                   (`ModuleType (`Root (<root>, "Ocamlary"), "COLLECTION"))));
           expansion = None},
        Odoc_model.Lang.ModuleType.With
         (Odoc_model.Lang.ModuleType.Path
           (`Resolved
              (`Identifier
                 (`ModuleType (`Root (<root>, "Ocamlary"), "COLLECTION")))),
         [Odoc_model.Lang.ModuleType.TypeEq
           (`Dot (`Resolved `Root, "collection"),
           {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
            manifest =
             Some
              (Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "list")),
                [Odoc_model.Lang.TypeExpr.Constr
                  (`Dot
                     (`Resolved
                        (`Identifier
                           (`Parameter
                              (`Module
                                 (`Root (<root>, "Ocamlary"), "Recollection"),
                               "C"))),
                      "element"),
                  [])]));
            constraints = []});
          Odoc_model.Lang.ModuleType.TypeEq
           (`Dot (`Resolved `Root, "element"),
           {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
            manifest =
             Some
              (Odoc_model.Lang.TypeExpr.Constr
                (`Dot
                   (`Resolved
                      (`Identifier
                         (`Parameter
                            (`Module
                               (`Root (<root>, "Ocamlary"), "Recollection"),
                             "C"))),
                    "collection"),
                []));
            constraints = []})])));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "MMM");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`ModuleType (`Root (<root>, "Ocamlary"), "MMM"), "C");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Path
               (`Resolved
                  (`Identifier
                     (`ModuleType (`Root (<root>, "Ocamlary"), "COLLECTION")))));
           canonical = None; hidden = false; display_type = None;
           expansion = None})]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "RECOLLECTION");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.With
        (Odoc_model.Lang.ModuleType.Path
          (`Resolved
             (`Identifier (`ModuleType (`Root (<root>, "Ocamlary"), "MMM")))),
        [Odoc_model.Lang.ModuleType.ModuleEq (`Dot (`Resolved `Root, "C"),
          Odoc_model.Lang.Module.Alias
           (`Apply
              (`Resolved
                 (`Identifier
                    (`Module (`Root (<root>, "Ocamlary"), "Recollection"))),
               `Resolved
                 (`Identifier
                    (`Module (`Root (<root>, "Ocamlary"), "CollectionModule"))))))]));
    expansion = None};
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "RecollectionModule");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Include
          {Odoc_model.Lang.Include.parent =
            `ModuleType (`Root (<root>, "Ocamlary"), "RecollectionModule");
           doc = [];
           decl =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.TypeOf
               (Odoc_model.Lang.Module.ModuleType
                 (Odoc_model.Lang.ModuleType.Signature
                   [Odoc_model.Lang.Signature.Type
                     (Odoc_model.Lang.Signature.Ordinary,
                     {Odoc_model.Lang.TypeDecl.id =
                       `Type
                         (`ModuleType
                            (`Root (<root>, "Ocamlary"),
                             "RecollectionModule"),
                          "collection");
                      doc = [];
                      equation =
                       {Odoc_model.Lang.TypeDecl.Equation.params = [];
                        private_ = false;
                        manifest =
                         Some
                          (Odoc_model.Lang.TypeExpr.Constr
                            (`Resolved (`Identifier (`CoreType "list")),
                            [Odoc_model.Lang.TypeExpr.Constr
                              (`Dot
                                 (`Resolved
                                    (`Identifier
                                       (`Module
                                          (`Root (<root>, "Ocamlary"),
                                           "CollectionModule"))),
                                  "element"),
                              [])]));
                        constraints = []};
                      representation = None});
                    Odoc_model.Lang.Signature.Type
                     (Odoc_model.Lang.Signature.Ordinary,
                     {Odoc_model.Lang.TypeDecl.id =
                       `Type
                         (`ModuleType
                            (`Root (<root>, "Ocamlary"),
                             "RecollectionModule"),
                          "element");
                      doc = [];
                      equation =
                       {Odoc_model.Lang.TypeDecl.Equation.params = [];
                        private_ = false;
                        manifest =
                         Some
                          (Odoc_model.Lang.TypeExpr.Constr
                            (`Dot
                               (`Resolved
                                  (`Identifier
                                     (`Module
                                        (`Root (<root>, "Ocamlary"),
                                         "CollectionModule"))),
                                "collection"),
                            []));
                        constraints = []};
                      representation = None});
                    Odoc_model.Lang.Signature.Module
                     (Odoc_model.Lang.Signature.Ordinary,
                     {Odoc_model.Lang.Module.id =
                       `Module
                         (`ModuleType
                            (`Root (<root>, "Ocamlary"),
                             "RecollectionModule"),
                          "InnerModuleA");
                      doc =
                       [`Paragraph
                          [`Word "This"; `Space; `Word "comment"; `Space;
                           `Word "is"; `Space; `Word "for"; `Space;
                           `Code_span "InnerModuleA"; `Word "."]];
                      type_ =
                       Odoc_model.Lang.Module.ModuleType
                        (Odoc_model.Lang.ModuleType.Signature
                          [Odoc_model.Lang.Signature.Type
                            (Odoc_model.Lang.Signature.Ordinary,
                            {Odoc_model.Lang.TypeDecl.id =
                              `Type
                                (`Module
                                   (`ModuleType
                                      (`Root (<root>, "Ocamlary"),
                                       "RecollectionModule"),
                                    "InnerModuleA"),
                                 "t");
                             doc =
                              [`Paragraph
                                 [`Word "This"; `Space; `Word "comment";
                                  `Space; `Word "is"; `Space; `Word "for";
                                  `Space; `Code_span "t"; `Word "."]];
                             equation =
                              {Odoc_model.Lang.TypeDecl.Equation.params = [];
                               private_ = false;
                               manifest =
                                Some
                                 (Odoc_model.Lang.TypeExpr.Constr
                                   (`Resolved
                                      (`Identifier
                                         (`Type
                                            (`ModuleType
                                               (`Root (<root>, "Ocamlary"),
                                                "RecollectionModule"),
                                             "collection"))),
                                   []));
                               constraints = []};
                             representation = None});
                           Odoc_model.Lang.Signature.Module
                            (Odoc_model.Lang.Signature.Ordinary,
                            {Odoc_model.Lang.Module.id =
                              `Module
                                (`Module
                                   (`ModuleType
                                      (`Root (<root>, "Ocamlary"),
                                       "RecollectionModule"),
                                    "InnerModuleA"),
                                 "InnerModuleA'");
                             doc =
                              [`Paragraph
                                 [`Word "This"; `Space; `Word "comment";
                                  `Space; `Word "is"; `Space; `Word "for";
                                  `Space; `Code_span "InnerModuleA'";
                                  `Word "."]];
                             type_ =
                              Odoc_model.Lang.Module.ModuleType
                               (Odoc_model.Lang.ModuleType.Signature
                                 [Odoc_model.Lang.Signature.Type
                                   (Odoc_model.Lang.Signature.Ordinary,
                                   {Odoc_model.Lang.TypeDecl.id =
                                     `Type
                                       (`Module
                                          (`Module
                                             (`ModuleType
                                                (`Root (<root>, "Ocamlary"),
                                                 "RecollectionModule"),
                                              "InnerModuleA"),
                                           "InnerModuleA'"),
                                        "t");
                                    doc =
                                     [`Paragraph
                                        [`Word "This"; `Space;
                                         `Word "comment"; `Space; `Word "is";
                                         `Space; `Word "for"; `Space;
                                         `Code_span "t"; `Word "."]];
                                    equation =
                                     {Odoc_model.Lang.TypeDecl.Equation.params
                                       = [];
                                      private_ = false;
                                      manifest =
                                       Some
                                        (Odoc_model.Lang.TypeExpr.Constr
                                          (`Resolved
                                             (`Identifier
                                                (`Type
                                                   (`Root
                                                      (<root>, "Ocamlary"),
                                                    "a_function"))),
                                          [Odoc_model.Lang.TypeExpr.Constr
                                            (`Resolved
                                               (`Identifier
                                                  (`CoreType "unit")),
                                            []);
                                           Odoc_model.Lang.TypeExpr.Constr
                                            (`Resolved
                                               (`Identifier
                                                  (`CoreType "unit")),
                                            [])]));
                                      constraints = []};
                                    representation = None})]);
                             canonical = None; hidden = false;
                             display_type = None;
                             expansion =
                              Some Odoc_model.Lang.Module.AlreadyASig});
                           Odoc_model.Lang.Signature.ModuleType
                            {Odoc_model.Lang.ModuleType.id =
                              `ModuleType
                                (`Module
                                   (`ModuleType
                                      (`Root (<root>, "Ocamlary"),
                                       "RecollectionModule"),
                                    "InnerModuleA"),
                                 "InnerModuleTypeA'");
                             doc =
                              [`Paragraph
                                 [`Word "This"; `Space; `Word "comment";
                                  `Space; `Word "is"; `Space; `Word "for";
                                  `Space; `Code_span "InnerModuleTypeA'";
                                  `Word "."]];
                             expr =
                              Some
                               (Odoc_model.Lang.ModuleType.Signature
                                 [Odoc_model.Lang.Signature.Type
                                   (Odoc_model.Lang.Signature.Ordinary,
                                   {Odoc_model.Lang.TypeDecl.id =
                                     `Type
                                       (`ModuleType
                                          (`Module
                                             (`ModuleType
                                                (`Root (<root>, "Ocamlary"),
                                                 "RecollectionModule"),
                                              "InnerModuleA"),
                                           "InnerModuleTypeA'"),
                                        "t");
                                    doc =
                                     [`Paragraph
                                        [`Word "This"; `Space;
                                         `Word "comment"; `Space; `Word "is";
                                         `Space; `Word "for"; `Space;
                                         `Code_span "t"; `Word "."]];
                                    equation =
                                     {Odoc_model.Lang.TypeDecl.Equation.params
                                       = [];
                                      private_ = false;
                                      manifest =
                                       Some
                                        (Odoc_model.Lang.TypeExpr.Constr
                                          (`Dot
                                             (`Resolved
                                                (`Identifier
                                                   (`Module
                                                      (`Module
                                                         (`ModuleType
                                                            (`Root
                                                               (<root>,
                                                                "Ocamlary"),
                                                             "RecollectionModule"),
                                                          "InnerModuleA"),
                                                       "InnerModuleA'"))),
                                              "t"),
                                          []));
                                      constraints = []};
                                    representation = None})]);
                             expansion =
                              Some Odoc_model.Lang.Module.AlreadyASig}]);
                      canonical = None; hidden = false; display_type = None;
                      expansion = Some Odoc_model.Lang.Module.AlreadyASig});
                    Odoc_model.Lang.Signature.ModuleType
                     {Odoc_model.Lang.ModuleType.id =
                       `ModuleType
                         (`ModuleType
                            (`Root (<root>, "Ocamlary"),
                             "RecollectionModule"),
                          "InnerModuleTypeA");
                      doc =
                       [`Paragraph
                          [`Word "This"; `Space; `Word "comment"; `Space;
                           `Word "is"; `Space; `Word "for"; `Space;
                           `Code_span "InnerModuleTypeA"; `Word "."]];
                      expr =
                       Some
                        (Odoc_model.Lang.ModuleType.Path
                          (`Dot
                             (`Resolved
                                (`Identifier
                                   (`Module
                                      (`ModuleType
                                         (`Root (<root>, "Ocamlary"),
                                          "RecollectionModule"),
                                       "InnerModuleA"))),
                              "InnerModuleTypeA'")));
                      expansion = None}])));
           expansion =
            {Odoc_model.Lang.Include.resolved = false;
             content =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`ModuleType
                       (`Root (<root>, "Ocamlary"), "RecollectionModule"),
                     "collection");
                 doc = [];
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false;
                   manifest =
                    Some
                     (Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved (`Identifier (`CoreType "list")),
                       [Odoc_model.Lang.TypeExpr.Constr
                         (`Dot
                            (`Resolved
                               (`Identifier
                                  (`Module
                                     (`Root (<root>, "Ocamlary"),
                                      "CollectionModule"))),
                             "element"),
                         [])]));
                   constraints = []};
                 representation = None});
               Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`ModuleType
                       (`Root (<root>, "Ocamlary"), "RecollectionModule"),
                     "element");
                 doc = [];
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false;
                   manifest =
                    Some
                     (Odoc_model.Lang.TypeExpr.Constr
                       (`Dot
                          (`Resolved
                             (`Identifier
                                (`Module
                                   (`Root (<root>, "Ocamlary"),
                                    "CollectionModule"))),
                           "collection"),
                       []));
                   constraints = []};
                 representation = None});
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`ModuleType
                       (`Root (<root>, "Ocamlary"), "RecollectionModule"),
                     "InnerModuleA");
                 doc =
                  [`Paragraph
                     [`Word "This"; `Space; `Word "comment"; `Space;
                      `Word "is"; `Space; `Word "for"; `Space;
                      `Code_span "InnerModuleA"; `Word "."]];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Signature
                     [Odoc_model.Lang.Signature.Type
                       (Odoc_model.Lang.Signature.Ordinary,
                       {Odoc_model.Lang.TypeDecl.id =
                         `Type
                           (`Module
                              (`ModuleType
                                 (`Root (<root>, "Ocamlary"),
                                  "RecollectionModule"),
                               "InnerModuleA"),
                            "t");
                        doc =
                         [`Paragraph
                            [`Word "This"; `Space; `Word "comment"; `Space;
                             `Word "is"; `Space; `Word "for"; `Space;
                             `Code_span "t"; `Word "."]];
                        equation =
                         {Odoc_model.Lang.TypeDecl.Equation.params = [];
                          private_ = false;
                          manifest =
                           Some
                            (Odoc_model.Lang.TypeExpr.Constr
                              (`Resolved
                                 (`Identifier
                                    (`Type
                                       (`ModuleType
                                          (`Root (<root>, "Ocamlary"),
                                           "RecollectionModule"),
                                        "collection"))),
                              []));
                          constraints = []};
                        representation = None});
                      Odoc_model.Lang.Signature.Module
                       (Odoc_model.Lang.Signature.Ordinary,
                       {Odoc_model.Lang.Module.id =
                         `Module
                           (`Module
                              (`ModuleType
                                 (`Root (<root>, "Ocamlary"),
                                  "RecollectionModule"),
                               "InnerModuleA"),
                            "InnerModuleA'");
                        doc =
                         [`Paragraph
                            [`Word "This"; `Space; `Word "comment"; `Space;
                             `Word "is"; `Space; `Word "for"; `Space;
                             `Code_span "InnerModuleA'"; `Word "."]];
                        type_ =
                         Odoc_model.Lang.Module.ModuleType
                          (Odoc_model.Lang.ModuleType.Signature
                            [Odoc_model.Lang.Signature.Type
                              (Odoc_model.Lang.Signature.Ordinary,
                              {Odoc_model.Lang.TypeDecl.id =
                                `Type
                                  (`Module
                                     (`Module
                                        (`ModuleType
                                           (`Root (<root>, "Ocamlary"),
                                            "RecollectionModule"),
                                         "InnerModuleA"),
                                      "InnerModuleA'"),
                                   "t");
                               doc =
                                [`Paragraph
                                   [`Word "This"; `Space; `Word "comment";
                                    `Space; `Word "is"; `Space; `Word "for";
                                    `Space; `Code_span "t"; `Word "."]];
                               equation =
                                {Odoc_model.Lang.TypeDecl.Equation.params =
                                  [];
                                 private_ = false;
                                 manifest =
                                  Some
                                   (Odoc_model.Lang.TypeExpr.Constr
                                     (`Resolved
                                        (`Identifier
                                           (`Type
                                              (`Root (<root>, "Ocamlary"),
                                               "a_function"))),
                                     [Odoc_model.Lang.TypeExpr.Constr
                                       (`Resolved
                                          (`Identifier (`CoreType "unit")),
                                       []);
                                      Odoc_model.Lang.TypeExpr.Constr
                                       (`Resolved
                                          (`Identifier (`CoreType "unit")),
                                       [])]));
                                 constraints = []};
                               representation = None})]);
                        canonical = None; hidden = false;
                        display_type = None;
                        expansion = Some Odoc_model.Lang.Module.AlreadyASig});
                      Odoc_model.Lang.Signature.ModuleType
                       {Odoc_model.Lang.ModuleType.id =
                         `ModuleType
                           (`Module
                              (`ModuleType
                                 (`Root (<root>, "Ocamlary"),
                                  "RecollectionModule"),
                               "InnerModuleA"),
                            "InnerModuleTypeA'");
                        doc =
                         [`Paragraph
                            [`Word "This"; `Space; `Word "comment"; `Space;
                             `Word "is"; `Space; `Word "for"; `Space;
                             `Code_span "InnerModuleTypeA'"; `Word "."]];
                        expr =
                         Some
                          (Odoc_model.Lang.ModuleType.Signature
                            [Odoc_model.Lang.Signature.Type
                              (Odoc_model.Lang.Signature.Ordinary,
                              {Odoc_model.Lang.TypeDecl.id =
                                `Type
                                  (`ModuleType
                                     (`Module
                                        (`ModuleType
                                           (`Root (<root>, "Ocamlary"),
                                            "RecollectionModule"),
                                         "InnerModuleA"),
                                      "InnerModuleTypeA'"),
                                   "t");
                               doc =
                                [`Paragraph
                                   [`Word "This"; `Space; `Word "comment";
                                    `Space; `Word "is"; `Space; `Word "for";
                                    `Space; `Code_span "t"; `Word "."]];
                               equation =
                                {Odoc_model.Lang.TypeDecl.Equation.params =
                                  [];
                                 private_ = false;
                                 manifest =
                                  Some
                                   (Odoc_model.Lang.TypeExpr.Constr
                                     (`Dot
                                        (`Resolved
                                           (`Identifier
                                              (`Module
                                                 (`Module
                                                    (`ModuleType
                                                       (`Root
                                                          (<root>,
                                                           "Ocamlary"),
                                                        "RecollectionModule"),
                                                     "InnerModuleA"),
                                                  "InnerModuleA'"))),
                                         "t"),
                                     []));
                                 constraints = []};
                               representation = None})]);
                        expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
                 canonical = None; hidden = false; display_type = None;
                 expansion = Some Odoc_model.Lang.Module.AlreadyASig});
               Odoc_model.Lang.Signature.ModuleType
                {Odoc_model.Lang.ModuleType.id =
                  `ModuleType
                    (`ModuleType
                       (`Root (<root>, "Ocamlary"), "RecollectionModule"),
                     "InnerModuleTypeA");
                 doc =
                  [`Paragraph
                     [`Word "This"; `Space; `Word "comment"; `Space;
                      `Word "is"; `Space; `Word "for"; `Space;
                      `Code_span "InnerModuleTypeA"; `Word "."]];
                 expr =
                  Some
                   (Odoc_model.Lang.ModuleType.Path
                     (`Dot
                        (`Resolved
                           (`Identifier
                              (`Module
                                 (`ModuleType
                                    (`Root (<root>, "Ocamlary"),
                                     "RecollectionModule"),
                                  "InnerModuleA"))),
                         "InnerModuleTypeA'")));
                 expansion = None}]}}]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "A");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`ModuleType (`Root (<root>, "Ocamlary"), "A"), "t");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`ModuleType (`Root (<root>, "Ocamlary"), "A"), "Q");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Path
               (`Resolved
                  (`Identifier
                     (`ModuleType (`Root (<root>, "Ocamlary"), "COLLECTION")))));
           canonical = None; hidden = false; display_type = None;
           expansion = None})]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "B");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`ModuleType (`Root (<root>, "Ocamlary"), "B"), "t");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`ModuleType (`Root (<root>, "Ocamlary"), "B"), "Q");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Path
               (`Resolved
                  (`Identifier
                     (`ModuleType (`Root (<root>, "Ocamlary"), "COLLECTION")))));
           canonical = None; hidden = false; display_type = None;
           expansion = None})]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "C");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "module"; `Space; `Word "type"; `Space;
         `Word "includes"; `Space; `Word "two"; `Space; `Word "signatures."];
      `List
        (`Unordered,
         [[`Paragraph
             [`Word "it"; `Space; `Word "includes"; `Space;
              `Reference (`Root ("A", `TUnknown), [])]];
          [`Paragraph
             [`Word "it"; `Space; `Word "includes"; `Space;
              `Reference (`Root ("B", `TUnknown), []); `Space; `Word "with";
              `Space; `Word "some"; `Space; `Word "substitution"]]])];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Include
          {Odoc_model.Lang.Include.parent =
            `ModuleType (`Root (<root>, "Ocamlary"), "C");
           doc = [];
           decl =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Path
               (`Resolved
                  (`Identifier
                     (`ModuleType (`Root (<root>, "Ocamlary"), "A")))));
           expansion =
            {Odoc_model.Lang.Include.resolved = false;
             content =
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type (`ModuleType (`Root (<root>, "Ocamlary"), "C"), "t");
                 doc = [];
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None});
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`ModuleType (`Root (<root>, "Ocamlary"), "C"), "Q");
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Path
                     (`Resolved
                        (`Identifier
                           (`ModuleType
                              (`Root (<root>, "Ocamlary"), "COLLECTION")))));
                 canonical = None; hidden = false; display_type = None;
                 expansion = None})]}};
         Odoc_model.Lang.Signature.Include
          {Odoc_model.Lang.Include.parent =
            `ModuleType (`Root (<root>, "Ocamlary"), "C");
           doc = [];
           decl =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.With
               (Odoc_model.Lang.ModuleType.Path
                 (`Resolved
                    (`Identifier
                       (`ModuleType (`Root (<root>, "Ocamlary"), "B")))),
               [Odoc_model.Lang.ModuleType.TypeSubst
                 (`Dot (`Resolved `Root, "t"),
                 {Odoc_model.Lang.TypeDecl.Equation.params = [];
                  private_ = false;
                  manifest =
                   Some
                    (Odoc_model.Lang.TypeExpr.Constr
                      (`Resolved
                         (`Identifier
                            (`Type
                               (`ModuleType (`Root (<root>, "Ocamlary"), "C"),
                                "t"))),
                      []));
                  constraints = []});
                Odoc_model.Lang.ModuleType.ModuleSubst
                 (`Dot (`Resolved `Root, "Q"),
                 `Resolved
                   (`Identifier
                      (`Module
                         (`ModuleType (`Root (<root>, "Ocamlary"), "C"), "Q"))))]));
           expansion =
            {Odoc_model.Lang.Include.resolved = false; content = []}}]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "FunctorTypeOf");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "FunctorTypeOf"; `Word "."]];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Functor
        (Some
          {Odoc_model.Lang.FunctorArgument.id =
            `Parameter
              (`Module (`Root (<root>, "Ocamlary"), "FunctorTypeOf"),
               "Collection");
           expr =
            Odoc_model.Lang.ModuleType.TypeOf
             (Odoc_model.Lang.Module.Alias
               (`Resolved
                  (`Identifier
                     (`Module
                        (`Root (<root>, "Ocamlary"), "CollectionModule")))));
           expansion = None},
        Odoc_model.Lang.ModuleType.Signature
         [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
           {Odoc_model.Lang.TypeDecl.id =
             `Type
               (`Result
                  (`Module (`Root (<root>, "Ocamlary"), "FunctorTypeOf")),
                "t");
            doc =
             [`Paragraph
                [`Word "This"; `Space; `Word "comment"; `Space; `Word "is";
                 `Space; `Word "for"; `Space; `Code_span "t"; `Word "."]];
            equation =
             {Odoc_model.Lang.TypeDecl.Equation.params = [];
              private_ = false;
              manifest =
               Some
                (Odoc_model.Lang.TypeExpr.Constr
                  (`Dot
                     (`Resolved
                        (`Identifier
                           (`Parameter
                              (`Module
                                 (`Root (<root>, "Ocamlary"),
                                  "FunctorTypeOf"),
                               "Collection"))),
                      "collection"),
                  []));
              constraints = []};
            representation = None})]));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "IncludeModuleType");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "IncludeModuleType"; `Word "."]];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Include
          {Odoc_model.Lang.Include.parent =
            `ModuleType (`Root (<root>, "Ocamlary"), "IncludeModuleType");
           doc =
            [`Paragraph
               [`Word "This"; `Space; `Word "comment"; `Space; `Word "is";
                `Space; `Word "for"; `Space;
                `Code_span "include EmptySigAlias"; `Word "."]];
           decl =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Path
               (`Resolved
                  (`Identifier
                     (`ModuleType
                        (`Root (<root>, "Ocamlary"), "EmptySigAlias")))));
           expansion =
            {Odoc_model.Lang.Include.resolved = false; content = []}}]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "ToInclude");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`ModuleType (`Root (<root>, "Ocamlary"), "ToInclude"),
               "IncludedA");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`Module
                        (`ModuleType
                           (`Root (<root>, "Ocamlary"), "ToInclude"),
                         "IncludedA"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           canonical = None; hidden = false; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType
              (`ModuleType (`Root (<root>, "Ocamlary"), "ToInclude"),
               "IncludedB");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`ModuleType
                           (`Root (<root>, "Ocamlary"), "ToInclude"),
                         "IncludedB"),
                      "s");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.Include
   {Odoc_model.Lang.Include.parent = `Root (<root>, "Ocamlary"); doc = [];
    decl =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Path
        (`Resolved
           (`Identifier
              (`ModuleType (`Root (<root>, "Ocamlary"), "ToInclude")))));
    expansion =
     {Odoc_model.Lang.Include.resolved = false;
      content =
       [Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
         {Odoc_model.Lang.Module.id =
           `Module (`Root (<root>, "Ocamlary"), "IncludedA");
          doc = [];
          type_ =
           Odoc_model.Lang.Module.ModuleType
            (Odoc_model.Lang.ModuleType.Signature
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`Module (`Root (<root>, "Ocamlary"), "IncludedA"), "t");
                 doc = [];
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})]);
          canonical = None; hidden = false; display_type = None;
          expansion = Some Odoc_model.Lang.Module.AlreadyASig});
        Odoc_model.Lang.Signature.ModuleType
         {Odoc_model.Lang.ModuleType.id =
           `ModuleType (`Root (<root>, "Ocamlary"), "IncludedB");
          doc = [];
          expr =
           Some
            (Odoc_model.Lang.ModuleType.Signature
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`ModuleType (`Root (<root>, "Ocamlary"), "IncludedB"),
                     "s");
                 doc = [];
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})]);
          expansion = Some Odoc_model.Lang.Module.AlreadyASig}]}};
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Subsubsection,
          `Label (`Root (<root>, "Ocamlary"), "advanced-type-stuff"),
          [`Word "Advanced"; `Space; `Word "Type"; `Space; `Word "Stuff"])]);
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "record");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "record"; `Word "."];
      `Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "also"; `Space; `Word "for"; `Space; `Code_span "record";
         `Word "."]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest = None; constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Record
        [{Odoc_model.Lang.TypeDecl.Field.id =
           `Field (`Type (`Root (<root>, "Ocamlary"), "record"), "field1");
          doc =
           [`Paragraph
              [`Word "This"; `Space; `Word "comment"; `Space; `Word "is";
               `Space; `Word "for"; `Space; `Code_span "field1"; `Word "."]];
          mutable_ = false;
          type_ =
           Odoc_model.Lang.TypeExpr.Constr
            (`Resolved (`Identifier (`CoreType "int")), [])};
         {Odoc_model.Lang.TypeDecl.Field.id =
           `Field (`Type (`Root (<root>, "Ocamlary"), "record"), "field2");
          doc =
           [`Paragraph
              [`Word "This"; `Space; `Word "comment"; `Space; `Word "is";
               `Space; `Word "for"; `Space; `Code_span "field2"; `Word "."]];
          mutable_ = false;
          type_ =
           Odoc_model.Lang.TypeExpr.Constr
            (`Resolved (`Identifier (`CoreType "int")), [])}])});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "mutable_record");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest = None; constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Record
        [{Odoc_model.Lang.TypeDecl.Field.id =
           `Field (`Type (`Root (<root>, "Ocamlary"), "mutable_record"), "a");
          doc =
           [`Paragraph
              [`Code_span "a"; `Space; `Word "is"; `Space; `Word "first";
               `Space; `Word "and"; `Space; `Word "mutable"]];
          mutable_ = true;
          type_ =
           Odoc_model.Lang.TypeExpr.Constr
            (`Resolved (`Identifier (`CoreType "int")), [])};
         {Odoc_model.Lang.TypeDecl.Field.id =
           `Field (`Type (`Root (<root>, "Ocamlary"), "mutable_record"), "b");
          doc =
           [`Paragraph
              [`Code_span "b"; `Space; `Word "is"; `Space; `Word "second";
               `Space; `Word "and"; `Space; `Word "immutable"]];
          mutable_ = false;
          type_ =
           Odoc_model.Lang.TypeExpr.Constr
            (`Resolved (`Identifier (`CoreType "unit")), [])};
         {Odoc_model.Lang.TypeDecl.Field.id =
           `Field (`Type (`Root (<root>, "Ocamlary"), "mutable_record"), "c");
          doc =
           [`Paragraph
              [`Code_span "c"; `Space; `Word "is"; `Space; `Word "third";
               `Space; `Word "and"; `Space; `Word "mutable"]];
          mutable_ = true;
          type_ =
           Odoc_model.Lang.TypeExpr.Constr
            (`Resolved (`Identifier (`CoreType "int")), [])}])});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "universe_record");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest = None; constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Record
        [{Odoc_model.Lang.TypeDecl.Field.id =
           `Field
             (`Type (`Root (<root>, "Ocamlary"), "universe_record"),
              "nihilate");
          doc = []; mutable_ = false;
          type_ =
           Odoc_model.Lang.TypeExpr.Poly (["a"],
            Odoc_model.Lang.TypeExpr.Arrow (None,
             Odoc_model.Lang.TypeExpr.Var "a",
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType "unit")), [])))}])});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "variant");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "variant"; `Word "."];
      `Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "also"; `Space; `Word "for"; `Space; `Code_span "variant";
         `Word "."]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest = None; constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Variant
        [{Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "variant"), "TagA");
          doc =
           [`Paragraph
              [`Word "This"; `Space; `Word "comment"; `Space; `Word "is";
               `Space; `Word "for"; `Space; `Code_span "TagA"; `Word "."]];
          args = Odoc_model.Lang.TypeDecl.Constructor.Tuple []; res = None};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "variant"), "ConstrB");
          doc =
           [`Paragraph
              [`Word "This"; `Space; `Word "comment"; `Space; `Word "is";
               `Space; `Word "for"; `Space; `Code_span "ConstrB"; `Word "."]];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType "int")), [])];
          res = None};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "variant"), "ConstrC");
          doc =
           [`Paragraph
              [`Word "This"; `Space; `Word "comment"; `Space; `Word "is";
               `Space; `Word "for"; `Space; `Word "binary"; `Space;
               `Code_span "ConstrC"; `Word "."]];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType "int")), []);
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType "int")), [])];
          res = None};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "variant"), "ConstrD");
          doc =
           [`Paragraph
              [`Word "This"; `Space; `Word "comment"; `Space; `Word "is";
               `Space; `Word "for"; `Space; `Word "unary"; `Space;
               `Code_span "ConstrD"; `Space; `Word "of"; `Space;
               `Word "binary"; `Space; `Word "tuple."]];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Tuple
              [Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "int")), []);
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "int")), [])]];
          res = None}])});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "poly_variant");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "poly_variant"; `Word "."];
      `Paragraph
        [`Word "Wow!"; `Space; `Word "It"; `Space; `Word "was"; `Space;
         `Word "a"; `Space; `Word "polymorphic"; `Space; `Word "variant!"]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Polymorphic_variant
          {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
            Odoc_model.Lang.TypeExpr.Polymorphic_variant.Fixed;
           elements =
            [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
              {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                = "TagA";
               constant = true; arguments = [];
               doc =
                [`Paragraph
                   [`Word "This"; `Space; `Word "comment"; `Space;
                    `Word "is"; `Space; `Word "for"; `Space;
                    `Code_span "`TagA"; `Word "."]]};
             Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
              {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                = "ConstrB";
               constant = false;
               arguments =
                [Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType "int")), [])];
               doc =
                [`Paragraph
                   [`Word "This"; `Space; `Word "comment"; `Space;
                    `Word "is"; `Space; `Word "for"; `Space;
                    `Code_span "`ConstrB"; `Word "."]]}]});
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "full_gadt");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "full_gadt"; `Word "."];
      `Paragraph
        [`Word "Wow!"; `Space; `Word "It"; `Space; `Word "was"; `Space;
         `Word "a"; `Space; `Word "GADT!"]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Any, None);
        (Odoc_model.Lang.TypeDecl.Any, None)];
      private_ = false; manifest = None; constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Variant
        [{Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "full_gadt"), "Tag");
          doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "full_gadt"))),
              [Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "unit")), []);
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "unit")), [])]))};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "full_gadt"), "First");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Var "a"];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "full_gadt"))),
              [Odoc_model.Lang.TypeExpr.Var "a";
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "unit")), [])]))};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "full_gadt"), "Second");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Var "a"];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "full_gadt"))),
              [Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "unit")), []);
               Odoc_model.Lang.TypeExpr.Var "a"]))};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "full_gadt"), "Exist");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Var "a";
             Odoc_model.Lang.TypeExpr.Var "b"];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "full_gadt"))),
              [Odoc_model.Lang.TypeExpr.Var "b";
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "unit")), [])]))}])});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "partial_gadt");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "partial_gadt"; `Word "."];
      `Paragraph
        [`Word "Wow!"; `Space; `Word "It"; `Space; `Word "was"; `Space;
         `Word "a"; `Space; `Word "mixed"; `Space; `Word "GADT!"]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false; manifest = None; constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Variant
        [{Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "partial_gadt"),
              "AscribeTag");
          doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "partial_gadt"))),
              [Odoc_model.Lang.TypeExpr.Var "a"]))};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "partial_gadt"), "OfTag");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "partial_gadt"))),
              [Odoc_model.Lang.TypeExpr.Var "a"])];
          res = None};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "partial_gadt"),
              "ExistGadtTag");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Arrow (None,
              Odoc_model.Lang.TypeExpr.Var "a",
              Odoc_model.Lang.TypeExpr.Var "b")];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "partial_gadt"))),
              [Odoc_model.Lang.TypeExpr.Var "a"]))}])});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "record_arg_gadt");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "record_arg_gadt"; `Word "."];
      `Paragraph
        [`Word "Wow!"; `Space; `Word "It"; `Space; `Word "was"; `Space;
         `Word "a"; `Space; `Word "GADT"; `Space; `Word "with"; `Space;
         `Word "record"; `Space; `Word "arguments"]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Any, None)];
      private_ = false; manifest = None; constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Variant
        [{Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "record_arg_gadt"),
              "With_rec");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Record
            [{Odoc_model.Lang.TypeDecl.Field.id =
               `Field
                 (`Type (`Root (<root>, "Ocamlary"), "record_arg_gadt"),
                  "foo");
              doc = []; mutable_ = false;
              type_ =
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "int")), [])}];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "record_arg_gadt"))),
              [Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "unit")), [])]))};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "record_arg_gadt"),
              "With_poly_rec");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Record
            [{Odoc_model.Lang.TypeDecl.Field.id =
               `Field
                 (`Type (`Root (<root>, "Ocamlary"), "record_arg_gadt"),
                  "bar");
              doc = []; mutable_ = false;
              type_ =
               Odoc_model.Lang.TypeExpr.Poly (["a"],
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Var "a",
                 Odoc_model.Lang.TypeExpr.Var "a"))}];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "record_arg_gadt"))),
              [Odoc_model.Lang.TypeExpr.Arrow (None,
                Odoc_model.Lang.TypeExpr.Var "a",
                Odoc_model.Lang.TypeExpr.Var "a")]))}])});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id = `Type (`Root (<root>, "Ocamlary"), "alias");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "alias"; `Word "."]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Resolved
             (`Identifier (`Type (`Root (<root>, "Ocamlary"), "variant"))),
          []));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id = `Type (`Root (<root>, "Ocamlary"), "tuple");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "tuple"; `Word "."]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Tuple
          [Odoc_model.Lang.TypeExpr.Tuple
            [Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier (`Type (`Root (<root>, "Ocamlary"), "alias"))),
              []);
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier (`Type (`Root (<root>, "Ocamlary"), "alias"))),
              [])];
           Odoc_model.Lang.TypeExpr.Constr
            (`Resolved
               (`Identifier (`Type (`Root (<root>, "Ocamlary"), "alias"))),
            []);
           Odoc_model.Lang.TypeExpr.Tuple
            [Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier (`Type (`Root (<root>, "Ocamlary"), "alias"))),
              []);
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier (`Type (`Root (<root>, "Ocamlary"), "alias"))),
              [])]]);
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "variant_alias");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "variant_alias"; `Word "."]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Resolved
             (`Identifier (`Type (`Root (<root>, "Ocamlary"), "variant"))),
          []));
      constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Variant
        [{Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "variant_alias"), "TagA");
          doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
          res = None};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "variant_alias"), "ConstrB");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType "int")), [])];
          res = None};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "variant_alias"), "ConstrC");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType "int")), []);
             Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType "int")), [])];
          res = None};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "variant_alias"), "ConstrD");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Tuple
              [Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "int")), []);
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "int")), [])]];
          res = None}])});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "record_alias");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "record_alias"; `Word "."]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Resolved
             (`Identifier (`Type (`Root (<root>, "Ocamlary"), "record"))),
          []));
      constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Record
        [{Odoc_model.Lang.TypeDecl.Field.id =
           `Field
             (`Type (`Root (<root>, "Ocamlary"), "record_alias"), "field1");
          doc = []; mutable_ = false;
          type_ =
           Odoc_model.Lang.TypeExpr.Constr
            (`Resolved (`Identifier (`CoreType "int")), [])};
         {Odoc_model.Lang.TypeDecl.Field.id =
           `Field
             (`Type (`Root (<root>, "Ocamlary"), "record_alias"), "field2");
          doc = []; mutable_ = false;
          type_ =
           Odoc_model.Lang.TypeExpr.Constr
            (`Resolved (`Identifier (`CoreType "int")), [])}])});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "poly_variant_union");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "poly_variant_union"; `Word "."]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Polymorphic_variant
          {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
            Odoc_model.Lang.TypeExpr.Polymorphic_variant.Fixed;
           elements =
            [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type
              (Odoc_model.Lang.TypeExpr.Constr
                (`Resolved
                   (`Identifier
                      (`Type (`Root (<root>, "Ocamlary"), "poly_variant"))),
                []));
             Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
              {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                = "TagC";
               constant = true; arguments = []; doc = []}]});
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "poly_poly_variant");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Polymorphic_variant
          {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
            Odoc_model.Lang.TypeExpr.Polymorphic_variant.Fixed;
           elements =
            [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
              {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                = "TagA";
               constant = false;
               arguments = [Odoc_model.Lang.TypeExpr.Var "a"]; doc = []}]});
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "bin_poly_poly_variant");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None);
        (Odoc_model.Lang.TypeDecl.Var "b", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Polymorphic_variant
          {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
            Odoc_model.Lang.TypeExpr.Polymorphic_variant.Fixed;
           elements =
            [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
              {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                = "TagA";
               constant = false;
               arguments = [Odoc_model.Lang.TypeExpr.Var "a"]; doc = []};
             Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
              {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                = "ConstrB";
               constant = false;
               arguments = [Odoc_model.Lang.TypeExpr.Var "b"]; doc = []}]});
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "open_poly_variant");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Alias
          (Odoc_model.Lang.TypeExpr.Polymorphic_variant
            {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
              Odoc_model.Lang.TypeExpr.Polymorphic_variant.Open;
             elements =
              [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                  = "TagA";
                 constant = true; arguments = []; doc = []}]},
          "a"));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "open_poly_variant2");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Alias
          (Odoc_model.Lang.TypeExpr.Polymorphic_variant
            {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
              Odoc_model.Lang.TypeExpr.Polymorphic_variant.Open;
             elements =
              [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                  = "ConstrB";
                 constant = false;
                 arguments =
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType "int")), [])];
                 doc = []}]},
          "a"));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "open_poly_variant_alias");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Resolved
             (`Identifier
                (`Type (`Root (<root>, "Ocamlary"), "open_poly_variant2"))),
          [Odoc_model.Lang.TypeExpr.Constr
            (`Resolved
               (`Identifier
                  (`Type (`Root (<root>, "Ocamlary"), "open_poly_variant"))),
            [Odoc_model.Lang.TypeExpr.Var "a"])]));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "poly_fun");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Arrow (None,
          Odoc_model.Lang.TypeExpr.Alias
           (Odoc_model.Lang.TypeExpr.Polymorphic_variant
             {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
               Odoc_model.Lang.TypeExpr.Polymorphic_variant.Open;
              elements =
               [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                 {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                   = "ConstrB";
                  constant = false;
                  arguments =
                   [Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved (`Identifier (`CoreType "int")), [])];
                  doc = []}]},
           "a"),
          Odoc_model.Lang.TypeExpr.Var "a"));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "poly_fun_constraint");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Arrow (None,
          Odoc_model.Lang.TypeExpr.Var "a", Odoc_model.Lang.TypeExpr.Var "a"));
      constraints =
       [(Odoc_model.Lang.TypeExpr.Var "a",
         Odoc_model.Lang.TypeExpr.Polymorphic_variant
          {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
            Odoc_model.Lang.TypeExpr.Polymorphic_variant.Open;
           elements =
            [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
              {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                = "TagA";
               constant = true; arguments = []; doc = []}]})]};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "closed_poly_variant");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Alias
          (Odoc_model.Lang.TypeExpr.Polymorphic_variant
            {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
              Odoc_model.Lang.TypeExpr.Polymorphic_variant.Closed [];
             elements =
              [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                  = "One";
                 constant = true; arguments = []; doc = []};
               Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                  = "Two";
                 constant = true; arguments = []; doc = []}]},
          "a"));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "clopen_poly_variant");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Alias
          (Odoc_model.Lang.TypeExpr.Polymorphic_variant
            {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
              Odoc_model.Lang.TypeExpr.Polymorphic_variant.Closed
               ["Two"; "Three"];
             elements =
              [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                  = "One";
                 constant = true; arguments = []; doc = []};
               Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                  = "Two";
                 constant = false;
                 arguments =
                  [Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType "int")), [])];
                 doc = []};
               Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                  = "Three";
                 constant = true; arguments = []; doc = []}]},
          "a"));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "nested_poly_variant");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Polymorphic_variant
          {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
            Odoc_model.Lang.TypeExpr.Polymorphic_variant.Fixed;
           elements =
            [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
              {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                = "A";
               constant = true; arguments = []; doc = []};
             Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
              {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                = "B";
               constant = false;
               arguments =
                [Odoc_model.Lang.TypeExpr.Polymorphic_variant
                  {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
                    Odoc_model.Lang.TypeExpr.Polymorphic_variant.Fixed;
                   elements =
                    [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                      {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                        = "B1";
                       constant = true; arguments = []; doc = []};
                     Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                      {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                        = "B2";
                       constant = true; arguments = []; doc = []}]}];
               doc = []};
             Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
              {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                = "C";
               constant = true; arguments = []; doc = []};
             Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
              {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                = "D";
               constant = false;
               arguments =
                [Odoc_model.Lang.TypeExpr.Polymorphic_variant
                  {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind =
                    Odoc_model.Lang.TypeExpr.Polymorphic_variant.Fixed;
                   elements =
                    [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                      {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                        = "D1";
                       constant = false;
                       arguments =
                        [Odoc_model.Lang.TypeExpr.Polymorphic_variant
                          {Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind
                            =
                            Odoc_model.Lang.TypeExpr.Polymorphic_variant.Fixed;
                           elements =
                            [Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor
                              {Odoc_model.Lang.TypeExpr.Polymorphic_variant.Constructor.name
                                = "D1a";
                               constant = true; arguments = []; doc = []}]}];
                       doc = []}]}];
               doc = []}]});
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "full_gadt_alias");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "full_gadt_alias"; `Word "."]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None);
        (Odoc_model.Lang.TypeDecl.Var "b", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Resolved
             (`Identifier (`Type (`Root (<root>, "Ocamlary"), "full_gadt"))),
          [Odoc_model.Lang.TypeExpr.Var "a";
           Odoc_model.Lang.TypeExpr.Var "b"]));
      constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Variant
        [{Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "full_gadt_alias"), "Tag");
          doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "full_gadt_alias"))),
              [Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "unit")), []);
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "unit")), [])]))};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "full_gadt_alias"), "First");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Var "a"];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "full_gadt_alias"))),
              [Odoc_model.Lang.TypeExpr.Var "a";
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "unit")), [])]))};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "full_gadt_alias"),
              "Second");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Var "a"];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "full_gadt_alias"))),
              [Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "unit")), []);
               Odoc_model.Lang.TypeExpr.Var "a"]))};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "full_gadt_alias"), "Exist");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Var "a";
             Odoc_model.Lang.TypeExpr.Var "b"];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "full_gadt_alias"))),
              [Odoc_model.Lang.TypeExpr.Var "b";
               Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "unit")), [])]))}])});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "partial_gadt_alias");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space; `Code_span "partial_gadt_alias"; `Word "."]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Resolved
             (`Identifier
                (`Type (`Root (<root>, "Ocamlary"), "partial_gadt"))),
          [Odoc_model.Lang.TypeExpr.Var "a"]));
      constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Variant
        [{Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "partial_gadt_alias"),
              "AscribeTag");
          doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "partial_gadt_alias"))),
              [Odoc_model.Lang.TypeExpr.Var "a"]))};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "partial_gadt_alias"),
              "OfTag");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "partial_gadt_alias"))),
              [Odoc_model.Lang.TypeExpr.Var "a"])];
          res = None};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "partial_gadt_alias"),
              "ExistGadtTag");
          doc = [];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Arrow (None,
              Odoc_model.Lang.TypeExpr.Var "a",
              Odoc_model.Lang.TypeExpr.Var "b")];
          res =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "partial_gadt_alias"))),
              [Odoc_model.Lang.TypeExpr.Var "a"]))}])});
  Odoc_model.Lang.Signature.Exception
   {Odoc_model.Lang.Exception.id =
     `Exception (`Root (<root>, "Ocamlary"), "Exn_arrow");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space;
         `Reference (`Root ("Exn_arrow", `TUnknown), []); `Word "."]];
    args =
     Odoc_model.Lang.TypeDecl.Constructor.Tuple
      [Odoc_model.Lang.TypeExpr.Constr
        (`Resolved (`Identifier (`CoreType "unit")), [])];
    res =
     Some
      (Odoc_model.Lang.TypeExpr.Constr
        (`Resolved (`Identifier (`CoreType "exn")), []))};
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "mutual_constr_a");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space;
         `Reference (`Root ("mutual_constr_a", `TUnknown), []); `Space;
         `Word "then"; `Space;
         `Reference (`Root ("mutual_constr_b", `TUnknown), []); `Word "."]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest = None; constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Variant
        [{Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "mutual_constr_a"), "A");
          doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
          res = None};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "mutual_constr_a"), "B_ish");
          doc =
           [`Paragraph
              [`Word "This"; `Space; `Word "comment"; `Space; `Word "is";
               `Space; `Word "between"; `Space;
               `Reference (`Root ("mutual_constr_a", `TUnknown), []); `Space;
               `Word "and"; `Space;
               `Reference (`Root ("mutual_constr_b", `TUnknown), []);
               `Word "."]];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "mutual_constr_b"))),
              [])];
          res = None}])});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.And,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "mutual_constr_b");
    doc =
     [`Paragraph
        [`Word "This"; `Space; `Word "comment"; `Space; `Word "is"; `Space;
         `Word "for"; `Space;
         `Reference (`Root ("mutual_constr_b", `TUnknown), []); `Space;
         `Word "then"; `Space;
         `Reference (`Root ("mutual_constr_a", `TUnknown), []); `Word "."]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest = None; constraints = []};
    representation =
     Some
      (Odoc_model.Lang.TypeDecl.Representation.Variant
        [{Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "mutual_constr_b"), "B");
          doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
          res = None};
         {Odoc_model.Lang.TypeDecl.Constructor.id =
           `Constructor
             (`Type (`Root (<root>, "Ocamlary"), "mutual_constr_b"), "A_ish");
          doc =
           [`Paragraph
              [`Word "This"; `Space; `Word "comment"; `Space; `Word "must";
               `Space; `Word "be"; `Space; `Word "here"; `Space; `Word "for";
               `Space; `Word "the"; `Space; `Word "next"; `Space; `Word "to";
               `Space; `Word "associate"; `Space; `Word "correctly."]];
          args =
           Odoc_model.Lang.TypeDecl.Constructor.Tuple
            [Odoc_model.Lang.TypeExpr.Constr
              (`Resolved
                 (`Identifier
                    (`Type (`Root (<root>, "Ocamlary"), "mutual_constr_a"))),
              [])];
          res = None}])});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "rec_obj");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Object
          {Odoc_model.Lang.TypeExpr.Object.fields =
            [Odoc_model.Lang.TypeExpr.Object.Method
              {Odoc_model.Lang.TypeExpr.Object.name = "f";
               type_ =
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType "int")), [])};
             Odoc_model.Lang.TypeExpr.Object.Method
              {Odoc_model.Lang.TypeExpr.Object.name = "g";
               type_ =
                Odoc_model.Lang.TypeExpr.Arrow (None,
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType "unit")), []),
                 Odoc_model.Lang.TypeExpr.Constr
                  (`Resolved (`Identifier (`CoreType "unit")), []))};
             Odoc_model.Lang.TypeExpr.Object.Method
              {Odoc_model.Lang.TypeExpr.Object.name = "h";
               type_ =
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved
                    (`Identifier
                       (`Type (`Root (<root>, "Ocamlary"), "rec_obj"))),
                 [])}];
           open_ = false});
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "open_obj");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Alias
          (Odoc_model.Lang.TypeExpr.Object
            {Odoc_model.Lang.TypeExpr.Object.fields =
              [Odoc_model.Lang.TypeExpr.Object.Method
                {Odoc_model.Lang.TypeExpr.Object.name = "f";
                 type_ =
                  Odoc_model.Lang.TypeExpr.Constr
                   (`Resolved (`Identifier (`CoreType "int")), [])};
               Odoc_model.Lang.TypeExpr.Object.Method
                {Odoc_model.Lang.TypeExpr.Object.name = "g";
                 type_ =
                  Odoc_model.Lang.TypeExpr.Arrow (None,
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType "unit")), []),
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType "unit")), []))}];
             open_ = true},
          "a"));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id = `Type (`Root (<root>, "Ocamlary"), "oof");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Arrow (None,
          Odoc_model.Lang.TypeExpr.Alias
           (Odoc_model.Lang.TypeExpr.Object
             {Odoc_model.Lang.TypeExpr.Object.fields =
               [Odoc_model.Lang.TypeExpr.Object.Method
                 {Odoc_model.Lang.TypeExpr.Object.name = "a";
                  type_ =
                   Odoc_model.Lang.TypeExpr.Constr
                    (`Resolved (`Identifier (`CoreType "unit")), [])}];
              open_ = true},
           "a"),
          Odoc_model.Lang.TypeExpr.Var "a"));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "any_obj");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Alias
          (Odoc_model.Lang.TypeExpr.Object
            {Odoc_model.Lang.TypeExpr.Object.fields = []; open_ = true},
          "a"));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "empty_obj");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Object
          {Odoc_model.Lang.TypeExpr.Object.fields = []; open_ = false});
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "one_meth");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Object
          {Odoc_model.Lang.TypeExpr.Object.fields =
            [Odoc_model.Lang.TypeExpr.Object.Method
              {Odoc_model.Lang.TypeExpr.Object.name = "meth";
               type_ =
                Odoc_model.Lang.TypeExpr.Constr
                 (`Resolved (`Identifier (`CoreType "unit")), [])}];
           open_ = false});
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id = `Type (`Root (<root>, "Ocamlary"), "ext");
    doc =
     [`Paragraph
        [`Word "A"; `Space; `Word "mystery"; `Space; `Word "wrapped"; `Space;
         `Word "in"; `Space; `Word "an"; `Space; `Word "ellipsis"]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest = None; constraints = []};
    representation = Some Odoc_model.Lang.TypeDecl.Representation.Extensible});
  Odoc_model.Lang.Signature.TypExt
   {Odoc_model.Lang.Extension.type_path =
     `Resolved (`Identifier (`Type (`Root (<root>, "Ocamlary"), "ext")));
    doc = []; type_params = []; private_ = false;
    constructors =
     [{Odoc_model.Lang.Extension.Constructor.id =
        `Extension (`Root (<root>, "Ocamlary"), "ExtA");
       doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
       res = None}]};
  Odoc_model.Lang.Signature.TypExt
   {Odoc_model.Lang.Extension.type_path =
     `Resolved (`Identifier (`Type (`Root (<root>, "Ocamlary"), "ext")));
    doc = []; type_params = []; private_ = false;
    constructors =
     [{Odoc_model.Lang.Extension.Constructor.id =
        `Extension (`Root (<root>, "Ocamlary"), "ExtB");
       doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
       res = None}]};
  Odoc_model.Lang.Signature.TypExt
   {Odoc_model.Lang.Extension.type_path =
     `Resolved (`Identifier (`Type (`Root (<root>, "Ocamlary"), "ext")));
    doc = []; type_params = []; private_ = false;
    constructors =
     [{Odoc_model.Lang.Extension.Constructor.id =
        `Extension (`Root (<root>, "Ocamlary"), "ExtC");
       doc = [];
       args =
        Odoc_model.Lang.TypeDecl.Constructor.Tuple
         [Odoc_model.Lang.TypeExpr.Constr
           (`Resolved (`Identifier (`CoreType "unit")), [])];
       res = None};
      {Odoc_model.Lang.Extension.Constructor.id =
        `Extension (`Root (<root>, "Ocamlary"), "ExtD");
       doc = [];
       args =
        Odoc_model.Lang.TypeDecl.Constructor.Tuple
         [Odoc_model.Lang.TypeExpr.Constr
           (`Resolved
              (`Identifier (`Type (`Root (<root>, "Ocamlary"), "ext"))),
           [])];
       res = None}]};
  Odoc_model.Lang.Signature.TypExt
   {Odoc_model.Lang.Extension.type_path =
     `Resolved (`Identifier (`Type (`Root (<root>, "Ocamlary"), "ext")));
    doc = []; type_params = []; private_ = false;
    constructors =
     [{Odoc_model.Lang.Extension.Constructor.id =
        `Extension (`Root (<root>, "Ocamlary"), "ExtE");
       doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
       res = None}]};
  Odoc_model.Lang.Signature.TypExt
   {Odoc_model.Lang.Extension.type_path =
     `Resolved (`Identifier (`Type (`Root (<root>, "Ocamlary"), "ext")));
    doc = []; type_params = []; private_ = true;
    constructors =
     [{Odoc_model.Lang.Extension.Constructor.id =
        `Extension (`Root (<root>, "Ocamlary"), "ExtF");
       doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
       res = None}]};
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "poly_ext");
    doc = [`Paragraph [`Word "'a"; `Space; `Word "poly_ext"]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false; manifest = None; constraints = []};
    representation = Some Odoc_model.Lang.TypeDecl.Representation.Extensible});
  Odoc_model.Lang.Signature.TypExt
   {Odoc_model.Lang.Extension.type_path =
     `Resolved (`Identifier (`Type (`Root (<root>, "Ocamlary"), "poly_ext")));
    doc = []; type_params = [(Odoc_model.Lang.TypeDecl.Var "b", None)];
    private_ = false;
    constructors =
     [{Odoc_model.Lang.Extension.Constructor.id =
        `Extension (`Root (<root>, "Ocamlary"), "Foo");
       doc = [];
       args =
        Odoc_model.Lang.TypeDecl.Constructor.Tuple
         [Odoc_model.Lang.TypeExpr.Var "b"];
       res = None};
      {Odoc_model.Lang.Extension.Constructor.id =
        `Extension (`Root (<root>, "Ocamlary"), "Bar");
       doc = [`Paragraph [`Word "'b"; `Space; `Word "poly_ext"]];
       args =
        Odoc_model.Lang.TypeDecl.Constructor.Tuple
         [Odoc_model.Lang.TypeExpr.Var "b"; Odoc_model.Lang.TypeExpr.Var "b"];
       res = None}]};
  Odoc_model.Lang.Signature.TypExt
   {Odoc_model.Lang.Extension.type_path =
     `Resolved (`Identifier (`Type (`Root (<root>, "Ocamlary"), "poly_ext")));
    doc = []; type_params = [(Odoc_model.Lang.TypeDecl.Var "c", None)];
    private_ = false;
    constructors =
     [{Odoc_model.Lang.Extension.Constructor.id =
        `Extension (`Root (<root>, "Ocamlary"), "Quux");
       doc = [`Paragraph [`Word "'c"; `Space; `Word "poly_ext"]];
       args =
        Odoc_model.Lang.TypeDecl.Constructor.Tuple
         [Odoc_model.Lang.TypeExpr.Var "c"];
       res = None}]};
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "ExtMod");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "ExtMod"), "t");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation =
            Some Odoc_model.Lang.TypeDecl.Representation.Extensible});
         Odoc_model.Lang.Signature.TypExt
          {Odoc_model.Lang.Extension.type_path =
            `Resolved
              (`Identifier
                 (`Type (`Module (`Root (<root>, "Ocamlary"), "ExtMod"), "t")));
           doc = []; type_params = []; private_ = false;
           constructors =
            [{Odoc_model.Lang.Extension.Constructor.id =
               `Extension
                 (`Module (`Root (<root>, "Ocamlary"), "ExtMod"),
                  "Leisureforce");
              doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
              res = None}]}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.TypExt
   {Odoc_model.Lang.Extension.type_path =
     `Dot
       (`Resolved
          (`Identifier (`Module (`Root (<root>, "Ocamlary"), "ExtMod"))),
        "t");
    doc = []; type_params = []; private_ = false;
    constructors =
     [{Odoc_model.Lang.Extension.Constructor.id =
        `Extension (`Root (<root>, "Ocamlary"), "ZzzTop0");
       doc =
        [`Paragraph
           [`Word "It's"; `Space; `Word "got"; `Space; `Word "the"; `Space;
            `Word "rock"]];
       args = Odoc_model.Lang.TypeDecl.Constructor.Tuple []; res = None}]};
  Odoc_model.Lang.Signature.TypExt
   {Odoc_model.Lang.Extension.type_path =
     `Dot
       (`Resolved
          (`Identifier (`Module (`Root (<root>, "Ocamlary"), "ExtMod"))),
        "t");
    doc = []; type_params = []; private_ = false;
    constructors =
     [{Odoc_model.Lang.Extension.Constructor.id =
        `Extension (`Root (<root>, "Ocamlary"), "ZzzTop");
       doc =
        [`Paragraph
           [`Word "and"; `Space; `Word "it"; `Space; `Word "packs"; `Space;
            `Word "a"; `Space; `Word "unit."]];
       args =
        Odoc_model.Lang.TypeDecl.Constructor.Tuple
         [Odoc_model.Lang.TypeExpr.Constr
           (`Resolved (`Identifier (`CoreType "unit")), [])];
       res = None}]};
  Odoc_model.Lang.Signature.External
   {Odoc_model.Lang.External.id =
     `Value (`Root (<root>, "Ocamlary"), "launch_missiles");
    doc =
     [`Paragraph
        [`Word "Rotate"; `Space; `Word "keys"; `Space; `Word "on"; `Space;
         `Word "my"; `Space; `Word "mark..."]];
    type_ =
     Odoc_model.Lang.TypeExpr.Arrow (None,
      Odoc_model.Lang.TypeExpr.Constr
       (`Resolved (`Identifier (`CoreType "unit")), []),
      Odoc_model.Lang.TypeExpr.Constr
       (`Resolved (`Identifier (`CoreType "unit")), []));
    primitives = ["tetris"]};
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "my_mod");
    doc =
     [`Paragraph
        [`Word "A"; `Space; `Word "brown"; `Space; `Word "paper"; `Space;
         `Word "package"; `Space; `Word "tied"; `Space; `Word "up"; `Space;
         `Word "with"; `Space; `Word "string"]];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Package
          {Odoc_model.Lang.TypeExpr.Package.path =
            `Resolved
              (`Identifier
                 (`ModuleType (`Root (<root>, "Ocamlary"), "COLLECTION")));
           substitutions = []});
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Class (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Class.id =
     `Class (`Root (<root>, "Ocamlary"), "empty_class");
    doc = []; virtual_ = false; params = [];
    type_ =
     Odoc_model.Lang.Class.ClassType
      (Odoc_model.Lang.ClassType.Signature
        {Odoc_model.Lang.ClassSignature.self = None; items = []});
    expansion = None});
  Odoc_model.Lang.Signature.Class (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Class.id =
     `Class (`Root (<root>, "Ocamlary"), "one_method_class");
    doc = []; virtual_ = false; params = [];
    type_ =
     Odoc_model.Lang.Class.ClassType
      (Odoc_model.Lang.ClassType.Signature
        {Odoc_model.Lang.ClassSignature.self = None;
         items =
          [Odoc_model.Lang.ClassSignature.Method
            {Odoc_model.Lang.Method.id =
              `Method
                (`Class (`Root (<root>, "Ocamlary"), "one_method_class"),
                 "go");
             doc = []; private_ = false; virtual_ = false;
             type_ =
              Odoc_model.Lang.TypeExpr.Constr
               (`Resolved (`Identifier (`CoreType "unit")), [])}]});
    expansion = None});
  Odoc_model.Lang.Signature.Class (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Class.id =
     `Class (`Root (<root>, "Ocamlary"), "two_method_class");
    doc = []; virtual_ = false; params = [];
    type_ =
     Odoc_model.Lang.Class.ClassType
      (Odoc_model.Lang.ClassType.Signature
        {Odoc_model.Lang.ClassSignature.self = None;
         items =
          [Odoc_model.Lang.ClassSignature.Method
            {Odoc_model.Lang.Method.id =
              `Method
                (`Class (`Root (<root>, "Ocamlary"), "two_method_class"),
                 "one");
             doc = []; private_ = false; virtual_ = false;
             type_ =
              Odoc_model.Lang.TypeExpr.Constr
               (`Resolved
                  (`Identifier
                     (`Class (`Root (<root>, "Ocamlary"), "one_method_class"))),
               [])};
           Odoc_model.Lang.ClassSignature.Method
            {Odoc_model.Lang.Method.id =
              `Method
                (`Class (`Root (<root>, "Ocamlary"), "two_method_class"),
                 "undo");
             doc = []; private_ = false; virtual_ = false;
             type_ =
              Odoc_model.Lang.TypeExpr.Constr
               (`Resolved (`Identifier (`CoreType "unit")), [])}]});
    expansion = None});
  Odoc_model.Lang.Signature.Class (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Class.id =
     `Class (`Root (<root>, "Ocamlary"), "param_class");
    doc = []; virtual_ = false;
    params = [(Odoc_model.Lang.TypeDecl.Var "a", None)];
    type_ =
     Odoc_model.Lang.Class.Arrow (None, Odoc_model.Lang.TypeExpr.Var "a",
      Odoc_model.Lang.Class.ClassType
       (Odoc_model.Lang.ClassType.Signature
         {Odoc_model.Lang.ClassSignature.self = None;
          items =
           [Odoc_model.Lang.ClassSignature.Method
             {Odoc_model.Lang.Method.id =
               `Method
                 (`Class (`Root (<root>, "Ocamlary"), "param_class"), "v");
              doc = []; private_ = false; virtual_ = false;
              type_ = Odoc_model.Lang.TypeExpr.Var "a"}]}));
    expansion = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "my_unit_object");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Resolved
             (`Identifier
                (`Class (`Root (<root>, "Ocamlary"), "param_class"))),
          [Odoc_model.Lang.TypeExpr.Constr
            (`Resolved (`Identifier (`CoreType "unit")), [])]));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id =
     `Type (`Root (<root>, "Ocamlary"), "my_unit_class");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params =
       [(Odoc_model.Lang.TypeDecl.Var "a", None)];
      private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Alias
          (Odoc_model.Lang.TypeExpr.Class
            (`Resolved
               (`Identifier
                  (`Class (`Root (<root>, "Ocamlary"), "param_class"))),
            [Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType "unit")), [])]),
          "a"));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Dep1");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType (`Module (`Root (<root>, "Ocamlary"), "Dep1"), "S");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Class
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Class.id =
                   `Class
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "Dep1"), "S"),
                      "c");
                  doc = []; virtual_ = false; params = [];
                  type_ =
                   Odoc_model.Lang.Class.ClassType
                    (Odoc_model.Lang.ClassType.Signature
                      {Odoc_model.Lang.ClassSignature.self = None;
                       items =
                        [Odoc_model.Lang.ClassSignature.Method
                          {Odoc_model.Lang.Method.id =
                            `Method
                              (`Class
                                 (`ModuleType
                                    (`Module
                                       (`Root (<root>, "Ocamlary"), "Dep1"),
                                     "S"),
                                  "c"),
                               "m");
                           doc = []; private_ = false; virtual_ = false;
                           type_ =
                            Odoc_model.Lang.TypeExpr.Constr
                             (`Resolved (`Identifier (`CoreType "int")),
                             [])}]});
                  expansion = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig};
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`Module (`Root (<root>, "Ocamlary"), "Dep1"), "X");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Dep1"), "X"),
                      "Y");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.ModuleType
                    (Odoc_model.Lang.ModuleType.Path
                      (`Resolved
                         (`Identifier
                            (`ModuleType
                               (`Module (`Root (<root>, "Ocamlary"), "Dep1"),
                                "S")))));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None})]);
           canonical = None; hidden = false; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig})]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Dep2");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Functor
        (Some
          {Odoc_model.Lang.FunctorArgument.id =
            `Parameter (`Module (`Root (<root>, "Ocamlary"), "Dep2"), "Arg");
           expr =
            Odoc_model.Lang.ModuleType.Signature
             [Odoc_model.Lang.Signature.ModuleType
               {Odoc_model.Lang.ModuleType.id =
                 `ModuleType
                   (`Parameter
                      (`Module (`Root (<root>, "Ocamlary"), "Dep2"), "Arg"),
                    "S");
                doc = []; expr = None; expansion = None};
              Odoc_model.Lang.Signature.Module
               (Odoc_model.Lang.Signature.Ordinary,
               {Odoc_model.Lang.Module.id =
                 `Module
                   (`Parameter
                      (`Module (`Root (<root>, "Ocamlary"), "Dep2"), "Arg"),
                    "X");
                doc = [];
                type_ =
                 Odoc_model.Lang.Module.ModuleType
                  (Odoc_model.Lang.ModuleType.Signature
                    [Odoc_model.Lang.Signature.Module
                      (Odoc_model.Lang.Signature.Ordinary,
                      {Odoc_model.Lang.Module.id =
                        `Module
                          (`Module
                             (`Parameter
                                (`Module (`Root (<root>, "Ocamlary"), "Dep2"),
                                 "Arg"),
                              "X"),
                           "Y");
                       doc = [];
                       type_ =
                        Odoc_model.Lang.Module.ModuleType
                         (Odoc_model.Lang.ModuleType.Path
                           (`Resolved
                              (`Identifier
                                 (`ModuleType
                                    (`Parameter
                                       (`Module
                                          (`Root (<root>, "Ocamlary"),
                                           "Dep2"),
                                        "Arg"),
                                     "S")))));
                       canonical = None; hidden = false; display_type = None;
                       expansion = None})]);
                canonical = None; hidden = false; display_type = None;
                expansion = Some Odoc_model.Lang.Module.AlreadyASig})];
           expansion = Some Odoc_model.Lang.Module.AlreadyASig},
        Odoc_model.Lang.ModuleType.Signature
         [Odoc_model.Lang.Signature.Module
           (Odoc_model.Lang.Signature.Ordinary,
           {Odoc_model.Lang.Module.id =
             `Module
               (`Result (`Module (`Root (<root>, "Ocamlary"), "Dep2")), "A");
            doc = [];
            type_ =
             Odoc_model.Lang.Module.ModuleType
              (Odoc_model.Lang.ModuleType.Signature
                [Odoc_model.Lang.Signature.Module
                  (Odoc_model.Lang.Signature.Ordinary,
                  {Odoc_model.Lang.Module.id =
                    `Module
                      (`Module
                         (`Result
                            (`Module (`Root (<root>, "Ocamlary"), "Dep2")),
                          "A"),
                       "Y");
                   doc = [];
                   type_ =
                    Odoc_model.Lang.Module.ModuleType
                     (Odoc_model.Lang.ModuleType.Path
                       (`Dot
                          (`Resolved
                             (`Identifier
                                (`Parameter
                                   (`Module
                                      (`Root (<root>, "Ocamlary"), "Dep2"),
                                    "Arg"))),
                           "S")));
                   canonical = None; hidden = false; display_type = None;
                   expansion = None})]);
            canonical = None; hidden = false; display_type = None;
            expansion = Some Odoc_model.Lang.Module.AlreadyASig});
          Odoc_model.Lang.Signature.Module
           (Odoc_model.Lang.Signature.Ordinary,
           {Odoc_model.Lang.Module.id =
             `Module
               (`Result (`Module (`Root (<root>, "Ocamlary"), "Dep2")), "B");
            doc = [];
            type_ =
             Odoc_model.Lang.Module.Alias
              (`Dot
                 (`Resolved
                    (`Identifier
                       (`Module
                          (`Result
                             (`Module (`Root (<root>, "Ocamlary"), "Dep2")),
                           "A"))),
                  "Y"));
            canonical = None; hidden = false; display_type = None;
            expansion = None})]));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id = `Type (`Root (<root>, "Ocamlary"), "dep1");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Dot
             (`Dot
                (`Apply
                   (`Resolved
                      (`Identifier
                         (`Module (`Root (<root>, "Ocamlary"), "Dep2"))),
                    `Resolved
                      (`Identifier
                         (`Module (`Root (<root>, "Ocamlary"), "Dep1")))),
                 "B"),
              "c"),
          []));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Dep3");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "Dep3"), "a");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None})]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Dep4");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType (`Module (`Root (<root>, "Ocamlary"), "Dep4"), "T");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "Dep4"), "T"),
                      "b");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig};
         Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType (`Module (`Root (<root>, "Ocamlary"), "Dep4"), "S");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "Dep4"), "S"),
                      "X");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.ModuleType
                    (Odoc_model.Lang.ModuleType.Path
                      (`Resolved
                         (`Identifier
                            (`ModuleType
                               (`Module (`Root (<root>, "Ocamlary"), "Dep4"),
                                "T")))));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "Dep4"), "S"),
                      "Y");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.ModuleType
                    (Odoc_model.Lang.ModuleType.Signature []);
                  canonical = None; hidden = false; display_type = None;
                  expansion = Some Odoc_model.Lang.Module.AlreadyASig})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig};
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`Module (`Root (<root>, "Ocamlary"), "Dep4"), "X");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Path
               (`Resolved
                  (`Identifier
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "Dep4"), "T")))));
           canonical = None; hidden = false; display_type = None;
           expansion = None})]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Dep5");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Functor
        (Some
          {Odoc_model.Lang.FunctorArgument.id =
            `Parameter (`Module (`Root (<root>, "Ocamlary"), "Dep5"), "Arg");
           expr =
            Odoc_model.Lang.ModuleType.Signature
             [Odoc_model.Lang.Signature.ModuleType
               {Odoc_model.Lang.ModuleType.id =
                 `ModuleType
                   (`Parameter
                      (`Module (`Root (<root>, "Ocamlary"), "Dep5"), "Arg"),
                    "T");
                doc = []; expr = None; expansion = None};
              Odoc_model.Lang.Signature.ModuleType
               {Odoc_model.Lang.ModuleType.id =
                 `ModuleType
                   (`Parameter
                      (`Module (`Root (<root>, "Ocamlary"), "Dep5"), "Arg"),
                    "S");
                doc = [];
                expr =
                 Some
                  (Odoc_model.Lang.ModuleType.Signature
                    [Odoc_model.Lang.Signature.Module
                      (Odoc_model.Lang.Signature.Ordinary,
                      {Odoc_model.Lang.Module.id =
                        `Module
                          (`ModuleType
                             (`Parameter
                                (`Module (`Root (<root>, "Ocamlary"), "Dep5"),
                                 "Arg"),
                              "S"),
                           "X");
                       doc = [];
                       type_ =
                        Odoc_model.Lang.Module.ModuleType
                         (Odoc_model.Lang.ModuleType.Path
                           (`Resolved
                              (`Identifier
                                 (`ModuleType
                                    (`Parameter
                                       (`Module
                                          (`Root (<root>, "Ocamlary"),
                                           "Dep5"),
                                        "Arg"),
                                     "T")))));
                       canonical = None; hidden = false; display_type = None;
                       expansion = None});
                     Odoc_model.Lang.Signature.Module
                      (Odoc_model.Lang.Signature.Ordinary,
                      {Odoc_model.Lang.Module.id =
                        `Module
                          (`ModuleType
                             (`Parameter
                                (`Module (`Root (<root>, "Ocamlary"), "Dep5"),
                                 "Arg"),
                              "S"),
                           "Y");
                       doc = [];
                       type_ =
                        Odoc_model.Lang.Module.ModuleType
                         (Odoc_model.Lang.ModuleType.Signature []);
                       canonical = None; hidden = false; display_type = None;
                       expansion = Some Odoc_model.Lang.Module.AlreadyASig})]);
                expansion = Some Odoc_model.Lang.Module.AlreadyASig};
              Odoc_model.Lang.Signature.Module
               (Odoc_model.Lang.Signature.Ordinary,
               {Odoc_model.Lang.Module.id =
                 `Module
                   (`Parameter
                      (`Module (`Root (<root>, "Ocamlary"), "Dep5"), "Arg"),
                    "X");
                doc = [];
                type_ =
                 Odoc_model.Lang.Module.ModuleType
                  (Odoc_model.Lang.ModuleType.Path
                    (`Resolved
                       (`Identifier
                          (`ModuleType
                             (`Parameter
                                (`Module (`Root (<root>, "Ocamlary"), "Dep5"),
                                 "Arg"),
                              "T")))));
                canonical = None; hidden = false; display_type = None;
                expansion = None})];
           expansion = Some Odoc_model.Lang.Module.AlreadyASig},
        Odoc_model.Lang.ModuleType.Signature
         [Odoc_model.Lang.Signature.Module
           (Odoc_model.Lang.Signature.Ordinary,
           {Odoc_model.Lang.Module.id =
             `Module
               (`Result (`Module (`Root (<root>, "Ocamlary"), "Dep5")), "Z");
            doc = [];
            type_ =
             Odoc_model.Lang.Module.ModuleType
              (Odoc_model.Lang.ModuleType.With
                (Odoc_model.Lang.ModuleType.Path
                  (`Dot
                     (`Resolved
                        (`Identifier
                           (`Parameter
                              (`Module (`Root (<root>, "Ocamlary"), "Dep5"),
                               "Arg"))),
                      "S")),
                [Odoc_model.Lang.ModuleType.ModuleEq
                  (`Dot (`Resolved `Root, "Y"),
                  Odoc_model.Lang.Module.Alias
                   (`Resolved
                      (`Identifier
                         (`Module (`Root (<root>, "Ocamlary"), "Dep3")))))]));
            canonical = None; hidden = false; display_type = None;
            expansion = None})]));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id = `Type (`Root (<root>, "Ocamlary"), "dep2");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Dot
             (`Dot
                (`Dot
                   (`Apply
                      (`Resolved
                         (`Identifier
                            (`Module (`Root (<root>, "Ocamlary"), "Dep5"))),
                       `Resolved
                         (`Identifier
                            (`Module (`Root (<root>, "Ocamlary"), "Dep4")))),
                    "Z"),
                 "X"),
              "b"),
          []));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id = `Type (`Root (<root>, "Ocamlary"), "dep3");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Dot
             (`Dot
                (`Dot
                   (`Apply
                      (`Resolved
                         (`Identifier
                            (`Module (`Root (<root>, "Ocamlary"), "Dep5"))),
                       `Resolved
                         (`Identifier
                            (`Module (`Root (<root>, "Ocamlary"), "Dep4")))),
                    "Z"),
                 "Y"),
              "a"),
          []));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Dep6");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType (`Module (`Root (<root>, "Ocamlary"), "Dep6"), "S");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "Dep6"), "S"),
                      "d");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig};
         Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType (`Module (`Root (<root>, "Ocamlary"), "Dep6"), "T");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.ModuleType
                 {Odoc_model.Lang.ModuleType.id =
                   `ModuleType
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "Dep6"), "T"),
                      "R");
                  doc = [];
                  expr =
                   Some
                    (Odoc_model.Lang.ModuleType.Path
                      (`Resolved
                         (`Identifier
                            (`ModuleType
                               (`Module (`Root (<root>, "Ocamlary"), "Dep6"),
                                "S")))));
                  expansion = None};
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "Dep6"), "T"),
                      "Y");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.ModuleType
                    (Odoc_model.Lang.ModuleType.Path
                      (`Resolved
                         (`Identifier
                            (`ModuleType
                               (`ModuleType
                                  (`Module
                                     (`Root (<root>, "Ocamlary"), "Dep6"),
                                   "T"),
                                "R")))));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig};
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`Module (`Root (<root>, "Ocamlary"), "Dep6"), "X");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Path
               (`Resolved
                  (`Identifier
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "Dep6"), "T")))));
           canonical = None; hidden = false; display_type = None;
           expansion = None})]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Dep7");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Functor
        (Some
          {Odoc_model.Lang.FunctorArgument.id =
            `Parameter (`Module (`Root (<root>, "Ocamlary"), "Dep7"), "Arg");
           expr =
            Odoc_model.Lang.ModuleType.Signature
             [Odoc_model.Lang.Signature.ModuleType
               {Odoc_model.Lang.ModuleType.id =
                 `ModuleType
                   (`Parameter
                      (`Module (`Root (<root>, "Ocamlary"), "Dep7"), "Arg"),
                    "S");
                doc = []; expr = None; expansion = None};
              Odoc_model.Lang.Signature.ModuleType
               {Odoc_model.Lang.ModuleType.id =
                 `ModuleType
                   (`Parameter
                      (`Module (`Root (<root>, "Ocamlary"), "Dep7"), "Arg"),
                    "T");
                doc = [];
                expr =
                 Some
                  (Odoc_model.Lang.ModuleType.Signature
                    [Odoc_model.Lang.Signature.ModuleType
                      {Odoc_model.Lang.ModuleType.id =
                        `ModuleType
                          (`ModuleType
                             (`Parameter
                                (`Module (`Root (<root>, "Ocamlary"), "Dep7"),
                                 "Arg"),
                              "T"),
                           "R");
                       doc = [];
                       expr =
                        Some
                         (Odoc_model.Lang.ModuleType.Path
                           (`Resolved
                              (`Identifier
                                 (`ModuleType
                                    (`Parameter
                                       (`Module
                                          (`Root (<root>, "Ocamlary"),
                                           "Dep7"),
                                        "Arg"),
                                     "S")))));
                       expansion = None};
                     Odoc_model.Lang.Signature.Module
                      (Odoc_model.Lang.Signature.Ordinary,
                      {Odoc_model.Lang.Module.id =
                        `Module
                          (`ModuleType
                             (`Parameter
                                (`Module (`Root (<root>, "Ocamlary"), "Dep7"),
                                 "Arg"),
                              "T"),
                           "Y");
                       doc = [];
                       type_ =
                        Odoc_model.Lang.Module.ModuleType
                         (Odoc_model.Lang.ModuleType.Path
                           (`Resolved
                              (`Identifier
                                 (`ModuleType
                                    (`ModuleType
                                       (`Parameter
                                          (`Module
                                             (`Root (<root>, "Ocamlary"),
                                              "Dep7"),
                                           "Arg"),
                                        "T"),
                                     "R")))));
                       canonical = None; hidden = false; display_type = None;
                       expansion = None})]);
                expansion = Some Odoc_model.Lang.Module.AlreadyASig};
              Odoc_model.Lang.Signature.Module
               (Odoc_model.Lang.Signature.Ordinary,
               {Odoc_model.Lang.Module.id =
                 `Module
                   (`Parameter
                      (`Module (`Root (<root>, "Ocamlary"), "Dep7"), "Arg"),
                    "X");
                doc = [];
                type_ =
                 Odoc_model.Lang.Module.ModuleType
                  (Odoc_model.Lang.ModuleType.Path
                    (`Resolved
                       (`Identifier
                          (`ModuleType
                             (`Parameter
                                (`Module (`Root (<root>, "Ocamlary"), "Dep7"),
                                 "Arg"),
                              "T")))));
                canonical = None; hidden = false; display_type = None;
                expansion = None})];
           expansion = Some Odoc_model.Lang.Module.AlreadyASig},
        Odoc_model.Lang.ModuleType.Signature
         [Odoc_model.Lang.Signature.Module
           (Odoc_model.Lang.Signature.Ordinary,
           {Odoc_model.Lang.Module.id =
             `Module
               (`Result (`Module (`Root (<root>, "Ocamlary"), "Dep7")), "M");
            doc = [];
            type_ =
             Odoc_model.Lang.Module.ModuleType
              (Odoc_model.Lang.ModuleType.Path
                (`Dot
                   (`Resolved
                      (`Identifier
                         (`Parameter
                            (`Module (`Root (<root>, "Ocamlary"), "Dep7"),
                             "Arg"))),
                    "T")));
            canonical = None; hidden = false; display_type = None;
            expansion = None})]));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id = `Type (`Root (<root>, "Ocamlary"), "dep4");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Dot
             (`Dot
                (`Dot
                   (`Apply
                      (`Resolved
                         (`Identifier
                            (`Module (`Root (<root>, "Ocamlary"), "Dep7"))),
                       `Resolved
                         (`Identifier
                            (`Module (`Root (<root>, "Ocamlary"), "Dep6")))),
                    "M"),
                 "Y"),
              "d"),
          []));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Dep8");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType (`Module (`Root (<root>, "Ocamlary"), "Dep8"), "T");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "Dep8"), "T"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Dep9");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Functor
        (Some
          {Odoc_model.Lang.FunctorArgument.id =
            `Parameter (`Module (`Root (<root>, "Ocamlary"), "Dep9"), "X");
           expr =
            Odoc_model.Lang.ModuleType.Signature
             [Odoc_model.Lang.Signature.ModuleType
               {Odoc_model.Lang.ModuleType.id =
                 `ModuleType
                   (`Parameter
                      (`Module (`Root (<root>, "Ocamlary"), "Dep9"), "X"),
                    "T");
                doc = []; expr = None; expansion = None}];
           expansion = Some Odoc_model.Lang.Module.AlreadyASig},
        Odoc_model.Lang.ModuleType.Signature
         [Odoc_model.Lang.Signature.ModuleType
           {Odoc_model.Lang.ModuleType.id =
             `ModuleType
               (`Result (`Module (`Root (<root>, "Ocamlary"), "Dep9")), "T");
            doc = [];
            expr =
             Some
              (Odoc_model.Lang.ModuleType.Path
                (`Dot
                   (`Resolved
                      (`Identifier
                         (`Parameter
                            (`Module (`Root (<root>, "Ocamlary"), "Dep9"),
                             "X"))),
                    "T")));
            expansion = None}]));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "Dep10");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.With
        (Odoc_model.Lang.ModuleType.Path
          (`Dot
             (`Apply
                (`Resolved
                   (`Identifier
                      (`Module (`Root (<root>, "Ocamlary"), "Dep9"))),
                 `Resolved
                   (`Identifier
                      (`Module (`Root (<root>, "Ocamlary"), "Dep8")))),
              "T")),
        [Odoc_model.Lang.ModuleType.TypeEq (`Dot (`Resolved `Root, "t"),
          {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
           manifest =
            Some
             (Odoc_model.Lang.TypeExpr.Constr
               (`Resolved (`Identifier (`CoreType "int")), []));
           constraints = []})]));
    expansion = None};
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Dep11");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType (`Module (`Root (<root>, "Ocamlary"), "Dep11"), "S");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Class
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Class.id =
                   `Class
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "Dep11"), "S"),
                      "c");
                  doc = []; virtual_ = false; params = [];
                  type_ =
                   Odoc_model.Lang.Class.ClassType
                    (Odoc_model.Lang.ClassType.Signature
                      {Odoc_model.Lang.ClassSignature.self = None;
                       items =
                        [Odoc_model.Lang.ClassSignature.Method
                          {Odoc_model.Lang.Method.id =
                            `Method
                              (`Class
                                 (`ModuleType
                                    (`Module
                                       (`Root (<root>, "Ocamlary"), "Dep11"),
                                     "S"),
                                  "c"),
                               "m");
                           doc = []; private_ = false; virtual_ = false;
                           type_ =
                            Odoc_model.Lang.TypeExpr.Constr
                             (`Resolved (`Identifier (`CoreType "int")),
                             [])}]});
                  expansion = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Dep12");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Functor
        (Some
          {Odoc_model.Lang.FunctorArgument.id =
            `Parameter (`Module (`Root (<root>, "Ocamlary"), "Dep12"), "Arg");
           expr =
            Odoc_model.Lang.ModuleType.Signature
             [Odoc_model.Lang.Signature.ModuleType
               {Odoc_model.Lang.ModuleType.id =
                 `ModuleType
                   (`Parameter
                      (`Module (`Root (<root>, "Ocamlary"), "Dep12"), "Arg"),
                    "S");
                doc = []; expr = None; expansion = None}];
           expansion = Some Odoc_model.Lang.Module.AlreadyASig},
        Odoc_model.Lang.ModuleType.Signature
         [Odoc_model.Lang.Signature.ModuleType
           {Odoc_model.Lang.ModuleType.id =
             `ModuleType
               (`Result (`Module (`Root (<root>, "Ocamlary"), "Dep12")), "T");
            doc = [];
            expr =
             Some
              (Odoc_model.Lang.ModuleType.Path
                (`Dot
                   (`Resolved
                      (`Identifier
                         (`Parameter
                            (`Module (`Root (<root>, "Ocamlary"), "Dep12"),
                             "Arg"))),
                    "S")));
            expansion = None}]));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "Dep13");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Path
        (`Dot
           (`Apply
              (`Resolved
                 (`Identifier (`Module (`Root (<root>, "Ocamlary"), "Dep12"))),
               `Resolved
                 (`Identifier (`Module (`Root (<root>, "Ocamlary"), "Dep11")))),
            "T")));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id = `Type (`Root (<root>, "Ocamlary"), "dep5");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Dot
             (`Resolved
                (`Identifier (`Module (`Root (<root>, "Ocamlary"), "Dep13"))),
              "c"),
          []));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "With1");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`ModuleType (`Root (<root>, "Ocamlary"), "With1"), "M");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.ModuleType
                 {Odoc_model.Lang.ModuleType.id =
                   `ModuleType
                     (`Module
                        (`ModuleType (`Root (<root>, "Ocamlary"), "With1"),
                         "M"),
                      "S");
                  doc = []; expr = None; expansion = None}]);
           canonical = None; hidden = false; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`ModuleType (`Root (<root>, "Ocamlary"), "With1"), "N");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Path
               (`Dot
                  (`Resolved
                     (`Identifier
                        (`Module
                           (`ModuleType (`Root (<root>, "Ocamlary"), "With1"),
                            "M"))),
                   "S")));
           canonical = None; hidden = false; display_type = None;
           expansion = None})]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "With2");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType (`Module (`Root (<root>, "Ocamlary"), "With2"), "S");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "With2"), "S"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "With3");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.With
        (Odoc_model.Lang.ModuleType.Path
          (`Resolved
             (`Identifier (`ModuleType (`Root (<root>, "Ocamlary"), "With1")))),
        [Odoc_model.Lang.ModuleType.ModuleEq (`Dot (`Resolved `Root, "M"),
          Odoc_model.Lang.Module.Alias
           (`Resolved
              (`Identifier (`Module (`Root (<root>, "Ocamlary"), "With2")))))]));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id = `Type (`Root (<root>, "Ocamlary"), "with1");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Dot
             (`Dot
                (`Resolved
                   (`Identifier
                      (`Module (`Root (<root>, "Ocamlary"), "With3"))),
                 "N"),
              "t"),
          []));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "With4");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.With
        (Odoc_model.Lang.ModuleType.Path
          (`Resolved
             (`Identifier (`ModuleType (`Root (<root>, "Ocamlary"), "With1")))),
        [Odoc_model.Lang.ModuleType.ModuleSubst (`Dot (`Resolved `Root, "M"),
          `Resolved
            (`Identifier (`Module (`Root (<root>, "Ocamlary"), "With2"))))]));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.TypeDecl.id = `Type (`Root (<root>, "Ocamlary"), "with2");
    doc = [];
    equation =
     {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
      manifest =
       Some
        (Odoc_model.Lang.TypeExpr.Constr
          (`Dot
             (`Dot
                (`Resolved
                   (`Identifier
                      (`Module (`Root (<root>, "Ocamlary"), "With4"))),
                 "N"),
              "t"),
          []));
      constraints = []};
    representation = None});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "With5");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType (`Module (`Root (<root>, "Ocamlary"), "With5"), "S");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "With5"), "S"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig};
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`Module (`Root (<root>, "Ocamlary"), "With5"), "N");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Path
               (`Resolved
                  (`Identifier
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "With5"), "S")))));
           canonical = None; hidden = false; display_type = None;
           expansion = None})]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "With6");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType (`Module (`Root (<root>, "Ocamlary"), "With6"), "T");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "With6"), "T"),
                      "M");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.ModuleType
                    (Odoc_model.Lang.ModuleType.Signature
                      [Odoc_model.Lang.Signature.ModuleType
                        {Odoc_model.Lang.ModuleType.id =
                          `ModuleType
                            (`Module
                               (`ModuleType
                                  (`Module
                                     (`Root (<root>, "Ocamlary"), "With6"),
                                   "T"),
                                "M"),
                             "S");
                         doc = []; expr = None; expansion = None};
                       Odoc_model.Lang.Signature.Module
                        (Odoc_model.Lang.Signature.Ordinary,
                        {Odoc_model.Lang.Module.id =
                          `Module
                            (`Module
                               (`ModuleType
                                  (`Module
                                     (`Root (<root>, "Ocamlary"), "With6"),
                                   "T"),
                                "M"),
                             "N");
                         doc = [];
                         type_ =
                          Odoc_model.Lang.Module.ModuleType
                           (Odoc_model.Lang.ModuleType.Path
                             (`Resolved
                                (`Identifier
                                   (`ModuleType
                                      (`Module
                                         (`ModuleType
                                            (`Module
                                               (`Root (<root>, "Ocamlary"),
                                                "With6"),
                                             "T"),
                                          "M"),
                                       "S")))));
                         canonical = None; hidden = false;
                         display_type = None; expansion = None})]);
                  canonical = None; hidden = false; display_type = None;
                  expansion = Some Odoc_model.Lang.Module.AlreadyASig})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "With7");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Functor
        (Some
          {Odoc_model.Lang.FunctorArgument.id =
            `Parameter (`Module (`Root (<root>, "Ocamlary"), "With7"), "X");
           expr =
            Odoc_model.Lang.ModuleType.Signature
             [Odoc_model.Lang.Signature.ModuleType
               {Odoc_model.Lang.ModuleType.id =
                 `ModuleType
                   (`Parameter
                      (`Module (`Root (<root>, "Ocamlary"), "With7"), "X"),
                    "T");
                doc = []; expr = None; expansion = None}];
           expansion = Some Odoc_model.Lang.Module.AlreadyASig},
        Odoc_model.Lang.ModuleType.Signature
         [Odoc_model.Lang.Signature.ModuleType
           {Odoc_model.Lang.ModuleType.id =
             `ModuleType
               (`Result (`Module (`Root (<root>, "Ocamlary"), "With7")), "T");
            doc = [];
            expr =
             Some
              (Odoc_model.Lang.ModuleType.Path
                (`Dot
                   (`Resolved
                      (`Identifier
                         (`Parameter
                            (`Module (`Root (<root>, "Ocamlary"), "With7"),
                             "X"))),
                    "T")));
            expansion = None}]));
    canonical = None; hidden = false; display_type = None; expansion = None});
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "With8");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.With
        (Odoc_model.Lang.ModuleType.Path
          (`Dot
             (`Apply
                (`Resolved
                   (`Identifier
                      (`Module (`Root (<root>, "Ocamlary"), "With7"))),
                 `Resolved
                   (`Identifier
                      (`Module (`Root (<root>, "Ocamlary"), "With6")))),
              "T")),
        [Odoc_model.Lang.ModuleType.ModuleEq (`Dot (`Resolved `Root, "M"),
          Odoc_model.Lang.Module.Alias
           (`Resolved
              (`Identifier (`Module (`Root (<root>, "Ocamlary"), "With5")))));
         Odoc_model.Lang.ModuleType.TypeEq
          (`Dot (`Dot (`Dot (`Resolved `Root, "M"), "N"), "t"),
          {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
           manifest =
            Some
             (Odoc_model.Lang.TypeExpr.Constr
               (`Dot
                  (`Dot
                     (`Resolved
                        (`Identifier
                           (`Module (`Root (<root>, "Ocamlary"), "With5"))),
                      "N"),
                   "t"),
               []));
           constraints = []})]));
    expansion = None};
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "With9");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType (`Module (`Root (<root>, "Ocamlary"), "With9"), "S");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "With9"), "S"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "With10");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType (`Module (`Root (<root>, "Ocamlary"), "With10"), "T");
           doc =
            [`Paragraph
               [`Reference (`Dot (`Root ("With10", `TUnknown), "T"), []);
                `Space; `Word "is"; `Space; `Word "a"; `Space;
                `Word "submodule"; `Space; `Word "type."]];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "With10"), "T"),
                      "M");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.ModuleType
                    (Odoc_model.Lang.ModuleType.Signature
                      [Odoc_model.Lang.Signature.ModuleType
                        {Odoc_model.Lang.ModuleType.id =
                          `ModuleType
                            (`Module
                               (`ModuleType
                                  (`Module
                                     (`Root (<root>, "Ocamlary"), "With10"),
                                   "T"),
                                "M"),
                             "S");
                         doc = []; expr = None; expansion = None}]);
                  canonical = None; hidden = false; display_type = None;
                  expansion = Some Odoc_model.Lang.Module.AlreadyASig});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`ModuleType
                        (`Module (`Root (<root>, "Ocamlary"), "With10"), "T"),
                      "N");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.ModuleType
                    (Odoc_model.Lang.ModuleType.Path
                      (`Dot
                         (`Resolved
                            (`Identifier
                               (`Module
                                  (`ModuleType
                                     (`Module
                                        (`Root (<root>, "Ocamlary"),
                                         "With10"),
                                      "T"),
                                   "M"))),
                          "S")));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "With11");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.With
        (Odoc_model.Lang.ModuleType.Path
          (`Dot
             (`Apply
                (`Resolved
                   (`Identifier
                      (`Module (`Root (<root>, "Ocamlary"), "With7"))),
                 `Resolved
                   (`Identifier
                      (`Module (`Root (<root>, "Ocamlary"), "With10")))),
              "T")),
        [Odoc_model.Lang.ModuleType.ModuleEq (`Dot (`Resolved `Root, "M"),
          Odoc_model.Lang.Module.Alias
           (`Resolved
              (`Identifier (`Module (`Root (<root>, "Ocamlary"), "With9")))));
         Odoc_model.Lang.ModuleType.TypeEq
          (`Dot (`Dot (`Resolved `Root, "N"), "t"),
          {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
           manifest =
            Some
             (Odoc_model.Lang.TypeExpr.Constr
               (`Resolved (`Identifier (`CoreType "int")), []));
           constraints = []})]));
    expansion = None};
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "NestedInclude1");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType
              (`ModuleType (`Root (<root>, "Ocamlary"), "NestedInclude1"),
               "NestedInclude2");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`ModuleType
                           (`Root (<root>, "Ocamlary"), "NestedInclude1"),
                         "NestedInclude2"),
                      "nested_include");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.Include
   {Odoc_model.Lang.Include.parent = `Root (<root>, "Ocamlary"); doc = [];
    decl =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Path
        (`Resolved
           (`Identifier
              (`ModuleType (`Root (<root>, "Ocamlary"), "NestedInclude1")))));
    expansion =
     {Odoc_model.Lang.Include.resolved = false;
      content =
       [Odoc_model.Lang.Signature.ModuleType
         {Odoc_model.Lang.ModuleType.id =
           `ModuleType (`Root (<root>, "Ocamlary"), "NestedInclude2");
          doc = [];
          expr =
           Some
            (Odoc_model.Lang.ModuleType.Signature
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`ModuleType
                       (`Root (<root>, "Ocamlary"), "NestedInclude2"),
                     "nested_include");
                 doc = [];
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})]);
          expansion = Some Odoc_model.Lang.Module.AlreadyASig}]}};
  Odoc_model.Lang.Signature.Include
   {Odoc_model.Lang.Include.parent = `Root (<root>, "Ocamlary"); doc = [];
    decl =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.With
        (Odoc_model.Lang.ModuleType.Path
          (`Resolved
             (`Identifier
                (`ModuleType (`Root (<root>, "Ocamlary"), "NestedInclude2")))),
        [Odoc_model.Lang.ModuleType.TypeEq
          (`Dot (`Resolved `Root, "nested_include"),
          {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
           manifest =
            Some
             (Odoc_model.Lang.TypeExpr.Constr
               (`Resolved (`Identifier (`CoreType "int")), []));
           constraints = []})]));
    expansion =
     {Odoc_model.Lang.Include.resolved = false;
      content =
       [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
         {Odoc_model.Lang.TypeDecl.id =
           `Type (`Root (<root>, "Ocamlary"), "nested_include");
          doc = [];
          equation =
           {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
            manifest =
             Some
              (Odoc_model.Lang.TypeExpr.Constr
                (`Resolved (`Identifier (`CoreType "int")), []));
            constraints = []};
          representation = None})]}};
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "DoubleInclude1");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "DoubleInclude1"),
               "DoubleInclude2");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "DoubleInclude1"),
                         "DoubleInclude2"),
                      "double_include");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           canonical = None; hidden = false; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig})]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "DoubleInclude3");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Include
          {Odoc_model.Lang.Include.parent =
            `Module (`Root (<root>, "Ocamlary"), "DoubleInclude3");
           doc = [];
           decl =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.TypeOf
               (Odoc_model.Lang.Module.Alias
                 (`Resolved
                    (`Identifier
                       (`Module
                          (`Root (<root>, "Ocamlary"), "DoubleInclude1"))))));
           expansion =
            {Odoc_model.Lang.Include.resolved = false;
             content =
              [Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`Module (`Root (<root>, "Ocamlary"), "DoubleInclude3"),
                     "DoubleInclude2");
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.ModuleType
                   (Odoc_model.Lang.ModuleType.Signature
                     [Odoc_model.Lang.Signature.Type
                       (Odoc_model.Lang.Signature.Ordinary,
                       {Odoc_model.Lang.TypeDecl.id =
                         `Type
                           (`Module
                              (`Module
                                 (`Root (<root>, "Ocamlary"),
                                  "DoubleInclude3"),
                               "DoubleInclude2"),
                            "double_include");
                        doc = [];
                        equation =
                         {Odoc_model.Lang.TypeDecl.Equation.params = [];
                          private_ = false; manifest = None;
                          constraints = []};
                        representation = None})]);
                 canonical = None; hidden = false; display_type = None;
                 expansion = Some Odoc_model.Lang.Module.AlreadyASig})]}}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Include
   {Odoc_model.Lang.Include.parent = `Root (<root>, "Ocamlary"); doc = [];
    decl =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.TypeOf
        (Odoc_model.Lang.Module.Alias
          (`Dot
             (`Resolved
                (`Identifier
                   (`Module (`Root (<root>, "Ocamlary"), "DoubleInclude3"))),
              "DoubleInclude2"))));
    expansion =
     {Odoc_model.Lang.Include.resolved = false;
      content =
       [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
         {Odoc_model.Lang.TypeDecl.id =
           `Type (`Root (<root>, "Ocamlary"), "double_include");
          doc = [];
          equation =
           {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
            manifest = None; constraints = []};
          representation = None})]}};
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "IncludeInclude1");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType
              (`Module (`Root (<root>, "Ocamlary"), "IncludeInclude1"),
               "IncludeInclude2");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`Module
                           (`Root (<root>, "Ocamlary"), "IncludeInclude1"),
                         "IncludeInclude2"),
                      "include_include");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig}]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Include
   {Odoc_model.Lang.Include.parent = `Root (<root>, "Ocamlary"); doc = [];
    decl =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.TypeOf
        (Odoc_model.Lang.Module.Alias
          (`Resolved
             (`Identifier
                (`Module (`Root (<root>, "Ocamlary"), "IncludeInclude1"))))));
    expansion =
     {Odoc_model.Lang.Include.resolved = false;
      content =
       [Odoc_model.Lang.Signature.ModuleType
         {Odoc_model.Lang.ModuleType.id =
           `ModuleType (`Root (<root>, "Ocamlary"), "IncludeInclude2");
          doc = [];
          expr =
           Some
            (Odoc_model.Lang.ModuleType.Signature
              [Odoc_model.Lang.Signature.Type
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.TypeDecl.id =
                  `Type
                    (`ModuleType
                       (`Root (<root>, "Ocamlary"), "IncludeInclude2"),
                     "include_include");
                 doc = [];
                 equation =
                  {Odoc_model.Lang.TypeDecl.Equation.params = [];
                   private_ = false; manifest = None; constraints = []};
                 representation = None})]);
          expansion = Some Odoc_model.Lang.Module.AlreadyASig}]}};
  Odoc_model.Lang.Signature.Include
   {Odoc_model.Lang.Include.parent = `Root (<root>, "Ocamlary"); doc = [];
    decl =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Path
        (`Resolved
           (`Identifier
              (`ModuleType (`Root (<root>, "Ocamlary"), "IncludeInclude2")))));
    expansion =
     {Odoc_model.Lang.Include.resolved = false;
      content =
       [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
         {Odoc_model.Lang.TypeDecl.id =
           `Type (`Root (<root>, "Ocamlary"), "include_include");
          doc = [];
          equation =
           {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
            manifest = None; constraints = []};
          representation = None})]}};
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Section, `Label (`Root (<root>, "Ocamlary"), "indexmodules"),
          [`Word "Trying"; `Space; `Word "the"; `Space; `Word "{!modules:";
           `Space; `Word "...}"; `Space; `Word "command."]);
       `Paragraph
         [`Word "With"; `Space; `Word "ocamldoc,"; `Space; `Word "toplevel";
          `Space; `Word "units"; `Space; `Word "will"; `Space; `Word "be";
          `Space; `Word "linked"; `Space; `Word "and"; `Space;
          `Word "documented,"; `Space; `Word "while"; `Space;
          `Word "submodules"; `Space; `Word "will"; `Space; `Word "behave";
          `Space; `Word "as"; `Space; `Word "simple"; `Space;
          `Word "references."];
       `Paragraph
         [`Word "With"; `Space; `Word "odoc,"; `Space; `Word "everything";
          `Space; `Word "should"; `Space; `Word "be"; `Space;
          `Word "resolved"; `Space; `Word "(and"; `Space; `Word "linked)";
          `Space; `Word "but"; `Space; `Word "only"; `Space;
          `Word "toplevel"; `Space; `Word "units"; `Space; `Word "will";
          `Space; `Word "be"; `Space; `Word "documented."];
       `Modules
         [`Dot (`Root ("Dep1", `TUnknown), "X");
          `Root ("DocOckTypes", `TUnknown);
          `Dot (`Root ("Ocamlary", `TUnknown), "IncludeInclude1");
          `Root ("Ocamlary", `TUnknown)];
       `Heading
         (`Subsubsection,
          `Label
            (`Root (<root>, "Ocamlary"),
             "weirder-usages-involving-module-types"),
          [`Word "Weirder"; `Space; `Word "usages"; `Space;
           `Word "involving"; `Space; `Word "module"; `Space; `Word "types"]);
       `Modules
         [`Dot (`Root ("IncludeInclude1", `TUnknown), "IncludeInclude2");
          `Dot (`Root ("Dep4", `TUnknown), "T");
          `Dot (`Root ("A", `TUnknown), "Q")]]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Section,
          `Label
            (`Root (<root>, "Ocamlary"), "playing-with-@canonical-paths"),
          [`Word "Playing"; `Space; `Word "with"; `Space; `Word "@canonical";
           `Space; `Word "paths"])]);
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "CanonicalTest");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "CanonicalTest"),
               "Base__List");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "CanonicalTest"),
                         "Base__List"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params =
                     [(Odoc_model.Lang.TypeDecl.Var "a", None)];
                    private_ = false; manifest = None; constraints = []};
                  representation = None});
                Odoc_model.Lang.Signature.Value
                 {Odoc_model.Lang.Value.id =
                   `Value
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "CanonicalTest"),
                         "Base__List"),
                      "id");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`Module
                                 (`Module
                                    (`Root (<root>, "Ocamlary"),
                                     "CanonicalTest"),
                                  "Base__List"),
                               "t"))),
                     [Odoc_model.Lang.TypeExpr.Var "a"]),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`Module
                                 (`Module
                                    (`Root (<root>, "Ocamlary"),
                                     "CanonicalTest"),
                                  "Base__List"),
                               "t"))),
                     [Odoc_model.Lang.TypeExpr.Var "a"]))}]);
           canonical = None; hidden = true; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "CanonicalTest"),
               "Base__");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "CanonicalTest"),
                         "Base__"),
                      "List");
                  doc =
                   [`Tag
                      (`Canonical
                         (`Dot
                            (`Dot
                               (`Dot (`Root "Ocamlary", "CanonicalTest"),
                                "Base"),
                             "List"),
                          `Dot
                            (`Dot
                               (`Dot
                                  (`Root ("Ocamlary", `TUnknown),
                                   "CanonicalTest"),
                                "Base"),
                             "List")))];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Resolved
                       (`Hidden
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"),
                                    "CanonicalTest"),
                                 "Base__List")))));
                  canonical =
                   Some
                    (`Dot
                       (`Dot
                          (`Dot (`Root "Ocamlary", "CanonicalTest"), "Base"),
                        "List"),
                     `Dot
                       (`Dot
                          (`Dot
                             (`Root ("Ocamlary", `TUnknown), "CanonicalTest"),
                           "Base"),
                        "List"));
                  hidden = false; display_type = None; expansion = None})]);
           canonical = None; hidden = true; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "CanonicalTest"), "Base");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "CanonicalTest"),
                         "Base"),
                      "List");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Hidden
                             (`Identifier
                                (`Module
                                   (`Module
                                      (`Root (<root>, "Ocamlary"),
                                       "CanonicalTest"),
                                    "Base__")))),
                        "List"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None})]);
           canonical = None; hidden = false; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "CanonicalTest"),
               "Base__Tests");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "CanonicalTest"),
                         "Base__Tests"),
                      "C");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.ModuleType
                    (Odoc_model.Lang.ModuleType.TypeOf
                      (Odoc_model.Lang.Module.Alias
                        (`Dot
                           (`Resolved
                              (`Hidden
                                 (`Identifier
                                    (`Module
                                       (`Module
                                          (`Root (<root>, "Ocamlary"),
                                           "CanonicalTest"),
                                        "Base__")))),
                            "List"))));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "CanonicalTest"),
                         "Base__Tests"),
                      "L");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Hidden
                             (`Identifier
                                (`Module
                                   (`Module
                                      (`Root (<root>, "Ocamlary"),
                                       "CanonicalTest"),
                                    "Base__")))),
                        "List"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None});
                Odoc_model.Lang.Signature.Value
                 {Odoc_model.Lang.Value.id =
                   `Value
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "CanonicalTest"),
                         "Base__Tests"),
                      "foo");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Resolved
                           (`Identifier
                              (`Module
                                 (`Module
                                    (`Module
                                       (`Root (<root>, "Ocamlary"),
                                        "CanonicalTest"),
                                     "Base__Tests"),
                                  "L"))),
                         "t"),
                     [Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved (`Identifier (`CoreType "int")), [])]),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Resolved
                           (`Identifier
                              (`Module
                                 (`Module
                                    (`Module
                                       (`Root (<root>, "Ocamlary"),
                                        "CanonicalTest"),
                                     "Base__Tests"),
                                  "L"))),
                         "t"),
                     [Odoc_model.Lang.TypeExpr.Constr
                       (`Resolved (`Identifier (`CoreType "float")),
                       [])]))};
                Odoc_model.Lang.Signature.Value
                 {Odoc_model.Lang.Value.id =
                   `Value
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "CanonicalTest"),
                         "Base__Tests"),
                      "bar");
                  doc =
                   [`Paragraph
                      [`Word "This"; `Space; `Word "is"; `Space;
                       `Word "just"; `Space;
                       `Reference
                         (`Dot (`Root ("List", `TUnknown), "id"), []);
                       `Word ","; `Space; `Word "or"; `Space; `Word "rather";
                       `Space;
                       `Reference (`Dot (`Root ("L", `TUnknown), "id"), [])]];
                  type_ =
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Dot
                           (`Resolved
                              (`Hidden
                                 (`Identifier
                                    (`Module
                                       (`Module
                                          (`Root (<root>, "Ocamlary"),
                                           "CanonicalTest"),
                                        "Base__")))),
                            "List"),
                         "t"),
                     [Odoc_model.Lang.TypeExpr.Var "a"]),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Dot
                           (`Resolved
                              (`Hidden
                                 (`Identifier
                                    (`Module
                                       (`Module
                                          (`Root (<root>, "Ocamlary"),
                                           "CanonicalTest"),
                                        "Base__")))),
                            "List"),
                         "t"),
                     [Odoc_model.Lang.TypeExpr.Var "a"]))};
                Odoc_model.Lang.Signature.Value
                 {Odoc_model.Lang.Value.id =
                   `Value
                     (`Module
                        (`Module
                           (`Root (<root>, "Ocamlary"), "CanonicalTest"),
                         "Base__Tests"),
                      "baz");
                  doc =
                   [`Paragraph
                      [`Word "Just"; `Space; `Word "seeing"; `Space;
                       `Word "if"; `Space;
                       `Reference
                         (`Dot
                            (`Dot (`Root ("Base__", `TUnknown), "List"), "t"),
                          []);
                       `Space; `Word "("; `Code_span "Base__.List.t";
                       `Word ")"; `Space; `Word "gets"; `Space;
                       `Word "rewriten"; `Space; `Word "to"; `Space;
                       `Reference
                         (`Dot
                            (`Dot (`Root ("Base", `TUnknown), "List"), "t"),
                          []);
                       `Space; `Word "("; `Code_span "Base.List.t";
                       `Word ")"]];
                  type_ =
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Dot
                        (`Dot
                           (`Resolved
                              (`Hidden
                                 (`Identifier
                                    (`Module
                                       (`Module
                                          (`Root (<root>, "Ocamlary"),
                                           "CanonicalTest"),
                                        "Base__")))),
                            "List"),
                         "t"),
                     [Odoc_model.Lang.TypeExpr.Var "a"]),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved (`Identifier (`CoreType "unit")), []))}]);
           canonical = None; hidden = true; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "CanonicalTest"),
               "List_modif");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.With
               (Odoc_model.Lang.ModuleType.TypeOf
                 (Odoc_model.Lang.Module.Alias
                   (`Dot
                      (`Resolved
                         (`Identifier
                            (`Module
                               (`Module
                                  (`Root (<root>, "Ocamlary"),
                                   "CanonicalTest"),
                                "Base"))),
                       "List"))),
               [Odoc_model.Lang.ModuleType.TypeEq
                 (`Dot (`Resolved `Root, "t"),
                 {Odoc_model.Lang.TypeDecl.Equation.params =
                   [(Odoc_model.Lang.TypeDecl.Var "c", None)];
                  private_ = false;
                  manifest =
                   Some
                    (Odoc_model.Lang.TypeExpr.Constr
                      (`Dot
                         (`Dot
                            (`Resolved
                               (`Hidden
                                  (`Identifier
                                     (`Module
                                        (`Module
                                           (`Root (<root>, "Ocamlary"),
                                            "CanonicalTest"),
                                         "Base__")))),
                             "List"),
                          "t"),
                      [Odoc_model.Lang.TypeExpr.Var "c"]));
                  constraints = []})]));
           canonical = None; hidden = false; display_type = None;
           expansion = None})]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Value
   {Odoc_model.Lang.Value.id = `Value (`Root (<root>, "Ocamlary"), "test");
    doc =
     [`Paragraph
        [`Word "Some"; `Space; `Word "ref"; `Space; `Word "to"; `Space;
         `Reference
           (`Dot
              (`Dot
                 (`Dot (`Root ("CanonicalTest", `TUnknown), "Base__Tests"),
                  "C"),
               "t"),
            []);
         `Space; `Word "and"; `Space;
         `Reference
           (`Dot
              (`Dot
                 (`Dot (`Root ("CanonicalTest", `TUnknown), "Base__Tests"),
                  "L"),
               "id"),
            []);
         `Word "."; `Space; `Word "But"; `Space; `Word "also"; `Space;
         `Word "to"; `Space;
         `Reference
           (`Dot
              (`Dot (`Root ("CanonicalTest", `TUnknown), "Base__"), "List"),
            []);
         `Space; `Word "and"; `Space;
         `Reference
           (`Dot
              (`Dot
                 (`Dot (`Root ("CanonicalTest", `TUnknown), "Base__"),
                  "List"),
               "t"),
            [])]];
    type_ =
     Odoc_model.Lang.TypeExpr.Arrow (None,
      Odoc_model.Lang.TypeExpr.Constr
       (`Dot
          (`Dot
             (`Dot
                (`Resolved
                   (`Identifier
                      (`Module (`Root (<root>, "Ocamlary"), "CanonicalTest"))),
                 "Base__"),
              "List"),
           "t"),
       [Odoc_model.Lang.TypeExpr.Var "a"]),
      Odoc_model.Lang.TypeExpr.Constr
       (`Resolved (`Identifier (`CoreType "unit")), []))};
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Section, `Label (`Root (<root>, "Ocamlary"), "aliases"),
          [`Word "Aliases"; `Space; `Word "again"])]);
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "Aliases");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Comment
          (`Docs
             [`Paragraph
                [`Word "Let's"; `Space; `Word "imitate"; `Space;
                 `Word "jst's"; `Space; `Word "layout."]]);
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "Foo__A");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__A"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None});
                Odoc_model.Lang.Signature.Value
                 {Odoc_model.Lang.Value.id =
                   `Value
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__A"),
                      "id");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`Module
                                 (`Module
                                    (`Root (<root>, "Ocamlary"), "Aliases"),
                                  "Foo__A"),
                               "t"))),
                     []),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`Module
                                 (`Module
                                    (`Root (<root>, "Ocamlary"), "Aliases"),
                                  "Foo__A"),
                               "t"))),
                     []))}]);
           canonical = None; hidden = true; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "Foo__B");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__B"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None});
                Odoc_model.Lang.Signature.Value
                 {Odoc_model.Lang.Value.id =
                   `Value
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__B"),
                      "id");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`Module
                                 (`Module
                                    (`Root (<root>, "Ocamlary"), "Aliases"),
                                  "Foo__B"),
                               "t"))),
                     []),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`Module
                                 (`Module
                                    (`Root (<root>, "Ocamlary"), "Aliases"),
                                  "Foo__B"),
                               "t"))),
                     []))}]);
           canonical = None; hidden = true; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "Foo__C");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__C"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None});
                Odoc_model.Lang.Signature.Value
                 {Odoc_model.Lang.Value.id =
                   `Value
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__C"),
                      "id");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`Module
                                 (`Module
                                    (`Root (<root>, "Ocamlary"), "Aliases"),
                                  "Foo__C"),
                               "t"))),
                     []),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`Module
                                 (`Module
                                    (`Root (<root>, "Ocamlary"), "Aliases"),
                                  "Foo__C"),
                               "t"))),
                     []))}]);
           canonical = None; hidden = true; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "Foo__D");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__D"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None});
                Odoc_model.Lang.Signature.Value
                 {Odoc_model.Lang.Value.id =
                   `Value
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__D"),
                      "id");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`Module
                                 (`Module
                                    (`Root (<root>, "Ocamlary"), "Aliases"),
                                  "Foo__D"),
                               "t"))),
                     []),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`Module
                                 (`Module
                                    (`Root (<root>, "Ocamlary"), "Aliases"),
                                  "Foo__D"),
                               "t"))),
                     []))}]);
           canonical = None; hidden = true; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "Foo__E");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__E"),
                      "t");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None});
                Odoc_model.Lang.Signature.Value
                 {Odoc_model.Lang.Value.id =
                   `Value
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__E"),
                      "id");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.TypeExpr.Arrow (None,
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`Module
                                 (`Module
                                    (`Root (<root>, "Ocamlary"), "Aliases"),
                                  "Foo__E"),
                               "t"))),
                     []),
                    Odoc_model.Lang.TypeExpr.Constr
                     (`Resolved
                        (`Identifier
                           (`Type
                              (`Module
                                 (`Module
                                    (`Root (<root>, "Ocamlary"), "Aliases"),
                                  "Foo__E"),
                               "t"))),
                     []))}]);
           canonical = None; hidden = true; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module
              (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "Foo__");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__"),
                      "A");
                  doc =
                   [`Tag
                      (`Canonical
                         (`Dot
                            (`Dot (`Dot (`Root "Ocamlary", "Aliases"), "Foo"),
                             "A"),
                          `Dot
                            (`Dot
                               (`Dot
                                  (`Root ("Ocamlary", `TUnknown), "Aliases"),
                                "Foo"),
                             "A")))];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Resolved
                       (`Hidden
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Foo__A")))));
                  canonical =
                   Some
                    (`Dot
                       (`Dot (`Dot (`Root "Ocamlary", "Aliases"), "Foo"),
                        "A"),
                     `Dot
                       (`Dot
                          (`Dot (`Root ("Ocamlary", `TUnknown), "Aliases"),
                           "Foo"),
                        "A"));
                  hidden = false; display_type = None; expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__"),
                      "B");
                  doc =
                   [`Tag
                      (`Canonical
                         (`Dot
                            (`Dot (`Dot (`Root "Ocamlary", "Aliases"), "Foo"),
                             "B"),
                          `Dot
                            (`Dot
                               (`Dot
                                  (`Root ("Ocamlary", `TUnknown), "Aliases"),
                                "Foo"),
                             "B")))];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Resolved
                       (`Hidden
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Foo__B")))));
                  canonical =
                   Some
                    (`Dot
                       (`Dot (`Dot (`Root "Ocamlary", "Aliases"), "Foo"),
                        "B"),
                     `Dot
                       (`Dot
                          (`Dot (`Root ("Ocamlary", `TUnknown), "Aliases"),
                           "Foo"),
                        "B"));
                  hidden = false; display_type = None; expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__"),
                      "C");
                  doc =
                   [`Tag
                      (`Canonical
                         (`Dot
                            (`Dot (`Dot (`Root "Ocamlary", "Aliases"), "Foo"),
                             "C"),
                          `Dot
                            (`Dot
                               (`Dot
                                  (`Root ("Ocamlary", `TUnknown), "Aliases"),
                                "Foo"),
                             "C")))];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Resolved
                       (`Hidden
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Foo__C")))));
                  canonical =
                   Some
                    (`Dot
                       (`Dot (`Dot (`Root "Ocamlary", "Aliases"), "Foo"),
                        "C"),
                     `Dot
                       (`Dot
                          (`Dot (`Root ("Ocamlary", `TUnknown), "Aliases"),
                           "Foo"),
                        "C"));
                  hidden = false; display_type = None; expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__"),
                      "D");
                  doc =
                   [`Tag
                      (`Canonical
                         (`Dot
                            (`Dot (`Dot (`Root "Ocamlary", "Aliases"), "Foo"),
                             "D"),
                          `Dot
                            (`Dot
                               (`Dot
                                  (`Root ("Ocamlary", `TUnknown), "Aliases"),
                                "Foo"),
                             "D")))];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Resolved
                       (`Hidden
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Foo__D")))));
                  canonical =
                   Some
                    (`Dot
                       (`Dot (`Dot (`Root "Ocamlary", "Aliases"), "Foo"),
                        "D"),
                     `Dot
                       (`Dot
                          (`Dot (`Root ("Ocamlary", `TUnknown), "Aliases"),
                           "Foo"),
                        "D"));
                  hidden = false; display_type = None; expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo__"),
                      "E");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Resolved
                       (`Hidden
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Foo__E")))));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None})]);
           canonical = None; hidden = true; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "Foo");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo"),
                      "A");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Hidden
                             (`Identifier
                                (`Module
                                   (`Module
                                      (`Root (<root>, "Ocamlary"), "Aliases"),
                                    "Foo__")))),
                        "A"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo"),
                      "B");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Hidden
                             (`Identifier
                                (`Module
                                   (`Module
                                      (`Root (<root>, "Ocamlary"), "Aliases"),
                                    "Foo__")))),
                        "B"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo"),
                      "C");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Hidden
                             (`Identifier
                                (`Module
                                   (`Module
                                      (`Root (<root>, "Ocamlary"), "Aliases"),
                                    "Foo__")))),
                        "C"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo"),
                      "D");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Hidden
                             (`Identifier
                                (`Module
                                   (`Module
                                      (`Root (<root>, "Ocamlary"), "Aliases"),
                                    "Foo__")))),
                        "D"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Foo"),
                      "E");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Hidden
                             (`Identifier
                                (`Module
                                   (`Module
                                      (`Root (<root>, "Ocamlary"), "Aliases"),
                                    "Foo__")))),
                        "E"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None})]);
           canonical = None; hidden = false; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "A'");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.Alias
             (`Dot
                (`Resolved
                   (`Identifier
                      (`Module
                         (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                          "Foo"))),
                 "A"));
           canonical = None; hidden = false; display_type = None;
           expansion = None});
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "tata");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest =
              Some
               (Odoc_model.Lang.TypeExpr.Constr
                 (`Dot
                    (`Dot
                       (`Resolved
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Foo"))),
                        "A"),
                     "t"),
                 []));
             constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "tbtb");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest =
              Some
               (Odoc_model.Lang.TypeExpr.Constr
                 (`Dot
                    (`Dot
                       (`Resolved
                          (`Hidden
                             (`Identifier
                                (`Module
                                   (`Module
                                      (`Root (<root>, "Ocamlary"), "Aliases"),
                                    "Foo__")))),
                        "B"),
                     "t"),
                 []));
             constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "tete");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest =
              Some
               (Odoc_model.Lang.TypeExpr.Constr
                 (`Dot
                    (`Dot
                       (`Resolved
                          (`Hidden
                             (`Identifier
                                (`Module
                                   (`Module
                                      (`Root (<root>, "Ocamlary"), "Aliases"),
                                    "Foo__")))),
                        "E"),
                     "t"),
                 []));
             constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "tata'");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest =
              Some
               (Odoc_model.Lang.TypeExpr.Constr
                 (`Dot
                    (`Resolved
                       (`Identifier
                          (`Module
                             (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                              "A'"))),
                     "t"),
                 []));
             constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "tete2");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest =
              Some
               (Odoc_model.Lang.TypeExpr.Constr
                 (`Dot
                    (`Dot
                       (`Resolved
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Foo"))),
                        "E"),
                     "t"),
                 []));
             constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "Std");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Std"),
                      "A");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Foo"))),
                        "A"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Std"),
                      "B");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Foo"))),
                        "B"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Std"),
                      "C");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Foo"))),
                        "C"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Std"),
                      "D");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Foo"))),
                        "D"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None});
                Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "Std"),
                      "E");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Foo"))),
                        "E"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None})]);
           canonical = None; hidden = false; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "stde");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest =
              Some
               (Odoc_model.Lang.TypeExpr.Constr
                 (`Dot
                    (`Dot
                       (`Resolved
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "Std"))),
                        "E"),
                     "t"),
                 []));
             constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Comment
          (`Docs
             [`Heading
                (`Subsubsection,
                 `Label
                   (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "incl"),
                 [`Word "include"; `Space; `Word "of"; `Space; `Word "Foo"]);
              `Paragraph
                [`Word "Just"; `Space; `Word "for"; `Space; `Word "giggle,";
                 `Space; `Word "let's"; `Space; `Word "see"; `Space;
                 `Word "what"; `Space; `Word "happens"; `Space; `Word "when";
                 `Space; `Word "we"; `Space; `Word "include"; `Space;
                 `Reference (`Root ("Foo", `TUnknown), []); `Word "."]]);
         Odoc_model.Lang.Signature.Include
          {Odoc_model.Lang.Include.parent =
            `Module (`Root (<root>, "Ocamlary"), "Aliases");
           doc = [];
           decl =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.TypeOf
               (Odoc_model.Lang.Module.Alias
                 (`Resolved
                    (`Identifier
                       (`Module
                          (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                           "Foo"))))));
           expansion =
            {Odoc_model.Lang.Include.resolved = false;
             content =
              [Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "A");
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.Alias
                   (`Dot
                      (`Resolved
                         (`Hidden
                            (`Identifier
                               (`Module
                                  (`Module
                                     (`Root (<root>, "Ocamlary"), "Aliases"),
                                   "Foo__")))),
                       "A"));
                 canonical = None; hidden = false; display_type = None;
                 expansion = None});
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "B");
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.Alias
                   (`Dot
                      (`Resolved
                         (`Hidden
                            (`Identifier
                               (`Module
                                  (`Module
                                     (`Root (<root>, "Ocamlary"), "Aliases"),
                                   "Foo__")))),
                       "B"));
                 canonical = None; hidden = false; display_type = None;
                 expansion = None});
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "C");
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.Alias
                   (`Dot
                      (`Resolved
                         (`Hidden
                            (`Identifier
                               (`Module
                                  (`Module
                                     (`Root (<root>, "Ocamlary"), "Aliases"),
                                   "Foo__")))),
                       "C"));
                 canonical = None; hidden = false; display_type = None;
                 expansion = None});
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "D");
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.Alias
                   (`Dot
                      (`Resolved
                         (`Hidden
                            (`Identifier
                               (`Module
                                  (`Module
                                     (`Root (<root>, "Ocamlary"), "Aliases"),
                                   "Foo__")))),
                       "D"));
                 canonical = None; hidden = false; display_type = None;
                 expansion = None});
               Odoc_model.Lang.Signature.Module
                (Odoc_model.Lang.Signature.Ordinary,
                {Odoc_model.Lang.Module.id =
                  `Module
                    (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "E");
                 doc = [];
                 type_ =
                  Odoc_model.Lang.Module.Alias
                   (`Dot
                      (`Resolved
                         (`Hidden
                            (`Identifier
                               (`Module
                                  (`Module
                                     (`Root (<root>, "Ocamlary"), "Aliases"),
                                   "Foo__")))),
                       "E"));
                 canonical = None; hidden = false; display_type = None;
                 expansion = None})]}};
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "testa");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest =
              Some
               (Odoc_model.Lang.TypeExpr.Constr
                 (`Dot
                    (`Resolved
                       (`Identifier
                          (`Module
                             (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                              "A"))),
                     "t"),
                 []));
             constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Comment
          (`Docs
             [`Paragraph
                [`Word "And"; `Space; `Word "also,"; `Space; `Word "let's";
                 `Space; `Word "refer"; `Space; `Word "to"; `Space;
                 `Reference (`Type (`Root ("A", `TUnknown), "t"), []);
                 `Space; `Word "and"; `Space;
                 `Reference
                   (`Dot (`Dot (`Root ("Foo", `TUnknown), "B"), "id"), [])]]);
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "P1");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "P1"),
                      "Y");
                  doc =
                   [`Tag
                      (`Canonical
                         (`Dot
                            (`Dot (`Dot (`Root "Ocamlary", "Aliases"), "P2"),
                             "Z"),
                          `Dot
                            (`Dot
                               (`Dot
                                  (`Root ("Ocamlary", `TUnknown), "Aliases"),
                                "P2"),
                             "Z")))];
                  type_ =
                   Odoc_model.Lang.Module.ModuleType
                    (Odoc_model.Lang.ModuleType.Signature
                      [Odoc_model.Lang.Signature.Type
                        (Odoc_model.Lang.Signature.Ordinary,
                        {Odoc_model.Lang.TypeDecl.id =
                          `Type
                            (`Module
                               (`Module
                                  (`Module
                                     (`Root (<root>, "Ocamlary"), "Aliases"),
                                   "P1"),
                                "Y"),
                             "t");
                         doc = [];
                         equation =
                          {Odoc_model.Lang.TypeDecl.Equation.params = [];
                           private_ = false; manifest = None;
                           constraints = []};
                         representation = None});
                       Odoc_model.Lang.Signature.Value
                        {Odoc_model.Lang.Value.id =
                          `Value
                            (`Module
                               (`Module
                                  (`Module
                                     (`Root (<root>, "Ocamlary"), "Aliases"),
                                   "P1"),
                                "Y"),
                             "id");
                         doc = [];
                         type_ =
                          Odoc_model.Lang.TypeExpr.Arrow (None,
                           Odoc_model.Lang.TypeExpr.Constr
                            (`Resolved
                               (`Identifier
                                  (`Type
                                     (`Module
                                        (`Module
                                           (`Module
                                              (`Root (<root>, "Ocamlary"),
                                               "Aliases"),
                                            "P1"),
                                         "Y"),
                                      "t"))),
                            []),
                           Odoc_model.Lang.TypeExpr.Constr
                            (`Resolved
                               (`Identifier
                                  (`Type
                                     (`Module
                                        (`Module
                                           (`Module
                                              (`Root (<root>, "Ocamlary"),
                                               "Aliases"),
                                            "P1"),
                                         "Y"),
                                      "t"))),
                            []))}]);
                  canonical =
                   Some
                    (`Dot
                       (`Dot (`Dot (`Root "Ocamlary", "Aliases"), "P2"), "Z"),
                     `Dot
                       (`Dot
                          (`Dot (`Root ("Ocamlary", `TUnknown), "Aliases"),
                           "P2"),
                        "Z"));
                  hidden = false; display_type = None;
                  expansion = Some Odoc_model.Lang.Module.AlreadyASig})]);
           canonical = None; hidden = false; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "P2");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.ModuleType
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Module
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.Module.id =
                   `Module
                     (`Module
                        (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                         "P2"),
                      "Z");
                  doc = [];
                  type_ =
                   Odoc_model.Lang.Module.Alias
                    (`Dot
                       (`Resolved
                          (`Identifier
                             (`Module
                                (`Module
                                   (`Root (<root>, "Ocamlary"), "Aliases"),
                                 "P1"))),
                        "Y"));
                  canonical = None; hidden = false; display_type = None;
                  expansion = None})]);
           canonical = None; hidden = false; display_type = None;
           expansion = Some Odoc_model.Lang.Module.AlreadyASig});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "X1");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.Alias
             (`Dot
                (`Resolved
                   (`Identifier
                      (`Module
                         (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                          "P1"))),
                 "Y"));
           canonical = None; hidden = false; display_type = None;
           expansion = None});
         Odoc_model.Lang.Signature.Module
          (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.Module.id =
            `Module (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "X2");
           doc = [];
           type_ =
            Odoc_model.Lang.Module.Alias
             (`Dot
                (`Resolved
                   (`Identifier
                      (`Module
                         (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                          "P2"))),
                 "Z"));
           canonical = None; hidden = false; display_type = None;
           expansion = None});
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "p1");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest =
              Some
               (Odoc_model.Lang.TypeExpr.Constr
                 (`Dot
                    (`Resolved
                       (`Identifier
                          (`Module
                             (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                              "X1"))),
                     "t"),
                 []));
             constraints = []};
           representation = None});
         Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "Aliases"), "p2");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest =
              Some
               (Odoc_model.Lang.TypeExpr.Constr
                 (`Dot
                    (`Resolved
                       (`Identifier
                          (`Module
                             (`Module (`Root (<root>, "Ocamlary"), "Aliases"),
                              "X2"))),
                     "t"),
                 []));
             constraints = []};
           representation = None})]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Section,
          `Label (`Root (<root>, "Ocamlary"), "section-title-splicing"),
          [`Word "Section"; `Space; `Word "title"; `Space; `Word "splicing"]);
       `Paragraph
         [`Word "I"; `Space; `Word "can"; `Space; `Word "refer"; `Space;
          `Word "to"];
       `List
         (`Unordered,
          [[`Paragraph
              [`Code_span "{!section:indexmodules}"; `Space; `Word ":";
               `Space; `Reference (`Root ("indexmodules", `TLabel), [])]];
           [`Paragraph
              [`Code_span "{!aliases}"; `Space; `Word ":"; `Space;
               `Reference (`Root ("aliases", `TUnknown), [])]]]);
       `Paragraph
         [`Word "But"; `Space; `Word "also"; `Space; `Word "to"; `Space;
          `Word "things"; `Space; `Word "in"; `Space; `Word "submodules:"];
       `List
         (`Unordered,
          [[`Paragraph
              [`Code_span "{!section:SuperSig.SubSigA.subSig}"; `Space;
               `Word ":"; `Space;
               `Reference
                 (`Label
                    (`Dot (`Root ("SuperSig", `TUnknown), "SubSigA"),
                     "subSig"),
                  [])]];
           [`Paragraph
              [`Code_span "{!Aliases.incl}"; `Space; `Word ":"; `Space;
               `Reference (`Dot (`Root ("Aliases", `TUnknown), "incl"), [])]]]);
       `Paragraph
         [`Word "And"; `Space; `Word "just"; `Space; `Word "to"; `Space;
          `Word "make"; `Space; `Word "sure"; `Space; `Word "we"; `Space;
          `Word "do"; `Space; `Word "not"; `Space; `Word "mess"; `Space;
          `Word "up:"];
       `List
         (`Unordered,
          [[`Paragraph
              [`Code_span "{{!section:indexmodules}A}"; `Space; `Word ":";
               `Space;
               `Reference (`Root ("indexmodules", `TLabel), [`Word "A"])]];
           [`Paragraph
              [`Code_span "{{!aliases}B}"; `Space; `Word ":"; `Space;
               `Reference (`Root ("aliases", `TUnknown), [`Word "B"])]];
           [`Paragraph
              [`Code_span "{{!section:SuperSig.SubSigA.subSig}C}"; `Space;
               `Word ":"; `Space;
               `Reference
                 (`Label
                    (`Dot (`Root ("SuperSig", `TUnknown), "SubSigA"),
                     "subSig"),
                  [`Word "C"])]];
           [`Paragraph
              [`Code_span "{{!Aliases.incl}D}"; `Space; `Word ":"; `Space;
               `Reference
                 (`Dot (`Root ("Aliases", `TUnknown), "incl"), [`Word "D"])]]])]);
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Heading
         (`Section,
          `Label (`Root (<root>, "Ocamlary"), "new-reference-syntax"),
          [`Word "New"; `Space; `Word "reference"; `Space; `Word "syntax"])]);
  Odoc_model.Lang.Signature.ModuleType
   {Odoc_model.Lang.ModuleType.id =
     `ModuleType (`Root (<root>, "Ocamlary"), "M");
    doc = [];
    expr =
     Some
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`ModuleType (`Root (<root>, "Ocamlary"), "M"), "t");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None})]);
    expansion = Some Odoc_model.Lang.Module.AlreadyASig};
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id = `Module (`Root (<root>, "Ocamlary"), "M");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type (`Module (`Root (<root>, "Ocamlary"), "M"), "t");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None})]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph [`Word "Here"; `Space; `Word "goes:"];
       `List
         (`Unordered,
          [[`Paragraph
              [`Code_span "{!M.t}"; `Space; `Word ":"; `Space;
               `Reference (`Dot (`Root ("M", `TUnknown), "t"), [])]];
           [`Paragraph
              [`Code_span "{!module-M.t}"; `Space; `Word ":"; `Space;
               `Reference (`Dot (`Root ("M", `TModule), "t"), [])]];
           [`Paragraph
              [`Code_span "{!module-type-M.t}"; `Space; `Word ":"; `Space;
               `Reference (`Dot (`Root ("M", `TModuleType), "t"), [])]]])]);
  Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
   {Odoc_model.Lang.Module.id =
     `Module (`Root (<root>, "Ocamlary"), "Only_a_module");
    doc = [];
    type_ =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
          {Odoc_model.Lang.TypeDecl.id =
            `Type
              (`Module (`Root (<root>, "Ocamlary"), "Only_a_module"), "t");
           doc = [];
           equation =
            {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
             manifest = None; constraints = []};
           representation = None})]);
    canonical = None; hidden = false; display_type = None;
    expansion = Some Odoc_model.Lang.Module.AlreadyASig});
  Odoc_model.Lang.Signature.Comment
   (`Docs
      [`Paragraph
         [`Word "Some"; `Space; `Word "here"; `Space; `Word "should"; `Space;
          `Word "fail:"];
       `List
         (`Unordered,
          [[`Paragraph
              [`Code_span "{!Only_a_module.t}"; `Space; `Word ":"; `Space;
               `Reference
                 (`Dot (`Root ("Only_a_module", `TUnknown), "t"), [])]];
           [`Paragraph
              [`Code_span "{!module-Only_a_module.t}"; `Space; `Word ":";
               `Space;
               `Reference (`Dot (`Root ("Only_a_module", `TModule), "t"), [])]];
           [`Paragraph
              [`Code_span "{!module-type-Only_a_module.t}"; `Space;
               `Word ":"; `Space;
               `Reference
                 (`Dot (`Root ("Only_a_module", `TModuleType), "t"), []);
               `Space; `Word ":"; `Space;
               `Reference
                 (`Dot (`Root ("Only_a_module", `TModuleType), "t"),
                  [`Word "test"])]]])])]
```
