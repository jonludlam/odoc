Verify the behavior on functors.

  $ odoc compile -c module-a -c src-source root.mld

  $ printf "s.ml\na.ml\nb.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map

  $ ocamlc -c -o s.cmo s.ml -dtypedtree -bin-annot -I .
  [
    structure_item (s.ml[1,0+0]..s.ml[4,41+3])
      Tstr_modtype "S/270"
        module_type (s.ml[1,0+16]..s.ml[4,41+3])
          Tmty_signature
          [
            signature_item (s.ml[2,20+2]..s.ml[2,20+8])
              Tsig_type Rec
              [
                type_declaration t/268 (s.ml[2,20+2]..s.ml[2,20+8])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    None
              ]
            signature_item (s.ml[3,29+2]..s.ml[3,29+11])
              Tsig_value
              value_description x/269 (s.ml[3,29+2]..s.ml[3,29+11])
                core_type (s.ml[3,29+10]..s.ml[3,29+11])
                  Ttyp_constr "t/268"
                  []
                []
          ]
  ]
  
  $ ocamlc -c -o a.cmo a.ml -dtypedtree -dshape -bin-annot -I .
  [
    structure_item (a.ml[1,0+0]..a.ml[4,57+3])
      Tstr_module
      F/274
        module_expr (a.ml[1,0+9]..a.ml[4,57+3])
          Tmod_functor "S/271"
          module_type (a.ml[1,0+14]..a.ml[1,0+17])
            Tmty_ident "S!.S"
          module_expr (a.ml[1,0+21]..a.ml[4,57+3])
            Tmod_structure
            [
              structure_item (a.ml[2,28+2]..a.ml[2,28+14])
                Tstr_type Rec
                [
                  type_declaration t/272 (a.ml[2,28+2]..a.ml[2,28+14])
                    ptype_params =
                      []
                    ptype_cstrs =
                      []
                    ptype_kind =
                      Ttype_abstract
                    ptype_private = Public
                    ptype_manifest =
                      Some
                        core_type (a.ml[2,28+11]..a.ml[2,28+14])
                          Ttyp_constr "S/271.t"
                          []
                ]
              structure_item (a.ml[3,43+2]..a.ml[3,43+13])
                Tstr_value Nonrec
                [
                  <def>
                    pattern (a.ml[3,43+6]..a.ml[3,43+7])
                      Tpat_var "y/273"
                    expression (a.ml[3,43+10]..a.ml[3,43+13])
                      Texp_ident "S/271.x"
                ]
            ]
  ]
  
  {<A>
   "F"[module] -> Abs<A.3>(S/271, {
                                   "t"[type] -> <A.1>;
                                   "y"[value] -> <A.2>;
                                   });
   }
  
  $ ocamlc -c -o b.cmo b.ml -dtypedtree -bin-annot -I .
  [
    structure_item (b.ml[1,0+0]..b.ml[5,46+3])
      Tstr_module
      S/270
        module_expr (b.ml[1,0+11]..b.ml[5,46+3])
          Tmod_structure
          [
            structure_item (b.ml[2,18+2]..b.ml[2,18+14])
              Tstr_type Rec
              [
                type_declaration t/268 (b.ml[2,18+2]..b.ml[2,18+14])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (b.ml[2,18+11]..b.ml[2,18+14])
                        Ttyp_constr "int/1!"
                        []
              ]
            structure_item (b.ml[4,34+2]..b.ml[4,34+11])
              Tstr_value Nonrec
              [
                <def>
                  pattern (b.ml[4,34+6]..b.ml[4,34+7])
                    Tpat_var "x/269"
                  expression (b.ml[4,34+10]..b.ml[4,34+11])
                    Texp_constant Const_int 2
              ]
          ]
    structure_item (b.ml[7,51+0]..b.ml[7,51+18])
      Tstr_module
      R/281
        module_expr (b.ml[7,51+11]..b.ml[7,51+18])
          Tmod_apply
          module_expr (b.ml[7,51+11]..b.ml[7,51+14])
            Tmod_ident "A!.F"
          module_expr (b.ml[7,51+16]..b.ml[7,51+17])
            Tmod_ident "S/270"
  ]
  
  $ odoc compile --source-name s.ml --source-parent-file src-source.odoc -I . s.cmt
  Shape: {<S>
          "S"[module type] -> <S.2>;
          }
  
  Struct
  Adding a 'Def' for 'def-1' at loc (31,40)
  Adding a 'Def' for 'def-0' at loc (22,28)
  Adding a 'Def' for 'def-2' at loc (0,44)
  uids (1 calculated vs 3 expected): [module-type-S]
  $ odoc compile --source-name a.ml --source-parent-file src-source.odoc -I . a.cmt
  Shape: {<A>
          "F"[module] ->
              Abs<A.3>(S/271, {
                               "t"[type] -> <A.1>;
                               "y"[value] -> <A.2>;
                               });
          }
  
  Struct
  Adding a 'Def' for 'def-2' at loc (49,50)
  Adding a 'Def' for 'def-1' at loc (30,42)
  Adding a 'Def' for 'def-3' at loc (0,60)
  uids (1 calculated vs 3 expected): [module-F]Adding a 'Def' for 'y_273' at loc (49,50)
  $ odoc compile --source-name b.ml --source-parent-file src-source.odoc -I . b.cmt
  Shape: {<B>
          "R"[module] ->
              CU A . "F"[module](
              {<B.2>
               "t"[type] -> <B.0>;
               "x"[value] -> <B.1>;
               })<B.3>;
          "S"[module] -> {<B.2>
                          "t"[type] -> <B.0>;
                          "x"[value] -> <B.1>;
                          };
          }
  
  Struct
  Adding a 'Def' for 'def-2' at loc (0,49)
  Adding a 'Def' for 'def-1' at loc (40,41)
  Adding a 'Def' for 'def-0' at loc (20,32)
  Adding a 'Def' for 'def-3' at loc (51,69)
  uids (4 calculated vs 4 expected): [module-S.val-x,module-S.type-t,module-S,module-R]Adding a 'Def' for 'x_269' at loc (40,41)
  $ odoc link -I . s.odoc
  Found shape: <S.2>
  
  Found shape: <S.2>
  
  Found shape: <S.2>
  
  $ odoc link -I . a.odoc
  Found shape: <A.1>
  
  Found shape: <A.2>
  
  Found shape: Abs<A.3>(S/271, {
                                "t"[type] -> <A.1>;
                                "y"[value] -> <A.2>;
                                })
  
  $ odoc link -I . b.odoc
  Found shape: <B.0>
  
  Found shape: <B.1>
  
  Found shape: {<B.2>
                "t"[type] -> <B.0>;
                "x"[value] -> <B.1>;
                }
  
  Found shape: <A.1>
  
  Found shape: <A.2>
  
  Found shape: {<B.3>
                "t"[type] -> <A.1>;
                "y"[value] -> <A.2>;
                }
  
  $ odoc html-generate --source s.ml --indent -o html s.odocl
  $ odoc html-generate --source a.ml --indent -o html a.odocl
  $ odoc html-generate --source b.ml --indent -o html b.odocl

  $ find html | sort
  html
  html/A
  html/A/F
  html/A/F/argument-1-S
  html/A/F/argument-1-S/index.html
  html/A/F/index.html
  html/A/index.html
  html/B
  html/B/R
  html/B/R/index.html
  html/B/S
  html/B/S/index.html
  html/B/index.html
  html/S
  html/S/index.html
  html/S/module-type-S
  html/S/module-type-S/index.html
  html/root
  html/root/source
  html/root/source/a.ml.html
  html/root/source/b.ml.html
  html/root/source/s.ml.html

In this test, the functor expansion contains the right link.

  $ cat html/A/F/index.html | grep source_link --context=1
     <h1>Module <code><span>A.F</span></code>
      <a href="../../root/source/a.ml.html#def-3" class="source_link">Source
      </a>
  --
       <a href="#type-t" class="anchor"></a>
       <a href="../../root/source/a.ml.html#def-1" class="source_link">Source
       </a>
  --
       <a href="#val-y" class="anchor"></a>
       <a href="../../root/source/a.ml.html#def-2" class="source_link">Source
       </a>

  $ cat html/root/source/a.ml.html | grep L3
  <a id="L3" class="source_line" href="#L3">3</a>

However, on functor results, there is a link to source in the file:

  $ cat html/B/R/index.html | grep source_link --context=2
    <header class="odoc-preamble">
     <h1>Module <code><span>B.R</span></code>
      <a href="../../root/source/b.ml.html#def-3" class="source_link">Source
      </a>
     </h1>
  --
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>
       <a href="../../root/source/a.ml.html#def-1" class="source_link">Source
       </a>
       <code><span><span class="keyword">type</span> t</span>
  --
      <div class="spec value anchored" id="val-y">
       <a href="#val-y" class="anchor"></a>
       <a href="../../root/source/a.ml.html#def-2" class="source_link">Source
       </a>
       <code>

Source links in functor parameters might not make sense. Currently we generate none:

  $ cat html/A/F/argument-1-S/index.html | grep source_link --context=1
  [1]
