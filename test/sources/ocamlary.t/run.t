  $ printf "{0 Root}" > root.mld
  $ odoc compile -c src-source root.mld
  $ printf "ocamlary.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map
  $ ocamlc -c ocamlary.mli -bin-annot -I .
  $ ocamlc -c ocamlary.ml -dtypedtree -bin-annot -I .
  [
    structure_item (ocamlary.ml[18,847+0]..ocamlary.ml[18,847+58])
      Tstr_attribute "ocaml.text"
      [
        structure_item (ocamlary.ml[18,847+0]..[18,847+58])
          Pstr_eval
          expression (ocamlary.ml[18,847+0]..[18,847+58])
            Pexp_constant PConst_string(" An interface with all of the module system features ",(ocamlary.ml[18,847+0]..[18,847+58]),None)
      ]
    structure_item (ocamlary.ml[20,907+0]..ocamlary.ml[20,907+34])
      Tstr_modtype "Empty/269"
        module_type (ocamlary.ml[20,907+20]..ocamlary.ml[20,907+34])
          Tmty_signature
          [
            signature_item (ocamlary.ml[20,907+24]..ocamlary.ml[20,907+30])
              Tsig_type Rec
              [
                type_declaration t/268 (ocamlary.ml[20,907+24]..ocamlary.ml[20,907+30])
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
          ]
    structure_item (ocamlary.ml[23,985+0]..ocamlary.ml[23,985+43])
      Tstr_modtype "MissingComment/271"
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[22,943+0]..[22,943+41])
              Pstr_eval
              expression (ocamlary.ml[22,943+0]..[22,943+41])
                Pexp_constant PConst_string(" An ambiguous, misnamed module type ",(ocamlary.ml[22,943+0]..[22,943+41]),None)
          ]
        module_type (ocamlary.ml[23,985+29]..ocamlary.ml[23,985+43])
          Tmty_signature
          [
            signature_item (ocamlary.ml[23,985+33]..ocamlary.ml[23,985+39])
              Tsig_type Rec
              [
                type_declaration t/270 (ocamlary.ml[23,985+33]..ocamlary.ml[23,985+39])
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
          ]
    structure_item (ocamlary.ml[26,1060+0]..ocamlary.ml[26,1060+25])
      Tstr_module
      Empty/272
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[25,1030+0]..[25,1030+29])
              Pstr_eval
              expression (ocamlary.ml[25,1030+0]..[25,1030+29])
                Pexp_constant PConst_string(" A plain, empty module. ",(ocamlary.ml[25,1030+0]..[25,1030+29]),None)
          ]
        module_expr (ocamlary.ml[26,1060+15]..ocamlary.ml[26,1060+25])
          Tmod_structure
          []
    structure_item (ocamlary.ml[29,1116+0]..ocamlary.ml[29,1116+25])
      Tstr_module
      EmptyAlias/273
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[28,1087+0]..[28,1087+28])
              Pstr_eval
              expression (ocamlary.ml[28,1087+0]..[28,1087+28])
                Pexp_constant PConst_string(" A plain module alias. ",(ocamlary.ml[28,1087+0]..[28,1087+28]),None)
          ]
        module_expr (ocamlary.ml[29,1116+20]..ocamlary.ml[29,1116+25])
          Tmod_ident "Empty/272"
    structure_item (ocamlary.ml[32,1183+0]..ocamlary.ml[32,1183+30])
      Tstr_modtype "EmptySig/274"
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[31,1143+0]..[31,1143+39])
              Pstr_eval
              expression (ocamlary.ml[31,1143+0]..[31,1143+39])
                Pexp_constant PConst_string(" A plain, empty module signature. ",(ocamlary.ml[31,1143+0]..[31,1143+39]),None)
          ]
        module_type (ocamlary.ml[32,1183+23]..ocamlary.ml[32,1183+30])
          Tmty_signature
          []
    structure_item (ocamlary.ml[35,1261+0]..ocamlary.ml[35,1261+36])
      Tstr_modtype "EmptySigAlias/275"
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[34,1215+0]..[34,1215+45])
              Pstr_eval
              expression (ocamlary.ml[34,1215+0]..[34,1215+45])
                Pexp_constant PConst_string(" A plain, empty module signature alias. ",(ocamlary.ml[34,1215+0]..[34,1215+45]),None)
          ]
        module_type (ocamlary.ml[35,1261+28]..ocamlary.ml[35,1261+36])
          Tmty_ident "EmptySig/274"
    structure_item (ocamlary.ml[38,1337+0]..ocamlary.ml[38,1337+39])
      Tstr_module
      ModuleWithSignature/276
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[37,1299+0]..[37,1299+37])
              Pstr_eval
              expression (ocamlary.ml[37,1299+0]..[37,1299+37])
                Pexp_constant PConst_string(" A plain module of a signature. ",(ocamlary.ml[37,1299+0]..[37,1299+37]),None)
          ]
        module_expr (ocamlary.ml[38,1337+29]..ocamlary.ml[38,1337+39])
          Tmod_structure
          []
    structure_item (ocamlary.ml[41,1425+0]..ocamlary.ml[41,1425+44])
      Tstr_module
      ModuleWithSignatureAlias/277
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[40,1378+0]..[40,1378+46])
              Pstr_eval
              expression (ocamlary.ml[40,1378+0]..[40,1378+46])
                Pexp_constant PConst_string(" A plain module with an alias signature. ",(ocamlary.ml[40,1378+0]..[40,1378+46]),None)
          ]
        module_expr (ocamlary.ml[41,1425+34]..ocamlary.ml[41,1425+44])
          Tmod_structure
          []
    structure_item (ocamlary.ml[44,1493+0]..ocamlary.ml[44,1493+32])
      Tstr_module
      One/279
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[43,1471+0]..[43,1471+21])
              Pstr_eval
              expression (ocamlary.ml[43,1471+0]..[43,1471+21])
                Pexp_constant PConst_string(" has type \"one\" ",(ocamlary.ml[43,1471+0]..[43,1471+21]),None)
          ]
        module_expr (ocamlary.ml[44,1493+13]..ocamlary.ml[44,1493+32])
          Tmod_structure
          [
            structure_item (ocamlary.ml[44,1493+20]..ocamlary.ml[44,1493+28])
              Tstr_type Rec
              [
                type_declaration one/278 (ocamlary.ml[44,1493+20]..ocamlary.ml[44,1493+28])
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
          ]
    structure_item (ocamlary.ml[47,1570+0]..ocamlary.ml[51,1657+3])
      Tstr_modtype "SigForMod/282"
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[46,1527+0]..[46,1527+42])
              Pstr_eval
              expression (ocamlary.ml[46,1527+0]..[46,1527+42])
                Pexp_constant PConst_string(" There's a module in this signature. ",(ocamlary.ml[46,1527+0]..[46,1527+42]),None)
          ]
        module_type (ocamlary.ml[47,1570+24]..ocamlary.ml[51,1657+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[48,1598+2]..ocamlary.ml[50,1651+5])
              Tsig_module "Inner/281"
              module_type (ocamlary.ml[48,1598+17]..ocamlary.ml[50,1651+5])
                Tmty_signature
                [
                  signature_item (ocamlary.ml[49,1619+4]..ocamlary.ml[49,1619+31])
                    Tsig_modtype "Empty/280"
                      module_type (ocamlary.ml[49,1619+24]..ocamlary.ml[49,1619+31])
                        Tmty_signature
                        []
                ]
          ]
    structure_item (ocamlary.ml[53,1662+0]..ocamlary.ml[73,2120+3])
      Tstr_modtype "SuperSig/294"
        module_type (ocamlary.ml[53,1662+23]..ocamlary.ml[73,2120+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[54,1689+2]..ocamlary.ml[62,1860+5])
              Tsig_modtype "SubSigA/286"
                module_type (ocamlary.ml[54,1689+24]..ocamlary.ml[62,1860+5])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[55,1717+4]..ocamlary.ml[55,1717+68])
                      Tsig_attribute "ocaml.text"
                      [
                        structure_item (ocamlary.ml[55,1717+4]..[55,1717+68])
                          Pstr_eval
                          expression (ocamlary.ml[55,1717+4]..[55,1717+68])
                            Pexp_constant PConst_string(" {3:SubSig A Labeled Section Header Inside of a Signature} ",(ocamlary.ml[55,1717+4]..[55,1717+68]),None)
                      ]
                    signature_item (ocamlary.ml[57,1787+4]..ocamlary.ml[57,1787+10])
                      Tsig_type Rec
                      [
                        type_declaration t/283 (ocamlary.ml[57,1787+4]..ocamlary.ml[57,1787+10])
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
                    signature_item (ocamlary.ml[59,1799+4]..ocamlary.ml[61,1852+7])
                      Tsig_module "SubSigAMod/285"
                      module_type (ocamlary.ml[59,1799+24]..ocamlary.ml[61,1852+7])
                        Tmty_signature
                        [
                          signature_item (ocamlary.ml[60,1827+6]..ocamlary.ml[60,1827+24])
                            Tsig_type Rec
                            [
                              type_declaration sub_sig_a_mod/284 (ocamlary.ml[60,1827+6]..ocamlary.ml[60,1827+24])
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
                        ]
                  ]
            signature_item (ocamlary.ml[63,1866+2]..ocamlary.ml[67,1981+5])
              Tsig_modtype "SubSigB/288"
                module_type (ocamlary.ml[63,1866+24]..ocamlary.ml[67,1981+5])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[64,1894+4]..ocamlary.ml[64,1894+74])
                      Tsig_attribute "ocaml.text"
                      [
                        structure_item (ocamlary.ml[64,1894+4]..[64,1894+74])
                          Pstr_eval
                          expression (ocamlary.ml[64,1894+4]..[64,1894+74])
                            Pexp_constant PConst_string(" {3:SubSig Another Labeled Section Header Inside of a Signature} ",(ocamlary.ml[64,1894+4]..[64,1894+74]),None)
                      ]
                    signature_item (ocamlary.ml[66,1970+4]..ocamlary.ml[66,1970+10])
                      Tsig_type Rec
                      [
                        type_declaration t/287 (ocamlary.ml[66,1970+4]..ocamlary.ml[66,1970+10])
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
                  ]
            signature_item (ocamlary.ml[68,1987+2]..ocamlary.ml[70,2044+5])
              Tsig_modtype "EmptySig/290"
                module_type (ocamlary.ml[68,1987+25]..ocamlary.ml[70,2044+5])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[69,2016+4]..ocamlary.ml[69,2016+27])
                      Tsig_type Rec
                      [
                        type_declaration not_actually_empty/289 (ocamlary.ml[69,2016+4]..ocamlary.ml[69,2016+27])
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
                  ]
            signature_item (ocamlary.ml[71,2050+2]..ocamlary.ml[71,2050+36])
              Tsig_modtype "One/292"
                module_type (ocamlary.ml[71,2050+20]..ocamlary.ml[71,2050+36])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[71,2050+24]..ocamlary.ml[71,2050+32])
                      Tsig_type Rec
                      [
                        type_declaration two/291 (ocamlary.ml[71,2050+24]..ocamlary.ml[71,2050+32])
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
                  ]
            signature_item (ocamlary.ml[72,2087+2]..ocamlary.ml[72,2087+32])
              Tsig_modtype "SuperSig/293"
                module_type (ocamlary.ml[72,2087+25]..ocamlary.ml[72,2087+32])
                  Tmty_signature
                  []
          ]
    structure_item (ocamlary.ml[76,2144+0]..ocamlary.ml[78,2182+3])
      Tstr_module
      Buffer/298
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[75,2125+0]..[75,2125+18])
              Pstr_eval
              expression (ocamlary.ml[75,2125+0]..[75,2125+18])
                Pexp_constant PConst_string(" {!Buffer.t} ",(ocamlary.ml[75,2125+0]..[75,2125+18]),None)
          ]
        module_expr (ocamlary.ml[76,2144+16]..ocamlary.ml[78,2182+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[77,2167+2]..ocamlary.ml[77,2167+14])
              Tstr_value Nonrec
              [
                <def>
                  pattern (ocamlary.ml[77,2167+6]..ocamlary.ml[77,2167+7])
                    Tpat_var "f/295"
                  expression (ocamlary.ml[77,2167+8]..ocamlary.ml[77,2167+14]) ghost
                    Texp_function
                    Nolabel
                    [
                      <case>
                        pattern (ocamlary.ml[77,2167+8]..ocamlary.ml[77,2167+9])
                          Tpat_any
                        expression (ocamlary.ml[77,2167+12]..ocamlary.ml[77,2167+14])
                          Texp_construct "()"
                          []
                    ]
              ]
          ]
    structure_item (ocamlary.ml[81,2222+0]..ocamlary.ml[81,2222+24])
      Tstr_exception
      type_exception
        ptyext_constructor =
          extension_constructor (ocamlary.ml[81,2222+0]..ocamlary.ml[81,2222+24])
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[80,2187+0]..[80,2187+34])
                  Pstr_eval
                  expression (ocamlary.ml[80,2187+0]..[80,2187+34])
                    Pexp_constant PConst_string(" Unary exception constructor ",(ocamlary.ml[80,2187+0]..[80,2187+34]),None)
              ]
            pext_name = "Kaboom/299"
            pext_kind =
              Text_decl
                [
                  core_type (ocamlary.ml[81,2222+20]..ocamlary.ml[81,2222+24])
                    Ttyp_constr "unit/6!"
                    []
                ]
                None
    structure_item (ocamlary.ml[84,2284+0]..ocamlary.ml[84,2284+31])
      Tstr_exception
      type_exception
        ptyext_constructor =
          extension_constructor (ocamlary.ml[84,2284+0]..ocamlary.ml[84,2284+31])
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[83,2248+0]..[83,2248+35])
                  Pstr_eval
                  expression (ocamlary.ml[83,2248+0]..[83,2248+35])
                    Pexp_constant PConst_string(" Binary exception constructor ",(ocamlary.ml[83,2248+0]..[83,2248+35]),None)
              ]
            pext_name = "Kablam/300"
            pext_kind =
              Text_decl
                [
                  core_type (ocamlary.ml[84,2284+20]..ocamlary.ml[84,2284+24])
                    Ttyp_constr "unit/6!"
                    []
                  core_type (ocamlary.ml[84,2284+27]..ocamlary.ml[84,2284+31])
                    Ttyp_constr "unit/6!"
                    []
                ]
                None
    structure_item (ocamlary.ml[87,2370+0]..ocamlary.ml[87,2370+33])
      Tstr_exception
      type_exception
        ptyext_constructor =
          extension_constructor (ocamlary.ml[87,2370+0]..ocamlary.ml[87,2370+33])
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[86,2317+0]..[86,2317+52])
                  Pstr_eval
                  expression (ocamlary.ml[86,2317+0]..[86,2317+52])
                    Pexp_constant PConst_string(" Unary exception constructor over binary tuple ",(ocamlary.ml[86,2317+0]..[86,2317+52]),None)
              ]
            pext_name = "Kapow/301"
            pext_kind =
              Text_decl
                [
                  core_type (ocamlary.ml[87,2370+21]..ocamlary.ml[87,2370+32])
                    Ttyp_tuple
                    [
                      core_type (ocamlary.ml[87,2370+21]..ocamlary.ml[87,2370+25])
                        Ttyp_constr "unit/6!"
                        []
                      core_type (ocamlary.ml[87,2370+28]..ocamlary.ml[87,2370+32])
                        Ttyp_constr "unit/6!"
                        []
                    ]
                ]
                None
    structure_item (ocamlary.ml[91,2519+0]..ocamlary.ml[91,2519+18])
      Tstr_exception
      type_exception
        ptyext_constructor =
          extension_constructor (ocamlary.ml[91,2519+0]..ocamlary.ml[91,2519+18])
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[89,2405+0]..[90,2471+47])
                  Pstr_eval
                  expression (ocamlary.ml[89,2405+0]..[90,2471+47])
                    Pexp_constant PConst_string(" {!EmptySig} is general but {!module:EmptySig} is a module and\n    {!exception:EmptySig} is this exception. ",(ocamlary.ml[89,2405+0]..[90,2471+47]),None)
              ]
            pext_name = "EmptySig/302"
            pext_kind =
              Text_decl
                []
                None
    structure_item (ocamlary.ml[94,2592+0]..ocamlary.ml[94,2592+23])
      Tstr_exception
      type_exception
        ptyext_constructor =
          extension_constructor (ocamlary.ml[94,2592+0]..ocamlary.ml[94,2592+23])
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[93,2539+0]..[93,2539+52])
                  Pstr_eval
                  expression (ocamlary.ml[93,2539+0]..[93,2539+52])
                    Pexp_constant PConst_string(" {!exception:EmptySigAlias} is this exception. ",(ocamlary.ml[93,2539+0]..[93,2539+52]),None)
              ]
            pext_name = "EmptySigAlias/303"
            pext_kind =
              Text_decl
                []
                None
    structure_item (ocamlary.ml[98,2731+0]..ocamlary.ml[98,2731+34])
      Tstr_type Rec
      [
        type_declaration a_function/304 (ocamlary.ml[98,2731+0]..ocamlary.ml[98,2731+34])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[96,2617+0]..[97,2686+44])
                Pstr_eval
                expression (ocamlary.ml[96,2617+0]..[97,2686+44])
                  Pexp_constant PConst_string(" {!a_function} is general but {!type:a_function} is this type and\n    {!val:a_function} is the value below. ",(ocamlary.ml[96,2617+0]..[97,2686+44]),None)
            ]
          ptype_params =
            [
              core_type (ocamlary.ml[98,2731+6]..ocamlary.ml[98,2731+8])
                Ttyp_var a
              core_type (ocamlary.ml[98,2731+9]..ocamlary.ml[98,2731+11])
                Ttyp_var b
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[98,2731+26]..ocamlary.ml[98,2731+34])
                Ttyp_arrow
                Nolabel
                core_type (ocamlary.ml[98,2731+26]..ocamlary.ml[98,2731+28])
                  Ttyp_var a
                core_type (ocamlary.ml[98,2731+32]..ocamlary.ml[98,2731+34])
                  Ttyp_var b
      ]
    structure_item (ocamlary.ml[104,2835+0]..ocamlary.ml[104,2835+21])
      Tstr_value Nonrec
      [
        <def>
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[100,2767+0]..[103,2832+2])
                  Pstr_eval
                  expression (ocamlary.ml[100,2767+0]..[103,2832+2])
                    Pexp_constant PConst_string("\n   @param x the [x] coordinate\n   @return the [y] coordinate\n",(ocamlary.ml[100,2767+0]..[103,2832+2]),None)
              ]
          pattern (ocamlary.ml[104,2835+4]..ocamlary.ml[104,2835+14])
            Tpat_var "a_function/305"
          expression (ocamlary.ml[104,2835+15]..ocamlary.ml[104,2835+21]) ghost
            Texp_function
            Labelled "x"
            [
              <case>
                pattern (ocamlary.ml[104,2835+16]..ocamlary.ml[104,2835+17])
                  Tpat_var "x/307"
                expression (ocamlary.ml[104,2835+20]..ocamlary.ml[104,2835+21])
                  Texp_ident "x/307"
            ]
      ]
    structure_item (ocamlary.ml[106,2858+0]..ocamlary.ml[106,2858+41])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[106,2858+4]..ocamlary.ml[106,2858+15])
            Tpat_var "fun_fun_fun/308"
          expression (ocamlary.ml[106,2858+16]..ocamlary.ml[106,2858+41]) ghost
            Texp_function
            Nolabel
            [
              <case>
                pattern (ocamlary.ml[106,2858+16]..ocamlary.ml[106,2858+24])
                  Tpat_var "_int_fun/310"
                expression (ocamlary.ml[106,2858+27]..ocamlary.ml[106,2858+41])
                  Texp_function
                  Nolabel
                  [
                    <case>
                      pattern (ocamlary.ml[106,2858+32]..ocamlary.ml[106,2858+34])
                        Tpat_construct "()"
                        []
                        None
                      expression (ocamlary.ml[106,2858+38]..ocamlary.ml[106,2858+40])
                        Texp_construct "()"
                        []
                  ]
            ]
      ]
    structure_item (ocamlary.ml[108,2901+0]..ocamlary.ml[108,2901+27])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[108,2901+4]..ocamlary.ml[108,2901+13])
            Tpat_var "fun_maybe/312"
          expression (ocamlary.ml[108,2901+14]..ocamlary.ml[108,2901+27]) ghost
            Texp_function
            Optional "yes"
            [
              <case>
                pattern (ocamlary.ml[108,2901+19]..ocamlary.ml[108,2901+20])
                  Tpat_any
                expression (ocamlary.ml[108,2901+21]..ocamlary.ml[108,2901+27]) ghost
                  Texp_function
                  Nolabel
                  [
                    <case>
                      pattern (ocamlary.ml[108,2901+21]..ocamlary.ml[108,2901+23])
                        Tpat_construct "()"
                        []
                        None
                      expression (ocamlary.ml[108,2901+26]..ocamlary.ml[108,2901+27])
                        Texp_constant Const_int 0
                  ]
            ]
      ]
    structure_item (ocamlary.ml[111,2973+0]..ocamlary.ml[111,2973+34])
      Tstr_value Nonrec
      [
        <def>
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[110,2930+0]..[110,2930+42])
                  Pstr_eval
                  expression (ocamlary.ml[110,2930+0]..[110,2930+42])
                    Pexp_constant PConst_string(" @raise Not_found That's all it does ",(ocamlary.ml[110,2930+0]..[110,2930+42]),None)
              ]
          pattern (ocamlary.ml[111,2973+4]..ocamlary.ml[111,2973+13])
            Tpat_var "not_found/316"
          expression (ocamlary.ml[111,2973+14]..ocamlary.ml[111,2973+34]) ghost
            Texp_function
            Nolabel
            [
              <case>
                pattern (ocamlary.ml[111,2973+14]..ocamlary.ml[111,2973+16])
                  Tpat_construct "()"
                  []
                  None
                expression (ocamlary.ml[111,2973+19]..ocamlary.ml[111,2973+34])
                  Texp_apply
                  expression (ocamlary.ml[111,2973+19]..ocamlary.ml[111,2973+24])
                    Texp_ident "Stdlib!.raise"
                  [
                    <arg>
                      Nolabel
                      expression (ocamlary.ml[111,2973+25]..ocamlary.ml[111,2973+34])
                        Texp_construct "Not_found"
                        []
                  ]
            ]
      ]
    structure_item (ocamlary.ml[114,3049+0]..ocamlary.ml[114,3049+33])
      Tstr_value Nonrec
      [
        <def>
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[113,3009+0]..[113,3009+39])
                  Pstr_eval
                  expression (ocamlary.ml[113,3009+0]..[113,3009+39])
                    Pexp_constant PConst_string(" @raise Kaboom That's all it does ",(ocamlary.ml[113,3009+0]..[113,3009+39]),None)
              ]
          pattern (ocamlary.ml[114,3049+4]..ocamlary.ml[114,3049+10])
            Tpat_var "kaboom/319"
          expression (ocamlary.ml[114,3049+11]..ocamlary.ml[114,3049+33]) ghost
            Texp_function
            Nolabel
            [
              <case>
                pattern (ocamlary.ml[114,3049+11]..ocamlary.ml[114,3049+13])
                  Tpat_construct "()"
                  []
                  None
                expression (ocamlary.ml[114,3049+16]..ocamlary.ml[114,3049+33])
                  Texp_apply
                  expression (ocamlary.ml[114,3049+16]..ocamlary.ml[114,3049+21])
                    Texp_ident "Stdlib!.raise"
                  [
                    <arg>
                      Nolabel
                      expression (ocamlary.ml[114,3049+22]..ocamlary.ml[114,3049+33])
                        Texp_construct "Kaboom"
                        [
                          expression (ocamlary.ml[114,3049+30]..ocamlary.ml[114,3049+32])
                            Texp_construct "()"
                            []
                        ]
                  ]
            ]
      ]
    structure_item (ocamlary.ml[117,3137+0]..ocamlary.ml[117,3137+35])
      Tstr_value Nonrec
      [
        <def>
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[116,3084+0]..[116,3084+52])
                  Pstr_eval
                  expression (ocamlary.ml[116,3084+0]..[116,3084+52])
                    Pexp_constant PConst_string(" @see < http://ocaml.org/ > The OCaml Web site ",(ocamlary.ml[116,3084+0]..[116,3084+52]),None)
              ]
          pattern (ocamlary.ml[117,3137+4]..ocamlary.ml[117,3137+13])
            Tpat_var "ocaml_org/322"
          expression (ocamlary.ml[117,3137+16]..ocamlary.ml[117,3137+35])
            Texp_constant Const_string("http://ocaml.org/",(ocamlary.ml[117,3137+17]..ocamlary.ml[117,3137+34]),None)
      ]
    structure_item (ocamlary.ml[120,3226+0]..ocamlary.ml[120,3226+27])
      Tstr_value Nonrec
      [
        <def>
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[119,3174+0]..[119,3174+51])
                  Pstr_eval
                  expression (ocamlary.ml[119,3174+0]..[119,3174+51])
                    Pexp_constant PConst_string(" @see 'some_file' The file called [some_file] ",(ocamlary.ml[119,3174+0]..[119,3174+51]),None)
              ]
          pattern (ocamlary.ml[120,3226+4]..ocamlary.ml[120,3226+13])
            Tpat_var "some_file/323"
          expression (ocamlary.ml[120,3226+16]..ocamlary.ml[120,3226+27])
            Texp_constant Const_string("some_file",(ocamlary.ml[120,3226+17]..ocamlary.ml[120,3226+26]),None)
      ]
    structure_item (ocamlary.ml[123,3309+0]..ocamlary.ml[123,3309+25])
      Tstr_value Nonrec
      [
        <def>
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[122,3255+0]..[122,3255+53])
                  Pstr_eval
                  expression (ocamlary.ml[122,3255+0]..[122,3255+53])
                    Pexp_constant PConst_string(" @see \"some_doc\" The document called [some_doc] ",(ocamlary.ml[122,3255+0]..[122,3255+53]),None)
              ]
          pattern (ocamlary.ml[123,3309+4]..ocamlary.ml[123,3309+12])
            Tpat_var "some_doc/324"
          expression (ocamlary.ml[123,3309+15]..ocamlary.ml[123,3309+25])
            Texp_constant Const_string("some_doc",(ocamlary.ml[123,3309+16]..ocamlary.ml[123,3309+24]),None)
      ]
    structure_item (ocamlary.ml[129,3412+0]..ocamlary.ml[129,3412+23])
      Tstr_value Nonrec
      [
        <def>
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[125,3336+0]..[128,3409+2])
                  Pstr_eval
                  expression (ocamlary.ml[125,3336+0]..[128,3409+2])
                    Pexp_constant PConst_string("\n   This value was introduced in the Mesozoic era.\n   @since mesozoic\n",(ocamlary.ml[125,3336+0]..[128,3409+2]),None)
              ]
          pattern (ocamlary.ml[129,3412+4]..ocamlary.ml[129,3412+18])
            Tpat_var "since_mesozoic/325"
          expression (ocamlary.ml[129,3412+21]..ocamlary.ml[129,3412+23])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[137,3580+0]..ocamlary.ml[137,3580+17])
      Tstr_value Nonrec
      [
        <def>
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[131,3437+0]..[136,3577+2])
                  Pstr_eval
                  expression (ocamlary.ml[131,3437+0]..[136,3577+2])
                    Pexp_constant PConst_string("\n   This value has had changes in 1.0.0, 1.1.0, and 1.2.0.\n   @before 1.0.0 before 1.0.0\n   @before 1.1.0 before 1.1.0\n   @version 1.2.0\n",(ocamlary.ml[131,3437+0]..[136,3577+2]),None)
              ]
          pattern (ocamlary.ml[137,3580+4]..ocamlary.ml[137,3580+12])
            Tpat_var "changing/326"
          expression (ocamlary.ml[137,3580+15]..ocamlary.ml[137,3580+17])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[139,3599+0]..ocamlary.ml[139,3599+26])
      Tstr_attribute "ocaml.text"
      [
        structure_item (ocamlary.ml[139,3599+0]..[139,3599+26])
          Pstr_eval
          expression (ocamlary.ml[139,3599+0]..[139,3599+26])
            Pexp_constant PConst_string(" {3 Some Operators } ",(ocamlary.ml[139,3599+0]..[139,3599+26]),None)
      ]
    structure_item (ocamlary.ml[141,3627+0]..ocamlary.ml[141,3627+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[141,3627+4]..ocamlary.ml[141,3627+10])
            Tpat_var "~-/327"
          expression (ocamlary.ml[141,3627+13]..ocamlary.ml[141,3627+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[142,3643+0]..ocamlary.ml[142,3643+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[142,3643+4]..ocamlary.ml[142,3643+9])
            Tpat_var "!/328"
          expression (ocamlary.ml[142,3643+13]..ocamlary.ml[142,3643+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[143,3659+0]..ocamlary.ml[143,3659+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[143,3659+4]..ocamlary.ml[143,3659+9])
            Tpat_var "@/329"
          expression (ocamlary.ml[143,3659+13]..ocamlary.ml[143,3659+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[144,3675+0]..ocamlary.ml[144,3675+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[144,3675+4]..ocamlary.ml[144,3675+9])
            Tpat_var "$/330"
          expression (ocamlary.ml[144,3675+13]..ocamlary.ml[144,3675+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[145,3691+0]..ocamlary.ml[145,3691+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[145,3691+4]..ocamlary.ml[145,3691+9])
            Tpat_var "%/331"
          expression (ocamlary.ml[145,3691+13]..ocamlary.ml[145,3691+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[146,3707+0]..ocamlary.ml[146,3707+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[146,3707+4]..ocamlary.ml[146,3707+9])
            Tpat_var "&/332"
          expression (ocamlary.ml[146,3707+13]..ocamlary.ml[146,3707+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[147,3723+0]..ocamlary.ml[147,3723+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[147,3723+4]..ocamlary.ml[147,3723+9])
            Tpat_var "*/333"
          expression (ocamlary.ml[147,3723+13]..ocamlary.ml[147,3723+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[148,3739+0]..ocamlary.ml[148,3739+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[148,3739+4]..ocamlary.ml[148,3739+9])
            Tpat_var "-/334"
          expression (ocamlary.ml[148,3739+13]..ocamlary.ml[148,3739+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[149,3755+0]..ocamlary.ml[149,3755+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[149,3755+4]..ocamlary.ml[149,3755+9])
            Tpat_var "+/335"
          expression (ocamlary.ml[149,3755+13]..ocamlary.ml[149,3755+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[150,3771+0]..ocamlary.ml[150,3771+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[150,3771+4]..ocamlary.ml[150,3771+10])
            Tpat_var "-?/336"
          expression (ocamlary.ml[150,3771+13]..ocamlary.ml[150,3771+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[151,3787+0]..ocamlary.ml[151,3787+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[151,3787+4]..ocamlary.ml[151,3787+9])
            Tpat_var "//337"
          expression (ocamlary.ml[151,3787+13]..ocamlary.ml[151,3787+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[152,3803+0]..ocamlary.ml[152,3803+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[152,3803+4]..ocamlary.ml[152,3803+10])
            Tpat_var ":=/338"
          expression (ocamlary.ml[152,3803+13]..ocamlary.ml[152,3803+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[153,3819+0]..ocamlary.ml[153,3819+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[153,3819+4]..ocamlary.ml[153,3819+9])
            Tpat_var "=/339"
          expression (ocamlary.ml[153,3819+13]..ocamlary.ml[153,3819+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[155,3836+0]..ocamlary.ml[155,3836+15])
      Tstr_value Nonrec
      [
        <def>
          pattern (ocamlary.ml[155,3836+4]..ocamlary.ml[155,3836+10])
            Tpat_var "land/340"
          expression (ocamlary.ml[155,3836+13]..ocamlary.ml[155,3836+15])
            Texp_construct "()"
            []
      ]
    structure_item (ocamlary.ml[157,3853+0]..ocamlary.ml[157,3853+32])
      Tstr_attribute "ocaml.text"
      [
        structure_item (ocamlary.ml[157,3853+0]..[157,3853+32])
          Pstr_eval
          expression (ocamlary.ml[157,3853+0]..[157,3853+32])
            Pexp_constant PConst_string(" {3 Advanced Module Stuff} ",(ocamlary.ml[157,3853+0]..[157,3853+32]),None)
      ]
    structure_item (ocamlary.ml[160,3934+0]..ocamlary.ml[185,4637+3])
      Tstr_module
      CollectionModule/350
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[159,3887+0]..[159,3887+46])
              Pstr_eval
              expression (ocamlary.ml[159,3887+0]..[159,3887+46])
                Pexp_constant PConst_string(" This comment is for [CollectionModule]. ",(ocamlary.ml[159,3887+0]..[159,3887+46]),None)
          ]
        module_expr (ocamlary.ml[160,3934+26]..ocamlary.ml[185,4637+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[162,4010+2]..ocamlary.ml[162,4010+17])
              Tstr_type Rec
              [
                type_declaration collection/341 (ocamlary.ml[162,4010+2]..ocamlary.ml[162,4010+17])
                  attribute "ocaml.doc"
                    [
                      structure_item (ocamlary.ml[161,3967+2]..[161,3967+42])
                        Pstr_eval
                        expression (ocamlary.ml[161,3967+2]..[161,3967+42])
                          Pexp_constant PConst_string(" This comment is for [collection]. ",(ocamlary.ml[161,3967+2]..[161,3967+42]),None)
                    ]
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
            structure_item (ocamlary.ml[163,4028+2]..ocamlary.ml[163,4028+14])
              Tstr_type Rec
              [
                type_declaration element/342 (ocamlary.ml[163,4028+2]..ocamlary.ml[163,4028+14])
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
            structure_item (ocamlary.ml[166,4089+2]..ocamlary.ml[181,4517+5])
              Tstr_module
              InnerModuleA/348
                attribute "ocaml.doc"
                  [
                    structure_item (ocamlary.ml[165,4044+2]..[165,4044+44])
                      Pstr_eval
                      expression (ocamlary.ml[165,4044+2]..[165,4044+44])
                        Pexp_constant PConst_string(" This comment is for [InnerModuleA]. ",(ocamlary.ml[165,4044+2]..[165,4044+44]),None)
                  ]
                module_expr (ocamlary.ml[166,4089+24]..ocamlary.ml[181,4517+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[168,4156+4]..ocamlary.ml[168,4156+23])
                      Tstr_type Rec
                      [
                        type_declaration t/343 (ocamlary.ml[168,4156+4]..ocamlary.ml[168,4156+23])
                          attribute "ocaml.doc"
                            [
                              structure_item (ocamlary.ml[167,4120+4]..[167,4120+35])
                                Pstr_eval
                                expression (ocamlary.ml[167,4120+4]..[167,4120+35])
                                  Pexp_constant PConst_string(" This comment is for [t]. ",(ocamlary.ml[167,4120+4]..[167,4120+35]),None)
                            ]
                          ptype_params =
                            []
                          ptype_cstrs =
                            []
                          ptype_kind =
                            Ttype_abstract
                          ptype_private = Public
                          ptype_manifest =
                            Some
                              core_type (ocamlary.ml[168,4156+13]..ocamlary.ml[168,4156+23])
                                Ttyp_constr "collection/341"
                                []
                      ]
                    structure_item (ocamlary.ml[171,4229+4]..ocamlary.ml[174,4339+7])
                      Tstr_module
                      InnerModuleA'/345
                        attribute "ocaml.doc"
                          [
                            structure_item (ocamlary.ml[170,4181+4]..[170,4181+47])
                              Pstr_eval
                              expression (ocamlary.ml[170,4181+4]..[170,4181+47])
                                Pexp_constant PConst_string(" This comment is for [InnerModuleA']. ",(ocamlary.ml[170,4181+4]..[170,4181+47]),None)
                          ]
                        module_expr (ocamlary.ml[171,4229+27]..ocamlary.ml[174,4339+7])
                          Tmod_structure
                          [
                            structure_item (ocamlary.ml[173,4301+6]..ocamlary.ml[173,4301+37])
                              Tstr_type Rec
                              [
                                type_declaration t/344 (ocamlary.ml[173,4301+6]..ocamlary.ml[173,4301+37])
                                  attribute "ocaml.doc"
                                    [
                                      structure_item (ocamlary.ml[172,4263+6]..[172,4263+37])
                                        Pstr_eval
                                        expression (ocamlary.ml[172,4263+6]..[172,4263+37])
                                          Pexp_constant PConst_string(" This comment is for [t]. ",(ocamlary.ml[172,4263+6]..[172,4263+37]),None)
                                    ]
                                  ptype_params =
                                    []
                                  ptype_cstrs =
                                    []
                                  ptype_kind =
                                    Ttype_abstract
                                  ptype_private = Public
                                  ptype_manifest =
                                    Some
                                      core_type (ocamlary.ml[173,4301+15]..ocamlary.ml[173,4301+37])
                                        Ttyp_constr "a_function/304"
                                        [
                                          core_type (ocamlary.ml[173,4301+16]..ocamlary.ml[173,4301+20])
                                            Ttyp_constr "unit/6!"
                                            []
                                          core_type (ocamlary.ml[173,4301+21]..ocamlary.ml[173,4301+25])
                                            Ttyp_constr "unit/6!"
                                            []
                                        ]
                              ]
                          ]
                    structure_item (ocamlary.ml[177,4400+4]..ocamlary.ml[180,4509+7])
                      Tstr_modtype "InnerModuleTypeA'/347"
                        attribute "ocaml.doc"
                          [
                            structure_item (ocamlary.ml[176,4348+4]..[176,4348+51])
                              Pstr_eval
                              expression (ocamlary.ml[176,4348+4]..[176,4348+51])
                                Pexp_constant PConst_string(" This comment is for [InnerModuleTypeA']. ",(ocamlary.ml[176,4348+4]..[176,4348+51]),None)
                          ]
                        module_type (ocamlary.ml[177,4400+36]..ocamlary.ml[180,4509+7])
                          Tmty_signature
                          [
                            signature_item (ocamlary.ml[179,4478+6]..ocamlary.ml[179,4478+30])
                              Tsig_type Rec
                              [
                                type_declaration t/346 (ocamlary.ml[179,4478+6]..ocamlary.ml[179,4478+30])
                                  attribute "ocaml.doc"
                                    [
                                      structure_item (ocamlary.ml[178,4440+6]..[178,4440+37])
                                        Pstr_eval
                                        expression (ocamlary.ml[178,4440+6]..[178,4440+37])
                                          Pexp_constant PConst_string(" This comment is for [t]. ",(ocamlary.ml[178,4440+6]..[178,4440+37]),None)
                                    ]
                                  ptype_params =
                                    []
                                  ptype_cstrs =
                                    []
                                  ptype_kind =
                                    Ttype_abstract
                                  ptype_private = Public
                                  ptype_manifest =
                                    Some
                                      core_type (ocamlary.ml[179,4478+15]..ocamlary.ml[179,4478+30])
                                        Ttyp_constr "InnerModuleA'/345.t"
                                        []
                              ]
                          ]
                  ]
            structure_item (ocamlary.ml[184,4573+2]..ocamlary.ml[184,4573+63])
              Tstr_modtype "InnerModuleTypeA/349"
                attribute "ocaml.doc"
                  [
                    structure_item (ocamlary.ml[183,4524+2]..[183,4524+48])
                      Pstr_eval
                      expression (ocamlary.ml[183,4524+2]..[183,4524+48])
                        Pexp_constant PConst_string(" This comment is for [InnerModuleTypeA]. ",(ocamlary.ml[183,4524+2]..[183,4524+48]),None)
                  ]
                module_type (ocamlary.ml[184,4573+33]..ocamlary.ml[184,4573+63])
                  Tmty_ident "InnerModuleA/348.InnerModuleTypeA'"
          ]
    structure_item (ocamlary.ml[188,4664+0]..ocamlary.ml[188,4664+56])
      Tstr_modtype "COLLECTION/351"
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[187,4642+0]..[187,4642+21])
              Pstr_eval
              expression (ocamlary.ml[187,4642+0]..[187,4642+21])
                Pexp_constant PConst_string(" module type of ",(ocamlary.ml[187,4642+0]..[187,4642+21]),None)
          ]
        module_type (ocamlary.ml[188,4664+25]..ocamlary.ml[188,4664+56])
          Tmty_typeof
          module_expr (ocamlary.ml[188,4664+40]..ocamlary.ml[188,4664+56])
            Tmod_ident "CollectionModule/350"
    structure_item (ocamlary.ml[190,4722+0]..ocamlary.ml[215,5511+3])
      Tstr_module
      Recollection/386
        module_expr (ocamlary.ml[190,4722+19]..ocamlary.ml[215,5511+3])
          Tmod_functor "C/352"
          module_type (ocamlary.ml[190,4722+24]..ocamlary.ml[190,4722+34])
            Tmty_ident "COLLECTION/351"
          module_expr (ocamlary.ml[190,4722+36]..ocamlary.ml[215,5511+3])
            Tmod_constraint
            module_expr (ocamlary.ml[191,4760+85]..ocamlary.ml[215,5511+3])
              Tmod_structure
              [
                structure_item (ocamlary.ml[192,4852+2]..ocamlary.ml[192,4852+34])
                  Tstr_type Rec
                  [
                    type_declaration collection/353 (ocamlary.ml[192,4852+2]..ocamlary.ml[192,4852+34])
                      ptype_params =
                        []
                      ptype_cstrs =
                        []
                      ptype_kind =
                        Ttype_abstract
                      ptype_private = Public
                      ptype_manifest =
                        Some
                          core_type (ocamlary.ml[192,4852+20]..ocamlary.ml[192,4852+34])
                            Ttyp_constr "list/9!"
                            [
                              core_type (ocamlary.ml[192,4852+20]..ocamlary.ml[192,4852+29])
                                Ttyp_constr "C/352.element"
                                []
                            ]
                  ]
                structure_item (ocamlary.ml[193,4887+2]..ocamlary.ml[193,4887+29])
                  Tstr_type Rec
                  [
                    type_declaration element/354 (ocamlary.ml[193,4887+2]..ocamlary.ml[193,4887+29])
                      ptype_params =
                        []
                      ptype_cstrs =
                        []
                      ptype_kind =
                        Ttype_abstract
                      ptype_private = Public
                      ptype_manifest =
                        Some
                          core_type (ocamlary.ml[193,4887+17]..ocamlary.ml[193,4887+29])
                            Ttyp_constr "C/352.collection"
                            []
                  ]
                structure_item (ocamlary.ml[196,4963+2]..ocamlary.ml[211,5391+5])
                  Tstr_module
                  InnerModuleA/360
                    attribute "ocaml.doc"
                      [
                        structure_item (ocamlary.ml[195,4918+2]..[195,4918+44])
                          Pstr_eval
                          expression (ocamlary.ml[195,4918+2]..[195,4918+44])
                            Pexp_constant PConst_string(" This comment is for [InnerModuleA]. ",(ocamlary.ml[195,4918+2]..[195,4918+44]),None)
                      ]
                    module_expr (ocamlary.ml[196,4963+24]..ocamlary.ml[211,5391+5])
                      Tmod_structure
                      [
                        structure_item (ocamlary.ml[198,5030+4]..ocamlary.ml[198,5030+23])
                          Tstr_type Rec
                          [
                            type_declaration t/355 (ocamlary.ml[198,5030+4]..ocamlary.ml[198,5030+23])
                              attribute "ocaml.doc"
                                [
                                  structure_item (ocamlary.ml[197,4994+4]..[197,4994+35])
                                    Pstr_eval
                                    expression (ocamlary.ml[197,4994+4]..[197,4994+35])
                                      Pexp_constant PConst_string(" This comment is for [t]. ",(ocamlary.ml[197,4994+4]..[197,4994+35]),None)
                                ]
                              ptype_params =
                                []
                              ptype_cstrs =
                                []
                              ptype_kind =
                                Ttype_abstract
                              ptype_private = Public
                              ptype_manifest =
                                Some
                                  core_type (ocamlary.ml[198,5030+13]..ocamlary.ml[198,5030+23])
                                    Ttyp_constr "collection/353"
                                    []
                          ]
                        structure_item (ocamlary.ml[201,5103+4]..ocamlary.ml[204,5213+7])
                          Tstr_module
                          InnerModuleA'/357
                            attribute "ocaml.doc"
                              [
                                structure_item (ocamlary.ml[200,5055+4]..[200,5055+47])
                                  Pstr_eval
                                  expression (ocamlary.ml[200,5055+4]..[200,5055+47])
                                    Pexp_constant PConst_string(" This comment is for [InnerModuleA']. ",(ocamlary.ml[200,5055+4]..[200,5055+47]),None)
                              ]
                            module_expr (ocamlary.ml[201,5103+27]..ocamlary.ml[204,5213+7])
                              Tmod_structure
                              [
                                structure_item (ocamlary.ml[203,5175+6]..ocamlary.ml[203,5175+37])
                                  Tstr_type Rec
                                  [
                                    type_declaration t/356 (ocamlary.ml[203,5175+6]..ocamlary.ml[203,5175+37])
                                      attribute "ocaml.doc"
                                        [
                                          structure_item (ocamlary.ml[202,5137+6]..[202,5137+37])
                                            Pstr_eval
                                            expression (ocamlary.ml[202,5137+6]..[202,5137+37])
                                              Pexp_constant PConst_string(" This comment is for [t]. ",(ocamlary.ml[202,5137+6]..[202,5137+37]),None)
                                        ]
                                      ptype_params =
                                        []
                                      ptype_cstrs =
                                        []
                                      ptype_kind =
                                        Ttype_abstract
                                      ptype_private = Public
                                      ptype_manifest =
                                        Some
                                          core_type (ocamlary.ml[203,5175+15]..ocamlary.ml[203,5175+37])
                                            Ttyp_constr "a_function/304"
                                            [
                                              core_type (ocamlary.ml[203,5175+16]..ocamlary.ml[203,5175+20])
                                                Ttyp_constr "unit/6!"
                                                []
                                              core_type (ocamlary.ml[203,5175+21]..ocamlary.ml[203,5175+25])
                                                Ttyp_constr "unit/6!"
                                                []
                                            ]
                                  ]
                              ]
                        structure_item (ocamlary.ml[207,5274+4]..ocamlary.ml[210,5383+7])
                          Tstr_modtype "InnerModuleTypeA'/359"
                            attribute "ocaml.doc"
                              [
                                structure_item (ocamlary.ml[206,5222+4]..[206,5222+51])
                                  Pstr_eval
                                  expression (ocamlary.ml[206,5222+4]..[206,5222+51])
                                    Pexp_constant PConst_string(" This comment is for [InnerModuleTypeA']. ",(ocamlary.ml[206,5222+4]..[206,5222+51]),None)
                              ]
                            module_type (ocamlary.ml[207,5274+36]..ocamlary.ml[210,5383+7])
                              Tmty_signature
                              [
                                signature_item (ocamlary.ml[209,5352+6]..ocamlary.ml[209,5352+30])
                                  Tsig_type Rec
                                  [
                                    type_declaration t/358 (ocamlary.ml[209,5352+6]..ocamlary.ml[209,5352+30])
                                      attribute "ocaml.doc"
                                        [
                                          structure_item (ocamlary.ml[208,5314+6]..[208,5314+37])
                                            Pstr_eval
                                            expression (ocamlary.ml[208,5314+6]..[208,5314+37])
                                              Pexp_constant PConst_string(" This comment is for [t]. ",(ocamlary.ml[208,5314+6]..[208,5314+37]),None)
                                        ]
                                      ptype_params =
                                        []
                                      ptype_cstrs =
                                        []
                                      ptype_kind =
                                        Ttype_abstract
                                      ptype_private = Public
                                      ptype_manifest =
                                        Some
                                          core_type (ocamlary.ml[209,5352+15]..ocamlary.ml[209,5352+30])
                                            Ttyp_constr "InnerModuleA'/357.t"
                                            []
                                  ]
                              ]
                      ]
                structure_item (ocamlary.ml[214,5447+2]..ocamlary.ml[214,5447+63])
                  Tstr_modtype "InnerModuleTypeA/361"
                    attribute "ocaml.doc"
                      [
                        structure_item (ocamlary.ml[213,5398+2]..[213,5398+48])
                          Pstr_eval
                          expression (ocamlary.ml[213,5398+2]..[213,5398+48])
                            Pexp_constant PConst_string(" This comment is for [InnerModuleTypeA]. ",(ocamlary.ml[213,5398+2]..[213,5398+48]),None)
                      ]
                    module_type (ocamlary.ml[214,5447+33]..ocamlary.ml[214,5447+63])
                      Tmty_ident "InnerModuleA/360.InnerModuleTypeA'"
              ]
            module_type (ocamlary.ml[191,4760+2]..ocamlary.ml[191,4760+82])
              Tmty_with
              module_type (ocamlary.ml[191,4760+2]..ocamlary.ml[191,4760+12])
                Tmty_ident "COLLECTION/351"
              [
                "collection/341"
                  Twith_type
                    type_declaration collection/341 (ocamlary.ml[191,4760+18]..ocamlary.ml[191,4760+50])
                      ptype_params =
                        []
                      ptype_cstrs =
                        []
                      ptype_kind =
                        Ttype_abstract
                      ptype_private = Public
                      ptype_manifest =
                        Some
                          core_type (ocamlary.ml[191,4760+36]..ocamlary.ml[191,4760+50])
                            Ttyp_constr "list/9!"
                            [
                              core_type (ocamlary.ml[191,4760+36]..ocamlary.ml[191,4760+45])
                                Ttyp_constr "C/352.element"
                                []
                            ]
                "element/342"
                  Twith_type
                    type_declaration element/342 (ocamlary.ml[191,4760+55]..ocamlary.ml[191,4760+82])
                      ptype_params =
                        []
                      ptype_cstrs =
                        []
                      ptype_kind =
                        Ttype_abstract
                      ptype_private = Public
                      ptype_manifest =
                        Some
                          core_type (ocamlary.ml[191,4760+70]..ocamlary.ml[191,4760+82])
                            Ttyp_constr "C/352.collection"
                            []
              ]
    structure_item (ocamlary.ml[217,5516+0]..ocamlary.ml[217,5516+47])
      Tstr_modtype "MMM/388"
        module_type (ocamlary.ml[217,5516+18]..ocamlary.ml[217,5516+47])
          Tmty_signature
          [
            signature_item (ocamlary.ml[217,5516+22]..ocamlary.ml[217,5516+43])
              Tsig_module "C/387"
              module_type (ocamlary.ml[217,5516+33]..ocamlary.ml[217,5516+43])
                Tmty_ident "COLLECTION/351"
          ]
    structure_item (ocamlary.ml[219,5565+0]..ocamlary.ml[219,5565+77])
      Tstr_modtype "RECOLLECTION/428"
        module_type (ocamlary.ml[219,5565+27]..ocamlary.ml[219,5565+77])
          Tmty_with
          module_type (ocamlary.ml[219,5565+27]..ocamlary.ml[219,5565+30])
            Tmty_ident "MMM/388"
          [
            "C/387"
              Twith_module "Recollection/386(CollectionModule/350)"
          ]
    structure_item (ocamlary.ml[221,5644+0]..ocamlary.ml[223,5737+3])
      Tstr_modtype "RecollectionModule/448"
        module_type (ocamlary.ml[221,5644+33]..ocamlary.ml[223,5737+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[222,5681+2]..ocamlary.ml[222,5681+55])
              Tsig_include
              module_type (ocamlary.ml[222,5681+10]..ocamlary.ml[222,5681+55])
                Tmty_typeof
                module_expr (ocamlary.ml[222,5681+25]..ocamlary.ml[222,5681+55])
                  Tmod_apply
                  module_expr (ocamlary.ml[222,5681+25]..ocamlary.ml[222,5681+37])
                    Tmod_ident "Recollection/386"
                  module_expr (ocamlary.ml[222,5681+38]..ocamlary.ml[222,5681+54])
                    Tmod_ident "CollectionModule/350"
          ]
    structure_item (ocamlary.ml[225,5742+0]..ocamlary.ml[228,5795+3])
      Tstr_modtype "A/451"
        module_type (ocamlary.ml[225,5742+16]..ocamlary.ml[228,5795+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[226,5762+2]..ocamlary.ml[226,5762+8])
              Tsig_type Rec
              [
                type_declaration t/449 (ocamlary.ml[226,5762+2]..ocamlary.ml[226,5762+8])
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
            signature_item (ocamlary.ml[227,5771+2]..ocamlary.ml[227,5771+23])
              Tsig_module "Q/450"
              module_type (ocamlary.ml[227,5771+13]..ocamlary.ml[227,5771+23])
                Tmty_ident "COLLECTION/351"
          ]
    structure_item (ocamlary.ml[230,5800+0]..ocamlary.ml[233,5853+3])
      Tstr_modtype "B/454"
        module_type (ocamlary.ml[230,5800+16]..ocamlary.ml[233,5853+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[231,5820+2]..ocamlary.ml[231,5820+8])
              Tsig_type Rec
              [
                type_declaration t/452 (ocamlary.ml[231,5820+2]..ocamlary.ml[231,5820+8])
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
            signature_item (ocamlary.ml[232,5829+2]..ocamlary.ml[232,5829+23])
              Tsig_module "Q/453"
              module_type (ocamlary.ml[232,5829+13]..ocamlary.ml[232,5829+23])
                Tmty_ident "COLLECTION/351"
          ]
    structure_item (ocamlary.ml[235,5858+0]..ocamlary.ml[238,5937+3])
      Tstr_modtype "C/459"
        module_type (ocamlary.ml[235,5858+16]..ocamlary.ml[238,5937+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[236,5878+2]..ocamlary.ml[236,5878+11])
              Tsig_include
              module_type (ocamlary.ml[236,5878+10]..ocamlary.ml[236,5878+11])
                Tmty_ident "A/451"
            signature_item (ocamlary.ml[237,5890+2]..ocamlary.ml[237,5890+46])
              Tsig_include
              module_type (ocamlary.ml[237,5890+10]..ocamlary.ml[237,5890+46])
                Tmty_with
                module_type (ocamlary.ml[237,5890+10]..ocamlary.ml[237,5890+11])
                  Tmty_ident "B/454"
                [
                  "t/452"
                    Twith_typesubst
                      type_declaration t/452 (ocamlary.ml[237,5890+17]..ocamlary.ml[237,5890+28])
                        ptype_params =
                          []
                        ptype_cstrs =
                          []
                        ptype_kind =
                          Ttype_abstract
                        ptype_private = Public
                        ptype_manifest =
                          Some
                            core_type (ocamlary.ml[237,5890+27]..ocamlary.ml[237,5890+28])
                              Ttyp_constr "t/455"
                              []
                  "Q/458"
                    Twith_modsubst "Q/456"
                ]
          ]
    structure_item (ocamlary.ml[249,6167+0]..ocamlary.ml[252,6310+3])
      Tstr_module
      FunctorTypeOf/462
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[248,6123+0]..[248,6123+43])
              Pstr_eval
              expression (ocamlary.ml[248,6123+0]..[248,6123+43])
                Pexp_constant PConst_string(" This comment is for [FunctorTypeOf]. ",(ocamlary.ml[248,6123+0]..[248,6123+43]),None)
          ]
        module_expr (ocamlary.ml[249,6167+20]..ocamlary.ml[252,6310+3])
          Tmod_functor "Collection/460"
          module_type (ocamlary.ml[249,6167+34]..ocamlary.ml[249,6167+65])
            Tmty_typeof
            module_expr (ocamlary.ml[249,6167+49]..ocamlary.ml[249,6167+65])
              Tmod_ident "CollectionModule/350"
          module_expr (ocamlary.ml[249,6167+69]..ocamlary.ml[252,6310+3])
            Tmod_structure
            [
              structure_item (ocamlary.ml[251,6277+2]..ocamlary.ml[251,6277+32])
                Tstr_type Rec
                [
                  type_declaration t/461 (ocamlary.ml[251,6277+2]..ocamlary.ml[251,6277+32])
                    attribute "ocaml.doc"
                      [
                        structure_item (ocamlary.ml[250,6243+2]..[250,6243+33])
                          Pstr_eval
                          expression (ocamlary.ml[250,6243+2]..[250,6243+33])
                            Pexp_constant PConst_string(" This comment is for [t]. ",(ocamlary.ml[250,6243+2]..[250,6243+33]),None)
                      ]
                    ptype_params =
                      []
                    ptype_cstrs =
                      []
                    ptype_kind =
                      Ttype_abstract
                    ptype_private = Public
                    ptype_manifest =
                      Some
                        core_type (ocamlary.ml[251,6277+11]..ocamlary.ml[251,6277+32])
                          Ttyp_constr "Collection/460.collection"
                          []
                ]
            ]
    structure_item (ocamlary.ml[255,6363+0]..ocamlary.ml[258,6477+3])
      Tstr_modtype "IncludeModuleType/463"
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[254,6315+0]..[254,6315+47])
              Pstr_eval
              expression (ocamlary.ml[254,6315+0]..[254,6315+47])
                Pexp_constant PConst_string(" This comment is for [IncludeModuleType]. ",(ocamlary.ml[254,6315+0]..[254,6315+47]),None)
          ]
        module_type (ocamlary.ml[255,6363+32]..ocamlary.ml[258,6477+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[257,6453+2]..ocamlary.ml[257,6453+23])
              Tsig_include
                attribute "ocaml.doc"
                  [
                    structure_item (ocamlary.ml[256,6399+2]..[256,6399+53])
                      Pstr_eval
                      expression (ocamlary.ml[256,6399+2]..[256,6399+53])
                        Pexp_constant PConst_string(" This comment is for [include EmptySigAlias]. ",(ocamlary.ml[256,6399+2]..[256,6399+53]),None)
                  ]
              module_type (ocamlary.ml[257,6453+10]..ocamlary.ml[257,6453+23])
                Tmty_ident "EmptySigAlias/275"
          ]
    structure_item (ocamlary.ml[260,6482+0]..ocamlary.ml[267,6599+3])
      Tstr_modtype "ToInclude/468"
        module_type (ocamlary.ml[260,6482+24]..ocamlary.ml[267,6599+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[261,6510+2]..ocamlary.ml[263,6546+5])
              Tsig_module "IncludedA/465"
              module_type (ocamlary.ml[261,6510+21]..ocamlary.ml[263,6546+5])
                Tmty_signature
                [
                  signature_item (ocamlary.ml[262,6535+4]..ocamlary.ml[262,6535+10])
                    Tsig_type Rec
                    [
                      type_declaration t/464 (ocamlary.ml[262,6535+4]..ocamlary.ml[262,6535+10])
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
                ]
            signature_item (ocamlary.ml[264,6552+2]..ocamlary.ml[266,6593+5])
              Tsig_modtype "IncludedB/467"
                module_type (ocamlary.ml[264,6552+26]..ocamlary.ml[266,6593+5])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[265,6582+4]..ocamlary.ml[265,6582+10])
                      Tsig_type Rec
                      [
                        type_declaration s/466 (ocamlary.ml[265,6582+4]..ocamlary.ml[265,6582+10])
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
                  ]
          ]
    structure_item (ocamlary.ml[269,6604+0]..ocamlary.ml[271,6639+3])
      Tstr_module
      IncludedA/470
        module_expr (ocamlary.ml[269,6604+19]..ocamlary.ml[271,6639+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[270,6630+2]..ocamlary.ml[270,6630+8])
              Tstr_type Rec
              [
                type_declaration t/469 (ocamlary.ml[270,6630+2]..ocamlary.ml[270,6630+8])
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
          ]
    structure_item (ocamlary.ml[273,6644+0]..ocamlary.ml[275,6681+3])
      Tstr_modtype "IncludedB/472"
        module_type (ocamlary.ml[273,6644+24]..ocamlary.ml[275,6681+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[274,6672+2]..ocamlary.ml[274,6672+8])
              Tsig_type Rec
              [
                type_declaration s/471 (ocamlary.ml[274,6672+2]..ocamlary.ml[274,6672+8])
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
          ]
    structure_item (ocamlary.ml[277,6686+0]..ocamlary.ml[277,6686+30])
      Tstr_attribute "ocaml.text"
      [
        structure_item (ocamlary.ml[277,6686+0]..[277,6686+30])
          Pstr_eval
          expression (ocamlary.ml[277,6686+0]..[277,6686+30])
            Pexp_constant PConst_string(" {3 Advanced Type Stuff} ",(ocamlary.ml[277,6686+0]..[277,6686+30]),None)
      ]
    structure_item (ocamlary.ml[280,6755+0]..ocamlary.ml[283,6877+1])
      Tstr_type Rec
      [
        type_declaration record/473 (ocamlary.ml[280,6755+0]..ocamlary.ml[283,6877+1])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[279,6718+0]..[279,6718+36])
                Pstr_eval
                expression (ocamlary.ml[279,6718+0]..[279,6718+36])
                  Pexp_constant PConst_string(" This comment is for [record]. ",(ocamlary.ml[279,6718+0]..[279,6718+36]),None)
            ]
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[284,6879+0]..[284,6879+41])
                Pstr_eval
                expression (ocamlary.ml[284,6879+0]..[284,6879+41])
                  Pexp_constant PConst_string(" This comment is also for [record]. ",(ocamlary.ml[284,6879+0]..[284,6879+41]),None)
            ]
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_record
              [
                (ocamlary.ml[281,6771+2]..ocamlary.ml[281,6771+15])
                  attribute "ocaml.doc"
                    [
                      structure_item (ocamlary.ml[281,6771+16]..[281,6771+52])
                        Pstr_eval
                        expression (ocamlary.ml[281,6771+16]..[281,6771+52])
                          Pexp_constant PConst_string(" This comment is for [field1]. ",(ocamlary.ml[281,6771+16]..[281,6771+52]),None)
                    ]
                  Immutable
                  field1/474                core_type (ocamlary.ml[281,6771+11]..ocamlary.ml[281,6771+14])
                    Ttyp_poly
                    core_type (ocamlary.ml[281,6771+11]..ocamlary.ml[281,6771+14])
                      Ttyp_constr "int/1!"
                      []
                (ocamlary.ml[282,6824+2]..ocamlary.ml[282,6824+15])
                  attribute "ocaml.doc"
                    [
                      structure_item (ocamlary.ml[282,6824+16]..[282,6824+52])
                        Pstr_eval
                        expression (ocamlary.ml[282,6824+16]..[282,6824+52])
                          Pexp_constant PConst_string(" This comment is for [field2]. ",(ocamlary.ml[282,6824+16]..[282,6824+52]),None)
                    ]
                  Immutable
                  field2/475                core_type (ocamlary.ml[282,6824+11]..ocamlary.ml[282,6824+14])
                    Ttyp_poly
                    core_type (ocamlary.ml[282,6824+11]..ocamlary.ml[282,6824+14])
                      Ttyp_constr "int/1!"
                      []
              ]
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (ocamlary.ml[286,6922+0]..ocamlary.ml[290,7095+1])
      Tstr_type Rec
      [
        type_declaration mutable_record/476 (ocamlary.ml[286,6922+0]..ocamlary.ml[290,7095+1])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_record
              [
                (ocamlary.ml[287,6946+2]..ocamlary.ml[287,6946+18])
                  attribute "ocaml.doc"
                    [
                      structure_item (ocamlary.ml[287,6946+19]..[287,6946+50])
                        Pstr_eval
                        expression (ocamlary.ml[287,6946+19]..[287,6946+50])
                          Pexp_constant PConst_string(" [a] is first and mutable ",(ocamlary.ml[287,6946+19]..[287,6946+50]),None)
                    ]
                  Mutable
                  a/477                core_type (ocamlary.ml[287,6946+14]..ocamlary.ml[287,6946+17])
                    Ttyp_poly
                    core_type (ocamlary.ml[287,6946+14]..ocamlary.ml[287,6946+17])
                      Ttyp_constr "int/1!"
                      []
                (ocamlary.ml[288,6997+2]..ocamlary.ml[288,6997+11])
                  attribute "ocaml.doc"
                    [
                      structure_item (ocamlary.ml[288,6997+12]..[288,6997+46])
                        Pstr_eval
                        expression (ocamlary.ml[288,6997+12]..[288,6997+46])
                          Pexp_constant PConst_string(" [b] is second and immutable ",(ocamlary.ml[288,6997+12]..[288,6997+46]),None)
                    ]
                  Immutable
                  b/478                core_type (ocamlary.ml[288,6997+6]..ocamlary.ml[288,6997+10])
                    Ttyp_poly
                    core_type (ocamlary.ml[288,6997+6]..ocamlary.ml[288,6997+10])
                      Ttyp_constr "unit/6!"
                      []
                (ocamlary.ml[289,7044+2]..ocamlary.ml[289,7044+18])
                  attribute "ocaml.doc"
                    [
                      structure_item (ocamlary.ml[289,7044+19]..[289,7044+50])
                        Pstr_eval
                        expression (ocamlary.ml[289,7044+19]..[289,7044+50])
                          Pexp_constant PConst_string(" [c] is third and mutable ",(ocamlary.ml[289,7044+19]..[289,7044+50]),None)
                    ]
                  Mutable
                  c/479                core_type (ocamlary.ml[289,7044+14]..ocamlary.ml[289,7044+17])
                    Ttyp_poly
                    core_type (ocamlary.ml[289,7044+14]..ocamlary.ml[289,7044+17])
                      Ttyp_constr "int/1!"
                      []
              ]
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (ocamlary.ml[292,7098+0]..ocamlary.ml[294,7152+1])
      Tstr_type Rec
      [
        type_declaration universe_record/480 (ocamlary.ml[292,7098+0]..ocamlary.ml[294,7152+1])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_record
              [
                (ocamlary.ml[293,7123+2]..ocamlary.ml[293,7123+28])
                  Immutable
                  nihilate/481                core_type (ocamlary.ml[293,7123+13]..ocamlary.ml[293,7123+27])
                    Ttyp_poly 'a
                    core_type (ocamlary.ml[293,7123+17]..ocamlary.ml[293,7123+27])
                      Ttyp_arrow
                      Nolabel
                      core_type (ocamlary.ml[293,7123+17]..ocamlary.ml[293,7123+19])
                        Ttyp_var a
                      core_type (ocamlary.ml[293,7123+23]..ocamlary.ml[293,7123+27])
                        Ttyp_constr "unit/6!"
                        []
              ]
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (ocamlary.ml[297,7193+0]..ocamlary.ml[301,7373+24])
      Tstr_type Rec
      [
        type_declaration variant/482 (ocamlary.ml[297,7193+0]..ocamlary.ml[301,7373+24])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[296,7155+0]..[296,7155+37])
                Pstr_eval
                expression (ocamlary.ml[296,7155+0]..[296,7155+37])
                  Pexp_constant PConst_string(" This comment is for [variant]. ",(ocamlary.ml[296,7155+0]..[296,7155+37]),None)
            ]
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[303,7458+0]..[303,7458+42])
                Pstr_eval
                expression (ocamlary.ml[303,7458+0]..[303,7458+42])
                  Pexp_constant PConst_string(" This comment is also for [variant]. ",(ocamlary.ml[303,7458+0]..[303,7458+42]),None)
            ]
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (ocamlary.ml[298,7208+0]..ocamlary.ml[298,7208+6])
                  TagA/483
                  attribute "ocaml.doc"
                    [
                      structure_item (ocamlary.ml[298,7208+7]..[298,7208+41])
                        Pstr_eval
                        expression (ocamlary.ml[298,7208+7]..[298,7208+41])
                          Pexp_constant PConst_string(" This comment is for [TagA]. ",(ocamlary.ml[298,7208+7]..[298,7208+41]),None)
                    ]
                  []
                  None
                (ocamlary.ml[299,7250+0]..ocamlary.ml[299,7250+16])
                  ConstrB/484
                  attribute "ocaml.doc"
                    [
                      structure_item (ocamlary.ml[299,7250+17]..[299,7250+54])
                        Pstr_eval
                        expression (ocamlary.ml[299,7250+17]..[299,7250+54])
                          Pexp_constant PConst_string(" This comment is for [ConstrB]. ",(ocamlary.ml[299,7250+17]..[299,7250+54]),None)
                    ]
                  [
                    core_type (ocamlary.ml[299,7250+13]..ocamlary.ml[299,7250+16])
                      Ttyp_constr "int/1!"
                      []
                  ]
                  None
                (ocamlary.ml[300,7305+0]..ocamlary.ml[300,7305+22])
                  ConstrC/485
                  attribute "ocaml.doc"
                    [
                      structure_item (ocamlary.ml[300,7305+23]..[300,7305+67])
                        Pstr_eval
                        expression (ocamlary.ml[300,7305+23]..[300,7305+67])
                          Pexp_constant PConst_string(" This comment is for binary [ConstrC]. ",(ocamlary.ml[300,7305+23]..[300,7305+67]),None)
                    ]
                  [
                    core_type (ocamlary.ml[300,7305+13]..ocamlary.ml[300,7305+16])
                      Ttyp_constr "int/1!"
                      []
                    core_type (ocamlary.ml[300,7305+19]..ocamlary.ml[300,7305+22])
                      Ttyp_constr "int/1!"
                      []
                  ]
                  None
                (ocamlary.ml[301,7373+0]..ocamlary.ml[301,7373+24])
                  ConstrD/486
                  attribute "ocaml.doc"
                    [
                      structure_item (ocamlary.ml[302,7398+0]..[302,7398+59])
                        Pstr_eval
                        expression (ocamlary.ml[302,7398+0]..[302,7398+59])
                          Pexp_constant PConst_string(" This comment is for unary [ConstrD] of binary tuple. ",(ocamlary.ml[302,7398+0]..[302,7398+59]),None)
                    ]
                  [
                    core_type (ocamlary.ml[301,7373+14]..ocamlary.ml[301,7373+23])
                      Ttyp_tuple
                      [
                        core_type (ocamlary.ml[301,7373+14]..ocamlary.ml[301,7373+17])
                          Ttyp_constr "int/1!"
                          []
                        core_type (ocamlary.ml[301,7373+20]..ocamlary.ml[301,7373+23])
                          Ttyp_constr "int/1!"
                          []
                      ]
                  ]
                  None
              ]
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (ocamlary.ml[306,7545+0]..ocamlary.ml[309,7668+1])
      Tstr_type Rec
      [
        type_declaration poly_variant/487 (ocamlary.ml[306,7545+0]..ocamlary.ml[309,7668+1])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[305,7502+0]..[305,7502+42])
                Pstr_eval
                expression (ocamlary.ml[305,7502+0]..[305,7502+42])
                  Pexp_constant PConst_string(" This comment is for [poly_variant]. ",(ocamlary.ml[305,7502+0]..[305,7502+42]),None)
            ]
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[310,7670+0]..[310,7670+41])
                Pstr_eval
                expression (ocamlary.ml[310,7670+0]..[310,7670+41])
                  Pexp_constant PConst_string(" Wow! It was a polymorphic variant! ",(ocamlary.ml[310,7670+0]..[310,7670+41]),None)
            ]
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[306,7545+20]..ocamlary.ml[309,7668+1])
                Ttyp_variant closed=Closed
                [
                  Ttag "TagA" true
                      attribute "ocaml.doc"
                        [
                          structure_item (ocamlary.ml[307,7567+8]..[307,7567+43])
                            Pstr_eval
                            expression (ocamlary.ml[307,7567+8]..[307,7567+43])
                              Pexp_constant PConst_string(" This comment is for [`TagA]. ",(ocamlary.ml[307,7567+8]..[307,7567+43]),None)
                        ]
                    []
                  Ttag "ConstrB" false
                      attribute "ocaml.doc"
                        [
                          structure_item (ocamlary.ml[308,7611+18]..[308,7611+56])
                            Pstr_eval
                            expression (ocamlary.ml[308,7611+18]..[308,7611+56])
                              Pexp_constant PConst_string(" This comment is for [`ConstrB]. ",(ocamlary.ml[308,7611+18]..[308,7611+56]),None)
                        ]
                    [
                      core_type (ocamlary.ml[308,7611+14]..ocamlary.ml[308,7611+17])
                        Ttyp_constr "int/1!"
                        []
                    ]
                ]
                None
      ]
    structure_item (ocamlary.ml[313,7753+0]..ocamlary.ml[317,7879+41])
      Tstr_type Rec
      [
        type_declaration full_gadt/488 (ocamlary.ml[313,7753+0]..ocamlary.ml[317,7879+41])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[312,7713+0]..[312,7713+39])
                Pstr_eval
                expression (ocamlary.ml[312,7713+0]..[312,7713+39])
                  Pexp_constant PConst_string(" This comment is for [full_gadt]. ",(ocamlary.ml[312,7713+0]..[312,7713+39]),None)
            ]
          ptype_params =
            [
              core_type (ocamlary.ml[313,7753+6]..ocamlary.ml[313,7753+7])
                Ttyp_any
              core_type (ocamlary.ml[313,7753+8]..ocamlary.ml[313,7753+9])
                Ttyp_any
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (ocamlary.ml[314,7776+0]..ocamlary.ml[314,7776+29])
                  Tag/489
                  []
                  Some
                    core_type (ocamlary.ml[314,7776+8]..ocamlary.ml[314,7776+29])
                      Ttyp_constr "full_gadt/488"
                      [
                        core_type (ocamlary.ml[314,7776+9]..ocamlary.ml[314,7776+13])
                          Ttyp_constr "unit/6!"
                          []
                        core_type (ocamlary.ml[314,7776+14]..ocamlary.ml[314,7776+18])
                          Ttyp_constr "unit/6!"
                          []
                      ]
                (ocamlary.ml[315,7806+0]..ocamlary.ml[315,7806+35])
                  First/490
                  [
                    core_type (ocamlary.ml[315,7806+10]..ocamlary.ml[315,7806+12])
                      Ttyp_var a
                  ]
                  Some
                    core_type (ocamlary.ml[315,7806+16]..ocamlary.ml[315,7806+35])
                      Ttyp_constr "full_gadt/488"
                      [
                        core_type (ocamlary.ml[315,7806+17]..ocamlary.ml[315,7806+19])
                          Ttyp_var a
                        core_type (ocamlary.ml[315,7806+20]..ocamlary.ml[315,7806+24])
                          Ttyp_constr "unit/6!"
                          []
                      ]
                (ocamlary.ml[316,7842+0]..ocamlary.ml[316,7842+36])
                  Second/491
                  [
                    core_type (ocamlary.ml[316,7842+11]..ocamlary.ml[316,7842+13])
                      Ttyp_var a
                  ]
                  Some
                    core_type (ocamlary.ml[316,7842+17]..ocamlary.ml[316,7842+36])
                      Ttyp_constr "full_gadt/488"
                      [
                        core_type (ocamlary.ml[316,7842+18]..ocamlary.ml[316,7842+22])
                          Ttyp_constr "unit/6!"
                          []
                        core_type (ocamlary.ml[316,7842+23]..ocamlary.ml[316,7842+25])
                          Ttyp_var a
                      ]
                (ocamlary.ml[317,7879+0]..ocamlary.ml[317,7879+41])
                  Exist/492
                  attribute "ocaml.doc"
                    [
                      structure_item (ocamlary.ml[318,7921+0]..[318,7921+26])
                        Pstr_eval
                        expression (ocamlary.ml[318,7921+0]..[318,7921+26])
                          Pexp_constant PConst_string(" Wow! It was a GADT! ",(ocamlary.ml[318,7921+0]..[318,7921+26]),None)
                    ]
                  [
                    core_type (ocamlary.ml[317,7879+10]..ocamlary.ml[317,7879+12])
                      Ttyp_var a
                    core_type (ocamlary.ml[317,7879+15]..ocamlary.ml[317,7879+17])
                      Ttyp_var b
                  ]
                  Some
                    core_type (ocamlary.ml[317,7879+21]..ocamlary.ml[317,7879+41])
                      Ttyp_constr "full_gadt/488"
                      [
                        core_type (ocamlary.ml[317,7879+22]..ocamlary.ml[317,7879+24])
                          Ttyp_var b
                        core_type (ocamlary.ml[317,7879+26]..ocamlary.ml[317,7879+30])
                          Ttyp_constr "unit/6!"
                          []
                      ]
              ]
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (ocamlary.ml[321,7992+0]..ocamlary.ml[324,8073+46])
      Tstr_type Rec
      [
        type_declaration partial_gadt/493 (ocamlary.ml[321,7992+0]..ocamlary.ml[324,8073+46])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[320,7949+0]..[320,7949+42])
                Pstr_eval
                expression (ocamlary.ml[320,7949+0]..[320,7949+42])
                  Pexp_constant PConst_string(" This comment is for [partial_gadt]. ",(ocamlary.ml[320,7949+0]..[320,7949+42]),None)
            ]
          ptype_params =
            [
              core_type (ocamlary.ml[321,7992+5]..ocamlary.ml[321,7992+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (ocamlary.ml[322,8015+0]..ocamlary.ml[322,8015+30])
                  AscribeTag/494
                  []
                  Some
                    core_type (ocamlary.ml[322,8015+15]..ocamlary.ml[322,8015+30])
                      Ttyp_constr "partial_gadt/493"
                      [
                        core_type (ocamlary.ml[322,8015+15]..ocamlary.ml[322,8015+17])
                          Ttyp_var a
                      ]
                (ocamlary.ml[323,8046+0]..ocamlary.ml[323,8046+26])
                  OfTag/495
                  [
                    core_type (ocamlary.ml[323,8046+11]..ocamlary.ml[323,8046+26])
                      Ttyp_constr "partial_gadt/493"
                      [
                        core_type (ocamlary.ml[323,8046+11]..ocamlary.ml[323,8046+13])
                          Ttyp_var a
                      ]
                  ]
                  None
                (ocamlary.ml[324,8073+0]..ocamlary.ml[324,8073+46])
                  ExistGadtTag/496
                  attribute "ocaml.doc"
                    [
                      structure_item (ocamlary.ml[325,8120+0]..[325,8120+32])
                        Pstr_eval
                        expression (ocamlary.ml[325,8120+0]..[325,8120+32])
                          Pexp_constant PConst_string(" Wow! It was a mixed GADT! ",(ocamlary.ml[325,8120+0]..[325,8120+32]),None)
                    ]
                  [
                    core_type (ocamlary.ml[324,8073+18]..ocamlary.ml[324,8073+26])
                      Ttyp_arrow
                      Nolabel
                      core_type (ocamlary.ml[324,8073+18]..ocamlary.ml[324,8073+20])
                        Ttyp_var a
                      core_type (ocamlary.ml[324,8073+24]..ocamlary.ml[324,8073+26])
                        Ttyp_var b
                  ]
                  Some
                    core_type (ocamlary.ml[324,8073+31]..ocamlary.ml[324,8073+46])
                      Ttyp_constr "partial_gadt/493"
                      [
                        core_type (ocamlary.ml[324,8073+31]..ocamlary.ml[324,8073+33])
                          Ttyp_var a
                      ]
              ]
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (ocamlary.ml[329,8191+0]..ocamlary.ml[329,8191+20])
      Tstr_type Rec
      [
        type_declaration alias/497 (ocamlary.ml[329,8191+0]..ocamlary.ml[329,8191+20])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[328,8155+0]..[328,8155+35])
                Pstr_eval
                expression (ocamlary.ml[328,8155+0]..[328,8155+35])
                  Pexp_constant PConst_string(" This comment is for [alias]. ",(ocamlary.ml[328,8155+0]..[328,8155+35]),None)
            ]
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[329,8191+13]..ocamlary.ml[329,8191+20])
                Ttyp_constr "variant/482"
                []
      ]
    structure_item (ocamlary.ml[332,8249+0]..ocamlary.ml[332,8249+54])
      Tstr_type Rec
      [
        type_declaration tuple/498 (ocamlary.ml[332,8249+0]..ocamlary.ml[332,8249+54])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[331,8213+0]..[331,8213+35])
                Pstr_eval
                expression (ocamlary.ml[331,8213+0]..[331,8213+35])
                  Pexp_constant PConst_string(" This comment is for [tuple]. ",(ocamlary.ml[331,8213+0]..[331,8213+35]),None)
            ]
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[332,8249+13]..ocamlary.ml[332,8249+54])
                Ttyp_tuple
                [
                  core_type (ocamlary.ml[332,8249+14]..ocamlary.ml[332,8249+27])
                    Ttyp_tuple
                    [
                      core_type (ocamlary.ml[332,8249+14]..ocamlary.ml[332,8249+19])
                        Ttyp_constr "alias/497"
                        []
                      core_type (ocamlary.ml[332,8249+22]..ocamlary.ml[332,8249+27])
                        Ttyp_constr "alias/497"
                        []
                    ]
                  core_type (ocamlary.ml[332,8249+31]..ocamlary.ml[332,8249+36])
                    Ttyp_constr "alias/497"
                    []
                  core_type (ocamlary.ml[332,8249+40]..ocamlary.ml[332,8249+53])
                    Ttyp_tuple
                    [
                      core_type (ocamlary.ml[332,8249+40]..ocamlary.ml[332,8249+45])
                        Ttyp_constr "alias/497"
                        []
                      core_type (ocamlary.ml[332,8249+48]..ocamlary.ml[332,8249+53])
                        Ttyp_constr "alias/497"
                        []
                    ]
                ]
      ]
    structure_item (ocamlary.ml[335,8349+0]..ocamlary.ml[339,8427+24])
      Tstr_type Rec
      [
        type_declaration variant_alias/499 (ocamlary.ml[335,8349+0]..ocamlary.ml[339,8427+24])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[334,8305+0]..[334,8305+43])
                Pstr_eval
                expression (ocamlary.ml[334,8305+0]..[334,8305+43])
                  Pexp_constant PConst_string(" This comment is for [variant_alias]. ",(ocamlary.ml[334,8305+0]..[334,8305+43]),None)
            ]
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (ocamlary.ml[336,8380+0]..ocamlary.ml[336,8380+6])
                  TagA/500
                  []
                  None
                (ocamlary.ml[337,8387+0]..ocamlary.ml[337,8387+16])
                  ConstrB/501
                  [
                    core_type (ocamlary.ml[337,8387+13]..ocamlary.ml[337,8387+16])
                      Ttyp_constr "int/1!"
                      []
                  ]
                  None
                (ocamlary.ml[338,8404+0]..ocamlary.ml[338,8404+22])
                  ConstrC/502
                  [
                    core_type (ocamlary.ml[338,8404+13]..ocamlary.ml[338,8404+16])
                      Ttyp_constr "int/1!"
                      []
                    core_type (ocamlary.ml[338,8404+19]..ocamlary.ml[338,8404+22])
                      Ttyp_constr "int/1!"
                      []
                  ]
                  None
                (ocamlary.ml[339,8427+0]..ocamlary.ml[339,8427+24])
                  ConstrD/503
                  [
                    core_type (ocamlary.ml[339,8427+14]..ocamlary.ml[339,8427+23])
                      Ttyp_tuple
                      [
                        core_type (ocamlary.ml[339,8427+14]..ocamlary.ml[339,8427+17])
                          Ttyp_constr "int/1!"
                          []
                        core_type (ocamlary.ml[339,8427+20]..ocamlary.ml[339,8427+23])
                          Ttyp_constr "int/1!"
                          []
                      ]
                  ]
                  None
              ]
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[335,8349+21]..ocamlary.ml[335,8349+28])
                Ttyp_constr "variant/482"
                []
      ]
    structure_item (ocamlary.ml[342,8496+0]..ocamlary.ml[345,8559+1])
      Tstr_type Rec
      [
        type_declaration record_alias/504 (ocamlary.ml[342,8496+0]..ocamlary.ml[345,8559+1])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[341,8453+0]..[341,8453+42])
                Pstr_eval
                expression (ocamlary.ml[341,8453+0]..[341,8453+42])
                  Pexp_constant PConst_string(" This comment is for [record_alias]. ",(ocamlary.ml[341,8453+0]..[341,8453+42]),None)
            ]
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_record
              [
                (ocamlary.ml[343,8527+2]..ocamlary.ml[343,8527+15])
                  Immutable
                  field1/505                core_type (ocamlary.ml[343,8527+11]..ocamlary.ml[343,8527+14])
                    Ttyp_poly
                    core_type (ocamlary.ml[343,8527+11]..ocamlary.ml[343,8527+14])
                      Ttyp_constr "int/1!"
                      []
                (ocamlary.ml[344,8543+2]..ocamlary.ml[344,8543+15])
                  Immutable
                  field2/506                core_type (ocamlary.ml[344,8543+11]..ocamlary.ml[344,8543+14])
                    Ttyp_poly
                    core_type (ocamlary.ml[344,8543+11]..ocamlary.ml[344,8543+14])
                      Ttyp_constr "int/1!"
                      []
              ]
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[342,8496+20]..ocamlary.ml[342,8496+26])
                Ttyp_constr "record/473"
                []
      ]
    structure_item (ocamlary.ml[348,8611+0]..ocamlary.ml[351,8662+1])
      Tstr_type Rec
      [
        type_declaration poly_variant_union/507 (ocamlary.ml[348,8611+0]..ocamlary.ml[351,8662+1])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[347,8562+0]..[347,8562+48])
                Pstr_eval
                expression (ocamlary.ml[347,8562+0]..[347,8562+48])
                  Pexp_constant PConst_string(" This comment is for [poly_variant_union]. ",(ocamlary.ml[347,8562+0]..[347,8562+48]),None)
            ]
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[348,8611+26]..ocamlary.ml[351,8662+1])
                Ttyp_variant closed=Closed
                [
                  Tinherit
                    core_type (ocamlary.ml[349,8639+2]..ocamlary.ml[349,8639+14])
                      Ttyp_constr "poly_variant/487"
                      []
                  Ttag "TagC" true
                    []
                ]
                None
      ]
    structure_item (ocamlary.ml[353,8665+0]..ocamlary.ml[355,8709+1])
      Tstr_type Rec
      [
        type_declaration poly_poly_variant/508 (ocamlary.ml[353,8665+0]..ocamlary.ml[355,8709+1])
          ptype_params =
            [
              core_type (ocamlary.ml[353,8665+5]..ocamlary.ml[353,8665+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[353,8665+28]..ocamlary.ml[355,8709+1])
                Ttyp_variant closed=Closed
                [
                  Ttag "TagA" false
                    [
                      core_type (ocamlary.ml[354,8695+11]..ocamlary.ml[354,8695+13])
                        Ttyp_var a
                    ]
                ]
                None
      ]
    structure_item (ocamlary.ml[357,8712+0]..ocamlary.ml[360,8782+1])
      Tstr_type Rec
      [
        type_declaration bin_poly_poly_variant/509 (ocamlary.ml[357,8712+0]..ocamlary.ml[360,8782+1])
          ptype_params =
            [
              core_type (ocamlary.ml[357,8712+6]..ocamlary.ml[357,8712+8])
                Ttyp_var a
              core_type (ocamlary.ml[357,8712+9]..ocamlary.ml[357,8712+11])
                Ttyp_var b
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[357,8712+37]..ocamlary.ml[360,8782+1])
                Ttyp_variant closed=Closed
                [
                  Ttag "TagA" false
                    [
                      core_type (ocamlary.ml[358,8751+11]..ocamlary.ml[358,8751+13])
                        Ttyp_var a
                    ]
                  Ttag "ConstrB" false
                    [
                      core_type (ocamlary.ml[359,8765+14]..ocamlary.ml[359,8765+16])
                        Ttyp_var b
                    ]
                ]
                None
      ]
    structure_item (ocamlary.ml[370,8936+0]..ocamlary.ml[370,8936+45])
      Tstr_type Rec
      [
        type_declaration open_poly_variant/510 (ocamlary.ml[370,8936+0]..ocamlary.ml[370,8936+45])
          ptype_params =
            [
              core_type (ocamlary.ml[370,8936+5]..ocamlary.ml[370,8936+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[370,8936+29]..ocamlary.ml[370,8936+45])
                Ttyp_alias "a"
                core_type (ocamlary.ml[370,8936+29]..ocamlary.ml[370,8936+39])
                  Ttyp_variant closed=Open
                  [
                    Ttag "TagA" true
                      []
                  ]
                  None
      ]
    structure_item (ocamlary.ml[372,8983+0]..ocamlary.ml[372,8983+55])
      Tstr_type Rec
      [
        type_declaration open_poly_variant2/511 (ocamlary.ml[372,8983+0]..ocamlary.ml[372,8983+55])
          ptype_params =
            [
              core_type (ocamlary.ml[372,8983+5]..ocamlary.ml[372,8983+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[372,8983+29]..ocamlary.ml[372,8983+55])
                Ttyp_alias "a"
                core_type (ocamlary.ml[372,8983+29]..ocamlary.ml[372,8983+49])
                  Ttyp_variant closed=Open
                  [
                    Ttag "ConstrB" false
                      [
                        core_type (ocamlary.ml[372,8983+44]..ocamlary.ml[372,8983+47])
                          Ttyp_constr "int/1!"
                          []
                      ]
                  ]
                  None
      ]
    structure_item (ocamlary.ml[374,9040+0]..ocamlary.ml[374,9040+73])
      Tstr_type Rec
      [
        type_declaration open_poly_variant_alias/512 (ocamlary.ml[374,9040+0]..ocamlary.ml[374,9040+73])
          ptype_params =
            [
              core_type (ocamlary.ml[374,9040+5]..ocamlary.ml[374,9040+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[374,9040+34]..ocamlary.ml[374,9040+73])
                Ttyp_constr "open_poly_variant2/511"
                [
                  core_type (ocamlary.ml[374,9040+34]..ocamlary.ml[374,9040+54])
                    Ttyp_constr "open_poly_variant/510"
                    [
                      core_type (ocamlary.ml[374,9040+34]..ocamlary.ml[374,9040+36])
                        Ttyp_var a
                    ]
                ]
      ]
    structure_item (ocamlary.ml[376,9115+0]..ocamlary.ml[376,9115+53])
      Tstr_type Rec
      [
        type_declaration poly_fun/513 (ocamlary.ml[376,9115+0]..ocamlary.ml[376,9115+53])
          ptype_params =
            [
              core_type (ocamlary.ml[376,9115+5]..ocamlary.ml[376,9115+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[376,9115+19]..ocamlary.ml[376,9115+53])
                Ttyp_arrow
                Nolabel
                core_type (ocamlary.ml[376,9115+20]..ocamlary.ml[376,9115+46])
                  Ttyp_alias "a"
                  core_type (ocamlary.ml[376,9115+20]..ocamlary.ml[376,9115+40])
                    Ttyp_variant closed=Open
                    [
                      Ttag "ConstrB" false
                        [
                          core_type (ocamlary.ml[376,9115+35]..ocamlary.ml[376,9115+38])
                            Ttyp_constr "int/1!"
                            []
                        ]
                    ]
                    None
                core_type (ocamlary.ml[376,9115+51]..ocamlary.ml[376,9115+53])
                  Ttyp_var a
      ]
    structure_item (ocamlary.ml[378,9170+0]..ocamlary.ml[378,9170+65])
      Tstr_type Rec
      [
        type_declaration poly_fun_constraint/514 (ocamlary.ml[378,9170+0]..ocamlary.ml[378,9170+65])
          ptype_params =
            [
              core_type (ocamlary.ml[378,9170+5]..ocamlary.ml[378,9170+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            [
              <constraint> (ocamlary.ml[378,9170+50]..ocamlary.ml[378,9170+65])
                core_type (ocamlary.ml[378,9170+50]..ocamlary.ml[378,9170+52])
                  Ttyp_var a
                core_type (ocamlary.ml[378,9170+55]..ocamlary.ml[378,9170+65])
                  Ttyp_variant closed=Open
                  [
                    Ttag "TagA" true
                      []
                  ]
                  None
            ]
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[378,9170+30]..ocamlary.ml[378,9170+38])
                Ttyp_arrow
                Nolabel
                core_type (ocamlary.ml[378,9170+30]..ocamlary.ml[378,9170+32])
                  Ttyp_var a
                core_type (ocamlary.ml[378,9170+36]..ocamlary.ml[378,9170+38])
                  Ttyp_var a
      ]
    structure_item (ocamlary.ml[380,9237+0]..ocamlary.ml[380,9237+52])
      Tstr_type Rec
      [
        type_declaration closed_poly_variant/515 (ocamlary.ml[380,9237+0]..ocamlary.ml[380,9237+52])
          ptype_params =
            [
              core_type (ocamlary.ml[380,9237+5]..ocamlary.ml[380,9237+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[380,9237+30]..ocamlary.ml[380,9237+52])
                Ttyp_alias "a"
                core_type (ocamlary.ml[380,9237+30]..ocamlary.ml[380,9237+46])
                  Ttyp_variant closed=Closed
                  [
                    Ttag "One" true
                      []
                    Ttag "Two" true
                      []
                  ]
                  Some
                    []
      ]
    structure_item (ocamlary.ml[382,9291+0]..ocamlary.ml[383,9321+51])
      Tstr_type Rec
      [
        type_declaration clopen_poly_variant/516 (ocamlary.ml[382,9291+0]..ocamlary.ml[383,9321+51])
          ptype_params =
            [
              core_type (ocamlary.ml[382,9291+5]..ocamlary.ml[382,9291+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[383,9321+0]..ocamlary.ml[383,9321+51])
                Ttyp_alias "a"
                core_type (ocamlary.ml[383,9321+0]..ocamlary.ml[383,9321+45])
                  Ttyp_variant closed=Closed
                  [
                    Ttag "One" true
                      []
                    Ttag "Two" false
                      [
                        core_type (ocamlary.ml[383,9321+18]..ocamlary.ml[383,9321+21])
                          Ttyp_constr "int/1!"
                          []
                      ]
                    Ttag "Three" true
                      []
                  ]
                  Some
                    [
                      "Two"
                      "Three"
                    ]
      ]
    structure_item (ocamlary.ml[385,9374+0]..ocamlary.ml[397,9479+1])
      Tstr_type Rec
      [
        type_declaration nested_poly_variant/517 (ocamlary.ml[385,9374+0]..ocamlary.ml[397,9479+1])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[385,9374+27]..ocamlary.ml[397,9479+1])
                Ttyp_variant closed=Closed
                [
                  Ttag "A" true
                    []
                  Ttag "B" false
                    [
                      core_type (ocamlary.ml[387,9408+8]..ocamlary.ml[390,9434+1])
                        Ttyp_variant closed=Closed
                        [
                          Ttag "B1" true
                            []
                          Ttag "B2" true
                            []
                        ]
                        None
                    ]
                  Ttag "C" true
                    []
                  Ttag "D" false
                    [
                      core_type (ocamlary.ml[392,9441+8]..ocamlary.ml[396,9477+1])
                        Ttyp_variant closed=Closed
                        [
                          Ttag "D1" false
                            [
                              core_type (ocamlary.ml[393,9451+11]..ocamlary.ml[395,9473+3])
                                Ttyp_variant closed=Closed
                                [
                                  Ttag "D1a" true
                                    []
                                ]
                                None
                            ]
                        ]
                        None
                    ]
                ]
                None
      ]
    structure_item (ocamlary.ml[400,9528+0]..ocamlary.ml[404,9700+47])
      Tstr_type Rec
      [
        type_declaration full_gadt_alias/518 (ocamlary.ml[400,9528+0]..ocamlary.ml[404,9700+47])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[399,9482+0]..[399,9482+45])
                Pstr_eval
                expression (ocamlary.ml[399,9482+0]..[399,9482+45])
                  Pexp_constant PConst_string(" This comment is for [full_gadt_alias]. ",(ocamlary.ml[399,9482+0]..[399,9482+45]),None)
            ]
          ptype_params =
            [
              core_type (ocamlary.ml[400,9528+6]..ocamlary.ml[400,9528+8])
                Ttyp_var a
              core_type (ocamlary.ml[400,9528+9]..ocamlary.ml[400,9528+11])
                Ttyp_var b
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (ocamlary.ml[401,9579+0]..ocamlary.ml[401,9579+35])
                  Tag/519
                  []
                  Some
                    core_type (ocamlary.ml[401,9579+8]..ocamlary.ml[401,9579+35])
                      Ttyp_constr "full_gadt_alias/518"
                      [
                        core_type (ocamlary.ml[401,9579+9]..ocamlary.ml[401,9579+13])
                          Ttyp_constr "unit/6!"
                          []
                        core_type (ocamlary.ml[401,9579+14]..ocamlary.ml[401,9579+18])
                          Ttyp_constr "unit/6!"
                          []
                      ]
                (ocamlary.ml[402,9615+0]..ocamlary.ml[402,9615+41])
                  First/520
                  [
                    core_type (ocamlary.ml[402,9615+10]..ocamlary.ml[402,9615+12])
                      Ttyp_var a
                  ]
                  Some
                    core_type (ocamlary.ml[402,9615+16]..ocamlary.ml[402,9615+41])
                      Ttyp_constr "full_gadt_alias/518"
                      [
                        core_type (ocamlary.ml[402,9615+17]..ocamlary.ml[402,9615+19])
                          Ttyp_var a
                        core_type (ocamlary.ml[402,9615+20]..ocamlary.ml[402,9615+24])
                          Ttyp_constr "unit/6!"
                          []
                      ]
                (ocamlary.ml[403,9657+0]..ocamlary.ml[403,9657+42])
                  Second/521
                  [
                    core_type (ocamlary.ml[403,9657+11]..ocamlary.ml[403,9657+13])
                      Ttyp_var a
                  ]
                  Some
                    core_type (ocamlary.ml[403,9657+17]..ocamlary.ml[403,9657+42])
                      Ttyp_constr "full_gadt_alias/518"
                      [
                        core_type (ocamlary.ml[403,9657+18]..ocamlary.ml[403,9657+22])
                          Ttyp_constr "unit/6!"
                          []
                        core_type (ocamlary.ml[403,9657+23]..ocamlary.ml[403,9657+25])
                          Ttyp_var a
                      ]
                (ocamlary.ml[404,9700+0]..ocamlary.ml[404,9700+47])
                  Exist/522
                  [
                    core_type (ocamlary.ml[404,9700+10]..ocamlary.ml[404,9700+12])
                      Ttyp_var a
                    core_type (ocamlary.ml[404,9700+15]..ocamlary.ml[404,9700+17])
                      Ttyp_var b
                  ]
                  Some
                    core_type (ocamlary.ml[404,9700+21]..ocamlary.ml[404,9700+47])
                      Ttyp_constr "full_gadt_alias/518"
                      [
                        core_type (ocamlary.ml[404,9700+22]..ocamlary.ml[404,9700+24])
                          Ttyp_var b
                        core_type (ocamlary.ml[404,9700+26]..ocamlary.ml[404,9700+30])
                          Ttyp_constr "unit/6!"
                          []
                      ]
              ]
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[400,9528+31]..ocamlary.ml[400,9528+48])
                Ttyp_constr "full_gadt/488"
                [
                  core_type (ocamlary.ml[400,9528+32]..ocamlary.ml[400,9528+34])
                    Ttyp_var a
                  core_type (ocamlary.ml[400,9528+35]..ocamlary.ml[400,9528+37])
                    Ttyp_var b
                ]
      ]
    structure_item (ocamlary.ml[407,9798+0]..ocamlary.ml[410,9915+52])
      Tstr_type Rec
      [
        type_declaration partial_gadt_alias/523 (ocamlary.ml[407,9798+0]..ocamlary.ml[410,9915+52])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[406,9749+0]..[406,9749+48])
                Pstr_eval
                expression (ocamlary.ml[406,9749+0]..[406,9749+48])
                  Pexp_constant PConst_string(" This comment is for [partial_gadt_alias]. ",(ocamlary.ml[406,9749+0]..[406,9749+48]),None)
            ]
          ptype_params =
            [
              core_type (ocamlary.ml[407,9798+5]..ocamlary.ml[407,9798+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (ocamlary.ml[408,9845+0]..ocamlary.ml[408,9845+36])
                  AscribeTag/524
                  []
                  Some
                    core_type (ocamlary.ml[408,9845+15]..ocamlary.ml[408,9845+36])
                      Ttyp_constr "partial_gadt_alias/523"
                      [
                        core_type (ocamlary.ml[408,9845+15]..ocamlary.ml[408,9845+17])
                          Ttyp_var a
                      ]
                (ocamlary.ml[409,9882+0]..ocamlary.ml[409,9882+32])
                  OfTag/525
                  [
                    core_type (ocamlary.ml[409,9882+11]..ocamlary.ml[409,9882+32])
                      Ttyp_constr "partial_gadt_alias/523"
                      [
                        core_type (ocamlary.ml[409,9882+11]..ocamlary.ml[409,9882+13])
                          Ttyp_var a
                      ]
                  ]
                  None
                (ocamlary.ml[410,9915+0]..ocamlary.ml[410,9915+52])
                  ExistGadtTag/526
                  [
                    core_type (ocamlary.ml[410,9915+18]..ocamlary.ml[410,9915+26])
                      Ttyp_arrow
                      Nolabel
                      core_type (ocamlary.ml[410,9915+18]..ocamlary.ml[410,9915+20])
                        Ttyp_var a
                      core_type (ocamlary.ml[410,9915+24]..ocamlary.ml[410,9915+26])
                        Ttyp_var b
                  ]
                  Some
                    core_type (ocamlary.ml[410,9915+31]..ocamlary.ml[410,9915+52])
                      Ttyp_constr "partial_gadt_alias/523"
                      [
                        core_type (ocamlary.ml[410,9915+31]..ocamlary.ml[410,9915+33])
                          Ttyp_var a
                      ]
              ]
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[407,9798+29]..ocamlary.ml[407,9798+44])
                Ttyp_constr "partial_gadt/493"
                [
                  core_type (ocamlary.ml[407,9798+29]..ocamlary.ml[407,9798+31])
                    Ttyp_var a
                ]
      ]
    structure_item (ocamlary.ml[413,10010+0]..ocamlary.ml[413,10010+33])
      Tstr_exception
      type_exception
        ptyext_constructor =
          extension_constructor (ocamlary.ml[413,10010+0]..ocamlary.ml[413,10010+33])
            attribute "ocaml.doc"
              [
                structure_item (ocamlary.ml[412,9969+0]..[412,9969+40])
                  Pstr_eval
                  expression (ocamlary.ml[412,9969+0]..[412,9969+40])
                    Pexp_constant PConst_string(" This comment is for {!exn_arrow}. ",(ocamlary.ml[412,9969+0]..[412,9969+40]),None)
              ]
            pext_name = "Exn_arrow/527"
            pext_kind =
              Text_decl
                [
                  core_type (ocamlary.ml[413,10010+22]..ocamlary.ml[413,10010+26])
                    Ttyp_constr "unit/6!"
                    []
                ]
                Some
                  core_type (ocamlary.ml[413,10010+30]..ocamlary.ml[413,10010+33])
                    Ttyp_constr "exn/7!"
                    []
    structure_item (ocamlary.ml[416,10115+0]..ocamlary.ml[421,10195+26])
      Tstr_type Rec
      [
        type_declaration mutual_constr_a/528 (ocamlary.ml[416,10115+0]..ocamlary.ml[418,10142+26])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[415,10045+0]..[415,10045+69])
                Pstr_eval
                expression (ocamlary.ml[415,10045+0]..[415,10045+69])
                  Pexp_constant PConst_string(" This comment is for {!mutual_constr_a} and {!mutual_constr_b}. ",(ocamlary.ml[415,10045+0]..[415,10045+69]),None)
            ]
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (ocamlary.ml[417,10138+0]..ocamlary.ml[417,10138+3])
                  A/530
                  []
                  None
                (ocamlary.ml[418,10142+0]..ocamlary.ml[418,10142+26])
                  B_ish/531
                  [
                    core_type (ocamlary.ml[418,10142+11]..ocamlary.ml[418,10142+26])
                      Ttyp_constr "mutual_constr_b/529"
                      []
                  ]
                  None
              ]
          ptype_private = Public
          ptype_manifest =
            None
        type_declaration mutual_constr_b/529 (ocamlary.ml[419,10169+0]..ocamlary.ml[421,10195+26])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (ocamlary.ml[420,10191+0]..ocamlary.ml[420,10191+3])
                  B/532
                  []
                  None
                (ocamlary.ml[421,10195+0]..ocamlary.ml[421,10195+26])
                  A_ish/533
                  [
                    core_type (ocamlary.ml[421,10195+11]..ocamlary.ml[421,10195+26])
                      Ttyp_constr "mutual_constr_a/528"
                      []
                  ]
                  None
              ]
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (ocamlary.ml[423,10223+0]..ocamlary.ml[423,10223+57])
      Tstr_type Rec
      [
        type_declaration rec_obj/534 (ocamlary.ml[423,10223+0]..ocamlary.ml[423,10223+57])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[423,10223+15]..ocamlary.ml[423,10223+57])
                Ttyp_object Closed
                  method f
                    core_type (ocamlary.ml[423,10223+21]..ocamlary.ml[423,10223+24])
                      Ttyp_poly
                      core_type (ocamlary.ml[423,10223+21]..ocamlary.ml[423,10223+24])
                        Ttyp_constr "int/1!"
                        []
                  method g
                    core_type (ocamlary.ml[423,10223+30]..ocamlary.ml[423,10223+42])
                      Ttyp_poly
                      core_type (ocamlary.ml[423,10223+30]..ocamlary.ml[423,10223+42])
                        Ttyp_arrow
                        Nolabel
                        core_type (ocamlary.ml[423,10223+30]..ocamlary.ml[423,10223+34])
                          Ttyp_constr "unit/6!"
                          []
                        core_type (ocamlary.ml[423,10223+38]..ocamlary.ml[423,10223+42])
                          Ttyp_constr "unit/6!"
                          []
                  method h
                    core_type (ocamlary.ml[423,10223+48]..ocamlary.ml[423,10223+55])
                      Ttyp_poly
                      core_type (ocamlary.ml[423,10223+48]..ocamlary.ml[423,10223+55])
                        Ttyp_constr "rec_obj/534"
                        []
      ]
    structure_item (ocamlary.ml[425,10282+0]..ocamlary.ml[425,10282+58])
      Tstr_type Rec
      [
        type_declaration open_obj/535 (ocamlary.ml[425,10282+0]..ocamlary.ml[425,10282+58])
          ptype_params =
            [
              core_type (ocamlary.ml[425,10282+5]..ocamlary.ml[425,10282+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[425,10282+19]..ocamlary.ml[425,10282+58])
                Ttyp_alias "a"
                core_type (ocamlary.ml[425,10282+19]..ocamlary.ml[425,10282+52])
                  Ttyp_object Open
                    method f
                      core_type (ocamlary.ml[425,10282+25]..ocamlary.ml[425,10282+28])
                        Ttyp_poly
                        core_type (ocamlary.ml[425,10282+25]..ocamlary.ml[425,10282+28])
                          Ttyp_constr "int/1!"
                          []
                    method g
                      core_type (ocamlary.ml[425,10282+34]..ocamlary.ml[425,10282+46])
                        Ttyp_poly
                        core_type (ocamlary.ml[425,10282+34]..ocamlary.ml[425,10282+46])
                          Ttyp_arrow
                          Nolabel
                          core_type (ocamlary.ml[425,10282+34]..ocamlary.ml[425,10282+38])
                            Ttyp_constr "unit/6!"
                            []
                          core_type (ocamlary.ml[425,10282+42]..ocamlary.ml[425,10282+46])
                            Ttyp_constr "unit/6!"
                            []
      ]
    structure_item (ocamlary.ml[427,10342+0]..ocamlary.ml[427,10342+44])
      Tstr_type Rec
      [
        type_declaration oof/536 (ocamlary.ml[427,10342+0]..ocamlary.ml[427,10342+44])
          ptype_params =
            [
              core_type (ocamlary.ml[427,10342+5]..ocamlary.ml[427,10342+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[427,10342+14]..ocamlary.ml[427,10342+44])
                Ttyp_arrow
                Nolabel
                core_type (ocamlary.ml[427,10342+15]..ocamlary.ml[427,10342+37])
                  Ttyp_alias "a"
                  core_type (ocamlary.ml[427,10342+15]..ocamlary.ml[427,10342+31])
                    Ttyp_object Open
                      method a
                        core_type (ocamlary.ml[427,10342+21]..ocamlary.ml[427,10342+25])
                          Ttyp_poly
                          core_type (ocamlary.ml[427,10342+21]..ocamlary.ml[427,10342+25])
                            Ttyp_constr "unit/6!"
                            []
                core_type (ocamlary.ml[427,10342+42]..ocamlary.ml[427,10342+44])
                  Ttyp_var a
      ]
    structure_item (ocamlary.ml[429,10388+0]..ocamlary.ml[429,10388+30])
      Tstr_type Rec
      [
        type_declaration any_obj/537 (ocamlary.ml[429,10388+0]..ocamlary.ml[429,10388+30])
          ptype_params =
            [
              core_type (ocamlary.ml[429,10388+5]..ocamlary.ml[429,10388+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[429,10388+18]..ocamlary.ml[429,10388+30])
                Ttyp_alias "a"
                core_type (ocamlary.ml[429,10388+18]..ocamlary.ml[429,10388+24])
                  Ttyp_object Open
      ]
    structure_item (ocamlary.ml[431,10420+0]..ocamlary.ml[431,10420+20])
      Tstr_type Rec
      [
        type_declaration empty_obj/538 (ocamlary.ml[431,10420+0]..ocamlary.ml[431,10420+20])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[431,10420+17]..ocamlary.ml[431,10420+20])
                Ttyp_object Closed
      ]
    structure_item (ocamlary.ml[433,10442+0]..ocamlary.ml[433,10442+30])
      Tstr_type Rec
      [
        type_declaration one_meth/539 (ocamlary.ml[433,10442+0]..ocamlary.ml[433,10442+30])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[433,10442+16]..ocamlary.ml[433,10442+30])
                Ttyp_object Closed
                  method meth
                    core_type (ocamlary.ml[433,10442+24]..ocamlary.ml[433,10442+28])
                      Ttyp_poly
                      core_type (ocamlary.ml[433,10442+24]..ocamlary.ml[433,10442+28])
                        Ttyp_constr "unit/6!"
                        []
      ]
    structure_item (ocamlary.ml[436,10514+0]..ocamlary.ml[436,10514+13])
      Tstr_type Rec
      [
        type_declaration ext/540 (ocamlary.ml[436,10514+0]..ocamlary.ml[436,10514+13])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[435,10474+0]..[435,10474+39])
                Pstr_eval
                expression (ocamlary.ml[435,10474+0]..[435,10474+39])
                  Pexp_constant PConst_string(" A mystery wrapped in an ellipsis ",(ocamlary.ml[435,10474+0]..[435,10474+39]),None)
            ]
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_open
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (ocamlary.ml[438,10529+0]..ocamlary.ml[438,10529+16])
      Tstr_typext
      type_extension
        ptyext_path = "ext/540"
        ptyext_params =
          []
        ptyext_constructors =
          [
            extension_constructor (ocamlary.ml[438,10529+12]..ocamlary.ml[438,10529+16])
              pext_name = "ExtA/541"
              pext_kind =
                Text_decl
                  []
                  None
          ]
        ptyext_private = Public
    structure_item (ocamlary.ml[439,10546+0]..ocamlary.ml[439,10546+16])
      Tstr_typext
      type_extension
        ptyext_path = "ext/540"
        ptyext_params =
          []
        ptyext_constructors =
          [
            extension_constructor (ocamlary.ml[439,10546+12]..ocamlary.ml[439,10546+16])
              pext_name = "ExtB/542"
              pext_kind =
                Text_decl
                  []
                  None
          ]
        ptyext_private = Public
    structure_item (ocamlary.ml[440,10563+0]..ocamlary.ml[442,10590+13])
      Tstr_typext
      type_extension
        ptyext_path = "ext/540"
        ptyext_params =
          []
        ptyext_constructors =
          [
            extension_constructor (ocamlary.ml[441,10575+0]..ocamlary.ml[441,10575+14])
              pext_name = "ExtC/543"
              pext_kind =
                Text_decl
                  [
                    core_type (ocamlary.ml[441,10575+10]..ocamlary.ml[441,10575+14])
                      Ttyp_constr "unit/6!"
                      []
                  ]
                  None
            extension_constructor (ocamlary.ml[442,10590+0]..ocamlary.ml[442,10590+13])
              pext_name = "ExtD/544"
              pext_kind =
                Text_decl
                  [
                    core_type (ocamlary.ml[442,10590+10]..ocamlary.ml[442,10590+13])
                      Ttyp_constr "ext/540"
                      []
                  ]
                  None
          ]
        ptyext_private = Public
    structure_item (ocamlary.ml[443,10604+0]..ocamlary.ml[443,10604+16])
      Tstr_typext
      type_extension
        ptyext_path = "ext/540"
        ptyext_params =
          []
        ptyext_constructors =
          [
            extension_constructor (ocamlary.ml[443,10604+12]..ocamlary.ml[443,10604+16])
              pext_name = "ExtE/545"
              pext_kind =
                Text_decl
                  []
                  None
          ]
        ptyext_private = Public
    structure_item (ocamlary.ml[445,10622+0]..ocamlary.ml[445,10622+24])
      Tstr_typext
      type_extension
        ptyext_path = "ext/540"
        ptyext_params =
          []
        ptyext_constructors =
          [
            extension_constructor (ocamlary.ml[445,10622+20]..ocamlary.ml[445,10622+24])
              pext_name = "ExtF/546"
              pext_kind =
                Text_decl
                  []
                  None
          ]
        ptyext_private = Private
    structure_item (ocamlary.ml[447,10648+0]..ocamlary.ml[447,10648+21])
      Tstr_type Rec
      [
        type_declaration poly_ext/547 (ocamlary.ml[447,10648+0]..ocamlary.ml[447,10648+21])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[448,10670+0]..[448,10670+18])
                Pstr_eval
                expression (ocamlary.ml[448,10670+0]..[448,10670+18])
                  Pexp_constant PConst_string(" 'a poly_ext ",(ocamlary.ml[448,10670+0]..[448,10670+18]),None)
            ]
          ptype_params =
            [
              core_type (ocamlary.ml[447,10648+5]..ocamlary.ml[447,10648+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_open
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (ocamlary.ml[450,10690+0]..ocamlary.ml[450,10690+46])
      Tstr_typext
      type_extension
        ptyext_path = "poly_ext/547"
        ptyext_params =
          [
            core_type (ocamlary.ml[450,10690+5]..ocamlary.ml[450,10690+7])
              Ttyp_var b
          ]
        ptyext_constructors =
          [
            extension_constructor (ocamlary.ml[450,10690+20]..ocamlary.ml[450,10690+29])
              pext_name = "Foo/548"
              pext_kind =
                Text_decl
                  [
                    core_type (ocamlary.ml[450,10690+27]..ocamlary.ml[450,10690+29])
                      Ttyp_var b
                  ]
                  None
            extension_constructor (ocamlary.ml[450,10690+30]..ocamlary.ml[450,10690+46])
              attribute "ocaml.doc"
                [
                  structure_item (ocamlary.ml[451,10737+0]..[451,10737+18])
                    Pstr_eval
                    expression (ocamlary.ml[451,10737+0]..[451,10737+18])
                      Pexp_constant PConst_string(" 'b poly_ext ",(ocamlary.ml[451,10737+0]..[451,10737+18]),None)
                ]
              pext_name = "Bar/549"
              pext_kind =
                Text_decl
                  [
                    core_type (ocamlary.ml[450,10690+39]..ocamlary.ml[450,10690+41])
                      Ttyp_var b
                    core_type (ocamlary.ml[450,10690+44]..ocamlary.ml[450,10690+46])
                      Ttyp_var b
                  ]
                  None
          ]
        ptyext_private = Public
    structure_item (ocamlary.ml[453,10757+0]..ocamlary.ml[453,10757+30])
      Tstr_typext
      type_extension
        ptyext_path = "poly_ext/547"
        ptyext_params =
          [
            core_type (ocamlary.ml[453,10757+5]..ocamlary.ml[453,10757+7])
              Ttyp_var c
          ]
        ptyext_constructors =
          [
            extension_constructor (ocamlary.ml[453,10757+20]..ocamlary.ml[453,10757+30])
              pext_name = "Quux/550"
              pext_kind =
                Text_decl
                  [
                    core_type (ocamlary.ml[453,10757+28]..ocamlary.ml[453,10757+30])
                      Ttyp_var c
                  ]
                  None
          ]
        ptyext_private = Public
    structure_item (ocamlary.ml[455,10789+0]..ocamlary.ml[459,10852+3])
      Tstr_module
      ExtMod/553
        module_expr (ocamlary.ml[455,10789+16]..ocamlary.ml[459,10852+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[456,10812+2]..ocamlary.ml[456,10812+13])
              Tstr_type Rec
              [
                type_declaration t/551 (ocamlary.ml[456,10812+2]..ocamlary.ml[456,10812+13])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_open
                  ptype_private = Public
                  ptype_manifest =
                    None
              ]
            structure_item (ocamlary.ml[458,10827+2]..ocamlary.ml[458,10827+24])
              Tstr_typext
              type_extension
                ptyext_path = "t/551"
                ptyext_params =
                  []
                ptyext_constructors =
                  [
                    extension_constructor (ocamlary.ml[458,10827+12]..ocamlary.ml[458,10827+24])
                      pext_name = "Leisureforce/552"
                      pext_kind =
                        Text_decl
                          []
                          None
                  ]
                ptyext_private = Public
          ]
    structure_item (ocamlary.ml[461,10857+0]..ocamlary.ml[461,10857+24])
      Tstr_typext
      type_extension
        ptyext_path = "ExtMod/553.t"
        ptyext_params =
          []
        ptyext_constructors =
          [
            extension_constructor (ocamlary.ml[461,10857+17]..ocamlary.ml[461,10857+24])
              attribute "ocaml.doc"
                [
                  structure_item (ocamlary.ml[462,10882+0]..[462,10882+24])
                    Pstr_eval
                    expression (ocamlary.ml[462,10882+0]..[462,10882+24])
                      Pexp_constant PConst_string(" It's got the rock ",(ocamlary.ml[462,10882+0]..[462,10882+24]),None)
                ]
              pext_name = "ZzzTop0/554"
              pext_kind =
                Text_decl
                  []
                  None
          ]
        ptyext_private = Public
    structure_item (ocamlary.ml[464,10908+0]..ocamlary.ml[464,10908+31])
      Tstr_typext
      type_extension
        ptyext_path = "ExtMod/553.t"
        ptyext_params =
          []
        ptyext_constructors =
          [
            extension_constructor (ocamlary.ml[464,10908+17]..ocamlary.ml[464,10908+31])
              attribute "ocaml.doc"
                [
                  structure_item (ocamlary.ml[465,10940+0]..[465,10940+27])
                    Pstr_eval
                    expression (ocamlary.ml[465,10940+0]..[465,10940+27])
                      Pexp_constant PConst_string(" and it packs a unit. ",(ocamlary.ml[465,10940+0]..[465,10940+27]),None)
                ]
              pext_name = "ZzzTop/555"
              pext_kind =
                Text_decl
                  [
                    core_type (ocamlary.ml[464,10908+27]..ocamlary.ml[464,10908+31])
                      Ttyp_constr "unit/6!"
                      []
                  ]
                  None
          ]
        ptyext_private = Public
    structure_item (ocamlary.ml[468,11002+0]..ocamlary.ml[468,11002+50])
      Tstr_primitive
      value_description launch_missiles/556 (ocamlary.ml[468,11002+0]..ocamlary.ml[468,11002+50])
        attribute "ocaml.doc"
          [
            structure_item (ocamlary.ml[467,10969+0]..[467,10969+32])
              Pstr_eval
              expression (ocamlary.ml[467,10969+0]..[467,10969+32])
                Pexp_constant PConst_string(" Rotate keys on my mark... ",(ocamlary.ml[467,10969+0]..[467,10969+32]),None)
          ]
        core_type (ocamlary.ml[468,11002+27]..ocamlary.ml[468,11002+39])
          Ttyp_arrow
          Nolabel
          core_type (ocamlary.ml[468,11002+27]..ocamlary.ml[468,11002+31])
            Ttyp_constr "unit/6!"
            []
          core_type (ocamlary.ml[468,11002+35]..ocamlary.ml[468,11002+39])
            Ttyp_constr "unit/6!"
            []
        [
          "tetris"
        ]
    structure_item (ocamlary.ml[471,11102+0]..ocamlary.ml[471,11102+33])
      Tstr_type Rec
      [
        type_declaration my_mod/557 (ocamlary.ml[471,11102+0]..ocamlary.ml[471,11102+33])
          attribute "ocaml.doc"
            [
              structure_item (ocamlary.ml[470,11054+0]..[470,11054+47])
                Pstr_eval
                expression (ocamlary.ml[470,11054+0]..[470,11054+47])
                  Pexp_constant PConst_string(" A brown paper package tied up with string",(ocamlary.ml[470,11054+0]..[470,11054+47]),None)
            ]
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[471,11102+14]..ocamlary.ml[471,11102+33])
                Ttyp_package "COLLECTION/351"
                []
      ]
    structure_item (ocamlary.ml[473,11137+0]..ocamlary.ml[473,11137+40])
      Tstr_class
      [
        class_declaration (ocamlary.ml[473,11137+0]..ocamlary.ml[473,11137+40])
          pci_virt = Concrete
          pci_params =
            []
          pci_name = "empty_class"
          pci_expr =
            class_expr (ocamlary.ml[473,11137+20]..ocamlary.ml[473,11137+40])
              Tcl_structure
              class_structure
                pattern (_none_[0,0+-1].._none_[0,0+-1]) ghost
                  Tpat_alias "selfpat-*/562"
                  pattern (ocamlary.ml[473,11137+26]..ocamlary.ml[473,11137+26]) ghost
                    Tpat_any
                [
                  class_field (ocamlary.ml[473,11137+27]..ocamlary.ml[473,11137+36])
                    Tcf_val "x" Immutable
                      Concrete Fresh
                      expression (ocamlary.ml[473,11137+35]..ocamlary.ml[473,11137+36])
                        Texp_constant Const_int 0
                ]
      ]
    structure_item (ocamlary.ml[475,11179+0]..ocamlary.ml[477,11228+3])
      Tstr_class
      [
        class_declaration (ocamlary.ml[475,11179+0]..ocamlary.ml[477,11228+3])
          pci_virt = Concrete
          pci_params =
            []
          pci_name = "one_method_class"
          pci_expr =
            class_expr (ocamlary.ml[475,11179+25]..ocamlary.ml[477,11228+3])
              Tcl_structure
              class_structure
                pattern (_none_[0,0+-1].._none_[0,0+-1]) ghost
                  Tpat_alias "selfpat-*/572"
                  pattern (ocamlary.ml[475,11179+31]..ocamlary.ml[475,11179+31]) ghost
                    Tpat_any
                [
                  class_field (ocamlary.ml[476,11211+2]..ocamlary.ml[476,11211+16])
                    Tcf_method "go" Public
                      Concrete Fresh
                      expression (ocamlary.ml[476,11211+14]..ocamlary.ml[476,11211+16]) ghost
                        Texp_function
                        Nolabel
                        [
                          <case>
                            pattern (ocamlary.ml[475,11179+31]..ocamlary.ml[475,11179+31]) ghost
                              Tpat_alias "self-2/577"
                              pattern (ocamlary.ml[475,11179+31]..ocamlary.ml[475,11179+31]) ghost
                                Tpat_var "self-*/576"
                            expression (ocamlary.ml[476,11211+14]..ocamlary.ml[476,11211+16])
                              extra
                                Texp_poly
                                None
                              Texp_construct "()"
                              []
                        ]
                ]
      ]
    structure_item (ocamlary.ml[479,11233+0]..ocamlary.ml[482,11320+3])
      Tstr_class
      [
        class_declaration (ocamlary.ml[479,11233+0]..ocamlary.ml[482,11320+3])
          pci_virt = Concrete
          pci_params =
            []
          pci_name = "two_method_class"
          pci_expr =
            class_expr (ocamlary.ml[479,11233+25]..ocamlary.ml[482,11320+3])
              Tcl_structure
              class_structure
                pattern (_none_[0,0+-1].._none_[0,0+-1]) ghost
                  Tpat_alias "selfpat-*/582"
                  pattern (ocamlary.ml[479,11233+31]..ocamlary.ml[479,11233+31]) ghost
                    Tpat_any
                [
                  class_field (ocamlary.ml[480,11265+2]..ocamlary.ml[480,11265+35])
                    Tcf_method "one" Public
                      Concrete Fresh
                      expression (ocamlary.ml[480,11265+15]..ocamlary.ml[480,11265+35]) ghost
                        Texp_function
                        Nolabel
                        [
                          <case>
                            pattern (ocamlary.ml[479,11233+31]..ocamlary.ml[479,11233+31]) ghost
                              Tpat_alias "self-3/588"
                              pattern (ocamlary.ml[479,11233+31]..ocamlary.ml[479,11233+31]) ghost
                                Tpat_var "self-*/587"
                            expression (ocamlary.ml[480,11265+15]..ocamlary.ml[480,11265+35])
                              extra
                                Texp_poly
                                None
                              Texp_new "one_method_class/571"
                        ]
                  class_field (ocamlary.ml[481,11301+2]..ocamlary.ml[481,11301+18])
                    Tcf_method "undo" Public
                      Concrete Fresh
                      expression (ocamlary.ml[481,11301+16]..ocamlary.ml[481,11301+18]) ghost
                        Texp_function
                        Nolabel
                        [
                          <case>
                            pattern (ocamlary.ml[479,11233+31]..ocamlary.ml[479,11233+31]) ghost
                              Tpat_alias "self-3/590"
                              pattern (ocamlary.ml[479,11233+31]..ocamlary.ml[479,11233+31]) ghost
                                Tpat_var "self-*/589"
                            expression (ocamlary.ml[481,11301+16]..ocamlary.ml[481,11301+18])
                              extra
                                Texp_poly
                                None
                              Texp_construct "()"
                              []
                        ]
                ]
      ]
    structure_item (ocamlary.ml[484,11325+0]..ocamlary.ml[486,11379+3])
      Tstr_class
      [
        class_declaration (ocamlary.ml[484,11325+0]..ocamlary.ml[486,11379+3])
          pci_virt = Concrete
          pci_params =
            [
              core_type (ocamlary.ml[484,11325+7]..ocamlary.ml[484,11325+9])
                Ttyp_var a
            ]
          pci_name = "param_class"
          pci_expr =
            class_expr (ocamlary.ml[484,11325+23]..ocamlary.ml[486,11379+3])
              Tcl_fun
              Nolabel
              pattern (ocamlary.ml[484,11325+23]..ocamlary.ml[484,11325+24])
                Tpat_var "x/595"
              class_expr (ocamlary.ml[484,11325+27]..ocamlary.ml[486,11379+3])
                Tcl_structure
                class_structure
                  pattern (_none_[0,0+-1].._none_[0,0+-1]) ghost
                    Tpat_alias "selfpat-*/597"
                    pattern (ocamlary.ml[484,11325+33]..ocamlary.ml[484,11325+33]) ghost
                      Tpat_any
                  [
                    class_field (ocamlary.ml[485,11359+2]..ocamlary.ml[485,11359+19])
                      Tcf_method "v" Public
                        Concrete Fresh
                        expression (ocamlary.ml[485,11359+13]..ocamlary.ml[485,11359+19]) ghost
                          Texp_function
                          Nolabel
                          [
                            <case>
                              pattern (ocamlary.ml[484,11325+33]..ocamlary.ml[484,11325+33]) ghost
                                Tpat_alias "self-4/602"
                                pattern (ocamlary.ml[484,11325+33]..ocamlary.ml[484,11325+33]) ghost
                                  Tpat_var "self-*/601"
                              expression (ocamlary.ml[485,11359+18]..ocamlary.ml[485,11359+19])
                                extra
                                  Texp_poly
                                  Some
                                    core_type (ocamlary.ml[485,11359+13]..ocamlary.ml[485,11359+15])
                                      Ttyp_poly
                                      core_type (ocamlary.ml[485,11359+13]..ocamlary.ml[485,11359+15])
                                        Ttyp_var a
                                Texp_instvar "x/596"
                          ]
                  ]
      ]
    structure_item (ocamlary.ml[489,11385+0]..ocamlary.ml[489,11385+38])
      Tstr_type Rec
      [
        type_declaration my_unit_object/603 (ocamlary.ml[489,11385+0]..ocamlary.ml[489,11385+38])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[489,11385+22]..ocamlary.ml[489,11385+38])
                Ttyp_constr "param_class/592"
                [
                  core_type (ocamlary.ml[489,11385+22]..ocamlary.ml[489,11385+26])
                    Ttyp_constr "unit/6!"
                    []
                ]
      ]
    structure_item (ocamlary.ml[491,11425+0]..ocamlary.ml[491,11425+47])
      Tstr_type Rec
      [
        type_declaration my_unit_class/604 (ocamlary.ml[491,11425+0]..ocamlary.ml[491,11425+47])
          ptype_params =
            [
              core_type (ocamlary.ml[491,11425+5]..ocamlary.ml[491,11425+7])
                Ttyp_var a
            ]
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[491,11425+24]..ocamlary.ml[491,11425+47])
                Ttyp_alias "a"
                core_type (ocamlary.ml[491,11425+24]..ocamlary.ml[491,11425+41])
                  Ttyp_class "#param_class/591"
                  [
                    core_type (ocamlary.ml[491,11425+24]..ocamlary.ml[491,11425+28])
                      Ttyp_constr "unit/6!"
                      []
                  ]
      ]
    structure_item (ocamlary.ml[494,11525+0]..ocamlary.ml[510,11737+3])
      Tstr_module
      Dep1/622
        module_expr (ocamlary.ml[494,11525+14]..ocamlary.ml[510,11737+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[496,11547+2]..ocamlary.ml[500,11619+5])
              Tstr_modtype "S/609"
                module_type (ocamlary.ml[496,11547+18]..ocamlary.ml[500,11619+5])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[497,11569+4]..ocamlary.ml[499,11611+7])
                      Tsig_class
                      [
                        class_description (ocamlary.ml[497,11569+4]..ocamlary.ml[499,11611+7])
                          pci_virt = Concrete
                          pci_params =
                            []
                          pci_name = "c"
                          pci_expr =
                            class_type (ocamlary.ml[497,11569+14]..ocamlary.ml[499,11611+7])
                              Tcty_signature
                              class_signature
                                core_type (ocamlary.ml[497,11569+20]..ocamlary.ml[497,11569+20])
                                  Ttyp_any
                                [
                                  class_type_field (ocamlary.ml[498,11590+6]..ocamlary.ml[498,11590+20])
                                    Tctf_method "m" Public Concrete
                                      core_type (ocamlary.ml[498,11590+6]..ocamlary.ml[498,11590+20])
                                        Ttyp_poly
                                        core_type (ocamlary.ml[498,11590+17]..ocamlary.ml[498,11590+20])
                                          Ttyp_constr "int/1!"
                                          []
                                ]
                      ]
                  ]
            structure_item (ocamlary.ml[502,11626+2]..ocamlary.ml[508,11730+5])
              Tstr_module
              X/621
                module_expr (ocamlary.ml[502,11626+13]..ocamlary.ml[508,11730+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[503,11646+4]..ocamlary.ml[507,11722+7])
                      Tstr_module
                      Y/620
                        module_expr (ocamlary.ml[503,11646+15]..ocamlary.ml[507,11722+7])
                          Tmod_structure
                          [
                            structure_item (ocamlary.ml[504,11668+6]..ocamlary.ml[506,11712+9])
                              Tstr_class
                              [
                                class_declaration (ocamlary.ml[504,11668+6]..ocamlary.ml[506,11712+9])
                                  pci_virt = Concrete
                                  pci_params =
                                    []
                                  pci_name = "c"
                                  pci_expr =
                                    class_expr (ocamlary.ml[504,11668+16]..ocamlary.ml[506,11712+9])
                                      Tcl_structure
                                      class_structure
                                        pattern (_none_[0,0+-1].._none_[0,0+-1]) ghost
                                          Tpat_alias "selfpat-*/614"
                                          pattern (ocamlary.ml[504,11668+22]..ocamlary.ml[504,11668+22]) ghost
                                            Tpat_any
                                        [
                                          class_field (ocamlary.ml[505,11691+8]..ocamlary.ml[505,11691+20])
                                            Tcf_method "m" Public
                                              Concrete Fresh
                                              expression (ocamlary.ml[505,11691+19]..ocamlary.ml[505,11691+20]) ghost
                                                Texp_function
                                                Nolabel
                                                [
                                                  <case>
                                                    pattern (ocamlary.ml[504,11668+22]..ocamlary.ml[504,11668+22]) ghost
                                                      Tpat_alias "self-5/619"
                                                      pattern (ocamlary.ml[504,11668+22]..ocamlary.ml[504,11668+22]) ghost
                                                        Tpat_var "self-*/618"
                                                    expression (ocamlary.ml[505,11691+19]..ocamlary.ml[505,11691+20])
                                                      extra
                                                        Texp_poly
                                                        None
                                                      Texp_constant Const_int 4
                                                ]
                                        ]
                              ]
                          ]
                  ]
          ]
    structure_item (ocamlary.ml[512,11742+0]..ocamlary.ml[516,11873+7])
      Tstr_module
      Dep2/630
        module_expr (ocamlary.ml[512,11742+12]..ocamlary.ml[516,11873+7])
          Tmod_functor "Arg/626"
          module_type (ocamlary.ml[512,11742+19]..ocamlary.ml[512,11742+72])
            Tmty_signature
            [
              signature_item (ocamlary.ml[512,11742+23]..ocamlary.ml[512,11742+36])
                Tsig_modtype "S/623"
                #abstract            signature_item (ocamlary.ml[512,11742+37]..ocamlary.ml[512,11742+68])
                Tsig_module "X/625"
                module_type (ocamlary.ml[512,11742+48]..ocamlary.ml[512,11742+68])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[512,11742+52]..ocamlary.ml[512,11742+64])
                      Tsig_module "Y/624"
                      module_type (ocamlary.ml[512,11742+63]..ocamlary.ml[512,11742+64])
                        Tmty_ident "S/623"
                  ]
            ]
          module_expr (ocamlary.ml[513,11818+4]..ocamlary.ml[516,11873+7])
            Tmod_structure
            [
              structure_item (ocamlary.ml[514,11829+6]..ocamlary.ml[514,11829+22])
                Tstr_module
                A/628
                  module_expr (ocamlary.ml[514,11829+17]..ocamlary.ml[514,11829+22])
                    Tmod_ident "Arg/626.X"
              structure_item (ocamlary.ml[515,11852+6]..ocamlary.ml[515,11852+20])
                Tstr_module
                B/629
                  module_expr (ocamlary.ml[515,11852+17]..ocamlary.ml[515,11852+20])
                    Tmod_ident "A/628.Y"
            ]
    structure_item (ocamlary.ml[518,11882+0]..ocamlary.ml[518,11882+26])
      Tstr_type Rec
      [
        type_declaration dep1/631 (ocamlary.ml[518,11882+0]..ocamlary.ml[518,11882+26])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[518,11882+12]..ocamlary.ml[518,11882+26])
                Ttyp_constr "Dep2/630(Dep1/622).B.c"
                []
      ]
    structure_item (ocamlary.ml[520,11912+0]..ocamlary.ml[520,11912+31])
      Tstr_module
      Dep3/655
        module_expr (ocamlary.ml[520,11912+14]..ocamlary.ml[520,11912+31])
          Tmod_structure
          [
            structure_item (ocamlary.ml[520,11912+21]..ocamlary.ml[520,11912+27])
              Tstr_type Rec
              [
                type_declaration a/654 (ocamlary.ml[520,11912+21]..ocamlary.ml[520,11912+27])
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
          ]
    structure_item (ocamlary.ml[522,11945+0]..ocamlary.ml[529,12098+3])
      Tstr_module
      Dep4/663
        module_expr (ocamlary.ml[522,11945+14]..ocamlary.ml[529,12098+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[523,11966+2]..ocamlary.ml[523,11966+32])
              Tstr_modtype "T/657"
                module_type (ocamlary.ml[523,11966+18]..ocamlary.ml[523,11966+32])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[523,11966+22]..ocamlary.ml[523,11966+28])
                      Tsig_type Rec
                      [
                        type_declaration b/656 (ocamlary.ml[523,11966+22]..ocamlary.ml[523,11966+28])
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
                  ]
            structure_item (ocamlary.ml[524,11999+2]..ocamlary.ml[527,12061+5])
              Tstr_modtype "S/660"
                module_type (ocamlary.ml[524,11999+18]..ocamlary.ml[527,12061+5])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[525,12021+4]..ocamlary.ml[525,12021+16])
                      Tsig_module "X/658"
                      module_type (ocamlary.ml[525,12021+15]..ocamlary.ml[525,12021+16])
                        Tmty_ident "T/657"
                    signature_item (ocamlary.ml[526,12038+4]..ocamlary.ml[526,12038+22])
                      Tsig_module "Y/659"
                      module_type (ocamlary.ml[526,12038+15]..ocamlary.ml[526,12038+22])
                        Tmty_signature
                        []
                  ]
            structure_item (ocamlary.ml[528,12067+2]..ocamlary.ml[528,12067+30])
              Tstr_module
              X/662
                module_expr (ocamlary.ml[528,12067+13]..ocamlary.ml[528,12067+30])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[528,12067+20]..ocamlary.ml[528,12067+26])
                      Tstr_type Rec
                      [
                        type_declaration b/661 (ocamlary.ml[528,12067+20]..ocamlary.ml[528,12067+26])
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
                  ]
          ]
    structure_item (ocamlary.ml[531,12103+0]..ocamlary.ml[543,12467+5])
      Tstr_module
      Dep5/678
        module_expr (ocamlary.ml[531,12103+12]..ocamlary.ml[543,12467+5])
          Tmod_functor "Arg/669"
          module_type (ocamlary.ml[531,12103+19]..ocamlary.ml[538,12327+17])
            Tmty_signature
            [
              signature_item (ocamlary.ml[532,12126+19]..ocamlary.ml[532,12126+32])
                Tsig_modtype "T/664"
                #abstract            signature_item (ocamlary.ml[533,12159+19]..ocamlary.ml[536,12272+22])
                Tsig_modtype "S/667"
                  module_type (ocamlary.ml[533,12159+35]..ocamlary.ml[536,12272+22])
                    Tmty_signature
                    [
                      signature_item (ocamlary.ml[534,12198+21]..ocamlary.ml[534,12198+33])
                        Tsig_module "X/665"
                        module_type (ocamlary.ml[534,12198+32]..ocamlary.ml[534,12198+33])
                          Tmty_ident "T/664"
                      signature_item (ocamlary.ml[535,12232+21]..ocamlary.ml[535,12232+39])
                        Tsig_module "Y/666"
                        module_type (ocamlary.ml[535,12232+32]..ocamlary.ml[535,12232+39])
                          Tmty_signature
                          []
                    ]
              signature_item (ocamlary.ml[537,12295+19]..ocamlary.ml[537,12295+31])
                Tsig_module "X/668"
                module_type (ocamlary.ml[537,12295+30]..ocamlary.ml[537,12295+31])
                  Tmty_ident "T/664"
            ]
          module_expr (ocamlary.ml[538,12327+21]..ocamlary.ml[543,12467+5])
            Tmod_structure
            [
              structure_item (ocamlary.ml[539,12355+6]..ocamlary.ml[542,12457+9])
                Tstr_module
                Z/677
                  module_expr (ocamlary.ml[539,12355+15]..ocamlary.ml[542,12457+9])
                    Tmod_constraint
                    module_expr (ocamlary.ml[539,12355+46]..ocamlary.ml[542,12457+9])
                      Tmod_structure
                      [
                        structure_item (ocamlary.ml[540,12408+8]..ocamlary.ml[540,12408+24])
                          Tstr_module
                          X/670
                            module_expr (ocamlary.ml[540,12408+19]..ocamlary.ml[540,12408+24])
                              Tmod_ident "Arg/669.X"
                        structure_item (ocamlary.ml[541,12433+8]..ocamlary.ml[541,12433+23])
                          Tstr_module
                          Y/671
                            module_expr (ocamlary.ml[541,12433+19]..ocamlary.ml[541,12433+23])
                              Tmod_ident "Dep3/655"
                      ]
                    module_type (ocamlary.ml[539,12355+17]..ocamlary.ml[539,12355+43])
                      Tmty_with
                      module_type (ocamlary.ml[539,12355+17]..ocamlary.ml[539,12355+22])
                        Tmty_ident "Arg/669.S"
                      [
                        "Y/673"
                          Twith_module "Dep3/655"
                      ]
            ]
    structure_item (ocamlary.ml[545,12474+0]..ocamlary.ml[545,12474+28])
      Tstr_type Rec
      [
        type_declaration dep2/679 (ocamlary.ml[545,12474+0]..ocamlary.ml[545,12474+28])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[545,12474+12]..ocamlary.ml[545,12474+28])
                Ttyp_constr "Dep5/678(Dep4/663).Z.X.b"
                []
      ]
    structure_item (ocamlary.ml[547,12504+0]..ocamlary.ml[547,12504+28])
      Tstr_type Rec
      [
        type_declaration dep3/699 (ocamlary.ml[547,12504+0]..ocamlary.ml[547,12504+28])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[547,12504+12]..ocamlary.ml[547,12504+28])
                Ttyp_constr "Dep5/678(Dep4/663).Z.Y.a"
                []
      ]
    structure_item (ocamlary.ml[549,12534+0]..ocamlary.ml[559,12736+3])
      Tstr_module
      Dep6/709
        module_expr (ocamlary.ml[549,12534+14]..ocamlary.ml[559,12736+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[550,12555+2]..ocamlary.ml[550,12555+32])
              Tstr_modtype "S/701"
                module_type (ocamlary.ml[550,12555+18]..ocamlary.ml[550,12555+32])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[550,12555+22]..ocamlary.ml[550,12555+28])
                      Tsig_type Rec
                      [
                        type_declaration d/700 (ocamlary.ml[550,12555+22]..ocamlary.ml[550,12555+28])
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
                  ]
            structure_item (ocamlary.ml[551,12588+2]..ocamlary.ml[554,12649+5])
              Tstr_modtype "T/704"
                module_type (ocamlary.ml[551,12588+18]..ocamlary.ml[554,12649+5])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[552,12610+4]..ocamlary.ml[552,12610+21])
                      Tsig_modtype "R/702"
                        module_type (ocamlary.ml[552,12610+20]..ocamlary.ml[552,12610+21])
                          Tmty_ident "S/701"
                    signature_item (ocamlary.ml[553,12632+4]..ocamlary.ml[553,12632+16])
                      Tsig_module "Y/703"
                      module_type (ocamlary.ml[553,12632+15]..ocamlary.ml[553,12632+16])
                        Tmty_ident "R/702"
                  ]
            structure_item (ocamlary.ml[555,12655+2]..ocamlary.ml[558,12730+5])
              Tstr_module
              X/708
                module_expr (ocamlary.ml[555,12655+13]..ocamlary.ml[558,12730+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[556,12675+4]..ocamlary.ml[556,12675+21])
                      Tstr_modtype "R/705"
                        module_type (ocamlary.ml[556,12675+20]..ocamlary.ml[556,12675+21])
                          Tmty_ident "S/701"
                    structure_item (ocamlary.ml[557,12697+4]..ocamlary.ml[557,12697+32])
                      Tstr_module
                      Y/707
                        module_expr (ocamlary.ml[557,12697+15]..ocamlary.ml[557,12697+32])
                          Tmod_structure
                          [
                            structure_item (ocamlary.ml[557,12697+22]..ocamlary.ml[557,12697+28])
                              Tstr_type Rec
                              [
                                type_declaration d/706 (ocamlary.ml[557,12697+22]..ocamlary.ml[557,12697+28])
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
                          ]
                  ]
          ]
    structure_item (ocamlary.ml[561,12741+0]..ocamlary.ml[570,13013+7])
      Tstr_module
      Dep7/719
        module_expr (ocamlary.ml[561,12741+12]..ocamlary.ml[570,13013+7])
          Tmod_functor "Arg/715"
          module_type (ocamlary.ml[561,12741+19]..ocamlary.ml[568,12964+15])
            Tmty_signature
            [
              signature_item (ocamlary.ml[562,12764+19]..ocamlary.ml[562,12764+32])
                Tsig_modtype "S/710"
                #abstract            signature_item (ocamlary.ml[563,12797+19]..ocamlary.ml[566,12909+22])
                Tsig_modtype "T/713"
                  module_type (ocamlary.ml[563,12797+35]..ocamlary.ml[566,12909+22])
                    Tmty_signature
                    [
                      signature_item (ocamlary.ml[564,12836+21]..ocamlary.ml[564,12836+38])
                        Tsig_modtype "R/711"
                          module_type (ocamlary.ml[564,12836+37]..ocamlary.ml[564,12836+38])
                            Tmty_ident "S/710"
                      signature_item (ocamlary.ml[565,12875+21]..ocamlary.ml[565,12875+33])
                        Tsig_module "Y/712"
                        module_type (ocamlary.ml[565,12875+32]..ocamlary.ml[565,12875+33])
                          Tmty_ident "R/711"
                    ]
              signature_item (ocamlary.ml[567,12932+19]..ocamlary.ml[567,12932+31])
                Tsig_module "X/714"
                module_type (ocamlary.ml[567,12932+30]..ocamlary.ml[567,12932+31])
                  Tmty_ident "T/713"
            ]
          module_expr (ocamlary.ml[568,12964+19]..ocamlary.ml[570,13013+7])
            Tmod_structure
            [
              structure_item (ocamlary.ml[569,12990+6]..ocamlary.ml[569,12990+22])
                Tstr_module
                M/718
                  module_expr (ocamlary.ml[569,12990+17]..ocamlary.ml[569,12990+22])
                    Tmod_ident "Arg/715.X"
            ]
    structure_item (ocamlary.ml[572,13022+0]..ocamlary.ml[572,13022+28])
      Tstr_type Rec
      [
        type_declaration dep4/720 (ocamlary.ml[572,13022+0]..ocamlary.ml[572,13022+28])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[572,13022+12]..ocamlary.ml[572,13022+28])
                Ttyp_constr "Dep7/719(Dep6/709).M.Y.d"
                []
      ]
    structure_item (ocamlary.ml[574,13054+0]..ocamlary.ml[576,13108+3])
      Tstr_module
      Dep8/743
        module_expr (ocamlary.ml[574,13054+14]..ocamlary.ml[576,13108+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[575,13075+2]..ocamlary.ml[575,13075+32])
              Tstr_modtype "T/742"
                module_type (ocamlary.ml[575,13075+18]..ocamlary.ml[575,13075+32])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[575,13075+22]..ocamlary.ml[575,13075+28])
                      Tsig_type Rec
                      [
                        type_declaration t/741 (ocamlary.ml[575,13075+22]..ocamlary.ml[575,13075+28])
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
                  ]
          ]
    structure_item (ocamlary.ml[578,13113+0]..ocamlary.ml[578,13113+42])
      Tstr_module
      Dep9/746
        module_expr (ocamlary.ml[578,13113+11]..ocamlary.ml[578,13113+42])
          Tmod_functor "X/745"
          module_type (ocamlary.ml[578,13113+16]..ocamlary.ml[578,13113+37])
            Tmty_signature
            [
              signature_item (ocamlary.ml[578,13113+20]..ocamlary.ml[578,13113+33])
                Tsig_modtype "T/744"
                #abstract          ]
          module_expr (ocamlary.ml[578,13113+41]..ocamlary.ml[578,13113+42])
            Tmod_ident "X/745"
    structure_item (ocamlary.ml[580,13157+0]..ocamlary.ml[580,13157+50])
      Tstr_modtype "Dep10/753"
        module_type (ocamlary.ml[580,13157+20]..ocamlary.ml[580,13157+50])
          Tmty_with
          module_type (ocamlary.ml[580,13157+20]..ocamlary.ml[580,13157+32])
            Tmty_ident "Dep9/746(Dep8/743).T"
          [
            "t/750"
              Twith_type
                type_declaration t/750 (ocamlary.ml[580,13157+38]..ocamlary.ml[580,13157+50])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[580,13157+47]..ocamlary.ml[580,13157+50])
                        Ttyp_constr "int/1!"
                        []
          ]
    structure_item (ocamlary.ml[582,13209+0]..ocamlary.ml[588,13309+3])
      Tstr_module
      Dep11/759
        module_expr (ocamlary.ml[582,13209+15]..ocamlary.ml[588,13309+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[583,13231+2]..ocamlary.ml[587,13303+5])
              Tstr_modtype "S/758"
                module_type (ocamlary.ml[583,13231+18]..ocamlary.ml[587,13303+5])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[584,13253+4]..ocamlary.ml[586,13295+7])
                      Tsig_class
                      [
                        class_description (ocamlary.ml[584,13253+4]..ocamlary.ml[586,13295+7])
                          pci_virt = Concrete
                          pci_params =
                            []
                          pci_name = "c"
                          pci_expr =
                            class_type (ocamlary.ml[584,13253+14]..ocamlary.ml[586,13295+7])
                              Tcty_signature
                              class_signature
                                core_type (ocamlary.ml[584,13253+20]..ocamlary.ml[584,13253+20])
                                  Ttyp_any
                                [
                                  class_type_field (ocamlary.ml[585,13274+6]..ocamlary.ml[585,13274+20])
                                    Tctf_method "m" Public Concrete
                                      core_type (ocamlary.ml[585,13274+6]..ocamlary.ml[585,13274+20])
                                        Ttyp_poly
                                        core_type (ocamlary.ml[585,13274+17]..ocamlary.ml[585,13274+20])
                                          Ttyp_constr "int/1!"
                                          []
                                ]
                      ]
                  ]
          ]
    structure_item (ocamlary.ml[590,13314+0]..ocamlary.ml[593,13407+3])
      Tstr_module
      Dep12/763
        module_expr (ocamlary.ml[591,13329+10]..ocamlary.ml[593,13407+3])
          Tmod_functor "Arg/761"
          module_type (ocamlary.ml[591,13329+17]..ocamlary.ml[591,13329+38])
            Tmty_signature
            [
              signature_item (ocamlary.ml[591,13329+21]..ocamlary.ml[591,13329+34])
                Tsig_modtype "S/760"
                #abstract          ]
          module_expr (ocamlary.ml[591,13329+43]..ocamlary.ml[593,13407+3])
            Tmod_structure
            [
              structure_item (ocamlary.ml[592,13379+6]..ocamlary.ml[592,13379+27])
                Tstr_modtype "T/762"
                  module_type (ocamlary.ml[592,13379+22]..ocamlary.ml[592,13379+27])
                    Tmty_ident "Arg/761.S"
            ]
    structure_item (ocamlary.ml[595,13412+0]..ocamlary.ml[599,13476+3])
      Tstr_module
      Dep13/774
        module_expr (ocamlary.ml[595,13412+15]..ocamlary.ml[599,13476+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[596,13434+2]..ocamlary.ml[598,13470+5])
              Tstr_class
              [
                class_declaration (ocamlary.ml[596,13434+2]..ocamlary.ml[598,13470+5])
                  pci_virt = Concrete
                  pci_params =
                    []
                  pci_name = "c"
                  pci_expr =
                    class_expr (ocamlary.ml[596,13434+12]..ocamlary.ml[598,13470+5])
                      Tcl_structure
                      class_structure
                        pattern (_none_[0,0+-1].._none_[0,0+-1]) ghost
                          Tpat_alias "selfpat-*/768"
                          pattern (ocamlary.ml[596,13434+18]..ocamlary.ml[596,13434+18]) ghost
                            Tpat_any
                        [
                          class_field (ocamlary.ml[597,13453+4]..ocamlary.ml[597,13453+16])
                            Tcf_method "m" Public
                              Concrete Fresh
                              expression (ocamlary.ml[597,13453+15]..ocamlary.ml[597,13453+16]) ghost
                                Texp_function
                                Nolabel
                                [
                                  <case>
                                    pattern (ocamlary.ml[596,13434+18]..ocamlary.ml[596,13434+18]) ghost
                                      Tpat_alias "self-6/773"
                                      pattern (ocamlary.ml[596,13434+18]..ocamlary.ml[596,13434+18]) ghost
                                        Tpat_var "self-*/772"
                                    expression (ocamlary.ml[597,13453+15]..ocamlary.ml[597,13453+16])
                                      extra
                                        Texp_poly
                                        None
                                      Texp_constant Const_int 4
                                ]
                        ]
              ]
          ]
    structure_item (ocamlary.ml[601,13481+0]..ocamlary.ml[601,13481+19])
      Tstr_type Rec
      [
        type_declaration dep5/775 (ocamlary.ml[601,13481+0]..ocamlary.ml[601,13481+19])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[601,13481+12]..ocamlary.ml[601,13481+19])
                Ttyp_constr "Dep13/774.c"
                []
      ]
    structure_item (ocamlary.ml[603,13502+0]..ocamlary.ml[608,13584+3])
      Tstr_modtype "With1/779"
        module_type (ocamlary.ml[603,13502+20]..ocamlary.ml[608,13584+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[604,13526+2]..ocamlary.ml[606,13561+5])
              Tsig_module "M/777"
              module_type (ocamlary.ml[604,13526+13]..ocamlary.ml[606,13561+5])
                Tmty_signature
                [
                  signature_item (ocamlary.ml[605,13543+4]..ocamlary.ml[605,13543+17])
                    Tsig_modtype "S/776"
                    #abstract              ]
            signature_item (ocamlary.ml[607,13567+2]..ocamlary.ml[607,13567+16])
              Tsig_module "N/778"
              module_type (ocamlary.ml[607,13567+13]..ocamlary.ml[607,13567+16])
                Tmty_ident "M/777.S"
          ]
    structure_item (ocamlary.ml[610,13589+0]..ocamlary.ml[612,13644+3])
      Tstr_module
      With2/782
        module_expr (ocamlary.ml[610,13589+15]..ocamlary.ml[612,13644+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[611,13611+2]..ocamlary.ml[611,13611+32])
              Tstr_modtype "S/781"
                module_type (ocamlary.ml[611,13611+18]..ocamlary.ml[611,13611+32])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[611,13611+22]..ocamlary.ml[611,13611+28])
                      Tsig_type Rec
                      [
                        type_declaration t/780 (ocamlary.ml[611,13611+22]..ocamlary.ml[611,13611+28])
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
                  ]
          ]
    structure_item (ocamlary.ml[614,13649+0]..ocamlary.ml[619,13733+3])
      Tstr_module
      With3/786
        module_expr (ocamlary.ml[614,13649+15]..ocamlary.ml[619,13733+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[615,13671+2]..ocamlary.ml[615,13671+18])
              Tstr_module
              M/783
                module_expr (ocamlary.ml[615,13671+13]..ocamlary.ml[615,13671+18])
                  Tmod_ident "With2/782"
            structure_item (ocamlary.ml[616,13690+2]..ocamlary.ml[618,13727+5])
              Tstr_module
              N/785
                module_expr (ocamlary.ml[616,13690+13]..ocamlary.ml[618,13727+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[617,13710+4]..ocamlary.ml[617,13710+16])
                      Tstr_type Rec
                      [
                        type_declaration t/784 (ocamlary.ml[617,13710+4]..ocamlary.ml[617,13710+16])
                          ptype_params =
                            []
                          ptype_cstrs =
                            []
                          ptype_kind =
                            Ttype_abstract
                          ptype_private = Public
                          ptype_manifest =
                            Some
                              core_type (ocamlary.ml[617,13710+13]..ocamlary.ml[617,13710+16])
                                Ttyp_constr "int/1!"
                                []
                      ]
                  ]
          ]
    structure_item (ocamlary.ml[621,13738+0]..ocamlary.ml[621,13738+22])
      Tstr_type Rec
      [
        type_declaration with1/787 (ocamlary.ml[621,13738+0]..ocamlary.ml[621,13738+22])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[621,13738+13]..ocamlary.ml[621,13738+22])
                Ttyp_constr "With3/786.N.t"
                []
      ]
    structure_item (ocamlary.ml[623,13762+0]..ocamlary.ml[627,13827+3])
      Tstr_module
      With4/790
        module_expr (ocamlary.ml[623,13762+15]..ocamlary.ml[627,13827+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[624,13784+2]..ocamlary.ml[626,13821+5])
              Tstr_module
              N/789
                module_expr (ocamlary.ml[624,13784+13]..ocamlary.ml[626,13821+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[625,13804+4]..ocamlary.ml[625,13804+16])
                      Tstr_type Rec
                      [
                        type_declaration t/788 (ocamlary.ml[625,13804+4]..ocamlary.ml[625,13804+16])
                          ptype_params =
                            []
                          ptype_cstrs =
                            []
                          ptype_kind =
                            Ttype_abstract
                          ptype_private = Public
                          ptype_manifest =
                            Some
                              core_type (ocamlary.ml[625,13804+13]..ocamlary.ml[625,13804+16])
                                Ttyp_constr "int/1!"
                                []
                      ]
                  ]
          ]
    structure_item (ocamlary.ml[629,13832+0]..ocamlary.ml[629,13832+22])
      Tstr_type Rec
      [
        type_declaration with2/791 (ocamlary.ml[629,13832+0]..ocamlary.ml[629,13832+22])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[629,13832+13]..ocamlary.ml[629,13832+22])
                Ttyp_constr "With4/790.N.t"
                []
      ]
    structure_item (ocamlary.ml[631,13856+0]..ocamlary.ml[634,13950+3])
      Tstr_module
      With5/796
        module_expr (ocamlary.ml[631,13856+15]..ocamlary.ml[634,13950+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[632,13878+2]..ocamlary.ml[632,13878+32])
              Tstr_modtype "S/793"
                module_type (ocamlary.ml[632,13878+18]..ocamlary.ml[632,13878+32])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[632,13878+22]..ocamlary.ml[632,13878+28])
                      Tsig_type Rec
                      [
                        type_declaration t/792 (ocamlary.ml[632,13878+22]..ocamlary.ml[632,13878+28])
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
                  ]
            structure_item (ocamlary.ml[633,13911+2]..ocamlary.ml[633,13911+38])
              Tstr_module
              N/795
                module_expr (ocamlary.ml[633,13911+13]..ocamlary.ml[633,13911+38])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[633,13911+20]..ocamlary.ml[633,13911+34])
                      Tstr_type Rec
                      [
                        type_declaration t/794 (ocamlary.ml[633,13911+20]..ocamlary.ml[633,13911+34])
                          ptype_params =
                            []
                          ptype_cstrs =
                            []
                          ptype_kind =
                            Ttype_abstract
                          ptype_private = Public
                          ptype_manifest =
                            Some
                              core_type (ocamlary.ml[633,13911+29]..ocamlary.ml[633,13911+34])
                                Ttyp_constr "float/4!"
                                []
                      ]
                  ]
          ]
    structure_item (ocamlary.ml[636,13955+0]..ocamlary.ml[643,14071+3])
      Tstr_module
      With6/801
        module_expr (ocamlary.ml[636,13955+15]..ocamlary.ml[643,14071+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[637,13977+2]..ocamlary.ml[642,14065+5])
              Tstr_modtype "T/800"
                module_type (ocamlary.ml[637,13977+18]..ocamlary.ml[642,14065+5])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[638,13999+4]..ocamlary.ml[641,14057+7])
                      Tsig_module "M/799"
                      module_type (ocamlary.ml[638,13999+15]..ocamlary.ml[641,14057+7])
                        Tmty_signature
                        [
                          signature_item (ocamlary.ml[639,14018+6]..ocamlary.ml[639,14018+19])
                            Tsig_modtype "S/797"
                            #abstract                        signature_item (ocamlary.ml[640,14038+6]..ocamlary.ml[640,14038+18])
                            Tsig_module "N/798"
                            module_type (ocamlary.ml[640,14038+17]..ocamlary.ml[640,14038+18])
                              Tmty_ident "S/797"
                        ]
                  ]
          ]
    structure_item (ocamlary.ml[645,14076+0]..ocamlary.ml[645,14076+44])
      Tstr_module
      With7/804
        module_expr (ocamlary.ml[645,14076+13]..ocamlary.ml[645,14076+44])
          Tmod_functor "X/803"
          module_type (ocamlary.ml[645,14076+18]..ocamlary.ml[645,14076+39])
            Tmty_signature
            [
              signature_item (ocamlary.ml[645,14076+22]..ocamlary.ml[645,14076+35])
                Tsig_modtype "T/802"
                #abstract          ]
          module_expr (ocamlary.ml[645,14076+43]..ocamlary.ml[645,14076+44])
            Tmod_ident "X/803"
    structure_item (ocamlary.ml[647,14122+0]..ocamlary.ml[647,14122+83])
      Tstr_modtype "With8/817"
        module_type (ocamlary.ml[647,14122+20]..ocamlary.ml[647,14122+83])
          Tmty_with
          module_type (ocamlary.ml[647,14122+20]..ocamlary.ml[647,14122+34])
            Tmty_ident "With7/804(With6/801).T"
          [
            "M/808"
              Twith_module "With5/796"
            "M/808.N.t"
              Twith_type
                type_declaration t/794 (ocamlary.ml[647,14122+61]..ocamlary.ml[647,14122+83])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[647,14122+74]..ocamlary.ml[647,14122+83])
                        Ttyp_constr "With5/796.N.t"
                        []
          ]
    structure_item (ocamlary.ml[649,14207+0]..ocamlary.ml[651,14262+3])
      Tstr_module
      With9/820
        module_expr (ocamlary.ml[649,14207+15]..ocamlary.ml[651,14262+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[650,14229+2]..ocamlary.ml[650,14229+32])
              Tstr_modtype "S/819"
                module_type (ocamlary.ml[650,14229+18]..ocamlary.ml[650,14229+32])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[650,14229+22]..ocamlary.ml[650,14229+28])
                      Tsig_type Rec
                      [
                        type_declaration t/818 (ocamlary.ml[650,14229+22]..ocamlary.ml[650,14229+28])
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
                  ]
          ]
    structure_item (ocamlary.ml[653,14267+0]..ocamlary.ml[660,14384+3])
      Tstr_module
      With10/825
        module_expr (ocamlary.ml[653,14267+16]..ocamlary.ml[660,14384+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[654,14290+2]..ocamlary.ml[659,14378+5])
              Tstr_modtype "T/824"
                module_type (ocamlary.ml[654,14290+18]..ocamlary.ml[659,14378+5])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[655,14312+4]..ocamlary.ml[657,14351+7])
                      Tsig_module "M/822"
                      module_type (ocamlary.ml[655,14312+15]..ocamlary.ml[657,14351+7])
                        Tmty_signature
                        [
                          signature_item (ocamlary.ml[656,14331+6]..ocamlary.ml[656,14331+19])
                            Tsig_modtype "S/821"
                            #abstract                      ]
                    signature_item (ocamlary.ml[658,14359+4]..ocamlary.ml[658,14359+18])
                      Tsig_module "N/823"
                      module_type (ocamlary.ml[658,14359+15]..ocamlary.ml[658,14359+18])
                        Tmty_ident "M/822.S"
                  ]
          ]
    structure_item (ocamlary.ml[662,14389+0]..ocamlary.ml[662,14389+77])
      Tstr_modtype "With11/837"
        module_type (ocamlary.ml[662,14389+21]..ocamlary.ml[662,14389+77])
          Tmty_with
          module_type (ocamlary.ml[662,14389+21]..ocamlary.ml[662,14389+36])
            Tmty_ident "With7/804(With10/825).T"
          [
            "M/827"
              Twith_module "With9/820"
            "N/828.t"
              Twith_type
                type_declaration t/830 (ocamlary.ml[662,14389+63]..ocamlary.ml[662,14389+77])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[662,14389+74]..ocamlary.ml[662,14389+77])
                        Ttyp_constr "int/1!"
                        []
          ]
    structure_item (ocamlary.ml[664,14468+0]..ocamlary.ml[668,14562+3])
      Tstr_modtype "NestedInclude1/840"
        module_type (ocamlary.ml[664,14468+29]..ocamlary.ml[668,14562+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[666,14502+2]..ocamlary.ml[666,14502+58])
              Tsig_modtype "NestedInclude2/839"
                module_type (ocamlary.ml[666,14502+31]..ocamlary.ml[666,14502+58])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[666,14502+35]..ocamlary.ml[666,14502+54])
                      Tsig_type Rec
                      [
                        type_declaration nested_include/838 (ocamlary.ml[666,14502+35]..ocamlary.ml[666,14502+54])
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
                  ]
          ]
    structure_item (ocamlary.ml[670,14567+0]..ocamlary.ml[672,14622+3])
      Tstr_modtype "NestedInclude2/842"
        module_type (ocamlary.ml[670,14567+29]..ocamlary.ml[672,14622+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[671,14600+2]..ocamlary.ml[671,14600+21])
              Tsig_type Rec
              [
                type_declaration nested_include/841 (ocamlary.ml[671,14600+2]..ocamlary.ml[671,14600+21])
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
          ]
    structure_item (ocamlary.ml[674,14627+0]..ocamlary.ml[674,14627+25])
      Tstr_type Rec
      [
        type_declaration nested_include/843 (ocamlary.ml[674,14627+0]..ocamlary.ml[674,14627+25])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_abstract
          ptype_private = Public
          ptype_manifest =
            Some
              core_type (ocamlary.ml[674,14627+22]..ocamlary.ml[674,14627+25])
                Ttyp_constr "int/1!"
                []
      ]
    structure_item (ocamlary.ml[676,14654+0]..ocamlary.ml[680,14748+3])
      Tstr_module
      DoubleInclude1/846
        module_expr (ocamlary.ml[676,14654+24]..ocamlary.ml[680,14748+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[677,14685+2]..ocamlary.ml[679,14742+5])
              Tstr_module
              DoubleInclude2/845
                module_expr (ocamlary.ml[677,14685+26]..ocamlary.ml[679,14742+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[678,14718+4]..ocamlary.ml[678,14718+23])
                      Tstr_type Rec
                      [
                        type_declaration double_include/844 (ocamlary.ml[678,14718+4]..ocamlary.ml[678,14718+23])
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
                  ]
          ]
    structure_item (ocamlary.ml[682,14753+0]..ocamlary.ml[684,14809+3])
      Tstr_module
      DoubleInclude3/848
        module_expr (ocamlary.ml[682,14753+24]..ocamlary.ml[684,14809+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[683,14784+2]..ocamlary.ml[683,14784+24])
              Tstr_include            module_expr (ocamlary.ml[683,14784+10]..ocamlary.ml[683,14784+24])
                Tmod_ident "DoubleInclude1/846"
          ]
    structure_item (ocamlary.ml[686,14814+0]..ocamlary.ml[686,14814+37])
      Tstr_include    module_expr (ocamlary.ml[686,14814+8]..ocamlary.ml[686,14814+37])
        module_expr (ocamlary.ml[686,14814+8]..ocamlary.ml[686,14814+37])
          Tmod_ident "DoubleInclude3/848.DoubleInclude2"
    structure_item (ocamlary.ml[688,14853+0]..ocamlary.ml[694,14994+3])
      Tstr_module
      IncludeInclude1/854
        module_expr (ocamlary.ml[688,14853+25]..ocamlary.ml[694,14994+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[689,14885+2]..ocamlary.ml[691,14946+5])
              Tstr_modtype "IncludeInclude2/852"
                module_type (ocamlary.ml[689,14885+32]..ocamlary.ml[691,14946+5])
                  Tmty_signature
                  [
                    signature_item (ocamlary.ml[690,14921+4]..ocamlary.ml[690,14921+24])
                      Tsig_type Rec
                      [
                        type_declaration include_include/851 (ocamlary.ml[690,14921+4]..ocamlary.ml[690,14921+24])
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
                  ]
            structure_item (ocamlary.ml[692,14952+2]..ocamlary.ml[693,14988+5])
              Tstr_module
              IncludeInclude2_M/853
                module_expr (ocamlary.ml[692,14952+29]..ocamlary.ml[693,14988+5])
                  Tmod_structure
                  []
          ]
    structure_item (ocamlary.ml[696,14999+0]..ocamlary.ml[696,14999+23])
      Tstr_include    module_expr (ocamlary.ml[696,14999+8]..ocamlary.ml[696,14999+23])
        Tmod_ident "IncludeInclude1/854"
    structure_item (ocamlary.ml[697,15023+0]..ocamlary.ml[697,15023+20])
      Tstr_type Rec
      [
        type_declaration include_include/857 (ocamlary.ml[697,15023+0]..ocamlary.ml[697,15023+20])
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
    structure_item (ocamlary.ml[699,15045+0]..ocamlary.ml[699,15045+23])
      Tstr_module
      Caml_list/858
        module_expr (ocamlary.ml[699,15045+19]..ocamlary.ml[699,15045+23])
          Tmod_ident "Stdlib!.List"
    structure_item (ocamlary.ml[701,15070+0]..ocamlary.ml[741,15885+3])
      Tstr_module
      CanonicalTest/948
        module_expr (ocamlary.ml[701,15070+23]..ocamlary.ml[741,15885+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[702,15100+2]..ocamlary.ml[706,15171+5])
              Tstr_module
              Base__List/863
                module_expr (ocamlary.ml[702,15100+22]..ocamlary.ml[706,15171+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[703,15129+4]..ocamlary.ml[703,15129+23])
                      Tstr_type Rec
                      [
                        type_declaration t/859 (ocamlary.ml[703,15129+4]..ocamlary.ml[703,15129+23])
                          ptype_params =
                            [
                              core_type (ocamlary.ml[703,15129+9]..ocamlary.ml[703,15129+11])
                                Ttyp_var a
                            ]
                          ptype_cstrs =
                            []
                          ptype_kind =
                            Ttype_abstract
                          ptype_private = Public
                          ptype_manifest =
                            Some
                              core_type (ocamlary.ml[703,15129+16]..ocamlary.ml[703,15129+23])
                                Ttyp_constr "list/9!"
                                [
                                  core_type (ocamlary.ml[703,15129+16]..ocamlary.ml[703,15129+18])
                                    Ttyp_var a
                                ]
                      ]
                    structure_item (ocamlary.ml[705,15154+4]..ocamlary.ml[705,15154+16])
                      Tstr_value Nonrec
                      [
                        <def>
                          pattern (ocamlary.ml[705,15154+8]..ocamlary.ml[705,15154+10])
                            Tpat_var "id/860"
                          expression (ocamlary.ml[705,15154+11]..ocamlary.ml[705,15154+16]) ghost
                            Texp_function
                            Nolabel
                            [
                              <case>
                                pattern (ocamlary.ml[705,15154+11]..ocamlary.ml[705,15154+12])
                                  Tpat_var "x/862"
                                expression (ocamlary.ml[705,15154+15]..ocamlary.ml[705,15154+16])
                                  Texp_ident "x/862"
                            ]
                      ]
                  ]
            structure_item (ocamlary.ml[708,15178+2]..ocamlary.ml[711,15287+5])
              Tstr_module
              Base__/865
                module_expr (ocamlary.ml[708,15178+18]..ocamlary.ml[711,15287+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[710,15258+4]..ocamlary.ml[710,15258+28])
                      Tstr_module
                      List/864
                        attribute "ocaml.doc"
                          [
                            structure_item (ocamlary.ml[709,15203+4]..[709,15203+54])
                              Pstr_eval
                              expression (ocamlary.ml[709,15203+4]..[709,15203+54])
                                Pexp_constant PConst_string(" @canonical Ocamlary.CanonicalTest.Base.List ",(ocamlary.ml[709,15203+4]..[709,15203+54]),None)
                          ]
                        module_expr (ocamlary.ml[710,15258+18]..ocamlary.ml[710,15258+28])
                          Tmod_ident "Base__List/863"
                  ]
            structure_item (ocamlary.ml[713,15294+2]..ocamlary.ml[715,15347+5])
              Tstr_module
              Base/867
                module_expr (ocamlary.ml[713,15294+16]..ocamlary.ml[715,15347+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[714,15317+4]..ocamlary.ml[714,15317+29])
                      Tstr_module
                      List/866
                        module_expr (ocamlary.ml[714,15317+18]..ocamlary.ml[714,15317+29])
                          Tmod_ident "Base__/865.List"
                  ]
            structure_item (ocamlary.ml[717,15354+2]..ocamlary.ml[736,15821+5])
              Tstr_module
              Base_Tests/944
                module_expr (ocamlary.ml[717,15354+22]..ocamlary.ml[736,15821+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[718,15383+4]..ocamlary.ml[720,15431+7])
                      Tstr_module
                      C/870
                        module_expr (ocamlary.ml[718,15383+15]..ocamlary.ml[720,15431+7])
                          Tmod_structure
                          [
                            structure_item (ocamlary.ml[719,15405+6]..ocamlary.ml[719,15405+25])
                              Tstr_include                            module_expr (ocamlary.ml[719,15405+14]..ocamlary.ml[719,15405+25])
                                module_expr (ocamlary.ml[719,15405+14]..ocamlary.ml[719,15405+25])
                                  Tmod_ident "Base__/865.List"
                          ]
                    structure_item (ocamlary.ml[722,15440+4]..ocamlary.ml[722,15440+15])
                      Tstr_open Fresh
                      module_expr (ocamlary.ml[722,15440+9]..ocamlary.ml[722,15440+15])
                        Tmod_ident "Base__/865"
                    structure_item (ocamlary.ml[724,15457+4]..ocamlary.ml[724,15457+19])
                      Tstr_module
                      L/871
                        module_expr (ocamlary.ml[724,15457+15]..ocamlary.ml[724,15457+19])
                          Tmod_ident "Base__/865.List"
                    structure_item (ocamlary.ml[726,15478+4]..ocamlary.ml[727,15518+34])
                      Tstr_value Nonrec
                      [
                        <def>
                          pattern (ocamlary.ml[726,15478+8]..ocamlary.ml[726,15478+11])
                            Tpat_var "foo/872"
                          expression (ocamlary.ml[726,15478+12]..ocamlary.ml[727,15518+34]) ghost
                            Texp_function
                            Nolabel
                            [
                              <case>
                                pattern (ocamlary.ml[726,15478+13]..ocamlary.ml[726,15478+14])
                                  extra
                                    Tpat_extra_constraint
                                    core_type (ocamlary.ml[726,15478+17]..ocamlary.ml[726,15478+24])
                                      Ttyp_constr "L/871.t"
                                      [
                                        core_type (ocamlary.ml[726,15478+17]..ocamlary.ml[726,15478+20])
                                          Ttyp_constr "int/1!"
                                          []
                                      ]
                                  Tpat_alias "l/874"
                                  pattern (ocamlary.ml[726,15478+13]..ocamlary.ml[726,15478+14])
                                    Tpat_any
                                expression (ocamlary.ml[727,15518+6]..ocamlary.ml[727,15518+34])
                                  extra
                                    Texp_constraint
                                    core_type (ocamlary.ml[726,15478+28]..ocamlary.ml[726,15478+37])
                                      Ttyp_constr "L/871.t"
                                      [
                                        core_type (ocamlary.ml[726,15478+28]..ocamlary.ml[726,15478+33])
                                          Ttyp_constr "float/4!"
                                          []
                                      ]
                                  Texp_apply
                                  expression (ocamlary.ml[727,15518+6]..ocamlary.ml[727,15518+19])
                                    Texp_ident "Caml_list/858.map"
                                  [
                                    <arg>
                                      Nolabel
                                      expression (ocamlary.ml[727,15518+20]..ocamlary.ml[727,15518+32])
                                        Texp_ident "Stdlib!.float_of_int"
                                    <arg>
                                      Nolabel
                                      expression (ocamlary.ml[727,15518+33]..ocamlary.ml[727,15518+34])
                                        Texp_ident "l/874"
                                  ]
                            ]
                      ]
                    structure_item (ocamlary.ml[730,15608+4]..ocamlary.ml[731,15650+12])
                      Tstr_value Nonrec
                      [
                        <def>
                            attribute "ocaml.doc"
                              [
                                structure_item (ocamlary.ml[729,15554+4]..[729,15554+53])
                                  Pstr_eval
                                  expression (ocamlary.ml[729,15554+4]..[729,15554+53])
                                    Pexp_constant PConst_string(" This is just {!List.id}, or rather {!L.id} ",(ocamlary.ml[729,15554+4]..[729,15554+53]),None)
                              ]
                          pattern (ocamlary.ml[730,15608+8]..ocamlary.ml[730,15608+11])
                            Tpat_var "bar/938"
                          expression (ocamlary.ml[730,15608+12]..ocamlary.ml[731,15650+12]) ghost
                            Texp_function
                            Nolabel
                            [
                              <case>
                                pattern (ocamlary.ml[730,15608+13]..ocamlary.ml[730,15608+14])
                                  extra
                                    Tpat_extra_constraint
                                    core_type (ocamlary.ml[730,15608+17]..ocamlary.ml[730,15608+26])
                                      Ttyp_constr "Base__/865.List.t"
                                      [
                                        core_type (ocamlary.ml[730,15608+17]..ocamlary.ml[730,15608+19])
                                          Ttyp_var a
                                      ]
                                  Tpat_alias "l/940"
                                  pattern (ocamlary.ml[730,15608+13]..ocamlary.ml[730,15608+14])
                                    Tpat_any
                                expression (ocamlary.ml[731,15650+6]..ocamlary.ml[731,15650+12])
                                  extra
                                    Texp_constraint
                                    core_type (ocamlary.ml[730,15608+30]..ocamlary.ml[730,15608+39])
                                      Ttyp_constr "Base__/865.List.t"
                                      [
                                        core_type (ocamlary.ml[730,15608+30]..ocamlary.ml[730,15608+32])
                                          Ttyp_var a
                                      ]
                                  Texp_apply
                                  expression (ocamlary.ml[731,15650+6]..ocamlary.ml[731,15650+10])
                                    Texp_ident "L/871.id"
                                  [
                                    <arg>
                                      Nolabel
                                      expression (ocamlary.ml[731,15650+11]..ocamlary.ml[731,15650+12])
                                        Texp_ident "l/940"
                                  ]
                            ]
                      ]
                    structure_item (ocamlary.ml[735,15781+4]..ocamlary.ml[735,15781+39])
                      Tstr_value Nonrec
                      [
                        <def>
                            attribute "ocaml.doc"
                              [
                                structure_item (ocamlary.ml[733,15664+4]..[734,15739+41])
                                  Pstr_eval
                                  expression (ocamlary.ml[733,15664+4]..[734,15739+41])
                                    Pexp_constant PConst_string(" Just seeing if {!Base__.List.t} ([Base__.List.t]) gets rewriten to\n        {!Base.List.t} ([Base.List.t]) ",(ocamlary.ml[733,15664+4]..[734,15739+41]),None)
                              ]
                          pattern (ocamlary.ml[735,15781+8]..ocamlary.ml[735,15781+11])
                            Tpat_var "baz/941"
                          expression (ocamlary.ml[735,15781+12]..ocamlary.ml[735,15781+39]) ghost
                            Texp_function
                            Nolabel
                            [
                              <case>
                                pattern (ocamlary.ml[735,15781+13]..ocamlary.ml[735,15781+14])
                                  extra
                                    Tpat_extra_constraint
                                    core_type (ocamlary.ml[735,15781+17]..ocamlary.ml[735,15781+33])
                                      Ttyp_constr "Base__/865.List.t"
                                      [
                                        core_type (ocamlary.ml[735,15781+17]..ocamlary.ml[735,15781+19])
                                          Ttyp_var a
                                      ]
                                  Tpat_any
                                expression (ocamlary.ml[735,15781+37]..ocamlary.ml[735,15781+39])
                                  Texp_construct "()"
                                  []
                            ]
                      ]
                  ]
            structure_item (ocamlary.ml[738,15828+2]..ocamlary.ml[740,15879+5])
              Tstr_module
              List_modif/947
                module_expr (ocamlary.ml[738,15828+22]..ocamlary.ml[740,15879+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[739,15857+4]..ocamlary.ml[739,15857+21])
                      Tstr_include                    module_expr (ocamlary.ml[739,15857+12]..ocamlary.ml[739,15857+21])
                        module_expr (ocamlary.ml[739,15857+12]..ocamlary.ml[739,15857+21])
                          Tmod_ident "Base/867.List"
                  ]
          ]
    structure_item (ocamlary.ml[743,15890+0]..ocamlary.ml[744,15978+82])
      Tstr_attribute "ocaml.text"
      [
        structure_item (ocamlary.ml[743,15890+0]..[744,15978+82])
          Pstr_eval
          expression (ocamlary.ml[743,15890+0]..[744,15978+82])
            Pexp_constant PConst_string(" Some ref to {!CanonicalTest.Base__Tests.C.t} and {!CanonicalTest.Base__Tests.D.id}.\n    But also to {!CanonicalTest.Base__.List} and {!CanonicalTest.Base__.List.t} ",(ocamlary.ml[743,15890+0]..[744,15978+82]),None)
      ]
    structure_item (ocamlary.ml[746,16062+0]..ocamlary.ml[746,16062+24])
      Tstr_attribute "ocaml.text"
      [
        structure_item (ocamlary.ml[746,16062+0]..[746,16062+24])
          Pstr_eval
          expression (ocamlary.ml[746,16062+0]..[746,16062+24])
            Pexp_constant PConst_string(" {1 Aliases again} ",(ocamlary.ml[746,16062+0]..[746,16062+24]),None)
      ]
    structure_item (ocamlary.ml[748,16088+0]..ocamlary.ml[855,17668+3])
      Tstr_module
      Aliases/1019
        module_expr (ocamlary.ml[748,16088+17]..ocamlary.ml[855,17668+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[749,16112+2]..ocamlary.ml[749,16112+36])
              Tstr_attribute "ocaml.text"
              [
                structure_item (ocamlary.ml[749,16112+2]..[749,16112+36])
                  Pstr_eval
                  expression (ocamlary.ml[749,16112+2]..[749,16112+36])
                    Pexp_constant PConst_string(" Let's imitate jst's layout. ",(ocamlary.ml[749,16112+2]..[749,16112+36]),None)
              ]
            structure_item (ocamlary.ml[751,16150+2]..ocamlary.ml[755,16204+5])
              Tstr_module
              Foo__A/953
                module_expr (ocamlary.ml[751,16150+18]..ocamlary.ml[755,16204+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[752,16175+4]..ocamlary.ml[752,16175+10])
                      Tstr_type Rec
                      [
                        type_declaration t/949 (ocamlary.ml[752,16175+4]..ocamlary.ml[752,16175+10])
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
                    structure_item (ocamlary.ml[754,16187+4]..ocamlary.ml[754,16187+16])
                      Tstr_value Nonrec
                      [
                        <def>
                          pattern (ocamlary.ml[754,16187+8]..ocamlary.ml[754,16187+10])
                            Tpat_var "id/950"
                          expression (ocamlary.ml[754,16187+11]..ocamlary.ml[754,16187+16]) ghost
                            Texp_function
                            Nolabel
                            [
                              <case>
                                pattern (ocamlary.ml[754,16187+11]..ocamlary.ml[754,16187+12])
                                  Tpat_var "t/952"
                                expression (ocamlary.ml[754,16187+15]..ocamlary.ml[754,16187+16])
                                  Texp_ident "t/952"
                            ]
                      ]
                  ]
            structure_item (ocamlary.ml[757,16211+2]..ocamlary.ml[761,16265+5])
              Tstr_module
              Foo__B/958
                module_expr (ocamlary.ml[757,16211+18]..ocamlary.ml[761,16265+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[758,16236+4]..ocamlary.ml[758,16236+10])
                      Tstr_type Rec
                      [
                        type_declaration t/954 (ocamlary.ml[758,16236+4]..ocamlary.ml[758,16236+10])
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
                    structure_item (ocamlary.ml[760,16248+4]..ocamlary.ml[760,16248+16])
                      Tstr_value Nonrec
                      [
                        <def>
                          pattern (ocamlary.ml[760,16248+8]..ocamlary.ml[760,16248+10])
                            Tpat_var "id/955"
                          expression (ocamlary.ml[760,16248+11]..ocamlary.ml[760,16248+16]) ghost
                            Texp_function
                            Nolabel
                            [
                              <case>
                                pattern (ocamlary.ml[760,16248+11]..ocamlary.ml[760,16248+12])
                                  Tpat_var "t/957"
                                expression (ocamlary.ml[760,16248+15]..ocamlary.ml[760,16248+16])
                                  Texp_ident "t/957"
                            ]
                      ]
                  ]
            structure_item (ocamlary.ml[763,16272+2]..ocamlary.ml[767,16326+5])
              Tstr_module
              Foo__C/963
                module_expr (ocamlary.ml[763,16272+18]..ocamlary.ml[767,16326+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[764,16297+4]..ocamlary.ml[764,16297+10])
                      Tstr_type Rec
                      [
                        type_declaration t/959 (ocamlary.ml[764,16297+4]..ocamlary.ml[764,16297+10])
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
                    structure_item (ocamlary.ml[766,16309+4]..ocamlary.ml[766,16309+16])
                      Tstr_value Nonrec
                      [
                        <def>
                          pattern (ocamlary.ml[766,16309+8]..ocamlary.ml[766,16309+10])
                            Tpat_var "id/960"
                          expression (ocamlary.ml[766,16309+11]..ocamlary.ml[766,16309+16]) ghost
                            Texp_function
                            Nolabel
                            [
                              <case>
                                pattern (ocamlary.ml[766,16309+11]..ocamlary.ml[766,16309+12])
                                  Tpat_var "t/962"
                                expression (ocamlary.ml[766,16309+15]..ocamlary.ml[766,16309+16])
                                  Texp_ident "t/962"
                            ]
                      ]
                  ]
            structure_item (ocamlary.ml[769,16333+2]..ocamlary.ml[773,16387+5])
              Tstr_module
              Foo__D/968
                module_expr (ocamlary.ml[769,16333+18]..ocamlary.ml[773,16387+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[770,16358+4]..ocamlary.ml[770,16358+10])
                      Tstr_type Rec
                      [
                        type_declaration t/964 (ocamlary.ml[770,16358+4]..ocamlary.ml[770,16358+10])
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
                    structure_item (ocamlary.ml[772,16370+4]..ocamlary.ml[772,16370+16])
                      Tstr_value Nonrec
                      [
                        <def>
                          pattern (ocamlary.ml[772,16370+8]..ocamlary.ml[772,16370+10])
                            Tpat_var "id/965"
                          expression (ocamlary.ml[772,16370+11]..ocamlary.ml[772,16370+16]) ghost
                            Texp_function
                            Nolabel
                            [
                              <case>
                                pattern (ocamlary.ml[772,16370+11]..ocamlary.ml[772,16370+12])
                                  Tpat_var "t/967"
                                expression (ocamlary.ml[772,16370+15]..ocamlary.ml[772,16370+16])
                                  Texp_ident "t/967"
                            ]
                      ]
                  ]
            structure_item (ocamlary.ml[775,16394+2]..ocamlary.ml[779,16448+5])
              Tstr_module
              Foo__E/973
                module_expr (ocamlary.ml[775,16394+18]..ocamlary.ml[779,16448+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[776,16419+4]..ocamlary.ml[776,16419+10])
                      Tstr_type Rec
                      [
                        type_declaration t/969 (ocamlary.ml[776,16419+4]..ocamlary.ml[776,16419+10])
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
                    structure_item (ocamlary.ml[778,16431+4]..ocamlary.ml[778,16431+16])
                      Tstr_value Nonrec
                      [
                        <def>
                          pattern (ocamlary.ml[778,16431+8]..ocamlary.ml[778,16431+10])
                            Tpat_var "id/970"
                          expression (ocamlary.ml[778,16431+11]..ocamlary.ml[778,16431+16]) ghost
                            Texp_function
                            Nolabel
                            [
                              <case>
                                pattern (ocamlary.ml[778,16431+11]..ocamlary.ml[778,16431+12])
                                  Tpat_var "t/972"
                                expression (ocamlary.ml[778,16431+15]..ocamlary.ml[778,16431+16])
                                  Texp_ident "t/972"
                            ]
                      ]
                  ]
            structure_item (ocamlary.ml[781,16455+2]..ocamlary.ml[796,16774+5])
              Tstr_module
              Foo__/979
                module_expr (ocamlary.ml[781,16455+17]..ocamlary.ml[796,16774+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[784,16525+4]..ocamlary.ml[784,16525+21])
                      Tstr_module
                      A/974
                        attribute "ocaml.doc"
                          [
                            structure_item (ocamlary.ml[783,16480+4]..[783,16480+44])
                              Pstr_eval
                              expression (ocamlary.ml[783,16480+4]..[783,16480+44])
                                Pexp_constant PConst_string(" @canonical Ocamlary.Aliases.Foo.A ",(ocamlary.ml[783,16480+4]..[783,16480+44]),None)
                          ]
                        module_expr (ocamlary.ml[784,16525+15]..ocamlary.ml[784,16525+21])
                          Tmod_ident "Foo__A/953"
                    structure_item (ocamlary.ml[787,16593+4]..ocamlary.ml[787,16593+21])
                      Tstr_module
                      B/975
                        attribute "ocaml.doc"
                          [
                            structure_item (ocamlary.ml[786,16548+4]..[786,16548+44])
                              Pstr_eval
                              expression (ocamlary.ml[786,16548+4]..[786,16548+44])
                                Pexp_constant PConst_string(" @canonical Ocamlary.Aliases.Foo.B ",(ocamlary.ml[786,16548+4]..[786,16548+44]),None)
                          ]
                        module_expr (ocamlary.ml[787,16593+15]..ocamlary.ml[787,16593+21])
                          Tmod_ident "Foo__B/958"
                    structure_item (ocamlary.ml[790,16661+4]..ocamlary.ml[790,16661+21])
                      Tstr_module
                      C/976
                        attribute "ocaml.doc"
                          [
                            structure_item (ocamlary.ml[789,16616+4]..[789,16616+44])
                              Pstr_eval
                              expression (ocamlary.ml[789,16616+4]..[789,16616+44])
                                Pexp_constant PConst_string(" @canonical Ocamlary.Aliases.Foo.C ",(ocamlary.ml[789,16616+4]..[789,16616+44]),None)
                          ]
                        module_expr (ocamlary.ml[790,16661+15]..ocamlary.ml[790,16661+21])
                          Tmod_ident "Foo__C/963"
                    structure_item (ocamlary.ml[793,16729+4]..ocamlary.ml[793,16729+21])
                      Tstr_module
                      D/977
                        attribute "ocaml.doc"
                          [
                            structure_item (ocamlary.ml[792,16684+4]..[792,16684+44])
                              Pstr_eval
                              expression (ocamlary.ml[792,16684+4]..[792,16684+44])
                                Pexp_constant PConst_string(" @canonical Ocamlary.Aliases.Foo.D ",(ocamlary.ml[792,16684+4]..[792,16684+44]),None)
                          ]
                        module_expr (ocamlary.ml[793,16729+15]..ocamlary.ml[793,16729+21])
                          Tmod_ident "Foo__D/968"
                    structure_item (ocamlary.ml[795,16752+4]..ocamlary.ml[795,16752+21])
                      Tstr_module
                      E/978
                        module_expr (ocamlary.ml[795,16752+15]..ocamlary.ml[795,16752+21])
                          Tmod_ident "Foo__E/973"
                  ]
            structure_item (ocamlary.ml[798,16781+2]..ocamlary.ml[807,16905+5])
              Tstr_module
              Foo/985
                module_expr (ocamlary.ml[798,16781+15]..ocamlary.ml[807,16905+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[799,16803+4]..ocamlary.ml[799,16803+14])
                      Tstr_open Fresh
                      module_expr (ocamlary.ml[799,16803+9]..ocamlary.ml[799,16803+14])
                        Tmod_ident "Foo__/979"
                    structure_item (ocamlary.ml[801,16819+4]..ocamlary.ml[801,16819+16])
                      Tstr_module
                      A/980
                        module_expr (ocamlary.ml[801,16819+15]..ocamlary.ml[801,16819+16])
                          Tmod_ident "Foo__/979.A"
                    structure_item (ocamlary.ml[802,16836+4]..ocamlary.ml[802,16836+16])
                      Tstr_module
                      B/981
                        module_expr (ocamlary.ml[802,16836+15]..ocamlary.ml[802,16836+16])
                          Tmod_ident "Foo__/979.B"
                    structure_item (ocamlary.ml[803,16853+4]..ocamlary.ml[803,16853+16])
                      Tstr_module
                      C/982
                        module_expr (ocamlary.ml[803,16853+15]..ocamlary.ml[803,16853+16])
                          Tmod_ident "Foo__/979.C"
                    structure_item (ocamlary.ml[804,16870+4]..ocamlary.ml[804,16870+16])
                      Tstr_module
                      D/983
                        module_expr (ocamlary.ml[804,16870+15]..ocamlary.ml[804,16870+16])
                          Tmod_ident "Foo__/979.D"
                    structure_item (ocamlary.ml[806,16888+4]..ocamlary.ml[806,16888+16])
                      Tstr_module
                      E/984
                        module_expr (ocamlary.ml[806,16888+15]..ocamlary.ml[806,16888+16])
                          Tmod_ident "Foo__/979.E"
                  ]
            structure_item (ocamlary.ml[809,16912+2]..ocamlary.ml[809,16912+19])
              Tstr_module
              A'/986
                module_expr (ocamlary.ml[809,16912+14]..ocamlary.ml[809,16912+19])
                  Tmod_ident "Foo/985.A"
            structure_item (ocamlary.ml[811,16933+2]..ocamlary.ml[811,16933+21])
              Tstr_type Rec
              [
                type_declaration tata/987 (ocamlary.ml[811,16933+2]..ocamlary.ml[811,16933+21])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[811,16933+14]..ocamlary.ml[811,16933+21])
                        Ttyp_constr "Foo/985.A.t"
                        []
              ]
            structure_item (ocamlary.ml[812,16955+2]..ocamlary.ml[812,16955+23])
              Tstr_type Rec
              [
                type_declaration tbtb/988 (ocamlary.ml[812,16955+2]..ocamlary.ml[812,16955+23])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[812,16955+14]..ocamlary.ml[812,16955+23])
                        Ttyp_constr "Foo__/979.B.t"
                        []
              ]
            structure_item (ocamlary.ml[813,16979+2]..ocamlary.ml[813,16979+23])
              Tstr_type Rec
              [
                type_declaration tete/989 (ocamlary.ml[813,16979+2]..ocamlary.ml[813,16979+23])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[813,16979+14]..ocamlary.ml[813,16979+23])
                        Ttyp_constr "Foo__/979.E.t"
                        []
              ]
            structure_item (ocamlary.ml[814,17003+2]..ocamlary.ml[814,17003+19])
              Tstr_type Rec
              [
                type_declaration tata'/990 (ocamlary.ml[814,17003+2]..ocamlary.ml[814,17003+19])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[814,17003+15]..ocamlary.ml[814,17003+19])
                        Ttyp_constr "A'/986.t"
                        []
              ]
            structure_item (ocamlary.ml[815,17023+2]..ocamlary.ml[815,17023+22])
              Tstr_type Rec
              [
                type_declaration tete2/991 (ocamlary.ml[815,17023+2]..ocamlary.ml[815,17023+22])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[815,17023+15]..ocamlary.ml[815,17023+22])
                        Ttyp_constr "Foo/985.E.t"
                        []
              ]
            structure_item (ocamlary.ml[817,17047+2]..ocamlary.ml[823,17174+5])
              Tstr_module
              Std/997
                module_expr (ocamlary.ml[817,17047+15]..ocamlary.ml[823,17174+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[818,17069+4]..ocamlary.ml[818,17069+20])
                      Tstr_module
                      A/992
                        module_expr (ocamlary.ml[818,17069+15]..ocamlary.ml[818,17069+20])
                          Tmod_ident "Foo/985.A"
                    structure_item (ocamlary.ml[819,17090+4]..ocamlary.ml[819,17090+20])
                      Tstr_module
                      B/993
                        module_expr (ocamlary.ml[819,17090+15]..ocamlary.ml[819,17090+20])
                          Tmod_ident "Foo/985.B"
                    structure_item (ocamlary.ml[820,17111+4]..ocamlary.ml[820,17111+20])
                      Tstr_module
                      C/994
                        module_expr (ocamlary.ml[820,17111+15]..ocamlary.ml[820,17111+20])
                          Tmod_ident "Foo/985.C"
                    structure_item (ocamlary.ml[821,17132+4]..ocamlary.ml[821,17132+20])
                      Tstr_module
                      D/995
                        module_expr (ocamlary.ml[821,17132+15]..ocamlary.ml[821,17132+20])
                          Tmod_ident "Foo/985.D"
                    structure_item (ocamlary.ml[822,17153+4]..ocamlary.ml[822,17153+20])
                      Tstr_module
                      E/996
                        module_expr (ocamlary.ml[822,17153+15]..ocamlary.ml[822,17153+20])
                          Tmod_ident "Foo/985.E"
                  ]
            structure_item (ocamlary.ml[825,17181+2]..ocamlary.ml[825,17181+21])
              Tstr_type Rec
              [
                type_declaration stde/998 (ocamlary.ml[825,17181+2]..ocamlary.ml[825,17181+21])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[825,17181+14]..ocamlary.ml[825,17181+21])
                        Ttyp_constr "Std/997.E.t"
                        []
              ]
            structure_item (ocamlary.ml[827,17204+2]..ocamlary.ml[829,17230+72])
              Tstr_attribute "ocaml.text"
              [
                structure_item (ocamlary.ml[827,17204+2]..[829,17230+72])
                  Pstr_eval
                  expression (ocamlary.ml[827,17204+2]..[829,17230+72])
                    Pexp_constant PConst_string(" {3 include of Foo}\n\n      Just for giggle, let's see what happens when we include {!Foo}. ",(ocamlary.ml[827,17204+2]..[829,17230+72]),None)
              ]
            structure_item (ocamlary.ml[831,17304+2]..ocamlary.ml[831,17304+13])
              Tstr_include              attribute "ocaml.doc"
                  [
                    structure_item (ocamlary.ml[831,17304+14]..[831,17304+28])
                      Pstr_eval
                      expression (ocamlary.ml[831,17304+14]..[831,17304+28])
                        Pexp_constant PConst_string(" @inline ",(ocamlary.ml[831,17304+14]..[831,17304+28]),None)
                  ]
              module_expr (ocamlary.ml[831,17304+10]..ocamlary.ml[831,17304+13])
                Tmod_ident "Foo/985"
            structure_item (ocamlary.ml[833,17334+2]..ocamlary.ml[833,17334+18])
              Tstr_type Rec
              [
                type_declaration testa/1004 (ocamlary.ml[833,17334+2]..ocamlary.ml[833,17334+18])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[833,17334+15]..ocamlary.ml[833,17334+18])
                        Ttyp_constr "A/999.t"
                        []
              ]
            structure_item (ocamlary.ml[835,17354+2]..ocamlary.ml[835,17354+56])
              Tstr_attribute "ocaml.text"
              [
                structure_item (ocamlary.ml[835,17354+2]..[835,17354+56])
                  Pstr_eval
                  expression (ocamlary.ml[835,17354+2]..[835,17354+56])
                    Pexp_constant PConst_string(" And also, let's refer to {!A.t} and {!Foo.B.id} ",(ocamlary.ml[835,17354+2]..[835,17354+56]),None)
              ]
            structure_item (ocamlary.ml[837,17412+2]..ocamlary.ml[844,17540+5])
              Tstr_module
              P1/1010
                module_expr (ocamlary.ml[837,17412+14]..ocamlary.ml[844,17540+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[839,17477+4]..ocamlary.ml[843,17532+7])
                      Tstr_module
                      Y/1009
                        attribute "ocaml.doc"
                          [
                            structure_item (ocamlary.ml[838,17433+4]..[838,17433+43])
                              Pstr_eval
                              expression (ocamlary.ml[838,17433+4]..[838,17433+43])
                                Pexp_constant PConst_string(" @canonical Ocamlary.Aliases.P2.Z ",(ocamlary.ml[838,17433+4]..[838,17433+43]),None)
                          ]
                        module_expr (ocamlary.ml[839,17477+15]..ocamlary.ml[843,17532+7])
                          Tmod_structure
                          [
                            structure_item (ocamlary.ml[840,17499+6]..ocamlary.ml[840,17499+12])
                              Tstr_type Rec
                              [
                                type_declaration t/1005 (ocamlary.ml[840,17499+6]..ocamlary.ml[840,17499+12])
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
                            structure_item (ocamlary.ml[842,17513+6]..ocamlary.ml[842,17513+18])
                              Tstr_value Nonrec
                              [
                                <def>
                                  pattern (ocamlary.ml[842,17513+10]..ocamlary.ml[842,17513+12])
                                    Tpat_var "id/1006"
                                  expression (ocamlary.ml[842,17513+13]..ocamlary.ml[842,17513+18]) ghost
                                    Texp_function
                                    Nolabel
                                    [
                                      <case>
                                        pattern (ocamlary.ml[842,17513+13]..ocamlary.ml[842,17513+14])
                                          Tpat_var "x/1008"
                                        expression (ocamlary.ml[842,17513+17]..ocamlary.ml[842,17513+18])
                                          Texp_ident "x/1008"
                                    ]
                              ]
                          ]
                  ]
            structure_item (ocamlary.ml[846,17547+2]..ocamlary.ml[848,17588+5])
              Tstr_module
              P2/1012
                module_expr (ocamlary.ml[846,17547+14]..ocamlary.ml[848,17588+5])
                  Tmod_structure
                  [
                    structure_item (ocamlary.ml[847,17568+4]..ocamlary.ml[847,17568+19])
                      Tstr_module
                      Z/1011
                        module_expr (ocamlary.ml[847,17568+15]..ocamlary.ml[847,17568+19])
                          Tmod_ident "P1/1010.Y"
                  ]
            structure_item (ocamlary.ml[850,17595+2]..ocamlary.ml[850,17595+18])
              Tstr_module
              X1/1013
                module_expr (ocamlary.ml[850,17595+14]..ocamlary.ml[850,17595+18])
                  Tmod_ident "P1/1010.Y"
            structure_item (ocamlary.ml[851,17614+2]..ocamlary.ml[851,17614+18])
              Tstr_module
              X2/1014
                module_expr (ocamlary.ml[851,17614+14]..ocamlary.ml[851,17614+18])
                  Tmod_ident "P2/1012.Z"
            structure_item (ocamlary.ml[853,17634+2]..ocamlary.ml[853,17634+16])
              Tstr_type Rec
              [
                type_declaration p1/1015 (ocamlary.ml[853,17634+2]..ocamlary.ml[853,17634+16])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[853,17634+12]..ocamlary.ml[853,17634+16])
                        Ttyp_constr "X1/1013.t"
                        []
              ]
            structure_item (ocamlary.ml[854,17651+2]..ocamlary.ml[854,17651+16])
              Tstr_type Rec
              [
                type_declaration p2/1018 (ocamlary.ml[854,17651+2]..ocamlary.ml[854,17651+16])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[854,17651+12]..ocamlary.ml[854,17651+16])
                        Ttyp_constr "X2/1014.t"
                        []
              ]
          ]
    structure_item (ocamlary.ml[857,17673+0]..ocamlary.ml[857,17673+31])
      Tstr_attribute "ocaml.text"
      [
        structure_item (ocamlary.ml[857,17673+0]..[857,17673+31])
          Pstr_eval
          expression (ocamlary.ml[857,17673+0]..[857,17673+31])
            Pexp_constant PConst_string(" {1 New reference syntax} ",(ocamlary.ml[857,17673+0]..[857,17673+31]),None)
      ]
    structure_item (ocamlary.ml[859,17706+0]..ocamlary.ml[861,17735+3])
      Tstr_modtype "M/1021"
        module_type (ocamlary.ml[859,17706+16]..ocamlary.ml[861,17735+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[860,17726+2]..ocamlary.ml[860,17726+8])
              Tsig_type Rec
              [
                type_declaration t/1020 (ocamlary.ml[860,17726+2]..ocamlary.ml[860,17726+8])
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
          ]
    structure_item (ocamlary.ml[863,17740+0]..ocamlary.ml[865,17767+3])
      Tstr_module
      M/1023
        module_expr (ocamlary.ml[863,17740+11]..ocamlary.ml[865,17767+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[864,17758+2]..ocamlary.ml[864,17758+8])
              Tstr_type Rec
              [
                type_declaration t/1022 (ocamlary.ml[864,17758+2]..ocamlary.ml[864,17758+8])
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
          ]
    structure_item (ocamlary.ml[867,17772+0]..ocamlary.ml[870,17849+50])
      Tstr_attribute "ocaml.text"
      [
        structure_item (ocamlary.ml[867,17772+0]..[870,17849+50])
          Pstr_eval
          expression (ocamlary.ml[867,17772+0]..[870,17849+50])
            Pexp_constant PConst_string(" Here goes:\n    - [{!M.t}] : {!M.t}\n    - [{!module-M.t}] : {!module-M.t}\n    - [{!module-type-M.t}] : {!module-type-M.t} ",(ocamlary.ml[867,17772+0]..[870,17849+50]),None)
      ]
    structure_item (ocamlary.ml[872,17901+0]..ocamlary.ml[874,17940+3])
      Tstr_module
      Only_a_module/1025
        module_expr (ocamlary.ml[872,17901+23]..ocamlary.ml[874,17940+3])
          Tmod_structure
          [
            structure_item (ocamlary.ml[873,17931+2]..ocamlary.ml[873,17931+8])
              Tstr_type Rec
              [
                type_declaration t/1024 (ocamlary.ml[873,17931+2]..ocamlary.ml[873,17931+8])
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
          ]
    structure_item (ocamlary.ml[876,17945+0]..ocamlary.ml[879,18082+74])
      Tstr_attribute "ocaml.text"
      [
        structure_item (ocamlary.ml[876,17945+0]..[879,18082+74])
          Pstr_eval
          expression (ocamlary.ml[876,17945+0]..[879,18082+74])
            Pexp_constant PConst_string(" Some here should fail:\n    - [{!Only_a_module.t}] : {!Only_a_module.t}\n    - [{!module-Only_a_module.t}] : {!module-Only_a_module.t}\n    - [{!module-type-Only_a_module.t}] : {!module-type-Only_a_module.t} ",(ocamlary.ml[876,17945+0]..[879,18082+74]),None)
      ]
    structure_item (ocamlary.ml[881,18158+0]..ocamlary.ml[887,18234+3])
      Tstr_modtype "TypeExt/1029"
        module_type (ocamlary.ml[881,18158+22]..ocamlary.ml[887,18234+3])
          Tmty_signature
          [
            signature_item (ocamlary.ml[882,18184+2]..ocamlary.ml[882,18184+13])
              Tsig_type Rec
              [
                type_declaration t/1026 (ocamlary.ml[882,18184+2]..ocamlary.ml[882,18184+13])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_open
                  ptype_private = Public
                  ptype_manifest =
                    None
              ]
            signature_item (ocamlary.ml[884,18199+2]..ocamlary.ml[884,18199+13])
              Tsig_typext
              type_extension
                ptyext_path = "t/1026"
                ptyext_params =
                  []
                ptyext_constructors =
                  [
                    extension_constructor (ocamlary.ml[884,18199+12]..ocamlary.ml[884,18199+13])
                      pext_name = "C/1027"
                      pext_kind =
                        Text_decl
                          []
                          None
                  ]
                ptyext_private = Public
            signature_item (ocamlary.ml[886,18214+2]..ocamlary.ml[886,18214+19])
              Tsig_value
              value_description f/1028 (ocamlary.ml[886,18214+2]..ocamlary.ml[886,18214+19])
                core_type (ocamlary.ml[886,18214+10]..ocamlary.ml[886,18214+19])
                  Ttyp_arrow
                  Nolabel
                  core_type (ocamlary.ml[886,18214+10]..ocamlary.ml[886,18214+11])
                    Ttyp_constr "t/1026"
                    []
                  core_type (ocamlary.ml[886,18214+15]..ocamlary.ml[886,18214+19])
                    Ttyp_constr "unit/6!"
                    []
                []
          ]
    structure_item (ocamlary.ml[889,18239+0]..ocamlary.ml[889,18239+15])
      Tstr_type Rec
      [
        type_declaration new_t/1030 (ocamlary.ml[889,18239+0]..ocamlary.ml[889,18239+15])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_open
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (ocamlary.ml[891,18256+0]..ocamlary.ml[891,18256+15])
      Tstr_typext
      type_extension
        ptyext_path = "new_t/1030"
        ptyext_params =
          []
        ptyext_constructors =
          [
            extension_constructor (ocamlary.ml[891,18256+14]..ocamlary.ml[891,18256+15])
              pext_name = "C/1031"
              pext_kind =
                Text_decl
                  []
                  None
          ]
        ptyext_private = Public
    structure_item (ocamlary.ml[893,18273+0]..ocamlary.ml[893,18273+56])
      Tstr_modtype "TypeExtPruned/1037"
        module_type (ocamlary.ml[893,18273+28]..ocamlary.ml[893,18273+56])
          Tmty_with
          module_type (ocamlary.ml[893,18273+28]..ocamlary.ml[893,18273+35])
            Tmty_ident "TypeExt/1029"
          [
            "t/1026"
              Twith_typesubst
                type_declaration t/1026 (ocamlary.ml[893,18273+41]..ocamlary.ml[893,18273+56])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (ocamlary.ml[893,18273+51]..ocamlary.ml[893,18273+56])
                        Ttyp_constr "new_t/1030"
                        []
          ]
  ]
  
  $ odoc compile --source-name ocamlary.ml --source-parent-file src-source.odoc -I . ocamlary.cmti
  Shape: {<Ocamlary>
          "!"[value] -> <Ocamlary.48>;
          "#empty_class"[type] -> <Ocamlary.195>;
          "#one_method_class"[type] -> <Ocamlary.198>;
          "#param_class"[type] -> <Ocamlary.208>;
          "#two_method_class"[type] -> <Ocamlary.202>;
          "$"[value] -> <Ocamlary.50>;
          "%"[value] -> <Ocamlary.51>;
          "&"[value] -> <Ocamlary.52>;
          "*"[value] -> <Ocamlary.53>;
          "+"[value] -> <Ocamlary.55>;
          "-"[value] -> <Ocamlary.54>;
          "-?"[value] -> <Ocamlary.56>;
          "/"[value] -> <Ocamlary.57>;
          ":="[value] -> <Ocamlary.58>;
          "="[value] -> <Ocamlary.59>;
          "@"[value] -> <Ocamlary.49>;
          "A"[module type] -> <Ocamlary.91>;
          "Aliases"[module] ->
              {<Ocamlary.426>
               "A"[module] ->
                   {<Ocamlary.372>
                    "id"[value] -> <Ocamlary.370>;
                    "t"[type] -> <Ocamlary.369>;
                    };
               "A'"[module] ->
                   {<Ocamlary.372>
                    "id"[value] -> <Ocamlary.370>;
                    "t"[type] -> <Ocamlary.369>;
                    };
               "B"[module] ->
                   {<Ocamlary.376>
                    "id"[value] -> <Ocamlary.374>;
                    "t"[type] -> <Ocamlary.373>;
                    };
               "C"[module] ->
                   {<Ocamlary.380>
                    "id"[value] -> <Ocamlary.378>;
                    "t"[type] -> <Ocamlary.377>;
                    };
               "D"[module] ->
                   {<Ocamlary.384>
                    "id"[value] -> <Ocamlary.382>;
                    "t"[type] -> <Ocamlary.381>;
                    };
               "E"[module] ->
                   {<Ocamlary.388>
                    "id"[value] -> <Ocamlary.386>;
                    "t"[type] -> <Ocamlary.385>;
                    };
               "Foo"[module] ->
                   {<Ocamlary.400>
                    "A"[module] ->
                        {<Ocamlary.372>
                         "id"[value] -> <Ocamlary.370>;
                         "t"[type] -> <Ocamlary.369>;
                         };
                    "B"[module] ->
                        {<Ocamlary.376>
                         "id"[value] -> <Ocamlary.374>;
                         "t"[type] -> <Ocamlary.373>;
                         };
                    "C"[module] ->
                        {<Ocamlary.380>
                         "id"[value] -> <Ocamlary.378>;
                         "t"[type] -> <Ocamlary.377>;
                         };
                    "D"[module] ->
                        {<Ocamlary.384>
                         "id"[value] -> <Ocamlary.382>;
                         "t"[type] -> <Ocamlary.381>;
                         };
                    "E"[module] ->
                        {<Ocamlary.388>
                         "id"[value] -> <Ocamlary.386>;
                         "t"[type] -> <Ocamlary.385>;
                         };
                    };
               "Foo__"[module] ->
                   {<Ocamlary.394>
                    "A"[module] ->
                        {<Ocamlary.372>
                         "id"[value] -> <Ocamlary.370>;
                         "t"[type] -> <Ocamlary.369>;
                         };
                    "B"[module] ->
                        {<Ocamlary.376>
                         "id"[value] -> <Ocamlary.374>;
                         "t"[type] -> <Ocamlary.373>;
                         };
                    "C"[module] ->
                        {<Ocamlary.380>
                         "id"[value] -> <Ocamlary.378>;
                         "t"[type] -> <Ocamlary.377>;
                         };
                    "D"[module] ->
                        {<Ocamlary.384>
                         "id"[value] -> <Ocamlary.382>;
                         "t"[type] -> <Ocamlary.381>;
                         };
                    "E"[module] ->
                        {<Ocamlary.388>
                         "id"[value] -> <Ocamlary.386>;
                         "t"[type] -> <Ocamlary.385>;
                         };
                    };
               "Foo__A"[module] ->
                   {<Ocamlary.372>
                    "id"[value] -> <Ocamlary.370>;
                    "t"[type] -> <Ocamlary.369>;
                    };
               "Foo__B"[module] ->
                   {<Ocamlary.376>
                    "id"[value] -> <Ocamlary.374>;
                    "t"[type] -> <Ocamlary.373>;
                    };
               "Foo__C"[module] ->
                   {<Ocamlary.380>
                    "id"[value] -> <Ocamlary.378>;
                    "t"[type] -> <Ocamlary.377>;
                    };
               "Foo__D"[module] ->
                   {<Ocamlary.384>
                    "id"[value] -> <Ocamlary.382>;
                    "t"[type] -> <Ocamlary.381>;
                    };
               "Foo__E"[module] ->
                   {<Ocamlary.388>
                    "id"[value] -> <Ocamlary.386>;
                    "t"[type] -> <Ocamlary.385>;
                    };
               "P1"[module] ->
                   {<Ocamlary.419>
                    "Y"[module] ->
                        {<Ocamlary.418>
                         "id"[value] -> <Ocamlary.416>;
                         "t"[type] -> <Ocamlary.415>;
                         };
                    };
               "P2"[module] ->
                   {<Ocamlary.421>
                    "Z"[module] ->
                        {<Ocamlary.418>
                         "id"[value] -> <Ocamlary.416>;
                         "t"[type] -> <Ocamlary.415>;
                         };
                    };
               "Std"[module] ->
                   {<Ocamlary.412>
                    "A"[module] ->
                        {<Ocamlary.372>
                         "id"[value] -> <Ocamlary.370>;
                         "t"[type] -> <Ocamlary.369>;
                         };
                    "B"[module] ->
                        {<Ocamlary.376>
                         "id"[value] -> <Ocamlary.374>;
                         "t"[type] -> <Ocamlary.373>;
                         };
                    "C"[module] ->
                        {<Ocamlary.380>
                         "id"[value] -> <Ocamlary.378>;
                         "t"[type] -> <Ocamlary.377>;
                         };
                    "D"[module] ->
                        {<Ocamlary.384>
                         "id"[value] -> <Ocamlary.382>;
                         "t"[type] -> <Ocamlary.381>;
                         };
                    "E"[module] ->
                        {<Ocamlary.388>
                         "id"[value] -> <Ocamlary.386>;
                         "t"[type] -> <Ocamlary.385>;
                         };
                    };
               "X1"[module] ->
                   {<Ocamlary.418>
                    "id"[value] -> <Ocamlary.416>;
                    "t"[type] -> <Ocamlary.415>;
                    };
               "X2"[module] ->
                   {<Ocamlary.418>
                    "id"[value] -> <Ocamlary.416>;
                    "t"[type] -> <Ocamlary.415>;
                    };
               "p1"[type] -> <Ocamlary.424>;
               "p2"[type] -> <Ocamlary.425>;
               "stde"[type] -> <Ocamlary.413>;
               "tata"[type] -> <Ocamlary.402>;
               "tata'"[type] -> <Ocamlary.405>;
               "tbtb"[type] -> <Ocamlary.403>;
               "testa"[type] -> <Ocamlary.414>;
               "tete"[type] -> <Ocamlary.404>;
               "tete2"[type] -> <Ocamlary.406>;
               };
          "B"[module type] -> <Ocamlary.94>;
          "Bar"[extension constructor] -> <Ocamlary.186>;
          "Buffer"[module] -> {<Ocamlary.28>
                               "f"[value] -> <Ocamlary.27>;
                               };
          "C"[module type] -> <Ocamlary.96>;
          "C"[extension constructor] -> <Ocamlary.438>;
          "COLLECTION"[module type] -> <Ocamlary.71>;
          "CanonicalTest"[module] ->
              {<Ocamlary.368>
               "Base"[module] ->
                   {<Ocamlary.358>
                    "List"[module] ->
                        {<Ocamlary.354>
                         "id"[value] -> <Ocamlary.352>;
                         "t"[type] -> <Ocamlary.351>;
                         };
                    };
               "Base_Tests"[module] ->
                   {<Ocamlary.366>
                    "C"[module] ->
                        {<Ocamlary.359>
                         "id"[value] -> <Ocamlary.352>;
                         "t"[type] -> <Ocamlary.351>;
                         };
                    "L"[module] ->
                        {<Ocamlary.354>
                         "id"[value] -> <Ocamlary.352>;
                         "t"[type] -> <Ocamlary.351>;
                         };
                    "bar"[value] -> <Ocamlary.363>;
                    "baz"[value] -> <Ocamlary.365>;
                    "foo"[value] -> <Ocamlary.361>;
                    };
               "Base__"[module] ->
                   {<Ocamlary.356>
                    "List"[module] ->
                        {<Ocamlary.354>
                         "id"[value] -> <Ocamlary.352>;
                         "t"[type] -> <Ocamlary.351>;
                         };
                    };
               "Base__List"[module] ->
                   {<Ocamlary.354>
                    "id"[value] -> <Ocamlary.352>;
                    "t"[type] -> <Ocamlary.351>;
                    };
               "List_modif"[module] ->
                   {<Ocamlary.367>
                    "id"[value] -> <Ocamlary.352>;
                    "t"[type] -> <Ocamlary.351>;
                    };
               };
          "CollectionModule"[module] ->
              {<Ocamlary.70>
               "InnerModuleA"[module] ->
                   {<Ocamlary.68>
                    "InnerModuleA'"[module] ->
                        {<Ocamlary.65>
                         "t"[type] -> <Ocamlary.64>;
                         };
                    "InnerModuleTypeA'"[module type] -> <Ocamlary.67>;
                    "t"[type] -> <Ocamlary.63>;
                    };
               "InnerModuleTypeA"[module type] -> <Ocamlary.69>;
               "collection"[type] -> <Ocamlary.61>;
               "element"[type] -> <Ocamlary.62>;
               };
          "Dep1"[module] ->
              {<Ocamlary.223>
               "S"[module type] -> <Ocamlary.216>;
               "X"[module] ->
                   {<Ocamlary.222>
                    "Y"[module] ->
                        {<Ocamlary.221>
                         "#c"[type] -> <Ocamlary.217>;
                         "c"[type] -> <Ocamlary.217>;
                         "c"[class] -> <Ocamlary.217>;
                         "c"[class type] -> <Ocamlary.217>;
                         };
                    };
               };
          "Dep10"[module type] -> <Ocamlary.280>;
          "Dep11"[module] ->
              {<Ocamlary.283>
               "S"[module type] -> <Ocamlary.282>;
               };
          "Dep12"[module] ->
              Abs<Ocamlary.287>(Arg/761, {
                                          "T"[module type] -> <Ocamlary.286>;
                                          });
          "Dep13"[module] ->
              {<Ocamlary.292>
               "#c"[type] -> <Ocamlary.288>;
               "c"[type] -> <Ocamlary.288>;
               "c"[class] -> <Ocamlary.288>;
               "c"[class type] -> <Ocamlary.288>;
               };
          "Dep2"[module] ->
              Abs<Ocamlary.230>
                 (Arg/626,
                  {
                   "A"[module] ->
                       (Arg/626<Ocamlary.227> . "X"[module])<Ocamlary.228>;
                   "B"[module] ->
                       ((Arg/626<Ocamlary.227> . "X"[module])<Ocamlary.228> .
                       "Y"[module])<Ocamlary.229>;
                   });
          "Dep3"[module] -> {<Ocamlary.233>
                             "a"[type] -> <Ocamlary.232>;
                             };
          "Dep4"[module] ->
              {<Ocamlary.241>
               "S"[module type] -> <Ocamlary.238>;
               "T"[module type] -> <Ocamlary.235>;
               "X"[module] -> {<Ocamlary.240>
                               "b"[type] -> <Ocamlary.239>;
                               };
               };
          "Dep5"[module] ->
              Abs<Ocamlary.251>
                 (Arg/669,
                  {
                   "Z"[module] ->
                       {<Ocamlary.250>
                        "X"[module] ->
                            (Arg/669<Ocamlary.247> . "X"[module])<Ocamlary.248>;
                        "Y"[module] ->
                            {<Ocamlary.233>
                             "a"[type] -> <Ocamlary.232>;
                             };
                        };
                   });
          "Dep6"[module] ->
              {<Ocamlary.263>
               "S"[module type] -> <Ocamlary.255>;
               "T"[module type] -> <Ocamlary.258>;
               "X"[module] ->
                   {<Ocamlary.262>
                    "R"[module type] -> <Ocamlary.259>;
                    "Y"[module] ->
                        {<Ocamlary.261>
                         "d"[type] -> <Ocamlary.260>;
                         };
                    };
               };
          "Dep7"[module] ->
              Abs<Ocamlary.271>
                 (Arg/715,
                  {
                   "M"[module] ->
                       (Arg/715<Ocamlary.269> . "X"[module])<Ocamlary.270>;
                   });
          "Dep8"[module] ->
              {<Ocamlary.275>
               "T"[module type] -> <Ocamlary.274>;
               };
          "Dep9"[module] -> Abs<Ocamlary.278>(X/745, X/745<Ocamlary.277>);
          "DoubleInclude1"[module] ->
              {<Ocamlary.343>
               "DoubleInclude2"[module] ->
                   {<Ocamlary.342>
                    "double_include"[type] -> <Ocamlary.341>;
                    };
               };
          "DoubleInclude3"[module] ->
              {<Ocamlary.344>
               "DoubleInclude2"[module] ->
                   {<Ocamlary.342>
                    "double_include"[type] -> <Ocamlary.341>;
                    };
               };
          "Empty"[module] -> {<Ocamlary.4>
                              };
          "Empty"[module type] -> <Ocamlary.1>;
          "EmptyAlias"[module] -> {<Ocamlary.4>
                                   };
          "EmptySig"[module type] -> <Ocamlary.6>;
          "EmptySig"[extension constructor] -> <Ocamlary.32>;
          "EmptySigAlias"[module type] -> <Ocamlary.7>;
          "EmptySigAlias"[extension constructor] -> <Ocamlary.33>;
          "Exn_arrow"[extension constructor] -> <Ocamlary.164>;
          "ExtA"[extension constructor] -> <Ocamlary.178>;
          "ExtB"[extension constructor] -> <Ocamlary.179>;
          "ExtC"[extension constructor] -> <Ocamlary.180>;
          "ExtD"[extension constructor] -> <Ocamlary.181>;
          "ExtE"[extension constructor] -> <Ocamlary.182>;
          "ExtF"[extension constructor] -> <Ocamlary.183>;
          "ExtMod"[module] ->
              {<Ocamlary.190>
               "Leisureforce"[extension constructor] -> <Ocamlary.189>;
               "t"[type] -> <Ocamlary.188>;
               };
          "Foo"[extension constructor] -> <Ocamlary.185>;
          "FunctorTypeOf"[module] ->
              Abs<Ocamlary.99>(Collection/460, {
                                                "t"[type] -> <Ocamlary.98>;
                                                });
          "IncludeInclude1"[module] ->
              {<Ocamlary.348>
               "IncludeInclude2"[module type] -> <Ocamlary.346>;
               "IncludeInclude2_M"[module] -> {<Ocamlary.347>
                                               };
               };
          "IncludeInclude2"[module type] -> <Ocamlary.346>;
          "IncludeInclude2_M"[module] -> {<Ocamlary.347>
                                          };
          "IncludeModuleType"[module type] -> <Ocamlary.100>;
          "IncludedA"[module] -> {<Ocamlary.107>
                                  "t"[type] -> <Ocamlary.106>;
                                  };
          "IncludedB"[module type] -> <Ocamlary.109>;
          "Kablam"[extension constructor] -> <Ocamlary.30>;
          "Kaboom"[extension constructor] -> <Ocamlary.29>;
          "Kapow"[extension constructor] -> <Ocamlary.31>;
          "M"[module] -> {<Ocamlary.430>
                          "t"[type] -> <Ocamlary.429>;
                          };
          "M"[module type] -> <Ocamlary.428>;
          "MMM"[module type] -> <Ocamlary.86>;
          "MissingComment"[module type] -> <Ocamlary.3>;
          "ModuleWithSignature"[module] -> {<Ocamlary.8>
                                            };
          "ModuleWithSignatureAlias"[module] -> {<Ocamlary.9>
                                                 };
          "NestedInclude1"[module type] -> <Ocamlary.337>;
          "NestedInclude2"[module type] -> <Ocamlary.339>;
          "One"[module] -> {<Ocamlary.11>
                            "one"[type] -> <Ocamlary.10>;
                            };
          "Only_a_module"[module] ->
              {<Ocamlary.432>
               "t"[type] -> <Ocamlary.431>;
               };
          "Quux"[extension constructor] -> <Ocamlary.187>;
          "RECOLLECTION"[module type] -> <Ocamlary.87>;
          "Recollection"[module] ->
              Abs<Ocamlary.84>
                 (C/352,
                  {
                   "InnerModuleA"[module] ->
                       {<Ocamlary.80>
                        "InnerModuleA'"[module] ->
                            {<Ocamlary.77>
                             "t"[type] -> <Ocamlary.76>;
                             };
                        "InnerModuleTypeA'"[module type] -> <Ocamlary.79>;
                        "t"[type] -> <Ocamlary.75>;
                        };
                   "InnerModuleTypeA"[module type] -> <Ocamlary.81>;
                   "collection"[type] -> <Ocamlary.73>;
                   "element"[type] -> <Ocamlary.74>;
                   });
          "RecollectionModule"[module type] -> <Ocamlary.88>;
          "SigForMod"[module type] -> <Ocamlary.14>;
          "SuperSig"[module type] -> <Ocamlary.26>;
          "ToInclude"[module type] -> <Ocamlary.105>;
          "TypeExt"[module type] -> <Ocamlary.436>;
          "TypeExtPruned"[module type] -> <Ocamlary.440>;
          "With1"[module type] -> <Ocamlary.297>;
          "With10"[module] ->
              {<Ocamlary.332>
               "T"[module type] -> <Ocamlary.331>;
               };
          "With11"[module type] -> <Ocamlary.334>;
          "With2"[module] ->
              {<Ocamlary.300>
               "S"[module type] -> <Ocamlary.299>;
               };
          "With3"[module] ->
              {<Ocamlary.304>
               "M"[module] ->
                   {<Ocamlary.300>
                    "S"[module type] -> <Ocamlary.299>;
                    };
               "N"[module] -> {<Ocamlary.303>
                               "t"[type] -> <Ocamlary.302>;
                               };
               };
          "With4"[module] ->
              {<Ocamlary.308>
               "N"[module] -> {<Ocamlary.307>
                               "t"[type] -> <Ocamlary.306>;
                               };
               };
          "With5"[module] ->
              {<Ocamlary.314>
               "N"[module] -> {<Ocamlary.313>
                               "t"[type] -> <Ocamlary.312>;
                               };
               "S"[module type] -> <Ocamlary.311>;
               };
          "With6"[module] ->
              {<Ocamlary.319>
               "T"[module type] -> <Ocamlary.318>;
               };
          "With7"[module] -> Abs<Ocamlary.322>(X/803, X/803<Ocamlary.321>);
          "With8"[module type] -> <Ocamlary.324>;
          "With9"[module] ->
              {<Ocamlary.327>
               "S"[module type] -> <Ocamlary.326>;
               };
          "ZzzTop"[extension constructor] -> <Ocamlary.192>;
          "ZzzTop0"[extension constructor] -> <Ocamlary.191>;
          "a_function"[value] -> <Ocamlary.35>;
          "a_function"[type] -> <Ocamlary.34>;
          "alias"[type] -> <Ocamlary.134>;
          "any_obj"[type] -> <Ocamlary.174>;
          "bin_poly_poly_variant"[type] -> <Ocamlary.146>;
          "changing"[value] -> <Ocamlary.46>;
          "clopen_poly_variant"[type] -> <Ocamlary.153>;
          "closed_poly_variant"[type] -> <Ocamlary.152>;
          "dep1"[type] -> <Ocamlary.231>;
          "dep2"[type] -> <Ocamlary.252>;
          "dep3"[type] -> <Ocamlary.253>;
          "dep4"[type] -> <Ocamlary.272>;
          "dep5"[type] -> <Ocamlary.293>;
          "double_include"[type] -> <Ocamlary.341>;
          "empty_class"[type] -> <Ocamlary.195>;
          "empty_class"[class] -> <Ocamlary.195>;
          "empty_class"[class type] -> <Ocamlary.195>;
          "empty_obj"[type] -> <Ocamlary.175>;
          "ext"[type] -> <Ocamlary.177>;
          "full_gadt"[type] -> <Ocamlary.125>;
          "full_gadt_alias"[type] -> <Ocamlary.155>;
          "fun_fun_fun"[value] -> <Ocamlary.37>;
          "fun_maybe"[value] -> <Ocamlary.39>;
          "include_include"[type] -> <Ocamlary.349>;
          "kaboom"[value] -> <Ocamlary.41>;
          "land"[value] -> <Ocamlary.60>;
          "launch_missiles"[value] -> <Ocamlary.193>;
          "mutable_record"[type] -> <Ocamlary.113>;
          "mutual_constr_a"[type] -> <Ocamlary.165>;
          "mutual_constr_b"[type] -> <Ocamlary.166>;
          "my_mod"[type] -> <Ocamlary.194>;
          "my_unit_class"[type] -> <Ocamlary.214>;
          "my_unit_object"[type] -> <Ocamlary.213>;
          "nested_include"[type] -> <Ocamlary.340>;
          "nested_poly_variant"[type] -> <Ocamlary.154>;
          "new_t"[type] -> <Ocamlary.437>;
          "not_found"[value] -> <Ocamlary.40>;
          "ocaml_org"[value] -> <Ocamlary.42>;
          "one_meth"[type] -> <Ocamlary.176>;
          "one_method_class"[type] -> <Ocamlary.198>;
          "one_method_class"[class] -> <Ocamlary.198>;
          "one_method_class"[class type] -> <Ocamlary.198>;
          "oof"[type] -> <Ocamlary.173>;
          "open_obj"[type] -> <Ocamlary.172>;
          "open_poly_variant"[type] -> <Ocamlary.147>;
          "open_poly_variant2"[type] -> <Ocamlary.148>;
          "open_poly_variant_alias"[type] -> <Ocamlary.149>;
          "param_class"[type] -> <Ocamlary.208>;
          "param_class"[class] -> <Ocamlary.208>;
          "param_class"[class type] -> <Ocamlary.208>;
          "partial_gadt"[type] -> <Ocamlary.130>;
          "partial_gadt_alias"[type] -> <Ocamlary.160>;
          "poly_ext"[type] -> <Ocamlary.184>;
          "poly_fun"[type] -> <Ocamlary.150>;
          "poly_fun_constraint"[type] -> <Ocamlary.151>;
          "poly_poly_variant"[type] -> <Ocamlary.145>;
          "poly_variant"[type] -> <Ocamlary.124>;
          "poly_variant_union"[type] -> <Ocamlary.144>;
          "rec_obj"[type] -> <Ocamlary.171>;
          "record"[type] -> <Ocamlary.110>;
          "record_alias"[type] -> <Ocamlary.141>;
          "since_mesozoic"[value] -> <Ocamlary.45>;
          "some_doc"[value] -> <Ocamlary.44>;
          "some_file"[value] -> <Ocamlary.43>;
          "tuple"[type] -> <Ocamlary.135>;
          "two_method_class"[type] -> <Ocamlary.202>;
          "two_method_class"[class] -> <Ocamlary.202>;
          "two_method_class"[class type] -> <Ocamlary.202>;
          "universe_record"[type] -> <Ocamlary.117>;
          "variant"[type] -> <Ocamlary.119>;
          "variant_alias"[type] -> <Ocamlary.136>;
          "with1"[type] -> <Ocamlary.305>;
          "with2"[type] -> <Ocamlary.309>;
          "~-"[value] -> <Ocamlary.47>;
          }
  
  Struct
  Adding a 'Def' for 'def-154' at loc (9374,9480)
  Adding a 'Def' for 'def-393' at loc (16756,16773)
  Adding a 'Def' for 'def-286' at loc (13385,13406)
  Adding a 'Def' for 'def-235' at loc (11968,11998)
  Adding a 'Def' for 'def-382' at loc (16378,16380)
  Adding a 'Def' for 'def-331' at loc (14292,14383)
  Adding a 'Def' for 'def-380' at loc (16274,16331)
  Adding a 'Def' for 'def-356' at loc (15180,15292)
  Adding a 'Def' for 'def-350' at loc (15045,15068)
  Adding a 'Def' for 'def-347' at loc (14954,14993)
  Adding a 'Def' for 'def-252' at loc (12474,12502)
  Adding a 'Def' for 'def-29' at loc (2222,2246)
  Adding a 'Def' for 'def-257' at loc (12636,12648)
  Adding a 'Def' for 'def-433' at loc (18186,18197)
  Adding a 'Def' for 'def-81' at loc (5449,5510)
  Adding a 'Def' for 'def-24' at loc (2052,2086)
  Adding a 'Def' for 'def-214' at loc (11425,11472)
  Adding a 'Def' for 'def-45' at loc (3416,3430)
  Adding a 'Def' for 'def-144' at loc (8611,8663)
  Adding a 'Def' for 'def-30' at loc (2284,2315)
  Adding a 'Def' for 'def-150' at loc (9115,9168)
  Adding a 'Def' for 'def-41' at loc (3053,3059)
  Adding a 'Def' for 'def-366' at loc (15356,15826)
  Adding a 'Def' for 'def-34' at loc (2731,2765)
  Adding a 'Def' for 'def-376' at loc (16213,16270)
  Adding a 'Def' for 'def-329' at loc (14316,14358)
  Adding a 'Def' for 'def-166' at loc (10169,10221)
  Adding a 'Def' for 'def-295' at loc (13528,13566)
  Adding a 'Def' for 'def-147' at loc (8936,8981)
  Adding a 'Def' for 'def-328' at loc (14337,14350)
  Adding a 'Def' for 'def-365' at loc (15789,15792)
  Adding a 'Def' for 'def-388' at loc (16396,16453)
  Adding a 'Def' for 'def-255' at loc (12557,12587)
  Adding a 'Def' for 'def-370' at loc (16195,16197)
  Adding a 'Def' for 'def-176' at loc (10442,10472)
  Adding a 'Def' for 'def-190' at loc (10789,10855)
  Adding a 'Def' for 'def-42' at loc (3141,3150)
  Adding a 'Def' for 'def-231' at loc (11882,11908)
  Adding a 'Def' for 'def-171' at loc (10223,10280)
  Adding a 'Def' for 'def-100' at loc (6363,6480)
  Adding a 'Def' for 'def-59' at loc (3823,3828)
  Adding a 'Def' for 'def-299' at loc (13613,13643)
  Adding a 'Def' for 'def-367' at loc (15830,15884)
  Adding a 'Def' for 'def-294' at loc (13547,13560)
  Adding a 'Def' for 'def-77' at loc (5107,5220)
  Adding a 'Def' for 'def-50' at loc (3679,3684)
  Adding a 'Def' for 'def-313' at loc (13913,13949)
  Adding a 'Def' for 'def-306' at loc (13808,13820)
  Adding a 'Def' for 'def-254' at loc (12577,12583)
  Adding a 'Def' for 'def-52' at loc (3711,3716)
  Adding a 'Def' for 'def-298' at loc (13633,13639)
  Adding a 'Def' for 'def-264' at loc (12783,12796)
  Adding a 'Def' for 'def-399' at loc (16892,16904)
  Adding a 'Def' for 'def-260' at loc (12719,12725)
  Adding a 'Def' for 'def-182' at loc (10616,10620)
  Adding a 'Def' for 'def-47' at loc (3631,3637)
  Adding a 'Def' for 'def-110' at loc (6755,6878)
  Adding a 'Def' for 'def-373' at loc (16240,16246)
  Adding a 'Def' for 'def-215' at loc (11573,11618)
  Adding a 'Def' for 'def-422' at loc (17597,17613)
  Adding a 'Def' for 'def-332' at loc (14267,14387)
  Adding a 'Def' for 'def-103' at loc (6586,6592)
  Adding a 'Def' for 'def-145' at loc (8665,8710)
  Adding a 'Def' for 'def-400' at loc (16783,16910)
  Adding a 'Def' for 'def-117' at loc (7098,7153)
  Adding a 'Def' for 'def-405' at loc (17005,17022)
  Adding a 'Def' for 'def-239' at loc (12087,12093)
  Adding a 'Def' for 'def-281' at loc (13257,13302)
  Adding a 'Def' for 'def-304' at loc (13649,13736)
  Adding a 'Def' for 'def-177' at loc (10514,10527)
  Adding a 'Def' for 'def-48' at loc (3647,3652)
  Adding a 'Def' for 'def-261' at loc (12701,12729)
  Adding a 'Def' for 'def-344' at loc (14753,14812)
  Adding a 'Def' for 'def-105' at loc (6482,6602)
  Adding a 'Def' for 'def-175' at loc (10420,10440)
  Adding a 'Def' for 'def-84' at loc (4722,5514)
  Adding a 'Def' for 'def-378' at loc (16317,16319)
  Adding a 'Def' for 'def-20' at loc (1868,1986)
  Adding a 'Def' for 'def-9' at loc (1425,1469)
  Adding a 'Def' for 'def-437' at loc (18239,18254)
  Adding a 'Def' for 'def-74' at loc (4889,4916)
  Adding a 'Def' for 'def-238' at loc (12001,12066)
  Adding a 'Def' for 'def-202' at loc (11239,11255)
  Adding a 'Def' for 'def-434' at loc (18211,18212)
  Adding a 'Def' for 'def-165' at loc (10115,10168)
  Adding a 'Def' for 'def-39' at loc (2905,2914)
  Adding a 'Def' for 'def-76' at loc (5181,5212)
  Adding a 'Def' for 'def-326' at loc (14231,14261)
  Adding a 'Def' for 'def-78' at loc (5358,5382)
  Adding a 'Def' for 'def-381' at loc (16362,16368)
  Adding a 'Def' for 'def-268' at loc (12951,12963)
  Adding a 'Def' for 'def-13' at loc (1600,1656)
  Adding a 'Def' for 'def-416' at loc (17523,17525)
  Adding a 'Def' for 'def-430' at loc (17740,17770)
  Adding a 'Def' for 'def-414' at loc (17336,17352)
  Adding a 'Def' for 'def-152' at loc (9237,9289)
  Adding a 'Def' for 'def-125' at loc (7753,7920)
  Adding a 'Def' for 'def-87' at loc (5565,5642)
  Adding a 'Def' for 'def-229' at loc (11858,11872)
  Adding a 'Def' for 'def-96' at loc (5858,5940)
  Adding a 'Def' for 'def-8' at loc (1337,1376)
  Adding a 'Def' for 'def-198' at loc (11185,11201)
  Adding a 'Def' for 'def-79' at loc (5278,5390)
  Adding a 'Def' for 'def-66' at loc (4484,4508)
  Adding a 'Def' for 'def-51' at loc (3695,3700)
  Adding a 'Def' for 'def-266' at loc (12896,12908)
  Adding a 'Def' for 'def-31' at loc (2370,2403)
  Adding a 'Def' for 'def-349' at loc (15023,15043)
  Adding a 'Def' for 'def-7' at loc (1261,1297)
  Adding a 'Def' for 'def-418' at loc (17481,17539)
  Adding a 'Def' for 'def-89' at loc (5764,5770)
  Adding a 'Def' for 'def-413' at loc (17183,17202)
  Adding a 'Def' for 'def-386' at loc (16439,16441)
  Adding a 'Def' for 'def-296' at loc (13569,13583)
  Adding a 'Def' for 'def-226' at loc (11779,11810)
  Adding a 'Def' for 'def-312' at loc (13931,13945)
  Adding a 'Def' for 'def-4' at loc (1060,1085)
  Adding a 'Def' for 'def-377' at loc (16301,16307)
  Adding a 'Def' for 'def-319' at loc (13955,14074)
  Adding a 'Def' for 'def-425' at loc (17653,17667)
  Adding a 'Def' for 'def-325' at loc (14251,14257)
  Adding a 'Def' for 'def-351' at loc (15133,15152)
  Adding a 'Def' for 'def-2' at loc (1018,1024)
  Adding a 'Def' for 'def-300' at loc (13589,13647)
  Adding a 'Def' for 'def-258' at loc (12590,12654)
  Adding a 'Def' for 'def-315' at loc (14024,14037)
  Adding a 'Def' for 'def-64' at loc (4307,4338)
  Adding a 'Def' for 'def-5' at loc (1116,1141)
  Adding a 'Def' for 'def-297' at loc (13502,13587)
  Adding a 'Def' for 'def-263' at loc (12534,12739)
  Adding a 'Def' for 'def-55' at loc (3759,3764)
  Adding a 'Def' for 'def-241' at loc (11945,12101)
  Adding a 'Def' for 'def-46' at loc (3584,3592)
  Adding a 'Def' for 'def-180' at loc (10575,10589)
  Adding a 'Def' for 'def-33' at loc (2592,2615)
  Adding a 'Def' for 'def-49' at loc (3663,3668)
  Adding a 'Def' for 'def-431' at loc (17933,17939)
  Adding a 'Def' for 'def-61' at loc (4012,4027)
  Adding a 'Def' for 'def-440' at loc (18273,18329)
  Adding a 'Def' for 'def-338' at loc (14602,14621)
  Adding a 'Def' for 'def-271' at loc (12741,13020)
  Adding a 'Def' for 'def-106' at loc (6632,6638)
  Adding a 'Def' for 'def-21' at loc (2020,2043)
  Adding a 'Def' for 'def-1' at loc (907,941)
  Adding a 'Def' for 'def-410' at loc (17136,17152)
  Adding a 'Def' for 'def-345' at loc (14925,14945)
  Adding a 'Def' for 'def-172' at loc (10282,10340)
  Adding a 'Def' for 'def-26' at loc (1662,2123)
  Adding a 'Def' for 'def-217' at loc (11680,11681)
  Adding a 'Def' for 'def-361' at loc (15486,15489)
  Adding a 'Def' for 'def-278' at loc (13113,13155)
  Adding a 'Def' for 'def-243' at loc (12219,12231)
  Adding a 'Def' for 'def-303' at loc (13692,13732)
  Adding a 'Def' for 'def-346' at loc (14887,14951)
  Adding a 'Def' for 'def-230' at loc (11742,11880)
  Adding a 'Def' for 'def-194' at loc (11102,11135)
  Adding a 'Def' for 'def-369' at loc (16179,16185)
  Adding a 'Def' for 'def-337' at loc (14468,14565)
  Adding a 'Def' for 'def-183' at loc (10642,10646)
  Adding a 'Def' for 'def-124' at loc (7545,7669)
  Adding a 'Def' for 'def-395' at loc (16823,16835)
  Adding a 'Def' for 'def-256' at loc (12614,12631)
  Adding a 'Def' for 'def-288' at loc (13442,13443)
  Adding a 'Def' for 'def-155' at loc (9528,9747)
  Adding a 'Def' for 'def-102' at loc (6512,6551)
  Adding a 'Def' for 'def-411' at loc (17157,17173)
  Adding a 'Def' for 'def-94' at loc (5800,5856)
  Adding a 'Def' for 'def-250' at loc (12361,12466)
  Adding a 'Def' for 'def-93' at loc (5831,5852)
  Adding a 'Def' for 'def-223' at loc (11525,11740)
  Adding a 'Def' for 'def-43' at loc (3230,3239)
  Adding a 'Def' for 'def-426' at loc (16088,17671)
  Adding a 'Def' for 'def-421' at loc (17549,17593)
  Adding a 'Def' for 'def-249' at loc (12441,12456)
  Adding a 'Def' for 'def-423' at loc (17616,17632)
  Adding a 'Def' for 'def-0' at loc (931,937)
  Adding a 'Def' for 'def-341' at loc (14722,14741)
  Adding a 'Def' for 'def-262' at loc (12657,12735)
  Adding a 'Def' for 'def-27' at loc (2173,2174)
  Adding a 'Def' for 'def-15' at loc (1791,1797)
  Adding a 'Def' for 'def-184' at loc (10648,10669)
  Adding a 'Def' for 'def-73' at loc (4854,4886)
  Adding a 'Def' for 'def-54' at loc (3743,3748)
  Adding a 'Def' for 'def-88' at loc (5644,5740)
  Adding a 'Def' for 'def-236' at loc (12025,12037)
  Adding a 'Def' for 'def-322' at loc (14076,14120)
  Adding a 'Def' for 'def-18' at loc (1691,1865)
  Adding a 'Def' for 'def-12' at loc (1623,1650)
  Adding a 'Def' for 'def-63' at loc (4160,4179)
  Adding a 'Def' for 'def-228' at loc (11835,11851)
  Adding a 'Def' for 'def-151' at loc (9170,9235)
  Adding a 'Def' for 'def-101' at loc (6539,6545)
  Adding a 'Def' for 'def-342' at loc (14687,14747)
  Adding a 'Def' for 'def-324' at loc (14122,14205)
  Adding a 'Def' for 'def-109' at loc (6644,6684)
  Adding a 'Def' for 'def-80' at loc (4965,5396)
  Adding a 'Def' for 'def-401' at loc (16914,16931)
  Adding a 'Def' for 'def-275' at loc (13054,13111)
  Adding a 'Def' for 'def-19' at loc (1974,1980)
  Adding a 'Def' for 'def-253' at loc (12504,12532)
  Adding a 'Def' for 'def-265' at loc (12857,12874)
  Adding a 'Def' for 'def-237' at loc (12042,12060)
  Adding a 'Def' for 'def-191' at loc (10874,10881)
  Adding a 'Def' for 'def-134' at loc (8191,8211)
  Adding a 'Def' for 'def-374' at loc (16256,16258)
  Adding a 'Def' for 'def-233' at loc (11912,11943)
  Adding a 'Def' for 'def-213' at loc (11385,11423)
  Adding a 'Def' for 'def-181' at loc (10590,10603)
  Adding a 'Def' for 'def-242' at loc (12145,12158)
  Adding a 'Def' for 'def-189' at loc (10839,10851)
  Adding a 'Def' for 'def-407' at loc (17073,17089)
  Adding a 'Def' for 'def-438' at loc (18270,18271)
  Adding a 'Def' for 'def-397' at loc (16857,16869)
  Adding a 'Def' for 'def-402' at loc (16935,16954)
  Adding a 'Def' for 'def-309' at loc (13832,13854)
  Adding a 'Def' for 'def-248' at loc (12416,12432)
  Adding a 'Def' for 'def-160' at loc (9798,9967)
  Adding a 'Def' for 'def-389' at loc (16529,16546)
  Adding a 'Def' for 'def-91' at loc (5742,5798)
  Adding a 'Def' for 'def-394' at loc (16457,16779)
  Adding a 'Def' for 'def-195' at loc (11143,11154)
  Adding a 'Def' for 'def-58' at loc (3807,3813)
  Adding a 'Def' for 'def-193' at loc (11002,11052)
  Adding a 'Def' for 'def-293' at loc (13481,13500)
  Adding a 'Def' for 'def-146' at loc (8712,8783)
  Adding a 'Def' for 'def-428' at loc (17706,17738)
  Adding a 'Def' for 'def-412' at loc (17049,17179)
  Adding a 'Def' for 'def-404' at loc (16981,17002)
  Adding a 'Def' for 'def-310' at loc (13900,13906)
  Adding a 'Def' for 'def-267' at loc (12816,12931)
  Adding a 'Def' for 'def-32' at loc (2519,2537)
  Adding a 'Def' for 'def-316' at loc (14044,14056)
  Adding a 'Def' for 'def-274' at loc (13077,13107)
  Adding a 'Def' for 'def-44' at loc (3313,3321)
  Adding a 'Def' for 'def-435' at loc (18216,18233)
  Adding a 'Def' for 'def-357' at loc (15321,15346)
  Adding a 'Def' for 'def-339' at loc (14567,14625)
  Adding a 'Def' for 'def-305' at loc (13738,13760)
  Adding a 'Def' for 'def-90' at loc (5773,5794)
  Adding a 'Def' for 'def-10' at loc (1513,1521)
  Adding a 'Def' for 'def-327' at loc (14207,14265)
  Adding a 'Def' for 'def-287' at loc (13314,13410)
  Adding a 'Def' for 'def-318' at loc (13979,14070)
  Adding a 'Def' for 'def-409' at loc (17115,17131)
  Adding a 'Def' for 'def-98' at loc (6279,6309)
  Adding a 'Def' for 'def-68' at loc (4091,4522)
  Adding a 'Def' for 'def-71' at loc (4664,4720)
  Adding a 'Def' for 'def-302' at loc (13714,13726)
  Adding a 'Def' for 'def-67' at loc (4404,4516)
  Adding a 'Def' for 'def-301' at loc (13673,13689)
  Adding a 'Def' for 'def-396' at loc (16840,16852)
  Adding a 'Def' for 'def-186' at loc (10720,10736)
  Adding a 'Def' for 'def-384' at loc (16335,16392)
  Adding a 'Def' for 'def-60' at loc (3840,3846)
  Adding a 'Def' for 'def-25' at loc (2089,2119)
  Adding a 'Def' for 'def-62' at loc (4030,4042)
  Adding a 'Def' for 'def-363' at loc (15616,15619)
  Adding a 'Def' for 'def-224' at loc (11765,11778)
  Adding a 'Def' for 'def-187' at loc (10777,10787)
  Adding a 'Def' for 'def-3' at loc (985,1028)
  Adding a 'Def' for 'def-311' at loc (13880,13910)
  Adding a 'Def' for 'def-372' at loc (16152,16209)
  Adding a 'Def' for 'def-336' at loc (14504,14560)
  Adding a 'Def' for 'def-37' at loc (2862,2873)
  Adding a 'Def' for 'def-35' at loc (2839,2849)
  Adding a 'Def' for 'def-403' at loc (16957,16978)
  Adding a 'Def' for 'def-307' at loc (13786,13826)
  Adding a 'Def' for 'def-192' at loc (10925,10939)
  Adding a 'Def' for 'def-135' at loc (8249,8303)
  Adding a 'Def' for 'def-65' at loc (4233,4346)
  Adding a 'Def' for 'def-57' at loc (3791,3796)
  Adding a 'Def' for 'def-408' at loc (17094,17110)
  Adding a 'Def' for 'def-188' at loc (10814,10825)
  Adding a 'Def' for 'def-85' at loc (5538,5559)
  Adding a 'Def' for 'def-368' at loc (15070,15888)
  Adding a 'Def' for 'def-179' at loc (10558,10562)
  Adding a 'Def' for 'def-70' at loc (3934,4640)
  Adding a 'Def' for 'def-232' at loc (11933,11939)
  Adding a 'Def' for 'def-406' at loc (17025,17045)
  Adding a 'Def' for 'def-330' at loc (14363,14377)
  Adding a 'Def' for 'def-136' at loc (8349,8451)
  Adding a 'Def' for 'def-56' at loc (3775,3781)
  Adding a 'Def' for 'def-40' at loc (2977,2986)
  Adding a 'Def' for 'def-16' at loc (1833,1851)
  Adding a 'Def' for 'def-424' at loc (17636,17650)
  Adding a 'Def' for 'def-419' at loc (17414,17545)
  Adding a 'Def' for 'def-173' at loc (10342,10386)
  Adding a 'Def' for 'def-148' at loc (8983,9038)
  Adding a 'Def' for 'def-92' at loc (5822,5828)
  Adding a 'Def' for 'def-28' at loc (2144,2185)
  Adding a 'Def' for 'def-308' at loc (13762,13830)
  Adding a 'Def' for 'def-276' at loc (13133,13146)
  Adding a 'Def' for 'def-185' at loc (10710,10719)
  Adding a 'Def' for 'def-251' at loc (12103,12472)
  Adding a 'Def' for 'def-245' at loc (12178,12294)
  Adding a 'Def' for 'def-174' at loc (10388,10418)
  Adding a 'Def' for 'def-107' at loc (6604,6642)
  Adding a 'Def' for 'def-432' at loc (17901,17943)
  Adding a 'Def' for 'def-392' at loc (16733,16750)
  Adding a 'Def' for 'def-221' at loc (11650,11729)
  Adding a 'Def' for 'def-244' at loc (12253,12271)
  Adding a 'Def' for 'def-391' at loc (16665,16682)
  Adding a 'Def' for 'def-320' at loc (14098,14111)
  Adding a 'Def' for 'def-164' at loc (10010,10043)
  Adding a 'Def' for 'def-358' at loc (15296,15352)
  Adding a 'Def' for 'def-108' at loc (6674,6680)
  Adding a 'Def' for 'def-99' at loc (6167,6313)
  Adding a 'Def' for 'def-355' at loc (15262,15286)
  Adding a 'Def' for 'def-284' at loc (13350,13363)
  Adding a 'Def' for 'def-53' at loc (3727,3732)
  Adding a 'Def' for 'def-436' at loc (18158,18237)
  Adding a 'Def' for 'def-390' at loc (16597,16614)
  Adding a 'Def' for 'def-216' at loc (11549,11624)
  Adding a 'Def' for 'def-23' at loc (2074,2082)
  Adding a 'Def' for 'def-354' at loc (15102,15176)
  Adding a 'Def' for 'def-259' at loc (12679,12696)
  Adding a 'Def' for 'def-246' at loc (12314,12326)
  Adding a 'Def' for 'def-178' at loc (10541,10545)
  Adding a 'Def' for 'def-104' at loc (6554,6598)
  Adding a 'Def' for 'def-385' at loc (16423,16429)
  Adding a 'Def' for 'def-427' at loc (17728,17734)
  Adding a 'Def' for 'def-335' at loc (14537,14556)
  Adding a 'Def' for 'def-22' at loc (1989,2049)
  Adding a 'Def' for 'def-343' at loc (14654,14751)
  Adding a 'Def' for 'def-420' at loc (17572,17587)
  Adding a 'Def' for 'def-282' at loc (13233,13308)
  Adding a 'Def' for 'def-149' at loc (9040,9113)
  Adding a 'Def' for 'def-14' at loc (1570,1660)
  Adding a 'Def' for 'def-359' at loc (15387,15438)
  Adding a 'Def' for 'def-270' at loc (12996,13012)
  Adding a 'Def' for 'def-119' at loc (7193,7397)
  Adding a 'Def' for 'def-11' at loc (1493,1525)
  Adding a 'Def' for 'def-415' at loc (17505,17511)
  Adding a 'Def' for 'def-352' at loc (15162,15164)
  Adding a 'Def' for 'def-141' at loc (8496,8560)
  Adding a 'Def' for 'def-6' at loc (1183,1213)
  Adding a 'Def' for 'def-398' at loc (16874,16886)
  Adding a 'Def' for 'def-225' at loc (11794,11806)
  Adding a 'Def' for 'def-113' at loc (6922,7096)
  Adding a 'Def' for 'def-86' at loc (5516,5563)
  Adding a 'Def' for 'def-280' at loc (13157,13207)
  Adding a 'Def' for 'def-273' at loc (13097,13103)
  Adding a 'Def' for 'def-272' at loc (13022,13050)
  Adding a 'Def' for 'def-17' at loc (1803,1859)
  Adding a 'Def' for 'def-360' at loc (15461,15476)
  Adding a 'Def' for 'def-340' at loc (14627,14652)
  Adding a 'Def' for 'def-314' at loc (13856,13953)
  Adding a 'Def' for 'def-234' at loc (11988,11994)
  Adding a 'Def' for 'def-334' at loc (14389,14466)
  Adding a 'Def' for 'def-208' at loc (11336,11347)
  Adding a 'Def' for 'def-240' at loc (12069,12097)
  Adding a 'Def' for 'def-69' at loc (4575,4636)
  Adding a 'Def' for 'def-429' at loc (17760,17766)
  Adding a 'Def' for 'def-348' at loc (14853,14997)
  Adding a 'Def' for 'def-292' at loc (13412,13479)
  Adding a 'Def' for 'def-317' at loc (14003,14064)
  Adding a 'Def' for 'def-130' at loc (7992,8119)
  Adding a 'Def' for 'def-75' at loc (5034,5053)
  Adding a 'Def' for 'def-283' at loc (13209,13312)
  Adding a 'Def' for 'def-222' at loc (11628,11735)
  Adding a 'Def' for 'def-153' at loc (9291,9372)
  uids (361 calculated vs 361 expected): [val-~-,type-with2,type-with1,type-variant_alias,type-variant,type-universe_record,class-type-two_method_class,class-two_method_class,type-two_method_class,type-tuple,val-some_file,val-some_doc,val-since_mesozoic,type-record_alias,type-record,type-rec_obj,type-poly_variant_union,type-poly_variant,type-poly_poly_variant,type-poly_fun_constraint,type-poly_fun,type-poly_ext,type-partial_gadt_alias,type-partial_gadt,class-type-param_class,class-param_class,type-param_class,type-open_poly_variant_alias,type-open_poly_variant2,type-open_poly_variant,type-open_obj,type-oof,class-type-one_method_class,class-one_method_class,type-one_method_class,type-one_meth,val-ocaml_org,val-not_found,type-new_t,type-nested_poly_variant,type-nested_include,type-my_unit_object,type-my_unit_class,type-my_mod,type-mutual_constr_b,type-mutual_constr_a,type-mutable_record,val-launch_missiles,val-land,val-kaboom,type-include_include,val-fun_maybe,val-fun_fun_fun,type-full_gadt_alias,type-full_gadt,type-ext,type-empty_obj,class-type-empty_class,class-empty_class,type-empty_class,type-double_include,type-dep5,type-dep4,type-dep3,type-dep2,type-dep1,type-closed_poly_variant,type-clopen_poly_variant,val-changing,type-bin_poly_poly_variant,type-any_obj,type-alias,type-a_function,val-a_function,ext-ZzzTop0,ext-ZzzTop,module-With9.module-type-S,module-With9,module-type-With8,module-With7,module-With7,module-With6.module-type-T,module-With6,module-With5.module-type-S,module-With5.module-N.type-t,module-With5.module-N,module-With5,module-With4.module-N.type-t,module-With4.module-N,module-With4,module-With3.module-N.type-t,module-With3.module-N,module-With3.module-M.module-type-S,module-With3.module-M,module-With3,module-With2.module-type-S,module-With2,module-type-With11,module-With10.module-type-T,module-With10,module-type-With1,module-type-TypeExtPruned,module-type-TypeExt,module-type-ToInclude,module-type-SuperSig,module-type-SigForMod,module-type-RecollectionModule,module-Recollection,module-type-RECOLLECTION,ext-Quux,module-Only_a_module.type-t,module-Only_a_module,module-One.type-one,module-One,module-type-NestedInclude2,module-type-NestedInclude1,module-ModuleWithSignatureAlias,module-ModuleWithSignature,module-type-MissingComment,module-type-MMM,module-type-M,module-M.type-t,module-M,ext-Kapow,ext-Kaboom,ext-Kablam,module-type-IncludedB,module-IncludedA.type-t,module-IncludedA,module-type-IncludeModuleType,module-IncludeInclude2_M,module-type-IncludeInclude2,module-IncludeInclude1.module-IncludeInclude2_M,module-IncludeInclude1.module-type-IncludeInclude2,module-IncludeInclude1,module-FunctorTypeOf,ext-Foo,module-ExtMod.type-t,module-ExtMod.ext-Leisureforce,module-ExtMod,ext-ExtF,ext-ExtE,ext-ExtD,ext-ExtC,ext-ExtB,ext-ExtA,ext-Exn_arrow,ext-EmptySigAlias,module-type-EmptySigAlias,ext-EmptySig,module-type-EmptySig,module-EmptyAlias,module-type-Empty,module-Empty,module-DoubleInclude3.module-DoubleInclude2.type-double_include,module-DoubleInclude3.module-DoubleInclude2,module-DoubleInclude3,module-DoubleInclude1.module-DoubleInclude2.type-double_include,module-DoubleInclude1.module-DoubleInclude2,module-DoubleInclude1,module-Dep9,module-Dep9,module-Dep8.module-type-T,module-Dep8,module-Dep7,module-Dep6.module-X.module-Y.type-d,module-Dep6.module-X.module-Y,module-Dep6.module-X.module-type-R,module-Dep6.module-X,module-Dep6.module-type-T,module-Dep6.module-type-S,module-Dep6,module-Dep5,module-Dep4.module-X.type-b,module-Dep4.module-X,module-Dep4.module-type-T,module-Dep4.module-type-S,module-Dep4,module-Dep3.type-a,module-Dep3,module-Dep2,module-Dep13.class-type-c,module-Dep13.class-c,module-Dep13.type-c,module-Dep13.type-#c,module-Dep13,module-Dep12,module-Dep11.module-type-S,module-Dep11,module-type-Dep10,module-Dep1.module-X.module-Y.class-type-c,module-Dep1.module-X.module-Y.class-c,module-Dep1.module-X.module-Y.type-c,module-Dep1.module-X.module-Y.type-#c,module-Dep1.module-X.module-Y,module-Dep1.module-X,module-Dep1.module-type-S,module-Dep1,module-CollectionModule.type-element,module-CollectionModule.type-collection,module-CollectionModule.module-type-InnerModuleTypeA,module-CollectionModule.module-InnerModuleA.type-t,module-CollectionModule.module-InnerModuleA.module-type-InnerModuleTypeA',module-CollectionModule.module-InnerModuleA.module-InnerModuleA'.type-t,module-CollectionModule.module-InnerModuleA.module-InnerModuleA',module-CollectionModule.module-InnerModuleA,module-CollectionModule,module-CanonicalTest.module-List_modif.type-t,module-CanonicalTest.module-List_modif.val-id,module-CanonicalTest.module-List_modif,module-CanonicalTest.module-Base__List.type-t,module-CanonicalTest.module-Base__List.val-id,module-CanonicalTest.module-Base__List,module-CanonicalTest.module-Base__.module-List.type-t,module-CanonicalTest.module-Base__.module-List.val-id,module-CanonicalTest.module-Base__.module-List,module-CanonicalTest.module-Base__,module-CanonicalTest.module-Base_Tests.val-foo,module-CanonicalTest.module-Base_Tests.val-baz,module-CanonicalTest.module-Base_Tests.val-bar,module-CanonicalTest.module-Base_Tests.module-L.type-t,module-CanonicalTest.module-Base_Tests.module-L.val-id,module-CanonicalTest.module-Base_Tests.module-L,module-CanonicalTest.module-Base_Tests.module-C.type-t,module-CanonicalTest.module-Base_Tests.module-C.val-id,module-CanonicalTest.module-Base_Tests.module-C,module-CanonicalTest.module-Base_Tests,module-CanonicalTest.module-Base.module-List.type-t,module-CanonicalTest.module-Base.module-List.val-id,module-CanonicalTest.module-Base.module-List,module-CanonicalTest.module-Base,module-CanonicalTest,module-type-COLLECTION,ext-C,module-type-C,module-Buffer.val-f,module-Buffer,ext-Bar,module-type-B,module-Aliases.type-tete2,module-Aliases.type-tete,module-Aliases.type-testa,module-Aliases.type-tbtb,module-Aliases.type-tata',module-Aliases.type-tata,module-Aliases.type-stde,module-Aliases.type-p2,module-Aliases.type-p1,module-Aliases.module-X2.type-t,module-Aliases.module-X2.val-id,module-Aliases.module-X2,module-Aliases.module-X1.type-t,module-Aliases.module-X1.val-id,module-Aliases.module-X1,module-Aliases.module-Std.module-E.type-t,module-Aliases.module-Std.module-E.val-id,module-Aliases.module-Std.module-E,module-Aliases.module-Std.module-D.type-t,module-Aliases.module-Std.module-D.val-id,module-Aliases.module-Std.module-D,module-Aliases.module-Std.module-C.type-t,module-Aliases.module-Std.module-C.val-id,module-Aliases.module-Std.module-C,module-Aliases.module-Std.module-B.type-t,module-Aliases.module-Std.module-B.val-id,module-Aliases.module-Std.module-B,module-Aliases.module-Std.module-A.type-t,module-Aliases.module-Std.module-A.val-id,module-Aliases.module-Std.module-A,module-Aliases.module-Std,module-Aliases.module-P2.module-Z.type-t,module-Aliases.module-P2.module-Z.val-id,module-Aliases.module-P2.module-Z,module-Aliases.module-P2,module-Aliases.module-P1.module-Y.type-t,module-Aliases.module-P1.module-Y.val-id,module-Aliases.module-P1.module-Y,module-Aliases.module-P1,module-Aliases.module-Foo__E.type-t,module-Aliases.module-Foo__E.val-id,module-Aliases.module-Foo__E,module-Aliases.module-Foo__D.type-t,module-Aliases.module-Foo__D.val-id,module-Aliases.module-Foo__D,module-Aliases.module-Foo__C.type-t,module-Aliases.module-Foo__C.val-id,module-Aliases.module-Foo__C,module-Aliases.module-Foo__B.type-t,module-Aliases.module-Foo__B.val-id,module-Aliases.module-Foo__B,module-Aliases.module-Foo__A.type-t,module-Aliases.module-Foo__A.val-id,module-Aliases.module-Foo__A,module-Aliases.module-Foo__.module-E.type-t,module-Aliases.module-Foo__.module-E.val-id,module-Aliases.module-Foo__.module-E,module-Aliases.module-Foo__.module-D.type-t,module-Aliases.module-Foo__.module-D.val-id,module-Aliases.module-Foo__.module-D,module-Aliases.module-Foo__.module-C.type-t,module-Aliases.module-Foo__.module-C.val-id,module-Aliases.module-Foo__.module-C,module-Aliases.module-Foo__.module-B.type-t,module-Aliases.module-Foo__.module-B.val-id,module-Aliases.module-Foo__.module-B,module-Aliases.module-Foo__.module-A.type-t,module-Aliases.module-Foo__.module-A.val-id,module-Aliases.module-Foo__.module-A,module-Aliases.module-Foo__,module-Aliases.module-Foo.module-E.type-t,module-Aliases.module-Foo.module-E.val-id,module-Aliases.module-Foo.module-E,module-Aliases.module-Foo.module-D.type-t,module-Aliases.module-Foo.module-D.val-id,module-Aliases.module-Foo.module-D,module-Aliases.module-Foo.module-C.type-t,module-Aliases.module-Foo.module-C.val-id,module-Aliases.module-Foo.module-C,module-Aliases.module-Foo.module-B.type-t,module-Aliases.module-Foo.module-B.val-id,module-Aliases.module-Foo.module-B,module-Aliases.module-Foo.module-A.type-t,module-Aliases.module-Foo.module-A.val-id,module-Aliases.module-Foo.module-A,module-Aliases.module-Foo,module-Aliases.module-E.type-t,module-Aliases.module-E.val-id,module-Aliases.module-E,module-Aliases.module-D.type-t,module-Aliases.module-D.val-id,module-Aliases.module-D,module-Aliases.module-C.type-t,module-Aliases.module-C.val-id,module-Aliases.module-C,module-Aliases.module-B.type-t,module-Aliases.module-B.val-id,module-Aliases.module-B,module-Aliases.module-A'.type-t,module-Aliases.module-A'.val-id,module-Aliases.module-A',module-Aliases.module-A.type-t,module-Aliases.module-A.val-id,module-Aliases.module-A,module-Aliases,module-type-A,val-@,val-=,val-:=,val-/,val--?,val--,val-+,val-*,val-&,val-%,val-$,type-#two_method_class,type-#param_class,type-#one_method_class,type-#empty_class,val-!]Adding a 'Def' for 'f_295' at loc (2173,2174)
  Adding a 'Def' for 'a_function_305' at loc (2839,2849)
  Adding a 'Def' for 'x_307' at loc (2851,2852)
  Adding a local occurrence for x_307 (pos (2855,2856))
  Adding a 'Def' for 'fun_fun_fun_308' at loc (2862,2873)
  Adding a 'Def' for '_int_fun_310' at loc (2874,2882)
  Adding a 'Def' for 'fun_maybe_312' at loc (2905,2914)
  Adding a 'Def' for 'not_found_316' at loc (2977,2986)
  Adding a 'Def' for 'kaboom_319' at loc (3053,3059)
  Adding a 'Def' for 'ocaml_org_322' at loc (3141,3150)
  Adding a 'Def' for 'some_file_323' at loc (3230,3239)
  Adding a 'Def' for 'some_doc_324' at loc (3313,3321)
  Adding a 'Def' for 'since_mesozoic_325' at loc (3416,3430)
  Adding a 'Def' for 'changing_326' at loc (3584,3592)
  Adding a 'Def' for '~-_327' at loc (3631,3637)
  Adding a 'Def' for '!_328' at loc (3647,3652)
  Adding a 'Def' for '@_329' at loc (3663,3668)
  Adding a 'Def' for '$_330' at loc (3679,3684)
  Adding a 'Def' for '%_331' at loc (3695,3700)
  Adding a 'Def' for '&_332' at loc (3711,3716)
  Adding a 'Def' for '*_333' at loc (3727,3732)
  Adding a 'Def' for '-_334' at loc (3743,3748)
  Adding a 'Def' for '+_335' at loc (3759,3764)
  Adding a 'Def' for '-?_336' at loc (3775,3781)
  Adding a 'Def' for '/_337' at loc (3791,3796)
  Adding a 'Def' for ':=_338' at loc (3807,3813)
  Adding a 'Def' for '=_339' at loc (3823,3828)
  Adding a 'Def' for 'land_340' at loc (3840,3846)
  Adding a 'Def' for 'x_595' at loc (11348,11349)
  Adding a 'Def' for 'id_860' at loc (15162,15164)
  Adding a 'Def' for 'x_862' at loc (15165,15166)
  Adding a local occurrence for x_862 (pos (15169,15170))
  Adding a 'Def' for 'foo_872' at loc (15486,15489)
  Adding a 'Def' for 'l_874' at loc (15491,15492)
  Adding a local occurrence for l_874 (pos (15551,15552))
  Adding a 'Def' for 'bar_938' at loc (15616,15619)
  Adding a 'Def' for 'l_940' at loc (15621,15622)
  Adding a global occurrence for def-352 (name id) (pos (15656,15660))
  Adding a local occurrence for l_940 (pos (15661,15662))
  Adding a 'Def' for 'baz_941' at loc (15789,15792)
  Adding a 'Def' for 'id_950' at loc (16195,16197)
  Adding a 'Def' for 't_952' at loc (16198,16199)
  Adding a local occurrence for t_952 (pos (16202,16203))
  Adding a 'Def' for 'id_955' at loc (16256,16258)
  Adding a 'Def' for 't_957' at loc (16259,16260)
  Adding a local occurrence for t_957 (pos (16263,16264))
  Adding a 'Def' for 'id_960' at loc (16317,16319)
  Adding a 'Def' for 't_962' at loc (16320,16321)
  Adding a local occurrence for t_962 (pos (16324,16325))
  Adding a 'Def' for 'id_965' at loc (16378,16380)
  Adding a 'Def' for 't_967' at loc (16381,16382)
  Adding a local occurrence for t_967 (pos (16385,16386))
  Adding a 'Def' for 'id_970' at loc (16439,16441)
  Adding a 'Def' for 't_972' at loc (16442,16443)
  Adding a local occurrence for t_972 (pos (16446,16447))
  Adding a 'Def' for 'id_1006' at loc (17523,17525)
  Adding a 'Def' for 'x_1008' at loc (17526,17527)
  Adding a local occurrence for x_1008 (pos (17530,17531))

