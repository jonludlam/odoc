  $ ocamlc -c -bin-annot a.ml
  $ ocamlc -c -bin-annot b.ml
  $ ocamlc -c -bin-annot c.ml
  $ odoc compile a.cmt
  Starting type_of pass
  Adding (root A).t to env
  Adding (root A).B.A to env
  Adding (root A).B.{t}1 to env
  Adding (root A).B.t to env
  Adding (root A).B.A.t to env
  Handling include in type_of
  Removing (root A).B.{t}1 from env
  Adding (root A).B.t to env
  Overriding duplicate env entry: t
  odoc: internal error, uncaught exception:
        Failure("error")
        Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
        Called from Odoc_xref2__Env.add_to_elts in file "src/xref2/env.ml", line 232, characters 8-24
        Called from Odoc_xref2__Env.add_type in file "src/xref2/env.ml", line 341, characters 12-72
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Type_of.signature in file "src/xref2/type_of.ml" (inlined), line 13, characters 12-37
        Called from Odoc_xref2__Type_of.simple_expansion in file "src/xref2/type_of.ml", line 104, characters 30-48
        Called from Odoc_xref2__Type_of.u_module_type_expr in file "src/xref2/type_of.ml", line 90, characters 45-70
        Called from Odoc_xref2__Type_of.include_ in file "src/xref2/type_of.ml", line 115, characters 33-68
        Called from Odoc_xref2__Type_of.signature_items.(fun) in file "src/xref2/type_of.ml", line 25, characters 31-47
        Called from Stdlib__List.map in file "list.ml", line 92, characters 20-23
        Called from Stdlib__List.map in file "list.ml", line 92, characters 32-39
        Called from Odoc_xref2__Type_of.signature_items in file "src/xref2/type_of.ml", line 20, characters 4-253
        Called from Odoc_xref2__Type_of.signature in file "src/xref2/type_of.ml" (inlined), line 14, characters 2-24
        Called from Odoc_xref2__Type_of.module_type_expr in file "src/xref2/type_of.ml", line 67, characters 30-48
        Called from Odoc_xref2__Type_of.module_type in file "src/xref2/type_of.ml", line 46, characters 20-72
        Called from Odoc_xref2__Type_of.signature_items.(fun) in file "src/xref2/type_of.ml", line 24, characters 38-58
        Called from Stdlib__List.map in file "list.ml", line 92, characters 20-23
        Called from Stdlib__List.map in file "list.ml", line 92, characters 32-39
        Called from Stdlib__List.map in file "list.ml", line 92, characters 32-39
        Called from Odoc_xref2__Type_of.signature_items in file "src/xref2/type_of.ml", line 20, characters 4-253
        Called from Odoc_xref2__Type_of.signature in file "src/xref2/type_of.ml" (inlined), line 14, characters 2-24
        Called from Odoc_xref2__Type_of.signature.loop in file "src/xref2/type_of.ml", line 128, characters 14-30
        Called from Odoc_xref2__Compile.content.(fun) in file "src/xref2/compile.ml", line 65, characters 15-38
        Called from Odoc_xref2__Compile.unit in file "src/xref2/compile.ml", line 58, characters 21-47
        Called from Odoc_xref2__Lookup_failures.with_ref in file "src/xref2/lookup_failures.ml", line 13, characters 10-14
        Called from Odoc_xref2__Lookup_failures.catch_failures in file "src/xref2/lookup_failures.ml", line 60, characters 20-37
        Called from Odoc_odoc__Compile.resolve_and_substitute in file "src/odoc/compile.ml", line 93, characters 4-49
        Called from Odoc_model__Error.catch in file "src/model/error.ml", line 52, characters 21-27
        Called from Odoc_model__Error.catch_warnings.(fun) in file "src/model/error.ml", line 87, characters 18-22
        Called from Odoc_model__Error.with_ref in file "src/model/error.ml", line 65, characters 12-16
        Re-raised at Odoc_model__Error.with_ref in file "src/model/error.ml", line 70, characters 4-11
        Called from Odoc_odoc__Compile.compile.(fun) in file "src/odoc/compile.ml", line 226, characters 6-136
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 25, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 23, characters 12-19
        Called from Cmdliner.Term.run in file "cmdliner.ml", line 117, characters 32-39
  [2]
  $ odoc compile -I . b.cmt
  Read include - shadowed.types = t
  content=type {t}1
  ...
  ...
  
  Starting type_of pass
  Adding (root B).{t}1 to env
  Adding (root B).t to env
  Handling include in type_of
  Removing (root B).{t}1 from env
  Adding (root B).B.A to env
  Adding (root B).B.t to env
  Adding (root B).B.A.t to env
  Finished handling include in type_of
  Finished type_of pass
  Adding (root B).{t}1 to env
  Adding (root B).t to env
  Handling include of : module type of struct include unresolvedroot(A) end
  Removing (root B).{t}1 from env
  Adding (root B).{t}1 to env
  File "b.cmt":
  Warning: Couldn't find the following modules:
    A
  $ odoc compile -I . c.cmt
  Read include - shadowed.types = t
  content=...
  ...
  type {t}3
  
  Read include - shadowed.types = 
  content=...
  ...
  type t
  
  Starting type_of pass
  Adding (root C).{t}3 to env
  Adding (root C).t to env
  Handling include in type_of
  Removing (root C).{t}3 from env
  Adding (root C).{t}1 to env
  Overriding duplicate env entry: B
  odoc: internal error, uncaught exception:
        Failure("error")
        Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
        Called from Odoc_xref2__Env.add_to_elts in file "src/xref2/env.ml", line 232, characters 8-24
        Called from Odoc_xref2__Env.add_module_type in file "src/xref2/env.ml", line 349, characters 4-76
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Type_of.signature in file "src/xref2/type_of.ml" (inlined), line 13, characters 12-37
        Called from Odoc_xref2__Type_of.simple_expansion in file "src/xref2/type_of.ml", line 104, characters 30-48
        Called from Odoc_xref2__Type_of.u_module_type_expr in file "src/xref2/type_of.ml", line 90, characters 45-70
        Called from Odoc_xref2__Type_of.include_ in file "src/xref2/type_of.ml", line 115, characters 33-68
        Called from Odoc_xref2__Type_of.signature_items.(fun) in file "src/xref2/type_of.ml", line 25, characters 31-47
        Called from Stdlib__List.map in file "list.ml", line 92, characters 20-23
        Called from Odoc_xref2__Type_of.signature_items in file "src/xref2/type_of.ml", line 20, characters 4-253
        Called from Odoc_xref2__Type_of.signature in file "src/xref2/type_of.ml" (inlined), line 14, characters 2-24
        Called from Odoc_xref2__Type_of.signature.loop in file "src/xref2/type_of.ml", line 128, characters 14-30
        Called from Odoc_xref2__Compile.content.(fun) in file "src/xref2/compile.ml", line 65, characters 15-38
        Called from Odoc_xref2__Compile.unit in file "src/xref2/compile.ml", line 58, characters 21-47
        Called from Odoc_xref2__Lookup_failures.with_ref in file "src/xref2/lookup_failures.ml", line 13, characters 10-14
        Called from Odoc_xref2__Lookup_failures.catch_failures in file "src/xref2/lookup_failures.ml", line 60, characters 20-37
        Called from Odoc_odoc__Compile.resolve_and_substitute in file "src/odoc/compile.ml", line 93, characters 4-49
        Called from Odoc_model__Error.catch in file "src/model/error.ml", line 52, characters 21-27
        Called from Odoc_model__Error.catch_warnings.(fun) in file "src/model/error.ml", line 87, characters 18-22
        Called from Odoc_model__Error.with_ref in file "src/model/error.ml", line 65, characters 12-16
        Re-raised at Odoc_model__Error.with_ref in file "src/model/error.ml", line 70, characters 4-11
        Called from Odoc_odoc__Compile.compile.(fun) in file "src/odoc/compile.ml", line 226, characters 6-136
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 25, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 23, characters 12-19
        Called from Cmdliner.Term.run in file "cmdliner.ml", line 117, characters 32-39
  [2]
  $ odoc_print b.odoc
  {
    "id": { "`Root": [ "None", "B" ] },
    "root": "<root>",
    "digest": "<digest>",
    "imports": [
      { "Unresolved": [ "A", { "Some": "<digest>" } ] },
      { "Unresolved": [ "CamlinternalFormatBasics", { "Some": "<digest>" } ] },
      { "Unresolved": [ "Stdlib", { "Some": "<digest>" } ] }
    ],
    "source": {
      "Some": {
        "file": "b.ml",
        "build_dir": "$TESTCASE_ROOT",
        "digest": "<digest>"
      }
    },
    "interface": "true",
    "hidden": "false",
    "content": {
      "Module": {
        "items": [
          {
            "Include": {
              "parent": { "`Root": [ "None", "B" ] },
              "doc": [],
              "decl": {
                "ModuleType": {
                  "TypeOf": {
                    "t_desc": { "StructInclude": { "`Root": "A" } },
                    "t_expansion": "None"
                  }
                }
              },
              "status": "`Default",
              "expansion": {
                "shadowed": {
                  "s_modules": [],
                  "s_module_types": [],
                  "s_values": [],
                  "s_types": [ "t" ],
                  "s_classes": [],
                  "s_class_types": []
                },
                "content": {
                  "items": [
                    {
                      "Type": [
                        "Ordinary",
                        {
                          "id": {
                            "`Type": [ { "`Root": [ "None", "B" ] }, "{t}1" ]
                          },
                          "doc": [],
                          "equation": {
                            "params": [],
                            "private_": "false",
                            "manifest": {
                              "Some": {
                                "Constr": [
                                  {
                                    "`Identifier": [
                                      { "`CoreType": "int" },
                                      "false"
                                    ]
                                  },
                                  []
                                ]
                              }
                            },
                            "constraints": []
                          },
                          "representation": "None"
                        }
                      ]
                    },
                    {
                      "Value": {
                        "id": {
                          "`Value": [ { "`Root": [ "None", "B" ] }, "f" ]
                        },
                        "doc": [],
                        "type_": {
                          "Constr": [
                            {
                              "`Identifier": [
                                {
                                  "`Type": [
                                    { "`Root": [ "None", "B" ] },
                                    "{t}1"
                                  ]
                                },
                                "false"
                              ]
                            },
                            []
                          ]
                        },
                        "value": "Abstract"
                      }
                    },
                    {
                      "ModuleType": {
                        "id": {
                          "`ModuleType": [ { "`Root": [ "None", "B" ] }, "B" ]
                        },
                        "doc": [],
                        "canonical": "None",
                        "expr": {
                          "Some": {
                            "Signature": {
                              "items": [
                                {
                                  "Module": [
                                    "Ordinary",
                                    {
                                      "id": {
                                        "`Module": [
                                          {
                                            "`ModuleType": [
                                              { "`Root": [ "None", "B" ] },
                                              "B"
                                            ]
                                          },
                                          "A"
                                        ]
                                      },
                                      "doc": [],
                                      "type_": {
                                        "ModuleType": {
                                          "Signature": {
                                            "items": [
                                              {
                                                "Type": [
                                                  "Ordinary",
                                                  {
                                                    "id": {
                                                      "`Type": [
                                                        {
                                                          "`Module": [
                                                            {
                                                              "`ModuleType": [
                                                                {
                                                                  "`Root": [
                                                                    "None", "B"
                                                                  ]
                                                                },
                                                                "B"
                                                              ]
                                                            },
                                                            "A"
                                                          ]
                                                        },
                                                        "t"
                                                      ]
                                                    },
                                                    "doc": [],
                                                    "equation": {
                                                      "params": [],
                                                      "private_": "false",
                                                      "manifest": {
                                                        "Some": {
                                                          "Constr": [
                                                            {
                                                              "`Identifier": [
                                                                {
                                                                  "`CoreType":
                                                                    "int"
                                                                },
                                                                "false"
                                                              ]
                                                            },
                                                            []
                                                          ]
                                                        }
                                                      },
                                                      "constraints": []
                                                    },
                                                    "representation": "None"
                                                  }
                                                ]
                                              }
                                            ],
                                            "compiled": "false",
                                            "doc": []
                                          }
                                        }
                                      },
                                      "canonical": "None",
                                      "hidden": "false"
                                    }
                                  ]
                                },
                                {
                                  "Type": [
                                    "Ordinary",
                                    {
                                      "id": {
                                        "`Type": [
                                          {
                                            "`ModuleType": [
                                              { "`Root": [ "None", "B" ] },
                                              "B"
                                            ]
                                          },
                                          "t"
                                        ]
                                      },
                                      "doc": [],
                                      "equation": {
                                        "params": [],
                                        "private_": "false",
                                        "manifest": {
                                          "Some": {
                                            "Constr": [
                                              {
                                                "`Identifier": [
                                                  { "`CoreType": "float" },
                                                  "false"
                                                ]
                                              },
                                              []
                                            ]
                                          }
                                        },
                                        "constraints": []
                                      },
                                      "representation": "None"
                                    }
                                  ]
                                }
                              ],
                              "compiled": "false",
                              "doc": []
                            }
                          }
                        }
                      }
                    }
                  ],
                  "compiled": "false",
                  "doc": []
                }
              }
            }
          },
          {
            "Type": [
              "Ordinary",
              {
                "id": { "`Type": [ { "`Root": [ "None", "B" ] }, "t" ] },
                "doc": [],
                "equation": {
                  "params": [],
                  "private_": "false",
                  "manifest": {
                    "Some": {
                      "Constr": [
                        {
                          "`Resolved": {
                            "`Identifier": { "`CoreType": "float" }
                          }
                        },
                        []
                      ]
                    }
                  },
                  "constraints": []
                },
                "representation": "None"
              }
            ]
          }
        ],
        "compiled": "true",
        "doc": []
      }
    },
    "expansion": "None",
    "canonical": "None"
  }

  $ odoc link -I . a.odoc
  odoc: file.odoc argument: no `a.odoc' file or directory
  Usage: odoc link [OPTION]... file.odoc
  Try `odoc link --help' or `odoc --help' for more information.
  [2]
  $ odoc link -I . b.odoc
  Adding (root B).{t}1 to env
  Adding (root B).t to env
  Adding (root B).B.A to env
  Adding (root B).B.t to env
  Adding (root B).B.A.t to env
  File "b.odoc":
  Warning: Couldn't find the following modules:
    A
  $ odoc link -I . c.odoc
  odoc: file.odoc argument: no `c.odoc' file or directory
  Usage: odoc link [OPTION]... file.odoc
  Try `odoc link --help' or `odoc --help' for more information.
  [2]
  $ odoc html-generate -o html a.odocl
  odoc: file.odoc argument: no `a.odocl' file or directory
  Usage: odoc html-generate [OPTION]... file.odoc
  Try `odoc html-generate --help' or `odoc --help' for more information.
  [2]
  $ odoc html-generate -o html b.odocl
  $ odoc html-generate -o html c.odocl
  odoc: file.odoc argument: no `c.odocl' file or directory
  Usage: odoc html-generate [OPTION]... file.odoc
  Try `odoc html-generate --help' or `odoc --help' for more information.
  [2]
  $ odoc support-files -o html
  $ rsync -avz html /tmp/html
  building file list ... done
  
  sent 231 bytes  received 20 bytes  502.00 bytes/sec
  total size is 32075  speedup is 127.79
