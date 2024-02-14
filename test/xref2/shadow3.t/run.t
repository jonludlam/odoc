3 module shadowing! modules A and B are identical and contain shadowed modules. 
Module `C` then includes them both, causing further shadowing.

  $ ocamlc -c -bin-annot a.mli
  $ ocamlc -c -bin-annot b.mli
  $ ocamlc -c -bin-annot c.mli
  $ ocamlc -i c.mli
  module type B = B.B
  module type B1 = B.B1
  module A : sig type t = B.A.t type b = B.A.b end

  $ odoc compile a.cmti
  $ odoc compile b.cmti
  $ odoc compile -I . c.cmti
 
  $ odoc_print -r A --show-include-expansions c.odoc 
  {
    "id": { "`Module": [ { "`Root": [ "None", "C" ] }, "A" ] },
    "locs": "None",
    "doc": [],
    "type_": {
      "ModuleType": {
        "Signature": {
          "items": [
            {
              "Include": {
                "parent": { "`Module": [ { "`Root": [ "None", "C" ] }, "A" ] },
                "doc": [],
                "decl": {
                  "ModuleType": {
                    "TypeOf": [
                      {
                        "StructInclude": {
                          "`Resolved": {
                            "`Alias": [
                              {
                                "`Module": [
                                  {
                                    "`Identifier": { "`Root": [ "None", "B" ] }
                                  },
                                  "A"
                                ]
                              },
                              {
                                "`Identifier": [
                                  {
                                    "`Module": [
                                      { "`Root": [ "None", "C" ] }, "{A}4"
                                    ]
                                  },
                                  "true"
                                ]
                              }
                            ]
                          }
                        }
                      },
                      {
                        "`Identifier": [
                          {
                            "`Module": [ { "`Root": [ "None", "C" ] }, "{A}4" ]
                          },
                          "true"
                        ]
                      }
                    ]
                  }
                },
                "status": "`Default",
                "expansion": {
                  "shadowed": {
                    "s_modules": [],
                    "s_module_types": [],
                    "s_values": [],
                    "s_types": [],
                    "s_classes": [],
                    "s_class_types": []
                  },
                  "content": {
                    "items": [
                      {
                        "Include": {
                          "parent": {
                            "`Module": [ { "`Root": [ "None", "C" ] }, "A" ]
                          },
                          "doc": [],
                          "decl": {
                            "ModuleType": {
                              "TypeOf": [
                                {
                                  "StructInclude": {
                                    "`Resolved": {
                                      "`Hidden": {
                                        "`Module": [
                                          {
                                            "`Identifier": {
                                              "`Root": [ "None", "B" ]
                                            }
                                          },
                                          "{A}1"
                                        ]
                                      }
                                    }
                                  }
                                },
                                {
                                  "`Identifier": [
                                    {
                                      "`Module": [
                                        { "`Root": [ "None", "B" ] }, "{A}1"
                                      ]
                                    },
                                    "true"
                                  ]
                                }
                              ]
                            }
                          },
                          "status": "`Default",
                          "expansion": {
                            "shadowed": {
                              "s_modules": [],
                              "s_module_types": [],
                              "s_values": [],
                              "s_types": [],
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
                                        "`Type": [
                                          {
                                            "`Module": [
                                              { "`Root": [ "None", "C" ] }, "A"
                                            ]
                                          },
                                          "t"
                                        ]
                                      },
                                      "locs": "None",
                                      "doc": [],
                                      "equation": {
                                        "params": [],
                                        "private_": "false",
                                        "manifest": {
                                          "Some": {
                                            "Constr": [
                                              {
                                                "`Resolved": {
                                                  "`Type": [
                                                    {
                                                      "`Alias": [
                                                        {
                                                          "`Module": [
                                                            {
                                                              "`Identifier": {
                                                                "`Root": [
                                                                  "None", "B"
                                                                ]
                                                              }
                                                            },
                                                            "A"
                                                          ]
                                                        },
                                                        {
                                                          "`Identifier": [
                                                            {
                                                              "`Module": [
                                                                {
                                                                  "`Root": [
                                                                    "None", "C"
                                                                  ]
                                                                },
                                                                "{A}4"
                                                              ]
                                                            },
                                                            "true"
                                                          ]
                                                        }
                                                      ]
                                                    },
                                                    "t"
                                                  ]
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
                          }
                        }
                      },
                      {
                        "Type": [
                          "Ordinary",
                          {
                            "id": {
                              "`Type": [
                                {
                                  "`Module": [
                                    { "`Root": [ "None", "C" ] }, "A"
                                  ]
                                },
                                "b"
                              ]
                            },
                            "locs": "None",
                            "doc": [],
                            "equation": {
                              "params": [],
                              "private_": "false",
                              "manifest": {
                                "Some": {
                                  "Constr": [
                                    {
                                      "`Resolved": {
                                        "`Type": [
                                          {
                                            "`Module": [
                                              {
                                                "`Identifier": {
                                                  "`Root": [ "None", "B" ]
                                                }
                                              },
                                              "A"
                                            ]
                                          },
                                          "b"
                                        ]
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
                }
              }
            }
          ],
          "compiled": "true",
          "doc": []
        }
      }
    },
    "canonical": "None",
    "hidden": "false"
  }
