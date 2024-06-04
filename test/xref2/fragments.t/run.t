  $ ocamlc -c -bin-annot x.mli
  $ odoc compile x.cmti
  $ odoc_print -r T3 x.odoc
  {
    "id": { "`ModuleType": [ { "`Root": [ "None", "X" ] }, "T3" ] },
    "source_loc": "None",
    "doc": [],
    "canonical": "None",
    "expr": {
      "Some": {
        "With": {
          "w_substitutions": [
            {
              "ModuleTypeEq": [
                {
                  "`Resolved": {
                    "`Module_type": [
                      {
                        "`Root": {
                          "`ModuleType": {
                            "`Identifier": {
                              "`ModuleType": [
                                { "`Root": [ "None", "X" ] }, "S"
                              ]
                            }
                          }
                        }
                      },
                      "S"
                    ]
                  }
                },
                {
                  "Path": {
                    "p_expansion": {
                      "Some": {
                        "Signature": {
                          "items": [
                            {
                              "Type": [
                                "Ordinary",
                                {
                                  "id": {
                                    "`Type": [
                                      {
                                        "`ModuleType": [
                                          { "`Root": [ "None", "X" ] }, "T3"
                                        ]
                                      },
                                      "t"
                                    ]
                                  },
                                  "source_loc": "None",
                                  "doc": [],
                                  "equation": {
                                    "params": [],
                                    "private_": "false",
                                    "manifest": "None",
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
                    },
                    "p_path": {
                      "`Resolved": {
                        "`Identifier": {
                          "`ModuleType": [ { "`Root": [ "None", "X" ] }, "T2" ]
                        }
                      }
                    }
                  }
                }
              ]
            },
            {
              "TypeEq": [
                {
                  "`Resolved": {
                    "`Type": [
                      {
                        "`Module": [
                          {
                            "`Root": {
                              "`ModuleType": {
                                "`Identifier": {
                                  "`ModuleType": [
                                    { "`Root": [ "None", "X" ] }, "S"
                                  ]
                                }
                              }
                            }
                          },
                          "T"
                        ]
                      },
                      "t"
                    ]
                  }
                },
                {
                  "params": [],
                  "private_": "false",
                  "manifest": {
                    "Some": {
                      "Constr": [
                        {
                          "`Resolved": {
                            "`Identifier": { "`CoreType": "int" }
                          }
                        },
                        []
                      ]
                    }
                  },
                  "constraints": []
                }
              ]
            }
          ],
          "w_expansion": {
            "Some": {
              "Signature": {
                "items": [
                  {
                    "ModuleType": {
                      "id": {
                        "`ModuleType": [
                          {
                            "`ModuleType": [
                              { "`Root": [ "None", "X" ] }, "T3"
                            ]
                          },
                          "S"
                        ]
                      },
                      "source_loc": "None",
                      "doc": [],
                      "canonical": "None",
                      "expr": {
                        "Some": {
                          "Path": {
                            "p_expansion": "None",
                            "p_path": {
                              "`Resolved": {
                                "`Identifier": {
                                  "`ModuleType": [
                                    { "`Root": [ "None", "X" ] }, "T2"
                                  ]
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  },
                  {
                    "Module": [
                      "Ordinary",
                      {
                        "id": {
                          "`Module": [
                            {
                              "`ModuleType": [
                                { "`Root": [ "None", "X" ] }, "T3"
                              ]
                            },
                            "T"
                          ]
                        },
                        "source_loc": "None",
                        "doc": [],
                        "type_": {
                          "ModuleType": {
                            "With": {
                              "w_substitutions": [
                                {
                                  "TypeEq": [
                                    {
                                      "`Resolved": {
                                        "`Type": [
                                          {
                                            "`Root": {
                                              "`ModuleType": {
                                                "`OpaqueModuleType": {
                                                  "`Identifier": {
                                                    "`ModuleType": [
                                                      {
                                                        "`ModuleType": [
                                                          {
                                                            "`Root": [
                                                              "None", "X"
                                                            ]
                                                          },
                                                          "T3"
                                                        ]
                                                      },
                                                      "S"
                                                    ]
                                                  }
                                                }
                                              }
                                            }
                                          },
                                          "t"
                                        ]
                                      }
                                    },
                                    {
                                      "params": [],
                                      "private_": "false",
                                      "manifest": {
                                        "Some": {
                                          "Constr": [
                                            {
                                              "`Resolved": {
                                                "`Identifier": {
                                                  "`CoreType": "int"
                                                }
                                              }
                                            },
                                            []
                                          ]
                                        }
                                      },
                                      "constraints": []
                                    }
                                  ]
                                }
                              ],
                              "w_expansion": {
                                "Some": {
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
                                                            "None", "X"
                                                          ]
                                                        },
                                                        "T3"
                                                      ]
                                                    },
                                                    "T"
                                                  ]
                                                },
                                                "t"
                                              ]
                                            },
                                            "source_loc": "None",
                                            "doc": [],
                                            "equation": {
                                              "params": [],
                                              "private_": "false",
                                              "manifest": {
                                                "Some": {
                                                  "Constr": [
                                                    {
                                                      "`Resolved": {
                                                        "`Identifier": {
                                                          "`CoreType": "int"
                                                        }
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
                              },
                              "w_expr": {
                                "Path": {
                                  "`Resolved": {
                                    "`OpaqueModuleType": {
                                      "`Identifier": {
                                        "`ModuleType": [
                                          {
                                            "`ModuleType": [
                                              { "`Root": [ "None", "X" ] },
                                              "T3"
                                            ]
                                          },
                                          "S"
                                        ]
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        },
                        "canonical": "None",
                        "hidden": "false"
                      }
                    ]
                  }
                ],
                "compiled": "true",
                "doc": []
              }
            }
          },
          "w_expr": {
            "Path": {
              "`Resolved": {
                "`Identifier": {
                  "`ModuleType": [ { "`Root": [ "None", "X" ] }, "S" ]
                }
              }
            }
          }
        }
      }
    }
  }
  $ odoc link x.odoc
  $ odoc html-generate -o html x.odocl
  $ odoc support-files -o html
  $ rsync -avz html /tmp/ > /dev/null
