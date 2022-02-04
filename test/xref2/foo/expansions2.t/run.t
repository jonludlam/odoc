  $ ocamlc -c -bin-annot slib__Bigarray.mli
  $ ocamlc -c -bin-annot slib.mli

  $ odoc compile -I . slib__Bigarray.cmti
  $ odoc compile -I . slib.cmti
  $ odoc link -I . slib.odoc
  $ odoc_print slib.odocl
  {
    "id": { "`Root": [ "None", "Slib" ] },
    "root": "<root>",
    "digest": "<digest>",
    "imports": [
      { "Unresolved": [ "CamlinternalFormatBasics", { "Some": "<digest>" } ] },
      { "Resolved": [ "<root>", "Slib__Bigarray" ] },
      { "Unresolved": [ "Stdlib", { "Some": "<digest>" } ] }
    ],
    "source": {
      "Some": {
        "file": "slib.mli",
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
            "Module": [
              "Ordinary",
              {
                "id": {
                  "`Module": [ { "`Root": [ "None", "Slib" ] }, "Bigarray" ]
                },
                "doc": [],
                "type_": {
                  "Alias": [
                    {
                      "`Resolved": {
                        "`Hidden": {
                          "`Identifier": {
                            "`Root": [ "None", "Slib__Bigarray" ]
                          }
                        }
                      }
                    },
                    {
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
                                          { "`Root": [ "None", "Slib" ] },
                                          "Bigarray"
                                        ]
                                      },
                                      "kind"
                                    ]
                                  },
                                  "doc": [],
                                  "equation": {
                                    "params": [],
                                    "private_": "false",
                                    "manifest": "None",
                                    "constraints": []
                                  },
                                  "representation": {
                                    "Some": {
                                      "Variant": [
                                        {
                                          "id": {
                                            "`Constructor": [
                                              {
                                                "`Type": [
                                                  {
                                                    "`Module": [
                                                      {
                                                        "`Root": [
                                                          "None", "Slib"
                                                        ]
                                                      },
                                                      "Bigarray"
                                                    ]
                                                  },
                                                  "kind"
                                                ]
                                              },
                                              "Foo"
                                            ]
                                          },
                                          "doc": [],
                                          "args": { "Tuple": [] },
                                          "res": "None"
                                        }
                                      ]
                                    }
                                  }
                                }
                              ]
                            },
                            {
                              "Module": [
                                "Ordinary",
                                {
                                  "id": {
                                    "`Module": [
                                      {
                                        "`Module": [
                                          { "`Root": [ "None", "Slib" ] },
                                          "Bigarray"
                                        ]
                                      },
                                      "Genarray"
                                    ]
                                  },
                                  "doc": [],
                                  "type_": {
                                    "Alias": [
                                      {
                                        "`Resolved": {
                                          "`Canonical": [
                                            {
                                              "`Alias": [
                                                {
                                                  "`Module": [
                                                    {
                                                      "`Hidden": {
                                                        "`Identifier": {
                                                          "`Root": [
                                                            "None",
                                                            "Slib__Bigarray"
                                                          ]
                                                        }
                                                      }
                                                    },
                                                    "Genarray"
                                                  ]
                                                },
                                                {
                                                  "`Module": [
                                                    {
                                                      "`Canonical": [
                                                        {
                                                          "`Alias": [
                                                            {
                                                              "`Hidden": {
                                                                "`Identifier": {
                                                                  "`Root": [
                                                                    "None",
                                                                    "Slib__Bigarray"
                                                                  ]
                                                                }
                                                              }
                                                            },
                                                            {
                                                              "`Identifier": {
                                                                "`Module": [
                                                                  {
                                                                    "`Root": [
                                                                      "None",
                                                                      "Slib"
                                                                    ]
                                                                  },
                                                                  "Bigarray"
                                                                ]
                                                              }
                                                            }
                                                          ]
                                                        },
                                                        {
                                                          "`Resolved": {
                                                            "`Alias": [
                                                              {
                                                                "`Hidden": {
                                                                  "`Identifier": {
                                                                    "`Root": [
                                                                      "None",
                                                                      "Slib__Bigarray"
                                                                    ]
                                                                  }
                                                                }
                                                              },
                                                              {
                                                                "`Identifier": {
                                                                  "`Module": [
                                                                    {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Slib"
                                                                      ]
                                                                    },
                                                                    "Bigarray"
                                                                  ]
                                                                }
                                                              }
                                                            ]
                                                          }
                                                        }
                                                      ]
                                                    },
                                                    "Genarray"
                                                  ]
                                                }
                                              ]
                                            },
                                            {
                                              "`Resolved": {
                                                "`Alias": [
                                                  {
                                                    "`Module": [
                                                      {
                                                        "`Hidden": {
                                                          "`Identifier": {
                                                            "`Root": [
                                                              "None",
                                                              "Slib__Bigarray"
                                                            ]
                                                          }
                                                        }
                                                      },
                                                      "Genarray"
                                                    ]
                                                  },
                                                  {
                                                    "`Module": [
                                                      {
                                                        "`Alias": [
                                                          {
                                                            "`Hidden": {
                                                              "`Identifier": {
                                                                "`Root": [
                                                                  "None",
                                                                  "Slib__Bigarray"
                                                                ]
                                                              }
                                                            }
                                                          },
                                                          {
                                                            "`Identifier": {
                                                              "`Module": [
                                                                {
                                                                  "`Root": [
                                                                    "None",
                                                                    "Slib"
                                                                  ]
                                                                },
                                                                "Bigarray"
                                                              ]
                                                            }
                                                          }
                                                        ]
                                                      },
                                                      "Genarray"
                                                    ]
                                                  }
                                                ]
                                              }
                                            }
                                          ]
                                        }
                                      },
                                      {
                                        "Some": {
                                          "Signature": {
                                            "items": [
                                              {
                                                "Value": {
                                                  "id": {
                                                    "`Value": [
                                                      {
                                                        "`Module": [
                                                          {
                                                            "`Module": [
                                                              {
                                                                "`Root": [
                                                                  "None",
                                                                  "Slib"
                                                                ]
                                                              },
                                                              "Bigarray"
                                                            ]
                                                          },
                                                          "Genarray"
                                                        ]
                                                      },
                                                      "f"
                                                    ]
                                                  },
                                                  "doc": [],
                                                  "type_": {
                                                    "Constr": [
                                                      {
                                                        "`Resolved": {
                                                          "`Type": [
                                                            {
                                                              "`Hidden": {
                                                                "`Identifier": {
                                                                  "`Root": [
                                                                    "None",
                                                                    "Slib__Bigarray"
                                                                  ]
                                                                }
                                                              }
                                                            },
                                                            "kind"
                                                          ]
                                                        }
                                                      },
                                                      []
                                                    ]
                                                  },
                                                  "value": "Abstract"
                                                }
                                              }
                                            ],
                                            "compiled": "true",
                                            "doc": []
                                          }
                                        }
                                      }
                                    ]
                                  },
                                  "canonical": {
                                    "Some": {
                                      "`Dot": [
                                        {
                                          "`Dot": [
                                            { "`Root": "Slib" },
                                            "Bigarray"
                                          ]
                                        },
                                        "Genarray"
                                      ]
                                    }
                                  },
                                  "hidden": "false"
                                }
                              ]
                            }
                          ],
                          "compiled": "false",
                          "doc": []
                        }
                      }
                    }
                  ]
                },
                "canonical": {
                  "Some": { "`Dot": [ { "`Root": "Slib" }, "Bigarray" ] }
                },
                "hidden": "false"
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
  $ odoc html-generate -o html slib.odocl
  Warning, resolved hidden path: Slib__Bigarray.kind
  $ odoc support-files -o html
  $ rsync -az html /tmp/myhtml
