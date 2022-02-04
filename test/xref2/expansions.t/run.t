  $ ocamlc -bin-annot -w -49 -c -impl sexplib0__.ml-gen -no-alias-deps
  $ ocamlc -bin-annot -c sexplib0__Sexp.mli
  $ ocamlc -bin-annot -c sexplib0.ml -open Sexplib0__

  $ odoc compile sexplib0__.cmt -I .
  $ odoc compile sexplib0__Sexp.cmti -I . 
  $ odoc compile sexplib0.cmt -I . 

  $ odoc link sexplib0.odoc -I .
  $ odoc_print sexplib0.odocl
  {
    "id": { "`Root": [ "None", "Sexplib0" ] },
    "root": "<root>",
    "digest": "<digest>",
    "imports": [
      { "Unresolved": [ "CamlinternalFormatBasics", { "Some": "<digest>" } ] },
      { "Resolved": [ "<root>", "Sexplib0__" ] },
      { "Unresolved": [ "Stdlib", { "Some": "<digest>" } ] }
    ],
    "source": {
      "Some": {
        "file": "sexplib0.ml",
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
                  "`Module": [ { "`Root": [ "None", "Sexplib0" ] }, "Sexp" ]
                },
                "doc": [],
                "type_": {
                  "Alias": [
                    {
                      "`Resolved": {
                        "`Canonical": [
                          {
                            "`Module": [
                              {
                                "`Hidden": {
                                  "`Identifier": {
                                    "`Root": [ "None", "Sexplib0__" ]
                                  }
                                }
                              },
                              "Sexp"
                            ]
                          },
                          {
                            "`Resolved": {
                              "`Alias": [
                                {
                                  "`Canonical": [
                                    {
                                      "`Module": [
                                        {
                                          "`Hidden": {
                                            "`Identifier": {
                                              "`Root": [ "None", "Sexplib0__" ]
                                            }
                                          }
                                        },
                                        "Sexp"
                                      ]
                                    },
                                    {
                                      "`Dot": [
                                        { "`Root": "Sexplib0" },
                                        "Sexp"
                                      ]
                                    }
                                  ]
                                },
                                {
                                  "`Identifier": {
                                    "`Module": [
                                      { "`Root": [ "None", "Sexplib0" ] },
                                      "Sexp"
                                    ]
                                  }
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
                              "Type": [
                                "Ordinary",
                                {
                                  "id": {
                                    "`Type": [
                                      {
                                        "`Module": [
                                          { "`Root": [ "None", "Sexplib0" ] },
                                          "Sexp"
                                        ]
                                      },
                                      "t"
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
                                                          "None", "Sexplib0"
                                                        ]
                                                      },
                                                      "Sexp"
                                                    ]
                                                  },
                                                  "t"
                                                ]
                                              },
                                              "Atom"
                                            ]
                                          },
                                          "doc": [],
                                          "args": {
                                            "Tuple": [
                                              {
                                                "Constr": [
                                                  {
                                                    "`Resolved": {
                                                      "`Identifier": {
                                                        "`CoreType": "string"
                                                      }
                                                    }
                                                  },
                                                  []
                                                ]
                                              }
                                            ]
                                          },
                                          "res": "None"
                                        },
                                        {
                                          "id": {
                                            "`Constructor": [
                                              {
                                                "`Type": [
                                                  {
                                                    "`Module": [
                                                      {
                                                        "`Root": [
                                                          "None", "Sexplib0"
                                                        ]
                                                      },
                                                      "Sexp"
                                                    ]
                                                  },
                                                  "t"
                                                ]
                                              },
                                              "List"
                                            ]
                                          },
                                          "doc": [],
                                          "args": {
                                            "Tuple": [
                                              {
                                                "Constr": [
                                                  {
                                                    "`Resolved": {
                                                      "`Identifier": {
                                                        "`CoreType": "list"
                                                      }
                                                    }
                                                  },
                                                  [
                                                    {
                                                      "Constr": [
                                                        {
                                                          "`Resolved": {
                                                            "`Identifier": {
                                                              "`Type": [
                                                                {
                                                                  "`Module": [
                                                                    {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0"
                                                                      ]
                                                                    },
                                                                    "Sexp"
                                                                  ]
                                                                },
                                                                "t"
                                                              ]
                                                            }
                                                          }
                                                        },
                                                        []
                                                      ]
                                                    }
                                                  ]
                                                ]
                                              }
                                            ]
                                          },
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
                                          { "`Root": [ "None", "Sexplib0" ] },
                                          "Sexp"
                                        ]
                                      },
                                      "Private"
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
                                                                    "Sexplib0__Sexp"
                                                                  ]
                                                                }
                                                              }
                                                            },
                                                            "Private"
                                                          ]
                                                        },
                                                        {
                                                          "`Module": [
                                                            {
                                                              "`Canonical": [
                                                                {
                                                                  "`Module": [
                                                                    {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0__"
                                                                      ]
                                                                      }
                                                                      }
                                                                    },
                                                                    "Sexp"
                                                                  ]
                                                                },
                                                                {
                                                                  "`Resolved": {
                                                                    "`Alias": [
                                                                      {
                                                                      "`Canonical": [
                                                                      {
                                                                      "`Module": [
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0__"
                                                                      ]
                                                                      }
                                                                      }
                                                                      },
                                                                      "Sexp"
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Dot": [
                                                                      {
                                                                      "`Root":
                                                                      "Sexplib0"
                                                                      },
                                                                      "Sexp"
                                                                      ]
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0"
                                                                      ]
                                                                      },
                                                                      "Sexp"
                                                                      ]
                                                                      }
                                                                      }
                                                                    ]
                                                                  }
                                                                }
                                                              ]
                                                            },
                                                            "Private"
                                                          ]
                                                        }
                                                      ]
                                                    },
                                                    {
                                                      "`Resolved": {
                                                        "`Alias": [
                                                          {
                                                            "`Alias": [
                                                              {
                                                                "`Module": [
                                                                  {
                                                                    "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0__Sexp"
                                                                      ]
                                                                      }
                                                                    }
                                                                  },
                                                                  "Private"
                                                                ]
                                                              },
                                                              {
                                                                "`Module": [
                                                                  {
                                                                    "`Canonical": [
                                                                      {
                                                                      "`Module": [
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0__"
                                                                      ]
                                                                      }
                                                                      }
                                                                      },
                                                                      "Sexp"
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Dot": [
                                                                      {
                                                                      "`Root":
                                                                      "Sexplib0"
                                                                      },
                                                                      "Sexp"
                                                                      ]
                                                                      }
                                                                    ]
                                                                  },
                                                                  "Private"
                                                                ]
                                                              }
                                                            ]
                                                          },
                                                          {
                                                            "`Module": [
                                                              {
                                                                "`Alias": [
                                                                  {
                                                                    "`Canonical": [
                                                                      {
                                                                      "`Module": [
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0__"
                                                                      ]
                                                                      }
                                                                      }
                                                                      },
                                                                      "Sexp"
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Dot": [
                                                                      {
                                                                      "`Root":
                                                                      "Sexplib0"
                                                                      },
                                                                      "Sexp"
                                                                      ]
                                                                      }
                                                                    ]
                                                                  },
                                                                  {
                                                                    "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0"
                                                                      ]
                                                                      },
                                                                      "Sexp"
                                                                      ]
                                                                    }
                                                                  }
                                                                ]
                                                              },
                                                              "Private"
                                                            ]
                                                          }
                                                        ]
                                                      }
                                                    }
                                                  ]
                                                },
                                                {
                                                  "`Module": [
                                                    {
                                                      "`Alias": [
                                                        {
                                                          "`Canonical": [
                                                            {
                                                              "`Module": [
                                                                {
                                                                  "`Hidden": {
                                                                    "`Identifier": {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0__"
                                                                      ]
                                                                    }
                                                                  }
                                                                },
                                                                "Sexp"
                                                              ]
                                                            },
                                                            {
                                                              "`Resolved": {
                                                                "`Alias": [
                                                                  {
                                                                    "`Canonical": [
                                                                      {
                                                                      "`Module": [
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0__"
                                                                      ]
                                                                      }
                                                                      }
                                                                      },
                                                                      "Sexp"
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Dot": [
                                                                      {
                                                                      "`Root":
                                                                      "Sexplib0"
                                                                      },
                                                                      "Sexp"
                                                                      ]
                                                                      }
                                                                    ]
                                                                  },
                                                                  {
                                                                    "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0"
                                                                      ]
                                                                      },
                                                                      "Sexp"
                                                                      ]
                                                                    }
                                                                  }
                                                                ]
                                                              }
                                                            }
                                                          ]
                                                        },
                                                        {
                                                          "`Identifier": {
                                                            "`Module": [
                                                              {
                                                                "`Root": [
                                                                  "None",
                                                                  "Sexplib0"
                                                                ]
                                                              },
                                                              "Sexp"
                                                            ]
                                                          }
                                                        }
                                                      ]
                                                    },
                                                    "Private"
                                                  ]
                                                }
                                              ]
                                            },
                                            {
                                              "`Resolved": {
                                                "`Alias": [
                                                  {
                                                    "`Alias": [
                                                      {
                                                        "`Module": [
                                                          {
                                                            "`Hidden": {
                                                              "`Identifier": {
                                                                "`Root": [
                                                                  "None",
                                                                  "Sexplib0__Sexp"
                                                                ]
                                                              }
                                                            }
                                                          },
                                                          "Private"
                                                        ]
                                                      },
                                                      {
                                                        "`Module": [
                                                          {
                                                            "`Canonical": [
                                                              {
                                                                "`Module": [
                                                                  {
                                                                    "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0__"
                                                                      ]
                                                                      }
                                                                    }
                                                                  },
                                                                  "Sexp"
                                                                ]
                                                              },
                                                              {
                                                                "`Dot": [
                                                                  {
                                                                    "`Root":
                                                                      "Sexplib0"
                                                                  },
                                                                  "Sexp"
                                                                ]
                                                              }
                                                            ]
                                                          },
                                                          "Private"
                                                        ]
                                                      }
                                                    ]
                                                  },
                                                  {
                                                    "`Module": [
                                                      {
                                                        "`Alias": [
                                                          {
                                                            "`Canonical": [
                                                              {
                                                                "`Module": [
                                                                  {
                                                                    "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Sexplib0__"
                                                                      ]
                                                                      }
                                                                    }
                                                                  },
                                                                  "Sexp"
                                                                ]
                                                              },
                                                              {
                                                                "`Dot": [
                                                                  {
                                                                    "`Root":
                                                                      "Sexplib0"
                                                                  },
                                                                  "Sexp"
                                                                ]
                                                              }
                                                            ]
                                                          },
                                                          {
                                                            "`Identifier": {
                                                              "`Module": [
                                                                {
                                                                  "`Root": [
                                                                    "None",
                                                                    "Sexplib0"
                                                                  ]
                                                                },
                                                                "Sexp"
                                                              ]
                                                            }
                                                          }
                                                        ]
                                                      },
                                                      "Private"
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
                                                                  "Sexplib0"
                                                                ]
                                                              },
                                                              "Sexp"
                                                            ]
                                                          },
                                                          "Private"
                                                        ]
                                                      },
                                                      "x"
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
                                                                    "Sexplib0__Sexp"
                                                                  ]
                                                                }
                                                              }
                                                            },
                                                            "t"
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
                                            { "`Root": "Sexplib0" },
                                            "Sexp"
                                          ]
                                        },
                                        "Private"
                                      ]
                                    }
                                  },
                                  "hidden": "false"
                                }
                              ]
                            }
                          ],
                          "compiled": "false",
                          "doc": [
                            {
                              "`Paragraph": [
                                { "`Word": "Type" },
                                "`Space",
                                { "`Word": "of" },
                                "`Space",
                                { "`Word": "S-expressions" }
                              ]
                            }
                          ]
                        }
                      }
                    }
                  ]
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
    },
    "expansion": "None",
    "canonical": "None"
  }
  $ odoc html-generate -o html sexplib0.odocl --indent
  Warning, resolved hidden path: Sexplib0__Sexp.t

  $ odoc support-files -o html

  $ cat html/Sexplib0/Sexp/Private/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>Private (Sexplib0.Sexp.Private)</title>
    <link rel="stylesheet" href="../../../odoc.css"/><meta charset="utf-8"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../../index.html">Sexplib0</a> &#x00BB; 
     <a href="../index.html">Sexp</a> &#x00BB; Private
    </nav>
    <header class="odoc-preamble">
     <h1>Module <code><span>Sexp.Private</span></code></h1>
    </header>
    <div class="odoc-content">
     <div class="odoc-spec">
      <div class="spec value" id="val-x" class="anchored">
       <a href="#val-x" class="anchor"></a>
       <code>
        <span><span class="keyword">val</span> x : 
         <span class="xref-unresolved">Sexplib0__Sexp.t</span>
        </span>
       </code>
      </div>
     </div>
    </div>
   </body>
  </html>

