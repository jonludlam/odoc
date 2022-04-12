  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  $ odoc link test.odoc
  $ odoc html-generate test.odocl -o html
  $ odoc support-files -o html
The following should be resolved as identifier Test.A
  $ odoc_print -r test test.odocl | jq '.equation.manifest.Some.Constr[0].v["`Resolved"].v["`Type"][0].v["`Canonical"][1].v'
  {
    "`Resolved": {
      "v": {
        "`Identifier": {
          "`Module": [
            {
              "`Root": [
                "None",
                "Test"
              ]
            },
            "A"
          ]
        }
      },
      "key": [
        "138",
        "test.odoc"
      ]
    }
  }

The following should be resolved as Test.Wrapper.X

  $ odoc_print -r test2 test.odocl | jq '.equation.manifest.Some.Constr[0].v["`Resolved"].v["`Type"][0].v["`Canonical"][1].v'
  {
    "`Resolved": {
      "v": {
        "`Module": [
          {
            "v": {
              "`Identifier": {
                "`Module": [
                  {
                    "`Root": [
                      "None",
                      "Test"
                    ]
                  },
                  "Wrapper"
                ]
              }
            },
            "key": [
              "169",
              "test.odoc"
            ]
          },
          "X"
        ]
      },
      "key": [
        "183",
        "test.odoc"
      ]
    }
  }

The following should be resolved as Test.Wrapper2.X


  $ odoc_print -r test3 test.odocl | jq '.equation.manifest.Some.Constr[0].v["`Resolved"].v["`Type"][0].v["`Canonical"][1].v'
  {
    "`Resolved": {
      "v": {
        "`AliasRD": [
          {
            "v": {
              "`Canonical": [
                {
                  "v": {
                    "`AliasRD": [
                      {
                        "v": {
                          "`Hidden": {
                            "v": {
                              "`Hidden": {
                                "v": {
                                  "`Identifier": {
                                    "`Module": [
                                      {
                                        "`Root": [
                                          "None",
                                          "Test"
                                        ]
                                      },
                                      "{Wrapped2__X}3"
                                    ]
                                  }
                                },
                                "key": [
                                  "148",
                                  "test.cmti"
                                ]
                              }
                            },
                            "key": [
                              "233",
                              "test.odoc"
                            ]
                          }
                        },
                        "key": [
                          "234",
                          "test.odoc"
                        ]
                      },
                      {
                        "v": {
                          "`Dot": [
                            {
                              "v": {
                                "`Identifier": [
                                  {
                                    "`Module": [
                                      {
                                        "`Root": [
                                          "None",
                                          "Test"
                                        ]
                                      },
                                      "{Wrapper2__}4"
                                    ]
                                  },
                                  "true"
                                ]
                              },
                              "key": [
                                "231",
                                "test.odoc"
                              ]
                            },
                            "X"
                          ]
                        },
                        "key": [
                          "232",
                          "test.odoc"
                        ]
                      }
                    ]
                  },
                  "key": [
                    "235",
                    "test.odoc"
                  ]
                },
                {
                  "v": {
                    "`Dot": [
                      {
                        "v": {
                          "`Dot": [
                            {
                              "v": {
                                "`Root": "Test"
                              },
                              "key": [
                                "53",
                                "test.cmti"
                              ]
                            },
                            "Wrapper2"
                          ]
                        },
                        "key": [
                          "62",
                          "test.cmti"
                        ]
                      },
                      "X"
                    ]
                  },
                  "key": [
                    "63",
                    "test.cmti"
                  ]
                }
              ]
            },
            "key": [
              "236",
              "test.odoc"
            ]
          },
          {
            "v": {
              "`Resolved": {
                "v": {
                  "`Module": [
                    {
                      "v": {
                        "`Identifier": {
                          "`Module": [
                            {
                              "`Root": [
                                "None",
                                "Test"
                              ]
                            },
                            "Wrapper2"
                          ]
                        }
                      },
                      "key": [
                        "199",
                        "test.odoc"
                      ]
                    },
                    "X"
                  ]
                },
                "key": [
                  "245",
                  "test.odoc"
                ]
              }
            },
            "key": [
              "247",
              "test.odoc"
            ]
          }
        ]
      },
      "key": [
        "248",
        "test.odoc"
      ]
    }
  }

This should probably not resolve at all, but that's a problem for another day. currently it resolves as Test.Wrapper3.X

  $ odoc_print -r test3a test.odocl | jq '.type_.Constr[0].v["`Resolved"].v["`Type"][0].v["`Canonical"][1].v'
  {
    "`Resolved": {
      "v": {
        "`Identifier": {
          "`Module": [
            {
              "`Module": [
                {
                  "`Root": [
                    "None",
                    "Test"
                  ]
                },
                "Wrapper3"
              ]
            },
            "X"
          ]
        }
      },
      "key": [
        "287",
        "test.odoc"
      ]
    }
  }

Should resolve as identifier Test.B
  $ odoc_print -r test4 test.odocl | jq '.equation.manifest.Some.Constr[0].v["`Resolved"].v["`Type"][0].v["`Canonical"][1].v'
  {
    "`Resolved": {
      "v": {
        "`Identifier": {
          "`Module": [
            {
              "`Root": [
                "None",
                "Test"
              ]
            },
            "B"
          ]
        }
      },
      "key": [
        "313",
        "test.odoc"
      ]
    }
  }

Should resove to be an alias!
  $ odoc_print -r test5 test.odocl | jq '.equation.manifest.Some.Constr[0].v["`Resolved"].v["`Type"][0].v["`Canonical"][1].v'
  {
    "`Resolved": {
      "v": {
        "`AliasRD": [
          {
            "v": {
              "`Identifier": {
                "`Module": [
                  {
                    "`Root": [
                      "None",
                      "Test"
                    ]
                  },
                  "C_"
                ]
              }
            },
            "key": [
              "193",
              "test.cmti"
            ]
          },
          {
            "v": {
              "`Resolved": {
                "v": {
                  "`Identifier": {
                    "`Module": [
                      {
                        "`Root": [
                          "None",
                          "Test"
                        ]
                      },
                      "C"
                    ]
                  }
                },
                "key": [
                  "338",
                  "test.odoc"
                ]
              }
            },
            "key": [
              "346",
              "test.odoc"
            ]
          }
        ]
      },
      "key": [
        "347",
        "test.odoc"
      ]
    }
  }
