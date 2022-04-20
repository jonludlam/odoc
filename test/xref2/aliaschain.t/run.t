A long chain of aliases should produce an odocl file that's a reasonable
size.

  $ ocamlc -c -bin-annot chain.mli
  $ odoc compile chain.cmti
  $ odoc link chain.odoc -I .
  $ odoc_print -r M5 chain.odocl
  {
    "id": { "`Module": [ { "`Root": [ "None", "Chain" ] }, "M5" ] },
    "doc": [],
    "type_": {
      "Alias": [
        {
          "`Resolved": {
            "`Canonical": [
              {
                "`Alias": [
                  {
                    "`Alias": [
                      {
                        "`Canonical": [
                          {
                            "`Alias": [
                              {
                                "`Alias": [
                                  {
                                    "`Canonical": [
                                      {
                                        "`Alias": [
                                          {
                                            "`Alias": [
                                              {
                                                "`Canonical": [
                                                  {
                                                    "`Alias": [
                                                      {
                                                        "`Alias": [
                                                          {
                                                            "`Canonical": [
                                                              {
                                                                "`Alias": [
                                                                  {
                                                                    "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                      ]
                                                                    }
                                                                  },
                                                                  {
                                                                    "`Identifier": [
                                                                      {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M1__}1"
                                                                      ]
                                                                      },
                                                                      "true"
                                                                    ]
                                                                  }
                                                                ]
                                                              },
                                                              {
                                                                "`Dot": [
                                                                  {
                                                                    "`Root":
                                                                      "Chain"
                                                                  },
                                                                  "M1"
                                                                ]
                                                              }
                                                            ]
                                                          },
                                                          {
                                                            "`Identifier": [
                                                              {
                                                                "`Module": [
                                                                  {
                                                                    "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                    ]
                                                                  },
                                                                  "M1"
                                                                ]
                                                              },
                                                              "false"
                                                            ]
                                                          }
                                                        ]
                                                      },
                                                      {
                                                        "`Identifier": [
                                                          {
                                                            "`Module": [
                                                              {
                                                                "`Root": [
                                                                  "None",
                                                                  "Chain"
                                                                ]
                                                              },
                                                              "{M2__}2"
                                                            ]
                                                          },
                                                          "true"
                                                        ]
                                                      }
                                                    ]
                                                  },
                                                  {
                                                    "`Dot": [
                                                      { "`Root": "Chain" },
                                                      "M2"
                                                    ]
                                                  }
                                                ]
                                              },
                                              {
                                                "`Identifier": [
                                                  {
                                                    "`Module": [
                                                      {
                                                        "`Root": [
                                                          "None", "Chain"
                                                        ]
                                                      },
                                                      "M2"
                                                    ]
                                                  },
                                                  "false"
                                                ]
                                              }
                                            ]
                                          },
                                          {
                                            "`Identifier": [
                                              {
                                                "`Module": [
                                                  {
                                                    "`Root": [
                                                      "None", "Chain"
                                                    ]
                                                  },
                                                  "{M3__}3"
                                                ]
                                              },
                                              "true"
                                            ]
                                          }
                                        ]
                                      },
                                      {
                                        "`Dot": [ { "`Root": "Chain" }, "M3" ]
                                      }
                                    ]
                                  },
                                  {
                                    "`Identifier": [
                                      {
                                        "`Module": [
                                          { "`Root": [ "None", "Chain" ] },
                                          "M3"
                                        ]
                                      },
                                      "false"
                                    ]
                                  }
                                ]
                              },
                              {
                                "`Identifier": [
                                  {
                                    "`Module": [
                                      { "`Root": [ "None", "Chain" ] },
                                      "{M4__}4"
                                    ]
                                  },
                                  "true"
                                ]
                              }
                            ]
                          },
                          { "`Dot": [ { "`Root": "Chain" }, "M4" ] }
                        ]
                      },
                      {
                        "`Identifier": [
                          {
                            "`Module": [
                              { "`Root": [ "None", "Chain" ] },
                              "M4"
                            ]
                          },
                          "false"
                        ]
                      }
                    ]
                  },
                  {
                    "`Identifier": [
                      {
                        "`Module": [
                          { "`Root": [ "None", "Chain" ] },
                          "{M5__}5"
                        ]
                      },
                      "true"
                    ]
                  }
                ]
              },
              {
                "`Resolved": {
                  "`Alias": [
                    {
                      "`Canonical": [
                        {
                          "`Alias": [
                            {
                              "`Alias": [
                                {
                                  "`Canonical": [
                                    {
                                      "`Alias": [
                                        {
                                          "`Alias": [
                                            {
                                              "`Canonical": [
                                                {
                                                  "`Alias": [
                                                    {
                                                      "`Alias": [
                                                        {
                                                          "`Canonical": [
                                                            {
                                                              "`Alias": [
                                                                {
                                                                  "`Alias": [
                                                                    {
                                                                      "`Canonical": [
                                                                      {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                      ]
                                                                      }
                                                                      },
                                                                      {
                                                                      "`Identifier": [
                                                                      {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M1__}1"
                                                                      ]
                                                                      },
                                                                      "true"
                                                                      ]
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Dot": [
                                                                      {
                                                                      "`Root":
                                                                      "Chain"
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      ]
                                                                    },
                                                                    {
                                                                      "`Identifier": [
                                                                      {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      },
                                                                      "false"
                                                                      ]
                                                                    }
                                                                  ]
                                                                },
                                                                {
                                                                  "`Identifier": [
                                                                    {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M2__}2"
                                                                      ]
                                                                    },
                                                                    "true"
                                                                  ]
                                                                }
                                                              ]
                                                            },
                                                            {
                                                              "`Dot": [
                                                                {
                                                                  "`Root":
                                                                    "Chain"
                                                                },
                                                                "M2"
                                                              ]
                                                            }
                                                          ]
                                                        },
                                                        {
                                                          "`Identifier": [
                                                            {
                                                              "`Module": [
                                                                {
                                                                  "`Root": [
                                                                    "None",
                                                                    "Chain"
                                                                  ]
                                                                },
                                                                "M2"
                                                              ]
                                                            },
                                                            "false"
                                                          ]
                                                        }
                                                      ]
                                                    },
                                                    {
                                                      "`Identifier": [
                                                        {
                                                          "`Module": [
                                                            {
                                                              "`Root": [
                                                                "None", "Chain"
                                                              ]
                                                            },
                                                            "{M3__}3"
                                                          ]
                                                        },
                                                        "true"
                                                      ]
                                                    }
                                                  ]
                                                },
                                                {
                                                  "`Dot": [
                                                    { "`Root": "Chain" },
                                                    "M3"
                                                  ]
                                                }
                                              ]
                                            },
                                            {
                                              "`Identifier": [
                                                {
                                                  "`Module": [
                                                    {
                                                      "`Root": [
                                                        "None", "Chain"
                                                      ]
                                                    },
                                                    "M3"
                                                  ]
                                                },
                                                "false"
                                              ]
                                            }
                                          ]
                                        },
                                        {
                                          "`Identifier": [
                                            {
                                              "`Module": [
                                                {
                                                  "`Root": [ "None", "Chain" ]
                                                },
                                                "{M4__}4"
                                              ]
                                            },
                                            "true"
                                          ]
                                        }
                                      ]
                                    },
                                    { "`Dot": [ { "`Root": "Chain" }, "M4" ] }
                                  ]
                                },
                                {
                                  "`Identifier": [
                                    {
                                      "`Module": [
                                        { "`Root": [ "None", "Chain" ] },
                                        "M4"
                                      ]
                                    },
                                    "false"
                                  ]
                                }
                              ]
                            },
                            {
                              "`Identifier": [
                                {
                                  "`Module": [
                                    { "`Root": [ "None", "Chain" ] },
                                    "{M5__}5"
                                  ]
                                },
                                "true"
                              ]
                            }
                          ]
                        },
                        { "`Dot": [ { "`Root": "Chain" }, "M5" ] }
                      ]
                    },
                    {
                      "`Identifier": [
                        {
                          "`Module": [ { "`Root": [ "None", "Chain" ] }, "M5" ]
                        },
                        "false"
                      ]
                    }
                  ]
                }
              }
            ]
          }
        },
        "None"
      ]
    },
    "canonical": "None",
    "hidden": "false"
  }
  $ find . -name chain.odocl -size +100000c 
  ./chain.odocl
