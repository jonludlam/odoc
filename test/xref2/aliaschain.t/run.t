A long chain of aliases should produce an odocl file that's a reasonable
size.

  $ ocamlc -c -bin-annot chain.mli
  $ odoc compile chain.cmti
  $ odoc link chain.odoc -I .
  module: (root Chain).A
  module: (root Chain).A.B
  module: (root Chain).{M1__}1
  module: (root Chain).M1
  module: (root Chain).M1.B
  module: (root Chain).M1B
  module: (root Chain).{M2__}2
  module: (root Chain).M2
  module: (root Chain).M2.B
  module: (root Chain).M2B
  module: (root Chain).{M3__}3
  module: (root Chain).M3
  module: (root Chain).M3.B
  module: (root Chain).M3B
  module: (root Chain).{M4__}4
  module: (root Chain).M4
  module: (root Chain).M4.B
  module: (root Chain).M4B
  module: (root Chain).{M5__}5
  module: (root Chain).M5
  module: (root Chain).M5.B
  module: (root Chain).M5B
  module: (root Chain).{M6__}6
  module: (root Chain).M6
  module: (root Chain).M6.B
  module: (root Chain).M6B
  module: (root Chain).{M7__}7
  module: (root Chain).M7
  module: (root Chain).M7.B
  module: (root Chain).M7B
  module: (root Chain).{M8__}8
  module: (root Chain).M8
  module: (root Chain).M8.B
  module: (root Chain).M8B
  module: (root Chain).{M9__}9
  module: (root Chain).M9
  module: (root Chain).M9.B
  module: (root Chain).M9B
  module: (root Chain).{M10__}10
  module: (root Chain).M10
  module: (root Chain).M10.B
  module: (root Chain).M10B
  module: (root Chain).{M11__}11
  module: (root Chain).M11
  module: (root Chain).M11.B
  module: (root Chain).M11B
  module: (root Chain).{M12__}12
  module: (root Chain).M12
  module: (root Chain).M12.B
  module: (root Chain).M12B
  module: (root Chain).{M13__}13
  module: (root Chain).M13
  module: (root Chain).M13.B
  module: (root Chain).M13B
  module: (root Chain).{M14__}14
  module: (root Chain).M14
  module: (root Chain).M14.B
  module: (root Chain).M14B
  module: (root Chain).{M15__}15
  module: (root Chain).M15
  module: (root Chain).M15.B
  module: (root Chain).M15B
  module: (root Chain).{M16__}16
  module: (root Chain).M16
  module: (root Chain).M16.B
  module: (root Chain).M16B
  module: (root Chain).{M17__}17
  module: (root Chain).M17
  module: (root Chain).M17.B
  module: (root Chain).M17B
  module: (root Chain).{M18__}18
  module: (root Chain).M18
  module: (root Chain).M18.B
  module: (root Chain).M18B
  module: (root Chain).{M19__}19
  module: (root Chain).M19
  module: (root Chain).M19.B
  module: (root Chain).M19B
  module: (root Chain).{M20__}20
  module: (root Chain).M20
  module: (root Chain).M20.B
  module: (root Chain).M20B
  module: (root Chain).{M21__}21
  module: (root Chain).M21
  module: (root Chain).M21.B
  module: (root Chain).M21B
  module: (root Chain).{M22__}22
  module: (root Chain).M22
  module: (root Chain).M22.B
  module: (root Chain).M22B
  module: (root Chain).{M23__}23
  module: (root Chain).M23
  module: (root Chain).M23.B
  module: (root Chain).M23B
  module: (root Chain).{M24__}24
  module: (root Chain).M24
  module: (root Chain).M24.B
  module: (root Chain).M24B
  module: (root Chain).{M25__}25
  module: (root Chain).M25
  module: (root Chain).M25.B
  module: (root Chain).M25B
  module: (root Chain).{M26__}26
  module: (root Chain).M26
  module: (root Chain).M26.B
  module: (root Chain).M26B
  module: (root Chain).{M27__}27
  module: (root Chain).M27
  module: (root Chain).M27.B
  module: (root Chain).M27B
  module: (root Chain).{M28__}28
  module: (root Chain).M28
  module: (root Chain).M28.B
  module: (root Chain).M28B
  module: (root Chain).{M29__}29
  module: (root Chain).M29
  module: (root Chain).M29.B
  module: (root Chain).M29B
  module: (root Chain).{M30__}30
  module: (root Chain).M30
  module: (root Chain).M30.B
  module: (root Chain).M30B
  module: (root Chain).{M31__}31
  module: (root Chain).M31
  module: (root Chain).M31.B
  module: (root Chain).M31B
  module: (root Chain).{M32__}32
  module: (root Chain).M32
  module: (root Chain).M32.B
  module: (root Chain).M32B
  module: (root Chain).{M33__}33
  module: (root Chain).M33
  module: (root Chain).M33.B
  module: (root Chain).M33B
  module: (root Chain).{M34__}34
  module: (root Chain).M34
  module: (root Chain).M34.B
  module: (root Chain).M34B
  module: (root Chain).{M35__}35
  module: (root Chain).M35
  module: (root Chain).M35.B
  module: (root Chain).M35B
  module: (root Chain).{M36__}36
  module: (root Chain).M36
  module: (root Chain).M36.B
  module: (root Chain).M36B
  module: (root Chain).{M37__}37
  module: (root Chain).M37
  module: (root Chain).M37.B
  module: (root Chain).M37B
  module: (root Chain).{M38__}38
  module: (root Chain).M38
  module: (root Chain).M38.B
  module: (root Chain).M38B
  module: (root Chain).{M39__}39
  module: (root Chain).M39
  module: (root Chain).M39.B
  module: (root Chain).M39B
  module: (root Chain).{M40__}40
  module: (root Chain).M40
  module: (root Chain).M40.B
  module: (root Chain).M40B
  module: (root Chain).{M41__}41
  module: (root Chain).M41
  module: (root Chain).M41.B
  module: (root Chain).M41B
  module: (root Chain).{M42__}42
  module: (root Chain).M42
  module: (root Chain).M42.B
  module: (root Chain).M42B
  module: (root Chain).{M43__}43
  module: (root Chain).M43
  module: (root Chain).M43.B
  module: (root Chain).M43B
  module: (root Chain).{M44__}44
  module: (root Chain).M44
  module: (root Chain).M44.B
  module: (root Chain).M44B
  module: (root Chain).{M45__}45
  module: (root Chain).M45
  module: (root Chain).M45.B
  module: (root Chain).M45B
  module: (root Chain).{M46__}46
  module: (root Chain).M46
  module: (root Chain).M46.B
  module: (root Chain).M46B
  module: (root Chain).{M47__}47
  module: (root Chain).M47
  module: (root Chain).M47.B
  module: (root Chain).M47B
  module: (root Chain).{M48__}48
  module: (root Chain).M48
  module: (root Chain).M48.B
  module: (root Chain).M48B
  module: (root Chain).{M49__}49
  module: (root Chain).M49
  module: (root Chain).M49.B
  module: (root Chain).M49B
  module: (root Chain).{M50__}50
  module: (root Chain).M50
  module: (root Chain).M50.B
  module: (root Chain).M50B
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
                  "`Identifier": {
                    "`Module": [ { "`Root": [ "None", "Chain" ] }, "M5" ]
                  }
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
                  "Module": [
                    "Ordinary",
                    {
                      "id": {
                        "`Module": [
                          {
                            "`Module": [
                              { "`Root": [ "None", "Chain" ] },
                              "M5"
                            ]
                          },
                          "B"
                        ]
                      },
                      "doc": [],
                      "type_": {
                        "Alias": [
                          {
                            "`Resolved": {
                              "`Alias": [
                                {
                                  "`Alias": [
                                    {
                                      "`Alias": [
                                        {
                                          "`Alias": [
                                            {
                                              "`Alias": [
                                                {
                                                  "`Alias": [
                                                    {
                                                      "`Alias": [
                                                        {
                                                          "`Alias": [
                                                            {
                                                              "`Module": [
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
                                                                "B"
                                                              ]
                                                            },
                                                            {
                                                              "`Dot": [
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
                                                                },
                                                                "B"
                                                              ]
                                                            }
                                                          ]
                                                        },
                                                        {
                                                          "`Dot": [
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
                                                            },
                                                            "B"
                                                          ]
                                                        }
                                                      ]
                                                    },
                                                    {
                                                      "`Dot": [
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
                                                        },
                                                        "B"
                                                      ]
                                                    }
                                                  ]
                                                },
                                                {
                                                  "`Dot": [
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
                                                    },
                                                    "B"
                                                  ]
                                                }
                                              ]
                                            },
                                            {
                                              "`Dot": [
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
                                                },
                                                "B"
                                              ]
                                            }
                                          ]
                                        },
                                        {
                                          "`Dot": [
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
                                            },
                                            "B"
                                          ]
                                        }
                                      ]
                                    },
                                    {
                                      "`Dot": [
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
                                        },
                                        "B"
                                      ]
                                    }
                                  ]
                                },
                                {
                                  "`Dot": [
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
                                    },
                                    "B"
                                  ]
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
    "canonical": "None",
    "hidden": "false"
  }
  $ find . -name chain.odocl -size +100000c 
  ./chain.odocl
