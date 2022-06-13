A long chain of aliases should produce an odocl file that's a reasonable
size.

  $ ocamlc -c -bin-annot chain.mli
  $ odoc compile chain.cmti
  $ odoc link chain.odoc -I .
  $ find . -name chain.odocl -size +100000c
  ./chain.odocl
  $ odoc_print chain.odocl -r 'M3'
  {
    "id": { "`Module": [ { "`Root": [ "None", "Chain" ] }, "M3" ] },
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
                                            "`Identifier": {
                                              "`Module": [
                                                {
                                                  "`Root": [ "None", "Chain" ]
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
                                                      "None", "Chain"
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
                                        "`Dot": [ { "`Root": "Chain" }, "M1" ]
                                      }
                                    ]
                                  },
                                  {
                                    "`Identifier": [
                                      {
                                        "`Module": [
                                          { "`Root": [ "None", "Chain" ] },
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
                                      { "`Root": [ "None", "Chain" ] },
                                      "{M2__}2"
                                    ]
                                  },
                                  "true"
                                ]
                              }
                            ]
                          },
                          { "`Dot": [ { "`Root": "Chain" }, "M2" ] }
                        ]
                      },
                      {
                        "`Identifier": [
                          {
                            "`Module": [
                              { "`Root": [ "None", "Chain" ] },
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
                          { "`Root": [ "None", "Chain" ] },
                          "{M3__}3"
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
                                                      "`Identifier": {
                                                        "`Module": [
                                                          {
                                                            "`Root": [
                                                              "None", "Chain"
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
                                                                "None", "Chain"
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
                                                    { "`Root": "Chain" },
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
                                                        "None", "Chain"
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
                                                  "`Root": [ "None", "Chain" ]
                                                },
                                                "{M2__}2"
                                              ]
                                            },
                                            "true"
                                          ]
                                        }
                                      ]
                                    },
                                    { "`Dot": [ { "`Root": "Chain" }, "M2" ] }
                                  ]
                                },
                                {
                                  "`Identifier": [
                                    {
                                      "`Module": [
                                        { "`Root": [ "None", "Chain" ] },
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
                                    { "`Root": [ "None", "Chain" ] },
                                    "{M3__}3"
                                  ]
                                },
                                "true"
                              ]
                            }
                          ]
                        },
                        { "`Dot": [ { "`Root": "Chain" }, "M3" ] }
                      ]
                    },
                    {
                      "`Identifier": [
                        {
                          "`Module": [ { "`Root": [ "None", "Chain" ] }, "M3" ]
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
  $ ls -l
  total 800
  -rw-r--r--  1 jon  staff   16512 Jun 13 13:48 chain.cmi
  -rw-r--r--  1 jon  staff   85031 Jun 13 13:48 chain.cmti
  lrwxr-xr-x  1 jon  staff      59 Jun 13 13:48 chain.mli -> ../../../../../../default/test/xref2/aliaschain.t/chain.mli
  -rw-r--r--  1 jon  staff   27706 Jun 13 13:48 chain.odoc
  -rw-r--r--  1 jon  staff  270342 Jun 13 13:48 chain.odocl
