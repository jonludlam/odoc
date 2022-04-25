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
        "133",
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
              "167",
              "test.odoc"
            ]
          },
          "X"
        ]
      },
      "key": [
        "179",
        "test.odoc"
      ]
    }
  }

The following should be resolved as Test.Wrapper2.X


  $ odoc_print -r test3 test.odocl | jq '.equation.manifest.Some.Constr[0].v["`Resolved"].v["`Type"][0].v["`Canonical"][1].v'
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
                  "Wrapper2"
                ]
              }
            },
            "key": [
              "195",
              "test.odoc"
            ]
          },
          "X"
        ]
      },
      "key": [
        "238",
        "test.odoc"
      ]
    }
  }

This should probably not resolve at all, but that's a problem for another day. currently it resolves as Test.Wrapper3.X

  $ odoc_print -r test3a test.odocl | jq '.type_.Constr[0].v["`Resolved"].v["`Type"][0].v["`Canonical"][1].v'
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
                  "Wrapper3"
                ]
              }
            },
            "key": [
              "251",
              "test.odoc"
            ]
          },
          "X"
        ]
      },
      "key": [
        "294",
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
        "309",
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
              "190",
              "test.cmti"
            ]
          },
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
                    "C"
                  ]
                },
                "false"
              ]
            },
            "key": [
              "328",
              "test.odoc"
            ]
          }
        ]
      },
      "key": [
        "329",
        "test.odoc"
      ]
    }
  }
