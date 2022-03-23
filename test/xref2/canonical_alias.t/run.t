  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  Starting type_of pass
  Adding (root Test).{A__}1 to env
  Adding (root Test).A to env
  Adding (root Test).test to env
  Adding (root Test).{Wrapped__X}2 to env
  Adding (root Test).Wrapper to env
  Adding (root Test).test2 to env
  Adding (root Test).{Wrapped2__X}3 to env
  Adding (root Test).{Wrapper2__}4 to env
  Adding (root Test).Wrapper2 to env
  Adding (root Test).test3 to env
  Adding (root Test).{Wrapped3__X}5 to env
  Adding (root Test).{Wrapper3__}6 to env
  Adding (root Test).Wrapper3 to env
  Adding (root Test).B_ to env
  Adding (root Test).B to env
  Adding (root Test).test4 to env
  Adding (root Test).C_ to env
  Adding (root Test).C to env
  Adding (root Test).D to env
  Adding (root Test).test5 to env
  Adding (root Test).{A__}1.t to env
  Adding (root Test).{Wrapped__X}2.t to env
  Adding (root Test).Wrapper.X to env
  Adding (root Test).{Wrapped2__X}3.t to env
  Adding (root Test).{Wrapper2__}4.X to env
  Adding (root Test).Wrapper2.X to env
  Adding (root Test).{Wrapped3__X}5.t to env
  Adding (root Test).{Wrapper3__}6.X to env
  Adding (root Test).Wrapper3.X to env
  Adding (root Test).Wrapper3.X.t to env
  Adding (root Test).B_.t to env
  Adding (root Test).C_.t to env
  Finished type_of pass
  Adding (root Test).{A__}1 to env
  Adding (root Test).A to env
  Adding (root Test).test to env
  Adding (root Test).{Wrapped__X}2 to env
  Adding (root Test).Wrapper to env
  Adding (root Test).test2 to env
  Adding (root Test).{Wrapped2__X}3 to env
  Adding (root Test).{Wrapper2__}4 to env
  Adding (root Test).Wrapper2 to env
  Adding (root Test).test3 to env
  Adding (root Test).{Wrapped3__X}5 to env
  Adding (root Test).{Wrapper3__}6 to env
  Adding (root Test).Wrapper3 to env
  Adding (root Test).B_ to env
  Adding (root Test).B to env
  Adding (root Test).test4 to env
  Adding (root Test).C_ to env
  Adding (root Test).C to env
  Adding (root Test).D to env
  Adding (root Test).test5 to env
  Adding (root Test).{A__}1.t to env
  Adding (root Test).Wrapper.X to env
  Adding (root Test).Wrapper2.X to env
  Adding (root Test).Wrapper3.X to env
  Adding (root Test).B_.t to env
  Adding (root Test).C_.t to env
  $ odoc link test.odoc
  Adding (root Test).{A__}1 to env
  Adding (root Test).A to env
  Adding (root Test).test to env
  Adding (root Test).{Wrapped__X}2 to env
  Adding (root Test).Wrapper to env
  Adding (root Test).test2 to env
  Adding (root Test).{Wrapped2__X}3 to env
  Adding (root Test).{Wrapper2__}4 to env
  Adding (root Test).Wrapper2 to env
  Adding (root Test).test3 to env
  Adding (root Test).{Wrapped3__X}5 to env
  Adding (root Test).{Wrapper3__}6 to env
  Adding (root Test).Wrapper3 to env
  Adding (root Test).B_ to env
  Adding (root Test).B to env
  Adding (root Test).test4 to env
  Adding (root Test).C_ to env
  Adding (root Test).C to env
  Adding (root Test).D to env
  Adding (root Test).test5 to env
  Adding (root Test).{A__}1.t to env
  Adding (root Test).A.t to env
  Adding (root Test).Wrapper.X to env
  Adding (root Test).Wrapper.X.t to env
  Adding (root Test).Wrapper2.X to env
  Adding (root Test).Wrapper2.X.t to env
  Adding (root Test).Wrapper3.X to env
  Adding (root Test).B_.t to env
  Adding (root Test).B.t to env
  Adding (root Test).C_.t to env
  $ odoc html-generate test.odocl -o html
  $ odoc support-files -o html

The following should be resolved as identifier Test.A
  $ odoc_print -r test test.odocl | jq '.equation.manifest.Some.Constr[0]["`Resolved"]["`Type"][0]["`Canonical"][1]'
  {
    "`Resolved": {
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
    }
  }

The following should be resolved as Test.Wrapper.X

  $ odoc_print -r test2 test.odocl | jq '.equation.manifest.Some.Constr[0]["`Resolved"]["`Type"][0]["`Canonical"][1]'
  {
    "`Resolved": {
      "`Module": [
        {
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
        "X"
      ]
    }
  }

The following should be resolved as Test.Wrapper2.X


  $ odoc_print -r test3 test.odocl | jq '.equation.manifest.Some.Constr[0]["`Resolved"]["`Type"][0]["`Canonical"][1]'
  {
    "`Resolved": {
      "`Module": [
        {
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
        "X"
      ]
    }
  }

This should probably not resolve at all, but that's a problem for another day. currently it resolves as Test.Wrapper3.X

  $ odoc_print -r test3a test.odocl | jq '.type_.Constr[0]["`Resolved"]["`Type"][0]["`Canonical"][1]'
  {
    "`Resolved": {
      "`Module": [
        {
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
        "X"
      ]
    }
  }

Should resolve as identifier Test.B
  $ odoc_print -r test4 test.odocl | jq '.equation.manifest.Some.Constr[0]["`Resolved"]["`Type"][0]["`Canonical"][1]'
  {
    "`Resolved": {
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
    }
  }

Should resove to be an alias!
  $ odoc_print -r test5 test.odocl | jq '.equation.manifest.Some.Constr[0]["`Resolved"]["`Type"][0]["`Canonical"][1]'
  {
    "`Resolved": {
      "`Alias": [
        {
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
        {
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
        }
      ]
    }
  }

