Check that types referring to class types (with `#` in them)
resolve correctly. All of the 'Class' json objects should contain
'Resolved'

  $ ocamlc -c -bin-annot b.ml
  $ ocamlc -c -bin-annot c.ml
  $ ocamlc -c -bin-annot d.mli
  $ ocamlc -c -bin-annot e.mli
  $ ocamlc -c -bin-annot f.mli

  $ odoc compile b.cmt
  $ odoc compile c.cmt -I .
  $ odoc compile d.cmti -I .
  $ odoc compile e.cmti -I .
  $ odoc compile f.cmti -I .
  $ odoc_print -r f f.odoc 
  {
    "id": { "`Value": [ { "`Root": [ "None", "F" ] }, "f" ] },
    "doc": [],
    "type_": {
      "Class": [
        {
          "v": {
            "`Resolved": {
              "v": {
                "`Identifier": {
                  "`ClassType": [ { "`Root": [ "None", "F" ] }, "u" ]
                }
              },
              "key": [ "38", "f.cmti" ]
            }
          },
          "key": [ "40", "f.cmti" ]
        },
        []
      ]
    },
    "value": "Abstract"
  }
  $ odoc_print e.odoc -r g
  {
    "id": { "`Value": [ { "`Root": [ "None", "E" ] }, "g" ] },
    "doc": [],
    "type_": {
      "Class": [
        {
          "v": {
            "`Resolved": {
              "v": {
                "`ClassType": [
                  {
                    "v": { "`Identifier": { "`Root": [ "None", "B" ] } },
                    "key": [ "43", "e.cmti" ]
                  },
                  "u"
                ]
              },
              "key": [ "54", "e.cmti" ]
            }
          },
          "key": [ "55", "e.cmti" ]
        },
        []
      ]
    },
    "value": "Abstract"
  }
  $ odoc_print e.odoc -r d | jq '.expr.Signature.items[1].Method.type_'
  {
    "Class": [
      {
        "v": {
          "`Resolved": {
            "v": {
              "`ClassType": [
                {
                  "v": {
                    "`Identifier": {
                      "`Root": [
                        "None",
                        "B"
                      ]
                    }
                  },
                  "key": [
                    "43",
                    "e.cmti"
                  ]
                },
                "u"
              ]
            },
            "key": [
              "54",
              "e.cmti"
            ]
          }
        },
        "key": [
          "55",
          "e.cmti"
        ]
      },
      []
    ]
  }

  $ odoc_print c.odoc -r g
  {
    "id": { "`Value": [ { "`Root": [ "None", "C" ] }, "g" ] },
    "doc": [],
    "type_": {
      "Arrow": [
        "None",
        {
          "Class": [
            {
              "v": {
                "`Resolved": {
                  "v": {
                    "`ClassType": [
                      {
                        "v": { "`Identifier": { "`Root": [ "None", "B" ] } },
                        "key": [ "45", "c.cmt" ]
                      },
                      "u"
                    ]
                  },
                  "key": [ "57", "c.cmt" ]
                }
              },
              "key": [ "58", "c.cmt" ]
            },
            []
          ]
        },
        {
          "Constr": [
            {
              "v": {
                "`Resolved": {
                  "v": { "`Identifier": { "`CoreType": "unit" } },
                  "key": [ "13", "predefined" ]
                }
              },
              "key": [ "14", "predefined" ]
            },
            []
          ]
        }
      ]
    },
    "value": "Abstract"
  }

