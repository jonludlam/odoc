Check that types referring to class types (with `#` in them)
resolve correctly. All of the 'Class' json objects should contain
'Resolved'

  $ ocamlc -c -bin-annot b.ml
  $ ocamlc -c -bin-annot c.ml
  $ ocamlc -c -bin-annot d.mli
  $ ocamlc -c -bin-annot e.mli
  $ ocamlc -c -bin-annot f.mli

  $ odoc compile b.cmt
  Starting type_of pass
  Adding (root B).t to env
  Finished type_of pass
  Adding (root B).t to env
  $ odoc compile c.cmt -I .
  Starting type_of pass
  Finished type_of pass
  $ odoc compile d.cmti -I .
  Starting type_of pass
  Adding (root D).t to env
  Finished type_of pass
  Adding (root D).t to env
  $ odoc compile e.cmti -I .
  Starting type_of pass
  Finished type_of pass
  $ odoc compile f.cmti -I .
  Starting type_of pass
  Finished type_of pass
  $ odoc_print -r f f.odoc 
  {
    "id": { "`Value": [ { "`Root": [ "None", "F" ] }, "f" ] },
    "doc": [],
    "type_": {
      "Class": [
        {
          "`Resolved": {
            "`Identifier": {
              "`ClassType": [ { "`Root": [ "None", "F" ] }, "u" ]
            }
          }
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
          "`Resolved": {
            "`ClassType": [
              { "`Identifier": { "`Root": [ "None", "B" ] } },
              "u"
            ]
          }
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
        "`Resolved": {
          "`ClassType": [
            {
              "`Identifier": {
                "`Root": [
                  "None",
                  "B"
                ]
              }
            },
            "u"
          ]
        }
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
              "`Resolved": {
                "`ClassType": [
                  { "`Identifier": { "`Root": [ "None", "B" ] } },
                  "u"
                ]
              }
            },
            []
          ]
        },
        {
          "Constr": [
            { "`Resolved": { "`Identifier": { "`CoreType": "unit" } } },
            []
          ]
        }
      ]
    },
    "value": "Abstract"
  }

