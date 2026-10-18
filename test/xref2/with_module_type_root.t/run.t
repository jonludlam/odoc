A `with` constraint on a module type has a fragment root that is a module
type, here [S]. The resolved fragments start from that root.

  $ ocamlc -bin-annot -c ok.mli
  $ odoc compile ok.cmti
  $ odoc link ok.odoc
  $ odoc_print ok.odocl | jq -c '.content.Module.items[] | .ModuleType? | select(. != null) | .expr.Some.With? | select(. != null) | .w_substitutions[] | to_entries[0] | [.key, .value[0]]'
  ["TypeEq",{"`Resolved":{"`Type":[{"`Root":{"`ModuleType":{"`Identifier":{"`ModuleType":[{"`Root":["None","Ok"]},"S"]}}}},"t"]}}]
  ["TypeEq",{"`Resolved":{"`Type":[{"`Module":[{"`Root":{"`ModuleType":{"`Identifier":{"`ModuleType":[{"`Root":["None","Ok"]},"S"]}}}},"A"]},"t"]}}]
  ["ModuleEq",{"`Resolved":{"`Module":[{"`Root":{"`ModuleType":{"`Identifier":{"`ModuleType":[{"`Root":["None","Ok"]},"S"]}}}},"A"]}}]
  ["TypeSubst",{"`Resolved":{"`Type":[{"`Root":{"`ModuleType":{"`Identifier":{"`ModuleType":[{"`Root":["None","Ok"]},"S"]}}}},"t"]}}]
  ["ModuleTypeEq",{"`Resolved":{"`Module_type":[{"`Root":{"`ModuleType":{"`Identifier":{"`ModuleType":[{"`Root":["None","Ok"]},"S"]}}}},"M"]}}]
  ["TypeEq",{"`Resolved":{"`Type":[{"`Module":[{"`Root":{"`ModuleType":{"`Identifier":{"`ModuleType":[{"`Root":["None","Ok"]},"S"]}}}},"N"]},"u"]}}]

When the fragment goes through an alias to a sibling module, odoc compile fails
while it makes the expansion of [T].

  $ ocamlc -bin-annot -c alias.mli
  $ odoc compile alias.cmti 2>&1 | grep -A1 "uncaught exception"
  odoc: internal error, uncaught exception:
        Failure("Not_found: A/4")
