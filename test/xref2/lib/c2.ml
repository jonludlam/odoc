open Common

let my_compilation_unit id s =
    { Odoc_model.Lang.Compilation_unit.
      id = id
    ; root = root
    ; digest = "nodigest"
    ; imports = []
    ; source = None
    ; interface = true
    ; hidden = false
    ; content = Module s
    ; expansion = None
    ; linked = false
    ; canonical = None
}

