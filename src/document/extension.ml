type code_block_handler = Odoc_model.Comment.code_block -> Types.Block.t option

let code_block_handlers : (string, code_block_handler) Hashtbl.t =
  Hashtbl.create 8

let register_code_block ~language handler =
  Hashtbl.replace code_block_handlers language handler

let find_code_block language = Hashtbl.find_opt code_block_handlers language
