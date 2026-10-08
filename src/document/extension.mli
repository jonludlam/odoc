(** Extensions to the way comments are rendered.

    A program that links odoc as a library can register extensions here and then
    run odoc with [Odoc_cli.main]. The extensions apply to every page that
    program renders. *)

type code_block_handler = Odoc_model.Comment.code_block -> Types.Block.t option
(** A handler returns [None] to leave a code block to the default rendering. *)

val register_code_block : language:string -> code_block_handler -> unit
(** [register_code_block ~language handler] makes [handler] render the code
    blocks written in [language]. A handler for ["ocaml"] also renders the code
    blocks that have no language. A later registration for the same language
    replaces an earlier one. *)

val find_code_block : string -> code_block_handler option
