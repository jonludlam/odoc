module type S = sig
  type args

  val renderer : args Odoc_document.Renderer.t

  val extra_args : args Cmdliner.Term.t
end

(** Build a set of cmdliner subcommands for a renderer, using the renderer's
    name to derive command names (e.g. [latex-generate]). *)
module Make (R : S) : sig
  val process : docs:string -> unit Cmdliner.Term.t * Cmdliner.Cmd.info

  val targets : docs:string -> unit Cmdliner.Term.t * Cmdliner.Cmd.info

  val targets_source : docs:string -> unit Cmdliner.Term.t * Cmdliner.Cmd.info

  val generate : docs:string -> unit Cmdliner.Term.t * Cmdliner.Cmd.info

  val generate_source : docs:string -> unit Cmdliner.Term.t * Cmdliner.Cmd.info

  val generate_asset : docs:string -> unit Cmdliner.Term.t * Cmdliner.Cmd.info
end

(** Build a set of cmdliner subcommands for a renderer, using explicit
    command names. *)
module Make_with_names (R : S) (_ : sig
  val process_name : string

  val generate_name : string

  val generate_source_name : string

  val generate_asset_name : string

  val targets_name : string

  val targets_source_name : string
end) : sig
  val process : docs:string -> unit Cmdliner.Term.t * Cmdliner.Cmd.info

  val targets : docs:string -> unit Cmdliner.Term.t * Cmdliner.Cmd.info

  val targets_source : docs:string -> unit Cmdliner.Term.t * Cmdliner.Cmd.info

  val generate : docs:string -> unit Cmdliner.Term.t * Cmdliner.Cmd.info

  val generate_source : docs:string -> unit Cmdliner.Term.t * Cmdliner.Cmd.info

  val generate_asset : docs:string -> unit Cmdliner.Term.t * Cmdliner.Cmd.info
end
