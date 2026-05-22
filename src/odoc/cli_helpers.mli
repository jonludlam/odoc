open Cmdliner

val convert_syntax : Odoc_document.Renderer.syntax Arg.conv

val convert_directory : ?create:bool -> unit -> Fs.Directory.t Arg.conv

val convert_fpath : Fs.File.t Arg.conv

val convert_named_root : (string * Fs.Directory.t) Arg.conv

val handle_error :
  (unit, [< `Cli_error of string | `Msg of string ]) result -> unit

module Antichain : sig
  val absolute_normalization : Fpath.t -> Fpath.t

  val check :
    opt:string -> Fs.Directory.t list -> (unit, [> `Msg of string ]) result
end

val docs : string

val odoc_file_directories : Fs.Directory.t list Term.t

val hidden : bool Term.t

val extra_suffix : string option Term.t

val warnings_options : Odoc_model.Error.warnings_options Term.t

val dst : ?create:bool -> unit -> Fs.Directory.t Term.t

val open_modules : string list Term.t
