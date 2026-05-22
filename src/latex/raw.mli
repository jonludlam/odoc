(** Raw latex primitives:
    - macro and environment definitions
    - text escaping *)

type pr = Format.formatter -> unit
(** {1 Helper types} *)

type 'a with_options = ?options:pr list -> 'a

type ('a, 'b) tr = (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit)

type 'a t = ('a, 'a) tr

(** {1 Helper functions} *)
module Escape : sig
  val text : code_hyphenation:bool -> string -> string

  val ref : Format.formatter -> string -> unit
end

val break : Format.formatter -> Types.break_hierarchy -> unit

val rightarrow : pr

val label : Format.formatter -> string -> unit

val verbatim : Format.formatter -> string -> unit

val pageref_star : Format.formatter -> string -> unit

val hyperref : string -> 'a t

val href : string -> 'a t

val ref : Format.formatter -> string -> unit

val url : Format.formatter -> string -> unit

val footnote : Format.formatter -> string -> unit

val emph : 'a t

val bold : 'a t

val subscript : 'a t

val superscript : 'a t

val section : 'a t

val subsection : 'a t

val subsubsection : 'a t

val paragraph : 'a t

val enumerate : 'a t

val itemize : 'a t

val description : ('a, ('a * 'a) list) tr

val item : 'a t with_options

val small_table : ('a, Types.alignment list option * 'a list list) tr

val input : Format.formatter -> Fpath.t -> unit

val includegraphics : 'a t

(** {1 Required OCaml-specific primitives}
    All the macro should be implemented as "ocaml"-suffixed macro in the latex
    preamble *)

val inline_code : 'a t
(** {2 Code block customization} *)

val code_fragment : 'a t

val code_block : 'a t

(** {2 Package-dependent primitives}*)

val indent : 'a t
(** expected to be implemented with changepage/adjustwidth*)

val ocamltabular : column_desc:pr -> 'a t
(** Any tabular implementation that works well with at most 10 rows *)

(** {2 Tags} *)

val ocamltag : string -> 'a t
(** tag (e.g keyword, type-var, ...) are rendered to
    {v \ocamltag{tagname}{content} v} *)

(** {2 Math mode} *)

val math : Format.formatter -> string -> unit

val equation : Format.formatter -> string -> unit
