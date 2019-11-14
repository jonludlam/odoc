module Tree = Tree
module Comment = Comment
module Targets = Targets

module Generator =
struct
  (** @canonical Odoc_html.Generator.ML *)
  module ML = ML

  (** @canonical Odoc_html.Generator.Reason *)
  module Reason = Reason
end

(* Exposed as an unstable public API for third-party packages to "hack" on, see

    https://github.com/ocaml/odoc/pull/252
    https://github.com/ocaml/odoc/issues/236. *)
module Url = Url
