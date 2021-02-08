(**
   Compatibility module reexporting ~equivalent functions based on the current
   OCaml version
 *)
module String =
struct
  include String

#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
  let lowercase_ascii = lowercase
  let capitalize_ascii = capitalize
  let uncapitalize_ascii = uncapitalize
#else
  let lowercase_ascii = lowercase_ascii
  let capitalize_ascii = capitalize_ascii
  let uncapitalize_ascii = uncapitalize_ascii
#endif

end

(**/**)
  module Simon = 
  struct
    let myName = "simon"
  end
(**/**)

module Char =
struct
  include Char
  include Simon

#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
  let lowercase_ascii = lowercase
#else
  let lowercase_ascii = lowercase_ascii
#endif

end

(**/**)
module Zilla = struct
  let simon = "zilla"
end
