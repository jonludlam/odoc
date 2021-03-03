type 'a with_location = 'a Location.with_location

type style = [ `Bold | `Italic | `Emphasis | `Superscript | `Subscript ]

type reference =
  [ `Dot of reference * (string option * string) with_location
  | `Root of (string option * string) with_location ]

type inline_element =
  [ `Space of string
  | `Word of string
  | `Code_span of string
  | `Raw_markup of string option * string
  | `Styled of style * inline_element with_location list
  | `Reference of
    string with_location option * reference * inline_element with_location list
  | `Link of string * inline_element with_location list ]

type nestable_block_element =
  [ `Paragraph of inline_element with_location list
  | `Code_block of string
  | `Verbatim of string
  | `Modules of reference list
  | `List of
    [ `Unordered | `Ordered ]
    * [ `Light | `Heavy ]
    * nestable_block_element with_location list list ]

type tag =
  [ `Author of string
  | `Deprecated of nestable_block_element with_location list
  | `Param of string * nestable_block_element with_location list
  | `Raise of string * nestable_block_element with_location list
  | `Return of nestable_block_element with_location list
  | `See of
    [ `Url | `File | `Document ]
    * string
    * nestable_block_element with_location list
  | `Since of string
  | `Before of string * nestable_block_element with_location list
  | `Version of string
  | `Canonical of reference
  | `Inline
  | `Open
  | `Closed ]

type block_element =
  [ nestable_block_element
  | `Heading of int * string option * inline_element with_location list
  | `Tag of tag ]

type docs = block_element with_location list

type sections_allowed = [ `All | `No_titles | `None ]
