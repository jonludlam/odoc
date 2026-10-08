(* Renders [{@shout[...]}] code blocks as a paragraph in capitals, ending with
   the [end=...] tag if there is one, and leaves the ones already in capitals
   to the default rendering. The pages with a shout get a style sheet. *)

open Odoc_document

let shout ({ meta; content; _ } : Odoc_model.Comment.code_block) =
  let content = Odoc_model.Location_.value content in
  if String.uppercase_ascii content = content then None
  else
    let tags = match meta with Some m -> m.tags | None -> [] in
    let ending =
      List.find_map (function `Binding ("end", e) -> Some e | _ -> None) tags
      |> Option.value ~default:""
    in
    let text =
      {
        Types.Inline.attr = [];
        desc = Text (String.uppercase_ascii content ^ ending);
      }
    in
    Some [ { Types.Block.attr = [ "shout" ]; desc = Paragraph [ text ] } ]

let head ~config:_ ~url:_ page =
  let is_shout (b : Types.Block.one) = List.mem "shout" b.attr in
  if Doctree.Exists.page ~block:is_shout ~inline:(fun _ -> false) page then
    [ Tyxml.Html.style [ Tyxml.Html.txt ".shout { font-weight: bold }" ] ]
  else []

let () =
  Extension.register_code_block ~language:"shout" shout;
  Odoc_html.Extension.register_head head;
  Odoc_cli.main ()
