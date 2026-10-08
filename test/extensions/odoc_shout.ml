(* Renders [{@shout[...]}] code blocks as a paragraph in capitals, ending with
   the [end=...] tag if there is one, and leaves the ones already in capitals
   to the default rendering. *)

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

let () =
  Extension.register_code_block ~language:"shout" shout;
  Odoc_cli.main ()
