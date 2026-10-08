(* Renders [{@shout[...]}] code blocks as a paragraph in capitals, ending with
   the [end=...] tag if there is one, and with the backend if the [backend] tag
   is there. Leaves the ones already in capitals to the default rendering.
   Warns about the ones with no letters. The pages with a shout get a style
   sheet. *)

open Odoc_document
module Context = Extension.Context

let shout ctx ({ meta; content; _ } : Odoc_model.Comment.code_block) =
  let content = Odoc_model.Location_.value content in
  if String.uppercase_ascii content = String.lowercase_ascii content then (
    Context.warn ctx "Nothing to shout.";
    None)
  else if String.uppercase_ascii content = content then None
  else
    let () = Context.require ctx "shout" in
    let tags = match meta with Some m -> m.tags | None -> [] in
    let ending =
      List.find_map (function `Binding ("end", e) -> Some e | _ -> None) tags
      |> Option.value ~default:""
    in
    let ending =
      if List.mem (`Tag "backend") tags then
        ending ^ " in " ^ Context.backend ctx
      else ending
    in
    let text =
      {
        Types.Inline.attr = [];
        desc = Text (String.uppercase_ascii content ^ ending);
      }
    in
    Some [ { Types.Block.attr = [ "shout" ]; desc = Paragraph [ text ] } ]

let head ctx =
  if Odoc_html.Extension.Context.requires ctx "shout" then
    [ Tyxml.Html.style [ Tyxml.Html.txt ".shout { font-weight: bold }" ] ]
  else []

let () =
  Extension.register_code_block ~language:"shout" shout;
  Odoc_html.Extension.register_head head;
  Odoc_cli.main ()
