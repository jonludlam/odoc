(* Renders [{@shout[...]}] code blocks as a paragraph in capitals, ending with
   the [end=...] tag if there is one, and with the backend if the [backend] tag
   is there. Leaves the ones already in capitals to the default rendering.
   Warns about the ones with no letters. Renders [@shout] tags in a box with an
   id, and leaves [@shout.quietly] to the default rendering. Renders
   [{%shout:...%}] in capitals, except when it already is. The pages with a
   shout get a style sheet. *)

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

let shout_tag ctx ~name content =
  if name = "shout.quietly" then None
  else
    let () = Context.require ctx "shout" in
    let html s = { Types.Block.attr = []; desc = Raw_markup ("html", s) } in
    Some
      (html
         (Printf.sprintf {|<div class="shout" id="%s">|}
            (Context.fresh_id ctx "shout"))
       :: Comment.nestable_block_element_list content
      @ [ html "</div>" ])

let shout_raw ctx content =
  if String.uppercase_ascii content = content then None
  else (
    Context.require ctx "shout";
    Some
      {
        Types.Inline.attr = [];
        desc =
          Raw_markup
            ("html", "<strong>" ^ String.uppercase_ascii content ^ "</strong>");
      })

let head ctx =
  let module Context = Odoc_html.Extension.Context in
  if Context.requires ctx "shout" then
    let href = Context.support_file_uri ctx "shout/shout.css" in
    [ Tyxml.Html.link ~rel:[ `Stylesheet ] ~href () ]
  else []

let extension =
  {
    (Odoc_extension.v ~name:"shout") with
    code_blocks = [ ("shout", shout) ];
    raw_markup = [ ("shout", shout_raw) ];
    tags = [ ("shout", shout_tag) ];
    head = Some head;
    support_files = [ ("shout/shout.css", ".shout { font-weight: bold }\n") ];
  }

let () = Odoc_cli.main ~extensions:[ extension ] ()
