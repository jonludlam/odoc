module Context = struct
  type t = {
    config : Config.t;
    url : Odoc_document.Url.Path.t;
    page : Odoc_document.Types.Page.t;
  }

  let config t = t.config

  let url t = t.url

  let page t = t.page

  let requires t feature =
    List.mem feature (Odoc_document.Extension.required t.url)
end

type head = Context.t -> Html_types.head_content_fun Tyxml.Html.elt list

let heads : head list ref = ref []

let register_head f = heads := !heads @ [ f ]

let head ~config ~url page =
  let ctx = { Context.config; url; page } in
  List.concat_map (fun f -> f ctx) !heads
