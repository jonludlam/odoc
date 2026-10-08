type head =
  config:Config.t ->
  url:Odoc_document.Url.Path.t ->
  Odoc_document.Types.Page.t ->
  Html_types.head_content_fun Tyxml.Html.elt list

let heads : head list ref = ref []

let register_head f = heads := !heads @ [ f ]

let head ~config ~url page =
  List.concat_map (fun f -> f ~config ~url page) !heads
