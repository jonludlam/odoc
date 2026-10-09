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

  let support_file_uri t path =
    Html_page.file_uri ~config:t.config ~url:t.url
      (Config.support_uri t.config)
      path
end

type head = Context.t -> Html_types.head_content_fun Tyxml.Html.elt list

let heads : head list ref = ref []

let register_head f = heads := !heads @ [ f ]

let head ~config ~url page =
  let ctx = { Context.config; url; page } in
  List.concat_map (fun f -> f ctx) !heads

let support_files_ : (string * string) list ref = ref []

let register_support_file ~path content =
  support_files_ :=
    List.filter (fun (p, _) -> p <> path) !support_files_ @ [ (path, content) ]

let support_files () = !support_files_
