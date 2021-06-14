open Odoc_document

type args = { extra_suffix : string }

let render args page =
  Odoc_manpage.Generator.render ~extra_suffix:args.extra_suffix page

let files_of_url url = Odoc_manpage.Link.files_of_url url

let renderer = { Renderer.name = "man"; render; files_of_url }
