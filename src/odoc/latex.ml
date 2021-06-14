open Odoc_document

type args = { with_children : bool; extra_suffix : string }

let render args page =
  Odoc_latex.Generator.render ~with_children:args.with_children
    ~extra_suffix:args.extra_suffix page

let files_of_url url = Odoc_latex.Generator.files_of_url url

let renderer = { Renderer.name = "latex"; render; files_of_url }
