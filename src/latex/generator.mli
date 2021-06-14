val files_of_url : Odoc_document.Url.Path.t -> Fpath.t list

val render :
  with_children:bool ->
  extra_suffix:string ->
  Odoc_document.Types.Page.t ->
  Odoc_document.Renderer.page
