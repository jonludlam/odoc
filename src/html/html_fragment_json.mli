module Html = Tyxml.Html

val make :
  config:Config.t ->
  preamble:Html_types.div_content Html.elt list ->
  url:Odoc_document.Url.Path.t ->
  breadcrumbs:Types.breadcrumbs ->
  toc:Types.toc list ->
  uses_katex:bool ->
  source_anchor:string option ->
  header:Html_types.flow5_without_header_footer Html.elt list ->
  ?extra_head:Html_types.head_content_fun Html.elt list ->
  Html_types.div_content Html.elt list ->
  Odoc_document.Renderer.page list ->
  Odoc_document.Renderer.page
(** [extra_head], the elements that extensions add to the [<head>] of the page,
    is in the field ["extra_head"], as HTML, when it is not empty. *)

val make_src :
  config:Config.t ->
  url:Odoc_document.Url.Path.t ->
  breadcrumbs:Types.breadcrumbs ->
  sidebar:Html_types.div_content Html.elt list option ->
  header:Html_types.flow5_without_header_footer Html.elt list ->
  Html_types.div_content Html.elt list ->
  Odoc_document.Renderer.page
