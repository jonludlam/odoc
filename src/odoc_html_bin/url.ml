let reference_to_url_html { Html_page.html_config = config; _ } root_url =
  let url_to_string url =
    let base =
      match root_url with
      | None | Some "" -> ""
      | Some base ->
          if base.[String.length base - 1] = '/' then base else base ^ "/"
    in
    Odoc_html.Link.(href ~config ~resolve:(Base base) url)
  in
  Odoc_odoc.Url.resolve url_to_string
