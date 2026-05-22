open Odoc_utils
module List = ListLabels
open Cmdliner

include Html_page

let semantic_uris =
  let doc = "Generate pretty (semantic) links." in
  Arg.(value & flag (info ~doc [ "semantic-uris"; "pretty-uris" ]))

let closed_details =
  let doc =
    "If this flag is passed <details> tags (used for includes) will be \
     closed by default."
  in
  Arg.(value & flag (info ~doc [ "closed-details" ]))

let indent =
  let doc = "Format the output HTML files with indentation." in
  Arg.(value & flag (info ~doc [ "indent" ]))

module Uri = struct
  (* Very basic validation and normalization for URI paths. *)

  open Odoc_html.Types

  let is_absolute str =
    List.exists [ "http"; "https"; "file"; "data"; "ftp" ] ~f:(fun scheme ->
        Astring.String.is_prefix ~affix:(scheme ^ ":") str)
    || str.[0] = '/'

  let conv_rel_dir rel =
    let l = String.cuts ~sep:"/" rel in
    List.fold_left
      ~f:(fun acc seg ->
        Some Odoc_document.Url.Path.{ kind = `Page; parent = acc; name = seg })
      l ~init:None

  let convert_dir : uri Arg.conv =
    let parser str =
      if String.length str = 0 then Error "invalid URI"
      else
        let last_char = str.[String.length str - 1] in
        let str =
          if last_char <> '/' then str
          else String.with_range ~len:(String.length str - 1) str
        in
        Ok
          (if is_absolute str then (Absolute str : uri)
           else
             Relative
               (let u = conv_rel_dir str in
                match u with
                | None -> None
                | Some u -> Some { u with kind = `Page }))
    in
    let printer ppf = function
      | (Absolute uri : uri) -> Format.pp_print_string ppf uri
      | Relative _uri -> Format.pp_print_string ppf ""
    in
    Arg.conv' (parser, printer)

  let convert_file_uri : Odoc_html.Types.file_uri Arg.conv =
    let parser str =
      if String.length str = 0 then Error "invalid URI"
      else
        let conv_rel_file rel =
          match String.cut ~rev:true ~sep:"/" rel with
          | Some (before, after) ->
              let base = conv_rel_dir before in
              Odoc_document.Url.Path.
                { kind = `File; parent = base; name = after }
          | None ->
              Odoc_document.Url.Path.
                { kind = `File; parent = None; name = rel }
        in
        Ok
          (if is_absolute str then (Absolute str : file_uri)
           else Relative (conv_rel_file str))
    in
    let printer ppf = function
      | Odoc_html.Types.Absolute uri -> Format.pp_print_string ppf uri
      | Odoc_html.Types.Relative _uri -> Format.pp_print_string ppf ""
    in
    Arg.conv' (parser, printer)
end

let home_breadcrumb =
  let doc =
    "Name for a 'Home' breadcrumb to go up the root of the given sidebar."
  in
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"escape" ~doc [ "home-breadcrumb" ])

let theme_uri =
  let doc =
    "Where to look for theme files (e.g. `URI/odoc.css'). Relative URIs are \
     resolved using `--output-dir' as a target."
  in
  let default : Odoc_html.Types.uri = Odoc_html.Types.Relative None in
  Arg.(
    value
    & opt Uri.convert_dir default
    & info ~docv:"URI" ~doc [ "theme-uri" ])

let support_uri =
  let doc =
    "Where to look for support files (e.g. `URI/highlite.pack.js'). Relative \
     URIs are resolved using `--output-dir' as a target."
  in
  let default : Odoc_html.Types.uri = Odoc_html.Types.Relative None in
  Arg.(
    value
    & opt Uri.convert_dir default
    & info ~docv:"URI" ~doc [ "support-uri" ])

let search_uri =
  let doc =
    "Where to look for search scripts. Relative URIs are resolved using \
     `--output-dir' as a target."
  in
  Arg.(
    value
    & opt_all Uri.convert_file_uri []
    & info ~docv:"URI" ~doc [ "search-uri" ])

let flat =
  let doc =
    "Output HTML files in 'flat' mode, where the hierarchy of modules / \
     module types / classes and class types are reflected in the filenames \
     rather than in the directory structure."
  in
  Arg.(value & flag & info ~docs:Odoc_odoc.Cli_helpers.docs ~doc [ "flat" ])

let as_json =
  let doc =
    "EXPERIMENTAL: Output HTML files in 'embeddable json' mode, where HTML \
     fragments (preamble, content) together with metadata (uses_katex, \
     breadcrumbs, table of contents) are emitted in JSON format. The \
     structure of the output should be considered unstable and no guarantees \
     are made about backward compatibility."
  in
  Arg.(value & flag & info ~doc [ "as-json" ])

let remap =
  let convert_remap =
    let parse inp =
      match String.cut ~sep:":" inp with
      | Some (orig, mapped) -> Ok (orig, mapped)
      | _ -> Error (`Msg "Map must be of the form '<orig>:https://...'")
    and print fmt (orig, mapped) = Format.fprintf fmt "%s:%s" orig mapped in
    Arg.conv (parse, print)
  in
  let doc = "Remap an identifier to an external URL." in
  Arg.(value & opt_all convert_remap [] & info [ "R" ] ~doc)

let remap_file =
  let doc = "File containing remap rules." in
  Arg.(value & opt (some file) None & info ~docv:"FILE" ~doc [ "remap-file" ])

let extra_args =
  let config semantic_uris closed_details indent theme_uri support_uri
      search_uris flat as_json remap remap_file home_breadcrumb =
    let open_details = not closed_details in
    let remap =
      match remap_file with
      | None -> remap
      | Some f ->
          Io_utils.fold_lines f
            (fun line acc ->
              match String.cut ~sep:":" line with
              | Some (orig, mapped) -> (orig, mapped) :: acc
              | None -> acc)
            []
    in
    let html_config =
      Odoc_html.Config.v ~theme_uri ~support_uri ~search_uris ~semantic_uris
        ~indent ~flat ~open_details ~as_json ~remap ?home_breadcrumb ()
    in
    { Html_page.html_config }
  in
  Term.(
    const config $ semantic_uris $ closed_details $ indent $ theme_uri
    $ support_uri $ search_uri $ flat $ as_json $ remap $ remap_file
    $ home_breadcrumb)
