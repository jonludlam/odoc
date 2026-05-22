open Odoc_utils
open ResultMonad

let generate ~output ~warnings_options:_ ~index =
  Odoc_file.load_index index >>= fun index ->
  let sidebar = Odoc_document.Sidebar.of_index index in
  Ok (Odoc_file.save_sidebar output sidebar)
