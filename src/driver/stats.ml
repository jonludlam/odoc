(* Progress, as the build steps report it, for the progress display. *)

type phase = Compile | Link | Index | Generate

let phases = [ Compile; Link; Index; Generate ]

let label = function
  | Compile -> "Compiling"
  | Link -> "Linking"
  | Index -> "Indexing"
  | Generate -> "Generating"

let index = function Compile -> 0 | Link -> 1 | Index -> 2 | Generate -> 3
let totals = Array.init (List.length phases) (fun _ -> Atomic.make 0)
let dones = Array.init (List.length phases) (fun _ -> Atomic.make 0)
let expect phase n = ignore (Atomic.fetch_and_add totals.(index phase) n)
let did phase = Atomic.incr dones.(index phase)

(* What each worker is running. *)
let running = Atomic.make 0
let activity = ref [||]
let init_nprocs n = activity := Array.init n (fun _ -> Atomic.make "idle")

let worker_busy id description =
  Atomic.incr running;
  Atomic.set !activity.(id) description

let worker_idle id =
  Atomic.decr running;
  Atomic.set !activity.(id) "idle"

let finished = Atomic.make false
let finish () = Atomic.set finished true

let render_stats env nprocs =
  match Logs.level () with
  | Some (App | Warning) | None ->
      let open Progress in
      let clock = Eio.Stdenv.clock env in
      let bar label total =
        Line.(
          list [ lpad 16 (const label); bar total; rpad 10 (count_to total) ])
      in
      let phase_bars =
        List.map (fun p -> bar (label p) (Atomic.get totals.(index p))) phases
      in
      let config = Config.v ~persistent:false () in
      with_reporters ~config
        Multi.(
          lines phase_bars
          ++ line (bar "Processes" nprocs)
          ++ lines (List.init nprocs (fun _ -> Line.string)))
        (fun phase_reporters processes activities ->
          (* A bar is told how far it has moved, so remember where each one
             is. *)
          let shown = Array.map (fun _ -> 0) dones and shown_running = ref 0 in
          let rec loop () =
            Eio.Time.sleep clock 0.1;
            List.iteri
              (fun i report ->
                let n = Atomic.get dones.(i) in
                report (n - shown.(i));
                shown.(i) <- n)
              phase_reporters;
            let n = Atomic.get running in
            processes (n - !shown_running);
            shown_running := n;
            List.iteri
              (fun i report -> report (Atomic.get !activity.(i)))
              activities;
            if not (Atomic.get finished) then loop ()
          in
          loop ())
  | _ -> ()

let k_longest_commands cmd k =
  let open Run in
  filter_commands cmd
  |> List.sort (fun a b -> Float.compare b.time a.time)
  |> List.filteri (fun i _ -> i < k)

let rec compute_min_max_avg min_ max_ total count = function
  | [] -> (min_, max_, total /. float count, count)
  | hd :: tl ->
      compute_min_max_avg (min min_ hd) (max max_ hd) (total +. hd) (count + 1)
        tl

let compute_min_max_avg = function
  | [] -> None
  | hd :: tl -> Some (compute_min_max_avg hd hd hd 1 tl)

let compute_metric_int prefix suffix description values =
  match compute_min_max_avg values with
  | None -> []
  | Some (min, max, avg, count) ->
      let min = int_of_float min in
      let max = int_of_float max in
      let avg = int_of_float avg in
      [
        `Assoc
          [
            ("name", `String (prefix ^ "-total-" ^ suffix));
            ("value", `Int count);
            ("description", `String ("Number of " ^ description));
          ];
        `Assoc
          [
            ("name", `String (prefix ^ "-size-" ^ suffix));
            ( "value",
              `Assoc [ ("min", `Int min); ("max", `Int max); ("avg", `Int avg) ]
            );
            ("units", `String "b");
            ("description", `String ("Size of " ^ description));
            ("trend", `String "lower-is-better");
          ];
      ]

let compute_metric_cmd cmd =
  let open Run in
  let cmds = filter_commands cmd in
  let times = List.map (fun c -> c.Run.time) cmds in
  match compute_min_max_avg times with
  | None -> []
  | Some (min, max, avg, count) ->
      [
        `Assoc
          [
            ("name", `String ("total-" ^ cmd));
            ("value", `Int count);
            ( "description",
              `String ("Number of time 'odoc " ^ cmd ^ "' has run.") );
          ];
        `Assoc
          [
            ("name", `String ("time-" ^ cmd));
            ( "value",
              `Assoc
                [
                  ("min", `Float min); ("max", `Float max); ("avg", `Float avg);
                ] );
            ("units", `String "s");
            ("description", `String ("Time taken by 'odoc " ^ cmd ^ "'"));
            ("trend", `String "lower-is-better");
          ];
      ]

(** Analyze the size of files produced by a command. *)
let compute_produced_cmd cmd =
  let output_file_size c =
    match c.Run.output_file with
    | Some f -> (
        match Bos.OS.Path.stat f with
        | Ok st -> Some (float st.Unix.st_size)
        | Error _ -> None)
    | None -> None
  in
  let sizes = List.filter_map output_file_size (Run.filter_commands cmd) in
  compute_metric_int "produced" cmd
    ("files produced by 'odoc " ^ cmd ^ "'")
    sizes

(** Analyze the size of files outputed to the given directory. *)
let compute_produced_tree cmd dir =
  let acc_file_sizes path acc =
    match Bos.OS.Path.stat path with
    | Ok st -> float st.Unix.st_size :: acc
    | Error _ -> acc
  in
  Bos.OS.Dir.fold_contents ~dotfiles:true ~elements:`Files acc_file_sizes [] dir
  |> Result.value ~default:[]
  |> compute_metric_int "produced" cmd ("files produced by 'odoc " ^ cmd ^ "'")

(** Analyze the running time of the slowest commands. *)
let compute_longest_cmd cmd =
  let k = 5 in
  let cmds = k_longest_commands cmd k in
  let times = List.map (fun c -> c.Run.time) cmds in
  match compute_min_max_avg times with
  | None -> []
  | Some (min, max, avg, _count) ->
      [
        `Assoc
          [
            ("name", `String ("longest-" ^ cmd));
            ( "value",
              `Assoc
                [
                  ("min", `Float min); ("max", `Float max); ("avg", `Float avg);
                ] );
            ("units", `String "s");
            ( "description",
              `String
                (Printf.sprintf
                   "Time taken by the %d longest calls to 'odoc %s'" k cmd) );
            ("trend", `String "lower-is-better");
          ];
      ]

let all_metrics html_dir =
  compute_metric_cmd "compile"
  @ compute_metric_cmd "compile-deps"
  @ compute_metric_cmd "link"
  @ compute_metric_cmd "html-generate"
  @ compute_longest_cmd "compile"
  @ compute_longest_cmd "link"
  @ compute_produced_cmd "compile"
  @ compute_produced_cmd "link"
  @ compute_produced_tree "html-generate" html_dir

let bench_results html_dir =
  let result =
    `Assoc
      [
        ("name", `String "odoc");
        ( "results",
          `List
            [
              `Assoc
                [
                  ("name", `String "driver.mld");
                  ("metrics", `List (all_metrics html_dir));
                ];
            ] );
      ]
  in
  Yojson.to_file "driver-benchmarks.json" result
