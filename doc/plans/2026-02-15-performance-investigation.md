# Performance Investigation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Instrument odoc and odoc_driver to measure time and memory usage, run against Jane Street's `core` library, and identify optimisation targets.

**Architecture:** Two levels of measurement. Level 1: wrap each driver subprocess with `/usr/bin/time -v` to capture peak RSS per odoc invocation. Level 2: add `Gc.Memprof`-based allocation profiling inside the odoc binary itself, activated via `--memprof` flag. Both levels feed into a single analysis report.

**Tech Stack:** OCaml 5.4, `Gc.Memprof` (stdlib), `/usr/bin/time -v` (GNU time), `Yojson` (already a driver dependency), `core` v0.17 (opam).

---

### Task 1: Install core in the current switch

**Files:**
- None (opam operation only)

**Step 1: Install core**

Run: `opam install core -y`
Expected: Installs `core` v0.17 and ~34 transitive dependencies. Exit code 0.

**Step 2: Verify installation**

Run: `opam list core`
Expected: Shows `core` with version `0.17.x`.

**Step 3: Commit**

No code changes — nothing to commit.

---

### Task 2: Add `peak_rss_kb` to `Run.t`

**Files:**
- Modify: `src/driver/run.ml:12-18` (Run.t type)

**Step 1: Add `peak_rss_kb` field to `Run.t`**

In `src/driver/run.ml`, add `peak_rss_kb : int option` to the record type:

```ocaml
type t = {
  cmd : string list;
  time : float;  (** Running time in seconds. *)
  output_file : Fpath.t option;
  output : string;
  errors : string;
  status : [ `Exited of int | `Signaled of int ];
  peak_rss_kb : int option;  (** Peak RSS in KB from /usr/bin/time -v *)
}
```

**Step 2: Build to see all type errors**

Run: `cd /home/jons-agent/workspace/odoc && dune build src/driver/ 2>&1 | head -40`
Expected: Type errors where `Run.t` is constructed without the new field.

**Step 3: Fix the construction site in `run` function**

In `src/driver/run.ml:77`, update the record construction to include `peak_rss_kb = None`:

```ocaml
  let result = { cmd; time; output_file; output; errors; status; peak_rss_kb = None } in
```

This is a placeholder — we'll wire in the actual parsing in Task 3.

**Step 4: Build again**

Run: `cd /home/jons-agent/workspace/odoc && dune build src/driver/`
Expected: Clean build (the field is optional, so existing pattern matches still work).

**Step 5: Commit**

```bash
cd /home/jons-agent/workspace/odoc
git add src/driver/run.ml
git commit -m "perf: add peak_rss_kb field to Run.t

Preparation for capturing peak RSS per subprocess via /usr/bin/time -v.
Currently always None — wired in next commit."
```

---

### Task 3: Wrap subprocess execution with `/usr/bin/time -v`

**Files:**
- Modify: `src/driver/run.ml:30-97` (the `run` function)

The key insight: `/usr/bin/time -v` writes its output to **stderr**. We need to
separate GNU time's stderr from odoc's stderr. Strategy: prepend
`/usr/bin/time -v` to the command. GNU time writes a fixed-format block to
stderr. We parse `Maximum resident set size (kbytes): NNN` from the combined
stderr and strip the GNU time lines.

**Step 1: Add a helper to parse GNU time output from stderr**

Add this above the `run` function in `src/driver/run.ml`:

```ocaml
(** Parse peak RSS from GNU time -v stderr output.
    Returns (peak_rss_kb option, cleaned_stderr). *)
let parse_gnu_time_stderr stderr =
  let lines = String.split_on_char '\n' stderr in
  let rss = ref None in
  let other_lines = Buffer.create (String.length stderr) in
  let in_time_block = ref false in
  List.iter (fun line ->
    let trimmed = String.trim line in
    if String.length trimmed > 0 &&
       (trimmed.[0] = '\t' || !in_time_block) then begin
      in_time_block := true;
      (* Try to parse "Maximum resident set size (kbytes): NNN" *)
      match String.split_on_char ':' trimmed with
      | [key; value] when String.trim key = "Maximum resident set size (kbytes)" ->
        (try rss := Some (int_of_string (String.trim value))
         with Failure _ -> ())
      | _ -> ()
    end else begin
      if Buffer.length other_lines > 0 || String.length line > 0 then begin
        if Buffer.length other_lines > 0 then Buffer.add_char other_lines '\n';
        Buffer.add_string other_lines line
      end
    end
  ) lines;
  (!rss, Buffer.contents other_lines)
```

**Step 2: Modify the `run` function to prepend `/usr/bin/time -v`**

In the `run` function, after `let cmd = Bos.Cmd.to_list cmd in` (line 31),
wrap the command:

```ocaml
  let time_cmd = "/usr/bin/time" :: "-v" :: cmd in
```

Then change the subprocess spawn (line 53) to use `time_cmd` instead of `cmd`:

```ocaml
      let child =
        Eio.Process.spawn ~sw proc_mgr ~stdout:w ~stderr:we ~env time_cmd
      in
```

After capturing `errors` (the combined stderr), parse it:

```ocaml
  let peak_rss_kb, errors = parse_gnu_time_stderr errors in
```

And update the result construction:

```ocaml
  let result = { cmd; time; output_file; output; errors; status; peak_rss_kb } in
```

Note: we keep `cmd` (not `time_cmd`) in the record so logs show the original
odoc command, not the `/usr/bin/time` wrapper.

**Step 3: Build and run a quick smoke test**

Run: `cd /home/jons-agent/workspace/odoc && dune build src/driver/`
Expected: Clean build.

Run a quick driver invocation to verify it still works (on a tiny package):

Run: `cd /home/jons-agent/workspace/odoc && dune exec -- odoc_driver --stats cmdliner 2>&1 | tail -5`
Expected: Completes without error. `driver-benchmarks.json` is produced.

**Step 4: Verify RSS is captured**

Check a few commands in the ref list have `peak_rss_kb` populated. We can do
this by adding a temporary debug print or by inspecting in the next task when
we add it to the JSON output.

**Step 5: Commit**

```bash
cd /home/jons-agent/workspace/odoc
git add src/driver/run.ml
git commit -m "perf: wrap odoc subprocesses with /usr/bin/time -v

Prepend /usr/bin/time -v to each subprocess invocation to capture peak RSS.
Parse 'Maximum resident set size' from GNU time's stderr output and store
in Run.t.peak_rss_kb. GNU time's output is stripped from the error output
so it doesn't pollute odoc's stderr."
```

---

### Task 4: Add memory metrics to `Stats.bench_results`

**Files:**
- Modify: `src/driver/stats.ml:214-328`

**Step 1: Add `compute_metric_rss` function**

Add a function to compute min/max/avg peak RSS per phase, analogous to
`compute_metric_cmd`:

```ocaml
let compute_metric_rss cmd =
  let open Run in
  let cmds = filter_commands cmd in
  let rss_values = List.filter_map (fun c -> Option.map float_of_int c.peak_rss_kb) cmds in
  match compute_min_max_avg rss_values with
  | None -> []
  | Some (min, max, avg, count) ->
    let min = int_of_float min in
    let max = int_of_float max in
    let avg = int_of_float avg in
    [
      `Assoc
        [
          ("name", `String ("rss-" ^ cmd));
          ("value",
           `Assoc
             [ ("min", `Int min); ("max", `Int max); ("avg", `Int avg);
               ("count", `Int count) ]);
          ("units", `String "kb");
          ("description",
           `String ("Peak RSS of 'odoc " ^ cmd ^ "' subprocesses"));
          ("trend", `String "lower-is-better");
        ];
    ]
```

**Step 2: Add `compute_heaviest_cmd` function**

Top-N-by-RSS, analogous to `compute_longest_cmd`:

```ocaml
let k_heaviest_commands cmd k =
  let open Run in
  filter_commands cmd
  |> List.filter (fun c -> c.peak_rss_kb <> None)
  |> List.sort (fun a b ->
       compare (Option.value ~default:0 b.peak_rss_kb)
               (Option.value ~default:0 a.peak_rss_kb))
  |> List.filteri (fun i _ -> i < k)

let compute_heaviest_cmd cmd =
  let k = 5 in
  let cmds = k_heaviest_commands cmd k in
  let rss = List.filter_map (fun c -> Option.map float_of_int c.Run.peak_rss_kb) cmds in
  match compute_min_max_avg rss with
  | None -> []
  | Some (min, max, avg, _count) ->
    let min = int_of_float min in
    let max = int_of_float max in
    let avg = int_of_float avg in
    [
      `Assoc
        [
          ("name", `String ("heaviest-" ^ cmd));
          ("value",
           `Assoc [ ("min", `Int min); ("max", `Int max); ("avg", `Int avg) ]);
          ("units", `String "kb");
          ("description",
           `String
             (Printf.sprintf
                "Peak RSS of the %d heaviest calls to 'odoc %s'" k cmd));
          ("trend", `String "lower-is-better");
        ];
    ]
```

**Step 3: Add memory metrics to `all_metrics`**

In `src/driver/stats.ml`, extend `all_metrics` (line 295):

```ocaml
let all_metrics html_dir =
  compute_metric_cmd "compile"
  @ compute_metric_cmd "compile-deps"
  @ compute_metric_cmd "link"
  @ compute_metric_cmd "html-generate"
  @ compute_longest_cmd "compile"
  @ compute_longest_cmd "link"
  @ compute_metric_rss "compile"
  @ compute_metric_rss "link"
  @ compute_metric_rss "html-generate"
  @ compute_heaviest_cmd "compile"
  @ compute_heaviest_cmd "link"
  @ compute_produced_cmd "compile"
  @ compute_produced_cmd "link"
  @ compute_produced_tree "html-generate" html_dir
```

**Step 4: Build**

Run: `cd /home/jons-agent/workspace/odoc && dune build src/driver/`
Expected: Clean build.

**Step 5: Commit**

```bash
cd /home/jons-agent/workspace/odoc
git add src/driver/stats.ml
git commit -m "perf: add peak RSS metrics to driver-benchmarks.json

Add per-phase RSS stats (min/max/avg) and top-5-heaviest-by-RSS for
compile and link phases alongside existing time metrics."
```

---

### Task 5: Add `Profiling` module to odoc

**Files:**
- Create: `src/odoc/profiling.ml`
- Modify: `src/odoc/dune`

**Step 1: Create `src/odoc/profiling.ml`**

```ocaml
(** Allocation profiling using Gc.Memprof (OCaml 5.4+). *)

type site = {
  mutable n_samples : int;
  mutable total_words : int;
  mutable n_minor : int;
  mutable n_major : int;
}

let sites : (string, site) Hashtbl.t = Hashtbl.create 256

let format_callstack cs =
  let slot = Printexc.get_raw_backtrace_slot cs 0 in
  match Printexc.Slot.location (Printexc.convert_raw_backtrace_slot slot) with
  | Some loc ->
    Printf.sprintf "%s:%d:%d" loc.filename loc.line_number loc.start_char
  | None -> "<unknown>"
  | exception _ -> "<unknown>"

let callstack_key cs =
  let len = Printexc.raw_backtrace_length cs in
  let buf = Buffer.create 128 in
  let n = min len 5 in
  for i = 0 to n - 1 do
    let slot = Printexc.get_raw_backtrace_slot cs i in
    (match Printexc.Slot.location (Printexc.convert_raw_backtrace_slot slot) with
     | Some loc ->
       Buffer.add_string buf loc.filename;
       Buffer.add_char buf ':';
       Buffer.add_string buf (string_of_int loc.line_number);
       Buffer.add_char buf ';'
     | None -> Buffer.add_string buf "?;"
     | exception _ -> Buffer.add_string buf "?;")
  done;
  Buffer.contents buf

let record_alloc ~is_major cs n_samples size =
  let key = callstack_key cs in
  let site =
    match Hashtbl.find_opt sites key with
    | Some s -> s
    | None ->
      let s = { n_samples = 0; total_words = 0; n_minor = 0; n_major = 0 } in
      Hashtbl.add sites key s;
      s
  in
  site.n_samples <- site.n_samples + n_samples;
  site.total_words <- site.total_words + (n_samples * size);
  if is_major then
    site.n_major <- site.n_major + n_samples
  else
    site.n_minor <- site.n_minor + n_samples

let start ?(rate = 0.01) ?(callstack_size = 20) () =
  Hashtbl.clear sites;
  let alloc_minor info =
    record_alloc ~is_major:false info.Gc.Memprof.callstack
      info.n_samples info.size;
    None
  in
  let alloc_major info =
    record_alloc ~is_major:true info.Gc.Memprof.callstack
      info.n_samples info.size;
    None
  in
  let tracker = {
    Gc.Memprof.null_tracker with
    alloc_minor;
    alloc_major;
  } in
  let _profile = Gc.Memprof.start
    ~sampling_rate:rate
    ~callstack_size
    tracker
  in
  ()

let report () =
  Gc.Memprof.stop ();
  let gc = Gc.stat () in
  let all_sites =
    Hashtbl.fold (fun key site acc -> (key, site) :: acc) sites []
    |> List.sort (fun (_, a) (_, b) -> compare b.total_words a.total_words)
  in
  Printf.eprintf "\n=== Memprof Report ===\n\n";
  Printf.eprintf "Gc.stat summary:\n";
  Printf.eprintf "  heap_words:    %.0f\n" gc.heap_words;
  Printf.eprintf "  live_words:    %.0f\n" gc.live_words;
  Printf.eprintf "  minor_collections: %d\n" gc.minor_collections;
  Printf.eprintf "  major_collections: %d\n" gc.major_collections;
  Printf.eprintf "  compactions:       %d\n" gc.compactions;
  Printf.eprintf "\nTop allocation sites (by sampled words):\n\n";
  let n = min 30 (List.length all_sites) in
  List.iteri (fun i (key, site) ->
    if i < n then begin
      (* Extract the first location from the key for display *)
      let loc = match String.split_on_char ';' key with
        | hd :: _ -> hd
        | [] -> key
      in
      Printf.eprintf "  %2d. %s\n" (i + 1) loc;
      Printf.eprintf "      samples=%d  words=%d  minor=%d  major=%d\n\n"
        site.n_samples site.total_words site.n_minor site.n_major
    end
  ) all_sites;
  let total_sampled = Hashtbl.fold (fun _ s acc -> acc + s.n_samples) sites 0 in
  let total_words = Hashtbl.fold (fun _ s acc -> acc + s.total_words) sites 0 in
  Printf.eprintf "Total: %d samples, %d words across %d unique sites\n"
    total_sampled total_words (Hashtbl.length sites)
```

**Step 2: Build the library**

Run: `cd /home/jons-agent/workspace/odoc && dune build src/odoc/`
Expected: Clean build. The module is auto-discovered by dune since `src/odoc/dune` uses `(library ...)` without an explicit `(modules ...)` list.

**Step 3: Commit**

```bash
cd /home/jons-agent/workspace/odoc
git add src/odoc/profiling.ml
git commit -m "perf: add Profiling module using Gc.Memprof

Provides start() and report() functions for statistical memory allocation
profiling. Records allocation sites with callstack keys, tracks minor vs
major heap samples, and prints a top-30 report on stderr at exit."
```

---

### Task 6: Add `--memprof` CLI flag to odoc

**Files:**
- Modify: `src/odoc/bin/main.ml:1774-1839`

**Step 1: Add `--memprof` argument definition**

Add before the `let () =` main entry point (before line 1774):

```ocaml
let memprof_rate =
  let doc =
    "Enable memory allocation profiling (statistical). Optional value is the \
     sampling rate (default 0.01 = 1%). Output goes to stderr."
  in
  Arg.(
    value
    & opt (some float) None
    & info ~doc ~docv:"RATE" [ "memprof" ])
```

**Step 2: Wire it into the main entry point**

Modify the main entry point to parse `--memprof` before dispatching subcommands.
The challenge: cmdliner's `Cmd.group` doesn't easily allow global options that
apply before subcommand dispatch. The simplest approach is to check `Sys.argv`
directly for `--memprof` before cmdliner runs:

```ocaml
let () =
  Printexc.record_backtrace true;
  (* Check for --memprof before cmdliner dispatches *)
  let memprof_active =
    let args = Array.to_list Sys.argv in
    let rec find = function
      | [] -> None
      | "--memprof" :: rate :: _ when rate <> "" && rate.[0] <> '-' ->
        (try Some (float_of_string rate) with Failure _ -> Some 0.01)
      | "--memprof" :: _ -> Some 0.01
      | arg :: rest ->
        (* Handle --memprof=RATE *)
        (match String.split_on_char '=' arg with
         | ["--memprof"; rate] ->
           (try Some (float_of_string rate) with Failure _ -> Some 0.01)
         | _ -> find rest)
    in
    find args
  in
  Option.iter (fun rate ->
    Profiling.start ~rate ();
    at_exit Profiling.report
  ) memprof_active;
  (* ... rest of existing main ... *)
```

Note: we parse `--memprof` manually from argv so that it works as a global
flag across all subcommands without modifying every subcommand's term. Cmdliner
will ignore the unknown flag since it's consumed before dispatch. Actually,
cmdliner *will* complain about unknown flags. So instead we should strip
`--memprof` and its value from `Sys.argv` before cmdliner sees it, OR we add it
as a global option to the group.

**Alternative approach (cleaner):** Add `--memprof` as a common option to the
group's default term, and use `Cmd.eval_value` to get access to it. But
cmdliner's `Cmd.group` only evaluates the default term when no subcommand is
given.

**Simplest working approach:** Filter `Sys.argv` in-place before cmdliner runs.
Since OCaml arrays are mutable, we can do:

```ocaml
let () =
  Printexc.record_backtrace true;
  (* Parse and remove --memprof from argv before cmdliner sees it *)
  let memprof_active = ref None in
  let new_argv = Array.to_list Sys.argv |> List.filter (fun arg ->
    match String.split_on_char '=' arg with
    | ["--memprof"] ->
      memprof_active := Some 0.01; false
    | ["--memprof"; rate] ->
      (try memprof_active := Some (float_of_string rate)
       with Failure _ -> memprof_active := Some 0.01);
      false
    | _ -> true
  ) |> Array.of_list in
  (* Replace Sys.argv — this is a known technique for pre-processing *)
  Obj.set_field (Obj.repr Sys.argv) 0 (Obj.repr new_argv.(0));
  (* Actually we can't easily resize Sys.argv. Better approach: *)
```

Actually, the cleanest approach is to **not modify Sys.argv** but instead use
an **environment variable**. But let me reconsider — the simplest correct
approach is:

**Final approach:** Parse `--memprof[=RATE]` from argv, remove it, and pass the
filtered argv to cmdliner via `Cmd.eval_value ~argv`.

In `src/odoc/bin/main.ml`, replace lines 1774-1839 with:

```ocaml
let () =
  Printexc.record_backtrace true;
  (* Parse --memprof[=RATE] before cmdliner dispatch.
     We strip it from argv so cmdliner doesn't complain about unknown flags. *)
  let argv = Array.to_list Sys.argv in
  let memprof_rate, filtered_argv =
    let rec loop acc = function
      | [] -> (None, List.rev acc)
      | "--memprof" :: rest ->
        (match rest with
         | rate_s :: rest' ->
           (match float_of_string_opt rate_s with
            | Some rate -> (Some rate, List.rev_append acc rest')
            | None -> (Some 0.01, List.rev_append acc (rate_s :: rest')))
         | [] -> (Some 0.01, List.rev acc))
      | arg :: rest ->
        (match String.split_on_char '=' arg with
         | ["--memprof"; rate_s] ->
           let rate = match float_of_string_opt rate_s with
             | Some r -> r | None -> 0.01
           in
           (Some rate, List.rev_append acc rest)
         | _ -> loop (arg :: acc) rest)
    in
    loop [] argv
  in
  let argv = Array.of_list filtered_argv in
  Option.iter (fun rate ->
    Profiling.start ~rate ();
    at_exit Profiling.report
  ) memprof_rate;
  let cmd_make (term, info) = Cmd.v info term in
  (* ... rest unchanged ... *)
```

The key change is:
1. Parse `--memprof` / `--memprof=0.05` / `--memprof 0.05` from argv
2. Strip it from the argv list
3. Start profiling if present
4. Register `at_exit Profiling.report`
5. Pass filtered `argv` to `Cmd.eval_value ~argv`

At the end, change `Cmd.eval_value ~err:Format.err_formatter main` to:
```ocaml
  match Cmd.eval_value ~argv ~err:Format.err_formatter main with
```

**Step 2: Build**

Run: `cd /home/jons-agent/workspace/odoc && dune build`
Expected: Clean build.

**Step 3: Test the flag**

Run: `cd /home/jons-agent/workspace/odoc && echo '(** Test *)' > /tmp/test.mld && dune exec -- odoc compile --parent test /tmp/test.mld -o /tmp/test.odoc --memprof=1.0 2>&1 | head -20`
Expected: Should see `=== Memprof Report ===` on stderr after compilation.

**Step 4: Verify it works without the flag**

Run: `cd /home/jons-agent/workspace/odoc && dune exec -- odoc compile --parent test /tmp/test.mld -o /tmp/test.odoc 2>&1`
Expected: Normal output, no profiling report.

**Step 5: Commit**

```bash
cd /home/jons-agent/workspace/odoc
git add src/odoc/bin/main.ml
git commit -m "perf: add --memprof[=RATE] flag to odoc

Parses --memprof before cmdliner dispatch so it works with all subcommands.
Activates Gc.Memprof allocation profiling and prints a report to stderr
at exit. Default rate is 0.01 (1% sampling)."
```

---

### Task 7: Run coarse-grained baseline on `core`

**Files:**
- None (data collection only)

**Step 1: Run odoc_driver on core with --stats**

Run:
```bash
cd /home/jons-agent/workspace/odoc
dune exec -- odoc_driver --stats -j 4 core 2>&1 | tee /tmp/odoc-core-run.log
```

Expected: Produces documentation for `core` and all its dependencies. Creates
`driver-benchmarks.json` with time and RSS metrics. This may take several
minutes.

**Step 2: Examine the benchmarks**

Run:
```bash
cd /home/jons-agent/workspace/odoc
python3 -m json.tool driver-benchmarks.json | head -80
```

Expected: JSON with `time-compile`, `time-link`, `time-html-generate`,
`rss-compile`, `rss-link`, `rss-html-generate`, `longest-compile`,
`longest-link`, `heaviest-compile`, `heaviest-link` metrics.

**Step 3: Save the results**

Run:
```bash
cp driver-benchmarks.json doc/plans/core-baseline-benchmarks.json
```

**Step 4: Identify top expensive files**

Look at the `longest-compile`, `longest-link`, `heaviest-compile`,
`heaviest-link` entries to identify the top 5-10 most expensive files by time
and memory. Note these file paths for Task 8.

---

### Task 8: Run fine-grained profiling on expensive files

**Files:**
- None (data collection only)

For each of the top 5 most expensive files identified in Task 7, run `odoc
compile --memprof` and `odoc link --memprof` directly.

**Step 1: Profile the most expensive compile**

Run (example — substitute actual file paths from Task 7):
```bash
cd /home/jons-agent/workspace/odoc
dune exec -- odoc compile --memprof=0.01 \
  -I <dep-dirs> \
  <path-to-expensive.cmti> \
  -o /tmp/profiled.odoc 2>&1 | tee /tmp/memprof-compile-1.log
```

Expected: Normal compilation output followed by `=== Memprof Report ===` with
top allocation sites and Gc.stat summary.

**Step 2: Profile the most expensive link**

Run (example):
```bash
cd /home/jons-agent/workspace/odoc
dune exec -- odoc link --memprof=0.01 \
  -I <dep-dirs> \
  <path-to-expensive.odoc> \
  -o /tmp/profiled.odocl 2>&1 | tee /tmp/memprof-link-1.log
```

**Step 3: Repeat for top 5 files**

Profile compile and link for the 5 most expensive files. Save each output.

**Step 4: Collate results**

Review all memprof reports and identify:
- Which allocation sites appear repeatedly across files
- Whether allocations are dominated by minor or major heap
- The top 5 source locations by total sampled words

---

### Task 9: Write up findings

**Files:**
- Create: `doc/plans/2026-02-15-performance-findings.md`

**Step 1: Write the analysis report**

Structure:
1. **Summary** — total time for `core`, phase breakdown (compile vs link vs generate)
2. **Time analysis** — top files by time per phase, distribution
3. **Memory analysis** — top files by peak RSS per phase
4. **Allocation hotspots** — top allocation sites from memprof, with source locations
5. **Optimisation targets** — ranked list of where effort would have the most impact
6. **Next steps** — specific optimisation ideas based on findings

**Step 2: Commit findings**

```bash
cd /home/jons-agent/workspace/odoc
git add doc/plans/2026-02-15-performance-findings.md
git add doc/plans/core-baseline-benchmarks.json
git commit -m "perf: write up performance investigation findings

Baseline measurements of odoc on Jane Street's core library.
Includes phase-level timing and RSS, plus allocation hotspot analysis."
```

---

## Summary of Changes

| Task | Files | Description |
|------|-------|-------------|
| 1 | (opam) | Install `core` v0.17 |
| 2 | `src/driver/run.ml` | Add `peak_rss_kb` field to `Run.t` |
| 3 | `src/driver/run.ml` | Wrap subprocesses with `/usr/bin/time -v`, parse RSS |
| 4 | `src/driver/stats.ml` | Add RSS metrics to `driver-benchmarks.json` |
| 5 | `src/odoc/profiling.ml` | New `Profiling` module using `Gc.Memprof` |
| 6 | `src/odoc/bin/main.ml` | Add `--memprof[=RATE]` global flag |
| 7 | (data) | Run driver on `core`, collect baseline |
| 8 | (data) | Run memprof on expensive files |
| 9 | `doc/plans/` | Write up findings |

## Risks

1. **`/usr/bin/time -v` overhead** — GNU time adds negligible overhead (<1ms per
   invocation). The main risk is that stderr parsing is fragile if odoc itself
   prints lines starting with `\t`. Mitigation: GNU time output always starts
   with `\tCommand being timed:` — we can anchor on that.

2. **`Gc.Memprof` in OCaml 5.4** — Confirmed working with callstacks. The API
   is `Gc.Memprof.stop ()` (not `stop profile`). Minor risk: profiling overhead
   at 1% rate is ~5-10% wall-clock slowdown. Acceptable for investigation.

3. **`core` compilation time** — Full driver run on `core` may take 10-30
   minutes depending on machine. Use `-j 4` to limit parallelism and reduce
   peak memory.
