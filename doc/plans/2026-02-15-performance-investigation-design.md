# Performance Investigation Design

## Goal

Identify where odoc spends its time and memory when processing real-world
libraries. Establish baseline measurements and find optimisation targets.

## Test Target

Jane Street's `core` library (v0.17, installed via opam in the current OCaml
5.4 switch). Core is large, widely used, and representative of the kind of
heavy documentation workload odoc handles. The `odoc_driver` will be pointed at
`core` specifically.

## Two Levels of Measurement

### Level 1: Driver-level (coarse-grained)

The driver (`odoc_driver`) already records wall-clock time per subprocess in
`Run.commands`. We extend this to also capture **peak RSS** per subprocess by
wrapping each `odoc` invocation with `/usr/bin/time -v` and parsing its stderr
output.

**Changes to `src/driver/run.ml`:**
- Prepend `/usr/bin/time -v` to each command
- Capture GNU time's stderr separately from odoc's stderr
- Parse `Maximum resident set size` from the time output
- Add `peak_rss_kb : int option` to `Run.t`

**Changes to `src/driver/stats.ml`:**
- Add memory metrics (min/max/avg peak RSS) per phase to `bench_results`
- Add top-N-by-memory alongside existing top-N-by-time

**Output:** `driver-benchmarks.json` enriched with memory data. Immediately
shows which files and phases are expensive.

### Level 2: Statmemprof inside odoc (fine-grained)

Add allocation profiling to the odoc binary using OCaml 5.4's `Gc.Memprof`
(statmemprof). This tells us **where in the odoc code** allocations happen,
not just how much memory the process uses.

**New module `src/odoc/profiling.ml`:**
- `start ()` — activates `Gc.Memprof` with a configurable sampling rate and
  callstack depth. Accumulates `(callstack_key, n_samples, total_words)` in a
  hashtable.
- `report ()` — called at exit. Prints:
  - Top N allocation sites by sampled words (with source locations)
  - Minor vs major heap breakdown
  - `Gc.stat()` summary (heap size, collections, live words)

**New CLI flag on odoc:** `--memprof[=RATE]` (default rate 0.01, i.e. 1%).
When passed, profiling is activated before the main work and reported at exit.

This requires no external dependencies — `Gc.Memprof` is in the stdlib since
OCaml 5.3.

### Analysis

After collecting data from both levels:

1. **Top files by time and peak RSS** per phase (compile, link, html-generate)
2. **Top allocation sites** in odoc's source code
3. **Distribution** of time and memory across all files
4. **Phase breakdown** — where does the total wall-clock time go?

## Concrete Steps

1. Install `core` in the current switch
2. Add `/usr/bin/time -v` wrapping to `Run.run`, parse peak RSS from stderr
3. Extend `Run.t` and `Stats.bench_results` with memory metrics
4. Add `--memprof` flag and `Profiling` module to the odoc binary
5. Run `odoc_driver --stats core` for the coarse baseline
6. Identify the top 5-10 most expensive files from step 5
7. Run `odoc compile --memprof` / `odoc link --memprof` on those files
8. Write up findings — where time and memory go, what the bottlenecks are
