(** Progress display and statistics.

    The counters are updated by the build steps ({!Compile}) and by the
    {!Worker_pool}, and read by the progress display. *)

type stats = {
  mutable total_units : int Atomic.t;
  mutable total_impls : int Atomic.t;
  mutable total_mlds : int Atomic.t;
  mutable total_assets : int Atomic.t;
  mutable total_indexes : int Atomic.t;
  mutable non_hidden_units : int Atomic.t;
  mutable compiled_units : int Atomic.t;
  mutable compiled_impls : int Atomic.t;
  mutable compiled_mlds : int Atomic.t;
  mutable compiled_assets : int Atomic.t;
  mutable linked_units : int Atomic.t;
  mutable linked_impls : int Atomic.t;
  mutable linked_mlds : int Atomic.t;
  mutable generated_indexes : int Atomic.t;
  mutable generated_units : int Atomic.t;
  mutable processes : int Atomic.t;  (** Commands running now. *)
  mutable process_activity : string Atomic.t Array.t;
      (** What each worker is doing now. *)
  mutable finished : bool;  (** Set by the driver when the build is complete. *)
}

val stats : stats
(** The counters of this run. *)

val init_nprocs : int -> unit
(** Size the per-worker activity table. Call before starting the workers. *)

val render_stats : Eio_unix.Stdenv.base -> generate_json:bool -> int -> unit
(** Show progress bars until {!stats}[.finished] is set. Meant to run in a fiber
    alongside the build. *)

val bench_results : Fpath.t -> unit
(** Write [driver-benchmarks.json] in the current directory. It holds the
    timings of this run's odoc commands, and the size of the output below the
    given HTML directory. *)
