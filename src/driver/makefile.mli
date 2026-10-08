(** Writing the build as a makefile instead of running it.

    The driver works out what to run and in what order. Running the commands is
    a separate step, so the same decisions can be written out as rules for
    [make] to run. The file it writes builds the same output as a run of the
    driver, and [make -j] gives it the same parallelism.

    Discovery stays outside the makefile. [odoc classify], [odoc compile-deps],
    opam and findlib all run while the file is written, because they decide what
    the units are. The file is therefore valid for the switch as it was at that
    moment. A package installed afterwards needs a new one. *)

val emit :
  Format.formatter ->
  html_dir:Fpath.t ->
  stamp_dir:Fpath.t ->
  remaps:(string * string) list ->
  generate_json:bool ->
  warnings_tags:string list ->
  Odoc_unit.pkg list ->
  unit
(** Write the rules for building these packages.

    A rule's target is the file the command writes. Where a command writes a
    tree rather than a file, as [odoc html-generate] does, the target is a stamp
    file below [stamp_dir]. Stamps also stand for the sets that the phases wait
    for: one per library, which depends on the [.odoc] files of its modules, and
    one per package, which depends on its libraries and its pages. Linking a
    unit waits for the stamp of its own package and of every package its scope
    names, which is what the driver waits for.

    [remaps], when there are any, are written to [remap.txt] below [stamp_dir],
    since the rules read them after the driver has exited.

    The rules cover every odoc command the driver would run. They do not cover
    the [status.json] file it writes for each package, which is its own output
    and not odoc's. *)
