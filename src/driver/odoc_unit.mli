(** What the driver builds: units, grouped into libraries and packages.

    A {e unit} is one artifact odoc compiles: a module interface, a module
    implementation, a page or an asset. A {e library} groups the units of its
    modules. A {e package} groups its libraries and its pages, and is the thing
    the driver builds in one go. See {!Odoc_units_of} for how these are made
    from what is installed, and {!Compile} and {!Build} for what is done with
    them. *)

(** {1 Units} *)

type +'a t = {
  parent_id : Odoc.Id.t;
      (** Decides the unit's identifier, and so how references to it resolve and
          where its HTML goes. *)
  input_file : Fpath.t;
      (** The [.cmti], [.cmt], [.mld], [.md] or asset file. *)
  input_copy : Fpath.t option;
      (** For the interface of a virtual library: a place to copy the [.cmti]
          to, next to the [.odoc] file, so that a later run documenting an
          implementation can find it. *)
  odoc_file : Fpath.t;  (** Where [odoc compile] writes. *)
  odocl_file : Fpath.t;  (** Where [odoc link] writes. *)
  enable_warnings : bool;  (** Report odoc's warnings for this unit. *)
  to_output : bool;  (** Link and render this unit. *)
  kind : 'a;
}
(** One unit. What all the units of a library or of a package share lives in
    {!lib} and {!pkg} instead. *)

type intf_extra = {
  hidden : bool;  (** Not shown to readers; compiled but not linked. *)
  hash : string;  (** The digest of the interface. *)
  deps : (string * Digest.t) list;
      (** The modules it depends on, which must be compiled first. *)
}

and intf = [ `Intf of intf_extra ]

type impl_extra = {
  src_id : Odoc.Id.t;  (** The identifier of the rendered source page. *)
  src_path : Fpath.t;  (** The source file. *)
}

type impl = [ `Impl of impl_extra ]
type mld = [ `Mld ]
type md = [ `Md ]
type asset = [ `Asset ]
type any = [ impl | intf | mld | asset | md ] t

type module_unit = [ intf | impl ] t
(** An interface or an implementation: the units of a library. *)

type page = [ mld | md | asset ] t
(** A page or an asset: the units of a package's documentation. *)

(** {1 Libraries and packages} *)

type scope = {
  page_roots : (string * Fpath.t) list;
      (** The packages whose pages may be referred to, with the directory of
          their [.odoc] files: the [-P] arguments. *)
  lib_roots : (string * Fpath.t) list;
      (** The libraries whose modules may be referred to, likewise: the [-L]
          arguments. *)
}
(** What a unit may refer to at link time. Every unit a package's authors wrote
    is linked with the same scope, the package's. *)

type lib = {
  lib_name : string;
  requires : string list;
      (** The libraries of this build that must be compiled before this one: the
          libraries it requires, restricted to those being built. A required
          library that provides no modules stands for the libraries it requires
          in turn. *)
  includes : (string * Fpath.t) list;
      (** The libraries this one was built against: itself and, transitively,
          what it requires, each with the directory holding its [.odoc] files.
          This is what the compiler could see. The units are compiled with it as
          [-L], which both finds the files and records the names in them, and
          linked with the directories as [-I]. *)
  units : module_unit list;
  page : mld t option;
      (** The page the driver writes to list the library's modules. [None] when
          the driver writes no pages for this package. It is linked with
          {!includes}, like the library's modules: what it shows of them is
          taken from this library, and odoc prefers a module found there to a
          same-named module of another library. *)
}
(** A library and its modules. *)

type index = {
  index_file : Fpath.t;  (** The [.odoc-index] file. *)
  sidebar_file : Fpath.t;  (** The [.odoc-sidebar] file derived from it. *)
  html_dir : Fpath.t;
      (** The package's directory in the HTML output, where the search database
          and the JSON sidebar go. *)
}
(** A package's index, built from the [.odocl] files of its units. *)

type pkg = {
  pkgname : string option;
      (** [None] for the top-level index, which belongs to no package. *)
  scope : scope;
  index : index option;  (** [None] for the top-level index. *)
  libs : lib list;
  pages : page list;  (** Including the landing pages the driver writes. *)
}
(** A package, the thing the driver builds in one go. *)

val include_dirs : lib -> Fpath.t list
(** The directories of {!lib.includes}. *)

val all_units : pkg -> any list

(** {1 Where things go}

    Identifiers follow one layout and files on disk another.

    {e Identifiers} put a package's pages under the package's name and a
    library's modules under [<pkg>/<lib>]. They decide the URLs.

    {e Files} mirror the switch. A library's [.odoc] files are written at the
    path of its object directory, [lib/<findlib dir>], so libraries that share a
    directory in the switch share one here and the [-I] search path is the
    compiler's. A package's pages are written below [doc/<pkg>]. Every location
    is a function of a name, so a run finds what earlier runs built without any
    record of it. The same paths hold below the odocl directory. *)

val pkg_dir : Packages.t -> Fpath.t
(** The identifier of the package's top-level page, and the root of its HTML. *)

val doc_dir : Packages.t -> Fpath.t
(** The identifier below which the package's pages live. *)

val lib_dir : Packages.t -> Packages.libty -> Fpath.t
(** The identifier of a library's modules. *)

val src_dir : Packages.t -> Fpath.t
(** The identifier below which rendered sources live. *)

val src_lib_dir : Packages.t -> Packages.libty -> Fpath.t
(** The identifier below which a library's rendered sources live. *)

val lib_obj_dir : Packages.libty -> Fpath.t
(** Where a library's [.odoc] files go, relative to the odoc directory. *)

val pages_dir : Packages.t -> Fpath.t
(** Where a package's pages go, relative to the odoc directory. *)

val page_obj_dir : Packages.t -> Fpath.t -> Fpath.t
(** [page_obj_dir pkg id] is where a page with identifier [id] goes: the
    position of [id] below {!doc_dir}, reproduced below {!pages_dir}. *)

val output_root : _ t -> Fpath.t
(** The directory to give as [--output-dir] to the odoc commands that have no
    [-o] option, so that they write the unit's [odoc_file]. *)

(** {1 Directories} *)

type dirs = {
  odoc_dir : Fpath.t;  (** [.odoc] files. *)
  odocl_dir : Fpath.t;  (** [.odocl] files. *)
  index_dir : Fpath.t;  (** Index and sidebar files. *)
  mld_dir : Fpath.t;  (** The pages the driver writes itself. *)
}
(** Where intermediate files go. *)
