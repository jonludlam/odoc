Repro of problem from uwt (https://github.com/ocaml/odoc/issues/691)

  $ cat uwt_base.mli
  (* This file is part of uwt, released under the MIT license. See LICENSE.md for
     details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)
  module Base : sig
    type 'a uv_result = 'a
  
  module Fs_types : sig
    type uv_open_flag =
      | O_RDONLY (** Open for reading *)
    (** Flags for {!Fs_functions.openfile}
  
        [O_CLOEXEC] doesn't exist, because this flag is unconditionally
        added by libuv. [O_SHARE_DELETE], [O_SHARE_WRITE], [O_SHARE_READ]
        are always added on Windows, unless [O_EXLOCK] is specified. *)
  
  end
  
  module type Fs_functions = sig
    include module type of Fs_types
    with type uv_open_flag = Fs_types.uv_open_flag
  
    type 'a t
  
    val openfile : ?perm:int -> mode:uv_open_flag list -> string -> int t
    (** Equivalent to open(2). perm defaults are 0o644 *)
  end
  end
  
  include module type of Base
    with type Fs_types.uv_open_flag = Base.Fs_types.uv_open_flag
  

What used to happen is that the `odoc link` command would cause an internal
error. If it doesn't here, that particular issue is fixed!

  $ ocamlc -c -bin-annot uwt_base.mli
  $ odoc compile uwt_base.cmti
  Starting type_of pass
  Adding (root Uwt_base).Base to env
  Adding (root Uwt_base).uv_result to env
  Adding (root Uwt_base).Fs_types to env
  Adding (root Uwt_base).Base.uv_result to env
  Adding (root Uwt_base).Base.Fs_types to env
  Adding (root Uwt_base).Base.Fs_types.uv_open_flag to env
  Adding (root Uwt_base).Base.Fs_functions.uv_open_flag to env
  Adding (root Uwt_base).Base.Fs_functions.t to env
  Handling include in type_of
  Removing (root Uwt_base).Base.Fs_functions.uv_open_flag from env
  Adding (root Uwt_base).Base.Fs_functions.uv_open_flag to env
  Finished handling include in type_of
  Handling include in type_of
  Removing (root Uwt_base).uv_result from env
  Removing (root Uwt_base).Fs_types from env
  Adding (root Uwt_base).uv_result to env
  Adding (root Uwt_base).Fs_types to env
  Adding (root Uwt_base).Fs_types.uv_open_flag to env
  Adding (root Uwt_base).Fs_functions.uv_open_flag to env
  Adding (root Uwt_base).Fs_functions.t to env
  Handling include in type_of
  Removing (root Uwt_base).Fs_functions.uv_open_flag from env
  Adding (root Uwt_base).Fs_functions.uv_open_flag to env
  Finished handling include in type_of
  Adding (root Uwt_base).Fs_types.uv_open_flag to env
  Adding (root Uwt_base).Fs_functions.uv_open_flag to env
  Adding (root Uwt_base).Fs_functions.t to env
  Finished handling include in type_of
  Finished type_of pass
  Adding (root Uwt_base).Base to env
  Adding (root Uwt_base).uv_result to env
  Adding (root Uwt_base).Fs_types to env
  Adding (root Uwt_base).Base.uv_result to env
  Adding (root Uwt_base).Base.Fs_types to env
  Adding (root Uwt_base).Base.Fs_types.uv_open_flag to env
  Adding (root Uwt_base).Base.Fs_functions.uv_open_flag to env
  Adding (root Uwt_base).Base.Fs_functions.t to env
  Handling include of : module type of identifier((root Uwt_base).Base.Fs_types, false) with [root.uv_open_flag = identifier((root Uwt_base).Base.Fs_types, false).uv_open_flag]
  Removing (root Uwt_base).Base.Fs_functions.uv_open_flag from env
  Adding (root Uwt_base).Base.Fs_functions.uv_open_flag to env
  Removing (root Uwt_base).Base.Fs_functions.uv_open_flag from env
  Adding (root Uwt_base).Base.Fs_functions.uv_open_flag to env
  Adding (root Uwt_base).Base.Fs_functions.uv_open_flag to env
  Adding (root Uwt_base).Base.Fs_functions.t to env
  Handling include of : module type of identifier((root Uwt_base).Base.Fs_types, false) with [root.uv_open_flag = identifier((root Uwt_base).Base.Fs_types, false).uv_open_flag]
  Removing (root Uwt_base).Base.Fs_functions.uv_open_flag from env
  Adding (root Uwt_base).Base.Fs_functions.uv_open_flag to env
  Removing (root Uwt_base).Base.Fs_functions.uv_open_flag from env
  Adding (root Uwt_base).Base.Fs_functions.uv_open_flag to env
  Handling include of : module type of identifier((root Uwt_base).Base, false) with [root.Fs_types.uv_open_flag = identifier((root Uwt_base).Base, false).Fs_types.uv_open_flag]
  Removing (root Uwt_base).uv_result from env
  Removing (root Uwt_base).Fs_types from env
  Adding (root Uwt_base).uv_result to env
  Adding (root Uwt_base).Fs_types to env
  Adding (root Uwt_base).Fs_types.uv_open_flag to env
  Adding (root Uwt_base).Fs_functions.uv_open_flag to env
  Adding (root Uwt_base).Fs_functions.t to env
  Handling include of : sig
  type uv_open_flag/100 = O_RDONLY of 
   (removed=[])end with [root.uv_open_flag = identifier((root Uwt_base).Fs_types, false).uv_open_flag]
  Removing (root Uwt_base).Fs_functions.uv_open_flag from env
  Adding (root Uwt_base).Fs_functions.uv_open_flag to env
  Removing (root Uwt_base).Fs_functions.uv_open_flag from env
  Adding (root Uwt_base).Fs_functions.uv_open_flag to env
  Adding (root Uwt_base).Fs_functions.uv_open_flag to env
  Adding (root Uwt_base).Fs_functions.t to env
  Handling include of : sig
  type uv_open_flag/116 = O_RDONLY of 
   (removed=[])end with [root.uv_open_flag = identifier((root Uwt_base).Fs_types, false).uv_open_flag]
  Removing (root Uwt_base).Fs_functions.uv_open_flag from env
  Adding (root Uwt_base).Fs_functions.uv_open_flag to env
  Removing (root Uwt_base).Fs_functions.uv_open_flag from env
  Adding (root Uwt_base).Fs_functions.uv_open_flag to env
  Removing (root Uwt_base).uv_result from env
  Removing (root Uwt_base).Fs_types from env
  Adding (root Uwt_base).uv_result to env
  Adding (root Uwt_base).Fs_types to env
  $ odoc link uwt_base.odoc
  Adding (root Uwt_base).Base to env
  Adding (root Uwt_base).uv_result to env
  Adding (root Uwt_base).Fs_types to env
  Adding (root Uwt_base).Base.uv_result to env
  Adding (root Uwt_base).Base.Fs_types to env
  Adding (root Uwt_base).Base.Fs_types.uv_open_flag to env
  Adding (root Uwt_base).Base.Fs_functions.uv_open_flag to env
  Adding (root Uwt_base).Base.Fs_functions.t to env
  Adding (root Uwt_base).Fs_types.uv_open_flag to env
  Adding (root Uwt_base).Fs_functions.uv_open_flag to env
  Adding (root Uwt_base).Fs_functions.t to env

