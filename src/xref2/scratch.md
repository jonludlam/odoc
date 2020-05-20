```ocaml env=e1
(* Prelude *)
#require "odoc.xref_test";;
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#install_printer Odoc_model.Names.ModuleName.fmt;;
#install_printer Odoc_model.Names.ParameterName.fmt;;
#install_printer Odoc_xref2.Component.Fmt.model_identifier;;
#install_printer Common.value_name_pp;;
#install_printer Common.type_name_pp;;
#install_printer Common.unit_name_pp;;
#print_length 60000;;
#print_depth 200;;

let mt_printer fmt mt = Format.fprintf fmt "%a" Odoc_xref2.Component.Fmt.model_identifier (mt : Odoc_model.Paths.Identifier.ModuleType.t :> Odoc_model.Paths.Identifier.t);;
#install_printer mt_printer;;
let m_printer fmt mt = Format.fprintf fmt "%a" Odoc_xref2.Component.Fmt.model_identifier (mt : Odoc_model.Paths.Identifier.Module.t :> Odoc_model.Paths.Identifier.t);;
#install_printer m_printer;;
let t_printer fmt t = Format.fprintf fmt "%a" Odoc_xref2.Component.Fmt.model_identifier (t : Odoc_model.Paths.Identifier.Type.t :> Odoc_model.Paths.Identifier.t);;
#install_printer t_printer;;
let doc_printer fmt (doc : Odoc_model.Comment.docs) = Format.fprintf fmt "<<docs>>";;
#install_printer doc_printer;;
#install_printer Ident.print_with_scope;;
#install_printer Ident.print;;
```

```ocaml env=e1
let test_data = {|
module M
  (S : sig
     module E : sig
       type 'a t
     end
  end)
  (D : sig type element end)
= struct
  include S
  module E = struct

    include E
    include struct
      open D
      type 'a t = element E.t
      let foo : unit -> element E.t = fun () -> failwith "foo"
    end
  end
end
|}
let sg = Common.cmt_of_string test_data;;
```

```ocaml env=e1
let test_data = {|
module M : sig
  type t
  val id : t -> t  
end

module Mextended : sig
  include module type of struct include M end
  type t
  val ignore : t -> unit
end
|};;
let sg = Common.cmti_of_string test_data;;
```

```
# Common.load_cmt "/Users/jon/.opam/4.09.0/lib/base/base__Container_intf.cmt";;
```

```ocaml env=e1
# sg;;
- : Typedtree.signature =
{Typedtree.sig_items =
  [{Typedtree.sig_desc =
     Typedtree.Tsig_module
      {Typedtree.md_id = <abstr>;
       md_name =
        {Asttypes.txt = "M";
         loc =
          {Location.loc_start =
            {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1; pos_cnum = 10};
           loc_end =
            {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1; pos_cnum = 11};
           loc_ghost = false}};
       md_presence = Types.Mp_present;
       md_type =
        {Typedtree.mty_desc =
          Typedtree.Tmty_signature
           {Typedtree.sig_items =
             [{Typedtree.sig_desc =
                Typedtree.Tsig_type (Asttypes.Recursive,
                 [{Typedtree.typ_id = <abstr>;
                   typ_name =
                    {Asttypes.txt = "t";
                     loc =
                      {Location.loc_start =
                        {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                         pos_cnum = 27};
                       loc_end =
                        {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                         pos_cnum = 28};
                       loc_ghost = false}};
                   typ_params = [];
                   typ_type =
                    {Types.type_params = []; type_arity = 0;
                     type_kind = Types.Type_abstract;
                     type_private = Asttypes.Public; type_manifest = None;
                     type_variance = []; type_is_newtype = false;
                     type_expansion_scope = 0;
                     type_loc =
                      {Location.loc_start =
                        {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                         pos_cnum = 22};
                       loc_end =
                        {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                         pos_cnum = 28};
                       loc_ghost = false};
                     type_attributes = []; type_immediate = false;
                     type_unboxed = {Types.unboxed = false; default = false}};
                   typ_cstrs = []; typ_kind = Typedtree.Ttype_abstract;
                   typ_private = Asttypes.Public; typ_manifest = None;
                   typ_loc =
                    {Location.loc_start =
                      {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                       pos_cnum = 22};
                     loc_end =
                      {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                       pos_cnum = 28};
                     loc_ghost = false};
                   typ_attributes = []}]);
               sig_env = <abstr>;
               sig_loc =
                {Location.loc_start =
                  {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                   pos_cnum = 22};
                 loc_end =
                  {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                   pos_cnum = 28};
                 loc_ghost = false}};
              {Typedtree.sig_desc =
                Typedtree.Tsig_value
                 {Typedtree.val_id = <abstr>;
                  val_name =
                   {Asttypes.txt = "id";
                    loc =
                     {Location.loc_start =
                       {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                        pos_cnum = 37};
                      loc_end =
                       {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                        pos_cnum = 39};
                      loc_ghost = false}};
                  val_desc =
                   {Typedtree.ctyp_desc =
                     Typedtree.Ttyp_arrow (Asttypes.Nolabel,
                      {Typedtree.ctyp_desc =
                        Typedtree.Ttyp_constr (Path.Pident <abstr>,
                         {Asttypes.txt = Longident.Lident "t";
                          loc =
                           {Location.loc_start =
                             {Lexing.pos_fname = ""; pos_lnum = 4;
                              pos_bol = 29; pos_cnum = 42};
                            loc_end =
                             {Lexing.pos_fname = ""; pos_lnum = 4;
                              pos_bol = 29; pos_cnum = 43};
                            loc_ghost = false}},
                         []);
                       ctyp_type =
                        {Types.desc =
                          Types.Tconstr (Path.Pident <abstr>, [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372058};
                       ctyp_env = <abstr>;
                       ctyp_loc =
                        {Location.loc_start =
                          {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                           pos_cnum = 42};
                         loc_end =
                          {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                           pos_cnum = 43};
                         loc_ghost = false};
                       ctyp_attributes = []},
                      {Typedtree.ctyp_desc =
                        Typedtree.Ttyp_constr (Path.Pident <abstr>,
                         {Asttypes.txt = Longident.Lident "t";
                          loc =
                           {Location.loc_start =
                             {Lexing.pos_fname = ""; pos_lnum = 4;
                              pos_bol = 29; pos_cnum = 47};
                            loc_end =
                             {Lexing.pos_fname = ""; pos_lnum = 4;
                              pos_bol = 29; pos_cnum = 48};
                            loc_ghost = false}},
                         []);
                       ctyp_type =
                        {Types.desc =
                          Types.Tconstr (Path.Pident <abstr>, [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372061};
                       ctyp_env = <abstr>;
                       ctyp_loc =
                        {Location.loc_start =
                          {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                           pos_cnum = 47};
                         loc_end =
                          {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                           pos_cnum = 48};
                         loc_ghost = false};
                       ctyp_attributes = []});
                    ctyp_type =
                     {Types.desc =
                       Types.Tarrow (Asttypes.Nolabel,
                        {Types.desc =
                          Types.Tconstr (Path.Pident <abstr>, [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372058},
                        {Types.desc =
                          Types.Tconstr (Path.Pident <abstr>, [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372061},
                        Types.Cok);
                      level = 100000000; scope = 0; id = 372064};
                    ctyp_env = <abstr>;
                    ctyp_loc =
                     {Location.loc_start =
                       {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                        pos_cnum = 42};
                      loc_end =
                       {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                        pos_cnum = 48};
                      loc_ghost = false};
                    ctyp_attributes = []};
                  val_val =
                   {Types.val_type =
                     {Types.desc =
                       Types.Tarrow (Asttypes.Nolabel,
                        {Types.desc =
                          Types.Tconstr (Path.Pident <abstr>, [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372058},
                        {Types.desc =
                          Types.Tconstr (Path.Pident <abstr>, [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372061},
                        Types.Cok);
                      level = 100000000; scope = 0; id = 372064};
                    val_kind = Types.Val_reg;
                    val_loc =
                     {Location.loc_start =
                       {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                        pos_cnum = 33};
                      loc_end =
                       {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                        pos_cnum = 48};
                      loc_ghost = false};
                    val_attributes = []};
                  val_prim = [];
                  val_loc =
                   {Location.loc_start =
                     {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                      pos_cnum = 33};
                    loc_end =
                     {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                      pos_cnum = 48};
                    loc_ghost = false};
                  val_attributes = []};
               sig_env = <abstr>;
               sig_loc =
                {Location.loc_start =
                  {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                   pos_cnum = 33};
                 loc_end =
                  {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                   pos_cnum = 48};
                 loc_ghost = false}}];
            sig_type =
             [Types.Sig_type (<abstr>,
               {Types.type_params = []; type_arity = 0;
                type_kind = Types.Type_abstract;
                type_private = Asttypes.Public; type_manifest = None;
                type_variance = []; type_is_newtype = false;
                type_expansion_scope = 0;
                type_loc =
                 {Location.loc_start =
                   {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                    pos_cnum = 22};
                  loc_end =
                   {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                    pos_cnum = 28};
                  loc_ghost = false};
                type_attributes = []; type_immediate = false;
                type_unboxed = {Types.unboxed = false; default = false}},
               Types.Trec_first, Types.Exported);
              Types.Sig_value (<abstr>,
               {Types.val_type =
                 {Types.desc =
                   Types.Tarrow (Asttypes.Nolabel,
                    {Types.desc =
                      Types.Tconstr (Path.Pident <abstr>, [],
                       {contents = Types.Mnil});
                     level = 100000000; scope = 0; id = 372058},
                    {Types.desc =
                      Types.Tconstr (Path.Pident <abstr>, [],
                       {contents = Types.Mnil});
                     level = 100000000; scope = 0; id = 372061},
                    Types.Cok);
                  level = 100000000; scope = 0; id = 372064};
                val_kind = Types.Val_reg;
                val_loc =
                 {Location.loc_start =
                   {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                    pos_cnum = 33};
                  loc_end =
                   {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                    pos_cnum = 48};
                  loc_ghost = false};
                val_attributes = []},
               Types.Exported)];
            sig_final_env = <abstr>};
         mty_type =
          Types.Mty_signature
           [Types.Sig_type (<abstr>,
             {Types.type_params = []; type_arity = 0;
              type_kind = Types.Type_abstract;
              type_private = Asttypes.Public; type_manifest = None;
              type_variance = []; type_is_newtype = false;
              type_expansion_scope = 0;
              type_loc =
               {Location.loc_start =
                 {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                  pos_cnum = 22};
                loc_end =
                 {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                  pos_cnum = 28};
                loc_ghost = false};
              type_attributes = []; type_immediate = false;
              type_unboxed = {Types.unboxed = false; default = false}},
             Types.Trec_first, Types.Exported);
            Types.Sig_value (<abstr>,
             {Types.val_type =
               {Types.desc =
                 Types.Tarrow (Asttypes.Nolabel,
                  {Types.desc =
                    Types.Tconstr (Path.Pident <abstr>, [],
                     {contents = Types.Mnil});
                   level = 100000000; scope = 0; id = 372058},
                  {Types.desc =
                    Types.Tconstr (Path.Pident <abstr>, [],
                     {contents = Types.Mnil});
                   level = 100000000; scope = 0; id = 372061},
                  Types.Cok);
                level = 100000000; scope = 0; id = 372064};
              val_kind = Types.Val_reg;
              val_loc =
               {Location.loc_start =
                 {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                  pos_cnum = 33};
                loc_end =
                 {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                  pos_cnum = 48};
                loc_ghost = false};
              val_attributes = []},
             Types.Exported)];
         mty_env = <abstr>;
         mty_loc =
          {Location.loc_start =
            {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1; pos_cnum = 14};
           loc_end =
            {Lexing.pos_fname = ""; pos_lnum = 5; pos_bol = 51;
             pos_cnum = 56};
           loc_ghost = false};
         mty_attributes = []};
       md_attributes = [];
       md_loc =
        {Location.loc_start =
          {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1; pos_cnum = 3};
         loc_end =
          {Lexing.pos_fname = ""; pos_lnum = 5; pos_bol = 51; pos_cnum = 56};
         loc_ghost = false}};
    sig_env = <abstr>;
    sig_loc =
     {Location.loc_start =
       {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1; pos_cnum = 3};
      loc_end =
       {Lexing.pos_fname = ""; pos_lnum = 5; pos_bol = 51; pos_cnum = 56};
      loc_ghost = false}};
   {Typedtree.sig_desc =
     Typedtree.Tsig_module
      {Typedtree.md_id = <abstr>;
       md_name =
        {Asttypes.txt = "Mextended";
         loc =
          {Location.loc_start =
            {Lexing.pos_fname = ""; pos_lnum = 7; pos_bol = 60;
             pos_cnum = 69};
           loc_end =
            {Lexing.pos_fname = ""; pos_lnum = 7; pos_bol = 60;
             pos_cnum = 78};
           loc_ghost = false}};
       md_presence = Types.Mp_present;
       md_type =
        {Typedtree.mty_desc =
          Typedtree.Tmty_signature
           {Typedtree.sig_items =
             [{Typedtree.sig_desc =
                Typedtree.Tsig_include
                 {Typedtree.incl_mod =
                   {Typedtree.mty_desc =
                     Typedtree.Tmty_typeof
                      {Typedtree.mod_desc =
                        Typedtree.Tmod_structure
                         {Typedtree.str_items =
                           [{Typedtree.str_desc =
                              Typedtree.Tstr_include
                               {Typedtree.incl_mod =
                                 {Typedtree.mod_desc =
                                   Typedtree.Tmod_ident (Path.Pident <abstr>,
                                    {Asttypes.txt = Longident.Lident "M";
                                     loc =
                                      {Location.loc_start =
                                        {Lexing.pos_fname = ""; pos_lnum = 8;
                                         pos_bol = 85; pos_cnum = 127};
                                       loc_end =
                                        {Lexing.pos_fname = ""; pos_lnum = 8;
                                         pos_bol = 85; pos_cnum = 128};
                                       loc_ghost = false}});
                                  mod_loc =
                                   {Location.loc_start =
                                     {Lexing.pos_fname = ""; pos_lnum = 8;
                                      pos_bol = 85; pos_cnum = 127};
                                    loc_end =
                                     {Lexing.pos_fname = ""; pos_lnum = 8;
                                      pos_bol = 85; pos_cnum = 128};
                                    loc_ghost = false};
                                  mod_type =
                                   Types.Mty_signature
                                    [Types.Sig_type (<abstr>,
                                      {Types.type_params = [];
                                       type_arity = 0;
                                       type_kind = Types.Type_abstract;
                                       type_private = Asttypes.Public;
                                       type_manifest =
                                        Some
                                         {Types.desc =
                                           Types.Tconstr
                                            (Path.Pdot (Path.Pident <abstr>,
                                              "t"),
                                            [], {contents = Types.Mnil});
                                          level = 100000000; scope = 0;
                                          id = 372065};
                                       type_variance = [];
                                       type_is_newtype = false;
                                       type_expansion_scope = 0;
                                       type_loc =
                                        {Location.loc_start =
                                          {Lexing.pos_fname = "";
                                           pos_lnum = 3; pos_bol = 18;
                                           pos_cnum = 22};
                                         loc_end =
                                          {Lexing.pos_fname = "";
                                           pos_lnum = 3; pos_bol = 18;
                                           pos_cnum = 28};
                                         loc_ghost = false};
                                       type_attributes = [];
                                       type_immediate = false;
                                       type_unboxed =
                                        {Types.unboxed = false;
                                         default = false}},
                                      Types.Trec_first, Types.Exported);
                                     Types.Sig_value (<abstr>,
                                      {Types.val_type =
                                        {Types.desc =
                                          Types.Tarrow (Asttypes.Nolabel,
                                           {Types.desc =
                                             Types.Tconstr
                                              (Path.Pident <abstr>, [],
                                              {contents = Types.Mnil});
                                            level = 100000000; scope = 0;
                                            id = 372058},
                                           {Types.desc =
                                             Types.Tconstr
                                              (Path.Pident <abstr>, [],
                                              {contents = Types.Mnil});
                                            level = 100000000; scope = 0;
                                            id = 372061},
                                           Types.Cok);
                                         level = 100000000; scope = 0;
                                         id = 372064};
                                       val_kind = Types.Val_reg;
                                       val_loc =
                                        {Location.loc_start =
                                          {Lexing.pos_fname = "";
                                           pos_lnum = 4; pos_bol = 29;
                                           pos_cnum = 33};
                                         loc_end =
                                          {Lexing.pos_fname = "";
                                           pos_lnum = 4; pos_bol = 29;
                                           pos_cnum = 48};
                                         loc_ghost = false};
                                       val_attributes = []},
                                      Types.Exported)];
                                  mod_env = <abstr>; mod_attributes = []};
                                incl_type =
                                 [Types.Sig_type (<abstr>,
                                   {Types.type_params = []; type_arity = 0;
                                    type_kind = Types.Type_abstract;
                                    type_private = Asttypes.Public;
                                    type_manifest =
                                     Some
                                      {Types.desc =
                                        Types.Tconstr
                                         (Path.Pdot (Path.Pident <abstr>,
                                           "t"),
                                         [], {contents = Types.Mnil});
                                       level = 100000000; scope = 0;
                                       id = 372069};
                                    type_variance = [];
                                    type_is_newtype = false;
                                    type_expansion_scope = 0;
                                    type_loc =
                                     {Location.loc_start =
                                       {Lexing.pos_fname = ""; pos_lnum = 3;
                                        pos_bol = 18; pos_cnum = 22};
                                      loc_end =
                                       {Lexing.pos_fname = ""; pos_lnum = 3;
                                        pos_bol = 18; pos_cnum = 28};
                                      loc_ghost = false};
                                    type_attributes = [];
                                    type_immediate = false;
                                    type_unboxed =
                                     {Types.unboxed = false; default = false}},
                                   Types.Trec_first, Types.Exported);
                                  Types.Sig_value (<abstr>,
                                   {Types.val_type =
                                     {Types.desc =
                                       Types.Tarrow (Asttypes.Nolabel,
                                        {Types.desc =
                                          Types.Tconstr (Path.Pident <abstr>,
                                           [], {contents = Types.Mnil});
                                         level = 100000000; scope = 0;
                                         id = 372068},
                                        {Types.desc =
                                          Types.Tconstr (Path.Pident <abstr>,
                                           [], {contents = Types.Mnil});
                                         level = 100000000; scope = 0;
                                         id = 372067},
                                        Types.Cok);
                                      level = 100000000; scope = 0;
                                      id = 372066};
                                    val_kind = Types.Val_reg;
                                    val_loc =
                                     {Location.loc_start =
                                       {Lexing.pos_fname = ""; pos_lnum = 4;
                                        pos_bol = 29; pos_cnum = 33};
                                      loc_end =
                                       {Lexing.pos_fname = ""; pos_lnum = 4;
                                        pos_bol = 29; pos_cnum = 48};
                                      loc_ghost = false};
                                    val_attributes = []},
                                   Types.Exported)];
                                incl_loc =
                                 {Location.loc_start =
                                   {Lexing.pos_fname = ""; pos_lnum = 8;
                                    pos_bol = 85; pos_cnum = 119};
                                  loc_end =
                                   {Lexing.pos_fname = ""; pos_lnum = 8;
                                    pos_bol = 85; pos_cnum = 128};
                                  loc_ghost = false};
                                incl_attributes = []};
                             str_loc =
                              {Location.loc_start =
                                {Lexing.pos_fname = ""; pos_lnum = 8;
                                 pos_bol = 85; pos_cnum = 119};
                               loc_end =
                                {Lexing.pos_fname = ""; pos_lnum = 8;
                                 pos_bol = 85; pos_cnum = 128};
                               loc_ghost = false};
                             str_env = <abstr>}];
                          str_type =
                           [Types.Sig_type (<abstr>,
                             {Types.type_params = []; type_arity = 0;
                              type_kind = Types.Type_abstract;
                              type_private = Asttypes.Public;
                              type_manifest =
                               Some
                                {Types.desc =
                                  Types.Tconstr
                                   (Path.Pdot (Path.Pident <abstr>, "t"),
                                   [], {contents = Types.Mnil});
                                 level = 100000000; scope = 0; id = 372069};
                              type_variance = []; type_is_newtype = false;
                              type_expansion_scope = 0;
                              type_loc =
                               {Location.loc_start =
                                 {Lexing.pos_fname = ""; pos_lnum = 3;
                                  pos_bol = 18; pos_cnum = 22};
                                loc_end =
                                 {Lexing.pos_fname = ""; pos_lnum = 3;
                                  pos_bol = 18; pos_cnum = 28};
                                loc_ghost = false};
                              type_attributes = []; type_immediate = false;
                              type_unboxed =
                               {Types.unboxed = false; default = false}},
                             Types.Trec_first, Types.Exported);
                            Types.Sig_value (<abstr>,
                             {Types.val_type =
                               {Types.desc =
                                 Types.Tarrow (Asttypes.Nolabel,
                                  {Types.desc =
                                    Types.Tconstr (Path.Pident <abstr>,
                                     [], {contents = Types.Mnil});
                                   level = 100000000; scope = 0; id = 372068},
                                  {Types.desc =
                                    Types.Tconstr (Path.Pident <abstr>,
                                     [], {contents = Types.Mnil});
                                   level = 100000000; scope = 0; id = 372067},
                                  Types.Cok);
                                level = 100000000; scope = 0; id = 372066};
                              val_kind = Types.Val_reg;
                              val_loc =
                               {Location.loc_start =
                                 {Lexing.pos_fname = ""; pos_lnum = 4;
                                  pos_bol = 29; pos_cnum = 33};
                                loc_end =
                                 {Lexing.pos_fname = ""; pos_lnum = 4;
                                  pos_bol = 29; pos_cnum = 48};
                                loc_ghost = false};
                              val_attributes = []},
                             Types.Exported)];
                          str_final_env = <abstr>};
                       mod_loc =
                        {Location.loc_start =
                          {Lexing.pos_fname = ""; pos_lnum = 8; pos_bol = 85;
                           pos_cnum = 112};
                         loc_end =
                          {Lexing.pos_fname = ""; pos_lnum = 8; pos_bol = 85;
                           pos_cnum = 132};
                         loc_ghost = false};
                       mod_type =
                        Types.Mty_signature
                         [Types.Sig_type (<abstr>,
                           {Types.type_params = []; type_arity = 0;
                            type_kind = Types.Type_abstract;
                            type_private = Asttypes.Public;
                            type_manifest =
                             Some
                              {Types.desc =
                                Types.Tconstr
                                 (Path.Pdot (Path.Pident <abstr>, "t"),
                                 [], {contents = Types.Mnil});
                               level = 100000000; scope = 0; id = 372069};
                            type_variance = []; type_is_newtype = false;
                            type_expansion_scope = 0;
                            type_loc =
                             {Location.loc_start =
                               {Lexing.pos_fname = ""; pos_lnum = 3;
                                pos_bol = 18; pos_cnum = 22};
                              loc_end =
                               {Lexing.pos_fname = ""; pos_lnum = 3;
                                pos_bol = 18; pos_cnum = 28};
                              loc_ghost = false};
                            type_attributes = []; type_immediate = false;
                            type_unboxed =
                             {Types.unboxed = false; default = false}},
                           Types.Trec_first, Types.Exported);
                          Types.Sig_value (<abstr>,
                           {Types.val_type =
                             {Types.desc =
                               Types.Tarrow (Asttypes.Nolabel,
                                {Types.desc =
                                  Types.Tconstr (Path.Pident <abstr>,
                                   [], {contents = Types.Mnil});
                                 level = 100000000; scope = 0; id = 372068},
                                {Types.desc =
                                  Types.Tconstr (Path.Pident <abstr>,
                                   [], {contents = Types.Mnil});
                                 level = 100000000; scope = 0; id = 372067},
                                Types.Cok);
                              level = 100000000; scope = 0; id = 372066};
                            val_kind = Types.Val_reg;
                            val_loc =
                             {Location.loc_start =
                               {Lexing.pos_fname = ""; pos_lnum = 4;
                                pos_bol = 29; pos_cnum = 33};
                              loc_end =
                               {Lexing.pos_fname = ""; pos_lnum = 4;
                                pos_bol = 29; pos_cnum = 48};
                              loc_ghost = false};
                            val_attributes = []},
                           Types.Exported)];
                       mod_env = <abstr>; mod_attributes = []};
                    mty_type =
                     Types.Mty_signature
                      [Types.Sig_type (<abstr>,
                        {Types.type_params = []; type_arity = 0;
                         type_kind = Types.Type_abstract;
                         type_private = Asttypes.Public;
                         type_manifest =
                          Some
                           {Types.desc =
                             Types.Tconstr
                              (Path.Pdot (Path.Pident <abstr>, "t"),
                              [], {contents = Types.Mnil});
                            level = 100000000; scope = 0; id = 372069};
                         type_variance = []; type_is_newtype = false;
                         type_expansion_scope = 0;
                         type_loc =
                          {Location.loc_start =
                            {Lexing.pos_fname = ""; pos_lnum = 3;
                             pos_bol = 18; pos_cnum = 22};
                           loc_end =
                            {Lexing.pos_fname = ""; pos_lnum = 3;
                             pos_bol = 18; pos_cnum = 28};
                           loc_ghost = false};
                         type_attributes = []; type_immediate = false;
                         type_unboxed =
                          {Types.unboxed = false; default = false}},
                        Types.Trec_first, Types.Exported);
                       Types.Sig_value (<abstr>,
                        {Types.val_type =
                          {Types.desc =
                            Types.Tarrow (Asttypes.Nolabel,
                             {Types.desc =
                               Types.Tconstr (Path.Pident <abstr>, [],
                                {contents = Types.Mnil});
                              level = 100000000; scope = 0; id = 372068},
                             {Types.desc =
                               Types.Tconstr (Path.Pident <abstr>, [],
                                {contents = Types.Mnil});
                              level = 100000000; scope = 0; id = 372067},
                             Types.Cok);
                           level = 100000000; scope = 0; id = 372066};
                         val_kind = Types.Val_reg;
                         val_loc =
                          {Location.loc_start =
                            {Lexing.pos_fname = ""; pos_lnum = 4;
                             pos_bol = 29; pos_cnum = 33};
                           loc_end =
                            {Lexing.pos_fname = ""; pos_lnum = 4;
                             pos_bol = 29; pos_cnum = 48};
                           loc_ghost = false};
                         val_attributes = []},
                        Types.Exported)];
                    mty_env = <abstr>;
                    mty_loc =
                     {Location.loc_start =
                       {Lexing.pos_fname = ""; pos_lnum = 8; pos_bol = 85;
                        pos_cnum = 97};
                      loc_end =
                       {Lexing.pos_fname = ""; pos_lnum = 8; pos_bol = 85;
                        pos_cnum = 132};
                      loc_ghost = false};
                    mty_attributes = []};
                  incl_type =
                   [Types.Sig_type (<abstr>,
                     {Types.type_params = []; type_arity = 0;
                      type_kind = Types.Type_abstract;
                      type_private = Asttypes.Public;
                      type_manifest =
                       Some
                        {Types.desc =
                          Types.Tconstr
                           (Path.Pdot (Path.Pident <abstr>, "t"), [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372073};
                      type_variance = []; type_is_newtype = false;
                      type_expansion_scope = 0;
                      type_loc =
                       {Location.loc_start =
                         {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                          pos_cnum = 22};
                        loc_end =
                         {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
                          pos_cnum = 28};
                        loc_ghost = false};
                      type_attributes = []; type_immediate = false;
                      type_unboxed = {Types.unboxed = false; default = false}},
                     Types.Trec_first, Types.Exported);
                    Types.Sig_value (<abstr>,
                     {Types.val_type =
                       {Types.desc =
                         Types.Tarrow (Asttypes.Nolabel,
                          {Types.desc =
                            Types.Tconstr (Path.Pident <abstr>, [],
                             {contents = Types.Mnil});
                           level = 100000000; scope = 0; id = 372072},
                          {Types.desc =
                            Types.Tconstr (Path.Pident <abstr>, [],
                             {contents = Types.Mnil});
                           level = 100000000; scope = 0; id = 372071},
                          Types.Cok);
                        level = 100000000; scope = 0; id = 372070};
                      val_kind = Types.Val_reg;
                      val_loc =
                       {Location.loc_start =
                         {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                          pos_cnum = 33};
                        loc_end =
                         {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                          pos_cnum = 48};
                        loc_ghost = false};
                      val_attributes = []},
                     Types.Exported)];
                  incl_loc =
                   {Location.loc_start =
                     {Lexing.pos_fname = ""; pos_lnum = 8; pos_bol = 85;
                      pos_cnum = 89};
                    loc_end =
                     {Lexing.pos_fname = ""; pos_lnum = 8; pos_bol = 85;
                      pos_cnum = 132};
                    loc_ghost = false};
                  incl_attributes = []};
               sig_env = <abstr>;
               sig_loc =
                {Location.loc_start =
                  {Lexing.pos_fname = ""; pos_lnum = 8; pos_bol = 85;
                   pos_cnum = 89};
                 loc_end =
                  {Lexing.pos_fname = ""; pos_lnum = 8; pos_bol = 85;
                   pos_cnum = 132};
                 loc_ghost = false}};
              {Typedtree.sig_desc =
                Typedtree.Tsig_type (Asttypes.Recursive,
                 [{Typedtree.typ_id = <abstr>;
                   typ_name =
                    {Asttypes.txt = "t";
                     loc =
                      {Location.loc_start =
                        {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
                         pos_cnum = 142};
                       loc_end =
                        {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
                         pos_cnum = 143};
                       loc_ghost = false}};
                   typ_params = [];
                   typ_type =
                    {Types.type_params = []; type_arity = 0;
                     type_kind = Types.Type_abstract;
                     type_private = Asttypes.Public; type_manifest = None;
                     type_variance = []; type_is_newtype = false;
                     type_expansion_scope = 0;
                     type_loc =
                      {Location.loc_start =
                        {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
                         pos_cnum = 137};
                       loc_end =
                        {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
                         pos_cnum = 143};
                       loc_ghost = false};
                     type_attributes = []; type_immediate = false;
                     type_unboxed = {Types.unboxed = false; default = false}};
                   typ_cstrs = []; typ_kind = Typedtree.Ttype_abstract;
                   typ_private = Asttypes.Public; typ_manifest = None;
                   typ_loc =
                    {Location.loc_start =
                      {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
                       pos_cnum = 137};
                     loc_end =
                      {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
                       pos_cnum = 143};
                     loc_ghost = false};
                   typ_attributes = []}]);
               sig_env = <abstr>;
               sig_loc =
                {Location.loc_start =
                  {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
                   pos_cnum = 137};
                 loc_end =
                  {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
                   pos_cnum = 143};
                 loc_ghost = false}};
              {Typedtree.sig_desc =
                Typedtree.Tsig_value
                 {Typedtree.val_id = <abstr>;
                  val_name =
                   {Asttypes.txt = "ignore";
                    loc =
                     {Location.loc_start =
                       {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                        pos_cnum = 152};
                      loc_end =
                       {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                        pos_cnum = 158};
                      loc_ghost = false}};
                  val_desc =
                   {Typedtree.ctyp_desc =
                     Typedtree.Ttyp_arrow (Asttypes.Nolabel,
                      {Typedtree.ctyp_desc =
                        Typedtree.Ttyp_constr (Path.Pident <abstr>,
                         {Asttypes.txt = Longident.Lident "t";
                          loc =
                           {Location.loc_start =
                             {Lexing.pos_fname = ""; pos_lnum = 10;
                              pos_bol = 144; pos_cnum = 161};
                            loc_end =
                             {Lexing.pos_fname = ""; pos_lnum = 10;
                              pos_bol = 144; pos_cnum = 162};
                            loc_ghost = false}},
                         []);
                       ctyp_type =
                        {Types.desc =
                          Types.Tconstr (Path.Pident <abstr>, [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372074};
                       ctyp_env = <abstr>;
                       ctyp_loc =
                        {Location.loc_start =
                          {Lexing.pos_fname = ""; pos_lnum = 10;
                           pos_bol = 144; pos_cnum = 161};
                         loc_end =
                          {Lexing.pos_fname = ""; pos_lnum = 10;
                           pos_bol = 144; pos_cnum = 162};
                         loc_ghost = false};
                       ctyp_attributes = []},
                      {Typedtree.ctyp_desc =
                        Typedtree.Ttyp_constr (Path.Pident <abstr>,
                         {Asttypes.txt = Longident.Lident "unit";
                          loc =
                           {Location.loc_start =
                             {Lexing.pos_fname = ""; pos_lnum = 10;
                              pos_bol = 144; pos_cnum = 166};
                            loc_end =
                             {Lexing.pos_fname = ""; pos_lnum = 10;
                              pos_bol = 144; pos_cnum = 170};
                            loc_ghost = false}},
                         []);
                       ctyp_type =
                        {Types.desc =
                          Types.Tconstr (Path.Pident <abstr>, [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372077};
                       ctyp_env = <abstr>;
                       ctyp_loc =
                        {Location.loc_start =
                          {Lexing.pos_fname = ""; pos_lnum = 10;
                           pos_bol = 144; pos_cnum = 166};
                         loc_end =
                          {Lexing.pos_fname = ""; pos_lnum = 10;
                           pos_bol = 144; pos_cnum = 170};
                         loc_ghost = false};
                       ctyp_attributes = []});
                    ctyp_type =
                     {Types.desc =
                       Types.Tarrow (Asttypes.Nolabel,
                        {Types.desc =
                          Types.Tconstr (Path.Pident <abstr>, [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372074},
                        {Types.desc =
                          Types.Tconstr (Path.Pident <abstr>, [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372077},
                        Types.Cok);
                      level = 100000000; scope = 0; id = 372080};
                    ctyp_env = <abstr>;
                    ctyp_loc =
                     {Location.loc_start =
                       {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                        pos_cnum = 161};
                      loc_end =
                       {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                        pos_cnum = 170};
                      loc_ghost = false};
                    ctyp_attributes = []};
                  val_val =
                   {Types.val_type =
                     {Types.desc =
                       Types.Tarrow (Asttypes.Nolabel,
                        {Types.desc =
                          Types.Tconstr (Path.Pident <abstr>, [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372074},
                        {Types.desc =
                          Types.Tconstr (Path.Pident <abstr>, [],
                           {contents = Types.Mnil});
                         level = 100000000; scope = 0; id = 372077},
                        Types.Cok);
                      level = 100000000; scope = 0; id = 372080};
                    val_kind = Types.Val_reg;
                    val_loc =
                     {Location.loc_start =
                       {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                        pos_cnum = 148};
                      loc_end =
                       {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                        pos_cnum = 170};
                      loc_ghost = false};
                    val_attributes = []};
                  val_prim = [];
                  val_loc =
                   {Location.loc_start =
                     {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                      pos_cnum = 148};
                    loc_end =
                     {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                      pos_cnum = 170};
                    loc_ghost = false};
                  val_attributes = []};
               sig_env = <abstr>;
               sig_loc =
                {Location.loc_start =
                  {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                   pos_cnum = 148};
                 loc_end =
                  {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                   pos_cnum = 170};
                 loc_ghost = false}}];
            sig_type =
             [Types.Sig_value (<abstr>,
               {Types.val_type =
                 {Types.desc =
                   Types.Tarrow (Asttypes.Nolabel,
                    {Types.desc =
                      Types.Tlink
                       {Types.desc =
                         Types.Tconstr (Path.Pdot (Path.Pident <abstr>, "t"),
                          [], {contents = Types.Mnil});
                        level = 100000000; scope = 0; id = 372089};
                     level = 100000000; scope = 0; id = 372090},
                    {Types.desc =
                      Types.Tlink
                       {Types.desc =
                         Types.Tconstr (Path.Pdot (Path.Pident <abstr>, "t"),
                          [], {contents = Types.Mnil});
                        level = 100000000; scope = 0; id = 372089};
                     level = 100000000; scope = 0; id = 372085},
                    Types.Cok);
                  level = 100000000; scope = 0; id = 372084};
                val_kind = Types.Val_reg;
                val_loc =
                 {Location.loc_start =
                   {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                    pos_cnum = 33};
                  loc_end =
                   {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                    pos_cnum = 48};
                  loc_ghost = false};
                val_attributes = []},
               Types.Exported);
              Types.Sig_type (<abstr>,
               {Types.type_params = []; type_arity = 0;
                type_kind = Types.Type_abstract;
                type_private = Asttypes.Public; type_manifest = None;
                type_variance = []; type_is_newtype = false;
                type_expansion_scope = 0;
                type_loc =
                 {Location.loc_start =
                   {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
                    pos_cnum = 137};
                  loc_end =
                   {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
                    pos_cnum = 143};
                  loc_ghost = false};
                type_attributes = []; type_immediate = false;
                type_unboxed = {Types.unboxed = false; default = false}},
               Types.Trec_first, Types.Exported);
              Types.Sig_value (<abstr>,
               {Types.val_type =
                 {Types.desc =
                   Types.Tarrow (Asttypes.Nolabel,
                    {Types.desc =
                      Types.Tconstr (Path.Pident <abstr>, [],
                       {contents = Types.Mnil});
                     level = 100000000; scope = 0; id = 372083},
                    {Types.desc =
                      Types.Tconstr (Path.Pident <abstr>, [],
                       {contents = Types.Mnil});
                     level = 100000000; scope = 0; id = 372082},
                    Types.Cok);
                  level = 100000000; scope = 0; id = 372081};
                val_kind = Types.Val_reg;
                val_loc =
                 {Location.loc_start =
                   {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                    pos_cnum = 148};
                  loc_end =
                   {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                    pos_cnum = 170};
                  loc_ghost = false};
                val_attributes = []},
               Types.Exported)];
            sig_final_env = <abstr>};
         mty_type =
          Types.Mty_signature
           [Types.Sig_value (<abstr>,
             {Types.val_type =
               {Types.desc =
                 Types.Tarrow (Asttypes.Nolabel,
                  {Types.desc =
                    Types.Tlink
                     {Types.desc =
                       Types.Tconstr (Path.Pdot (Path.Pident <abstr>, "t"),
                        [], {contents = Types.Mnil});
                      level = 100000000; scope = 0; id = 372089};
                   level = 100000000; scope = 0; id = 372090},
                  {Types.desc =
                    Types.Tlink
                     {Types.desc =
                       Types.Tconstr (Path.Pdot (Path.Pident <abstr>, "t"),
                        [], {contents = Types.Mnil});
                      level = 100000000; scope = 0; id = 372089};
                   level = 100000000; scope = 0; id = 372085},
                  Types.Cok);
                level = 100000000; scope = 0; id = 372084};
              val_kind = Types.Val_reg;
              val_loc =
               {Location.loc_start =
                 {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                  pos_cnum = 33};
                loc_end =
                 {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
                  pos_cnum = 48};
                loc_ghost = false};
              val_attributes = []},
             Types.Exported);
            Types.Sig_type (<abstr>,
             {Types.type_params = []; type_arity = 0;
              type_kind = Types.Type_abstract;
              type_private = Asttypes.Public; type_manifest = None;
              type_variance = []; type_is_newtype = false;
              type_expansion_scope = 0;
              type_loc =
               {Location.loc_start =
                 {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
                  pos_cnum = 137};
                loc_end =
                 {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
                  pos_cnum = 143};
                loc_ghost = false};
              type_attributes = []; type_immediate = false;
              type_unboxed = {Types.unboxed = false; default = false}},
             Types.Trec_first, Types.Exported);
            Types.Sig_value (<abstr>,
             {Types.val_type =
               {Types.desc =
                 Types.Tarrow (Asttypes.Nolabel,
                  {Types.desc =
                    Types.Tconstr (Path.Pident <abstr>, [],
                     {contents = Types.Mnil});
                   level = 100000000; scope = 0; id = 372083},
                  {Types.desc =
                    Types.Tconstr (Path.Pident <abstr>, [],
                     {contents = Types.Mnil});
                   level = 100000000; scope = 0; id = 372082},
                  Types.Cok);
                level = 100000000; scope = 0; id = 372081};
              val_kind = Types.Val_reg;
              val_loc =
               {Location.loc_start =
                 {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                  pos_cnum = 148};
                loc_end =
                 {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
                  pos_cnum = 170};
                loc_ghost = false};
              val_attributes = []},
             Types.Exported)];
         mty_env = <abstr>;
         mty_loc =
          {Location.loc_start =
            {Lexing.pos_fname = ""; pos_lnum = 7; pos_bol = 60;
             pos_cnum = 81};
           loc_end =
            {Lexing.pos_fname = ""; pos_lnum = 11; pos_bol = 171;
             pos_cnum = 176};
           loc_ghost = false};
         mty_attributes = []};
       md_attributes = [];
       md_loc =
        {Location.loc_start =
          {Lexing.pos_fname = ""; pos_lnum = 7; pos_bol = 60; pos_cnum = 62};
         loc_end =
          {Lexing.pos_fname = ""; pos_lnum = 11; pos_bol = 171;
           pos_cnum = 176};
         loc_ghost = false}};
    sig_env = <abstr>;
    sig_loc =
     {Location.loc_start =
       {Lexing.pos_fname = ""; pos_lnum = 7; pos_bol = 60; pos_cnum = 62};
      loc_end =
       {Lexing.pos_fname = ""; pos_lnum = 11; pos_bol = 171; pos_cnum = 176};
      loc_ghost = false}}];
 sig_type =
  [Types.Sig_module (<abstr>, Types.Mp_present,
    {Types.md_type =
      Types.Mty_signature
       [Types.Sig_type (<abstr>,
         {Types.type_params = []; type_arity = 0;
          type_kind = Types.Type_abstract; type_private = Asttypes.Public;
          type_manifest = None; type_variance = []; type_is_newtype = false;
          type_expansion_scope = 0;
          type_loc =
           {Location.loc_start =
             {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
              pos_cnum = 22};
            loc_end =
             {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 18;
              pos_cnum = 28};
            loc_ghost = false};
          type_attributes = []; type_immediate = false;
          type_unboxed = {Types.unboxed = false; default = false}},
         Types.Trec_first, Types.Exported);
        Types.Sig_value (<abstr>,
         {Types.val_type =
           {Types.desc =
             Types.Tarrow (Asttypes.Nolabel,
              {Types.desc =
                Types.Tconstr (Path.Pident <abstr>, [],
                 {contents = Types.Mnil});
               level = 100000000; scope = 0; id = 372058},
              {Types.desc =
                Types.Tconstr (Path.Pident <abstr>, [],
                 {contents = Types.Mnil});
               level = 100000000; scope = 0; id = 372061},
              Types.Cok);
            level = 100000000; scope = 0; id = 372064};
          val_kind = Types.Val_reg;
          val_loc =
           {Location.loc_start =
             {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
              pos_cnum = 33};
            loc_end =
             {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
              pos_cnum = 48};
            loc_ghost = false};
          val_attributes = []},
         Types.Exported)];
     md_attributes = [];
     md_loc =
      {Location.loc_start =
        {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1; pos_cnum = 3};
       loc_end =
        {Lexing.pos_fname = ""; pos_lnum = 5; pos_bol = 51; pos_cnum = 56};
       loc_ghost = false}},
    Types.Trec_not, Types.Exported);
   Types.Sig_module (<abstr>, Types.Mp_present,
    {Types.md_type =
      Types.Mty_signature
       [Types.Sig_value (<abstr>,
         {Types.val_type =
           {Types.desc =
             Types.Tarrow (Asttypes.Nolabel,
              {Types.desc =
                Types.Tlink
                 {Types.desc =
                   Types.Tconstr (Path.Pdot (Path.Pident <abstr>, "t"),
                    [], {contents = Types.Mnil});
                  level = 100000000; scope = 0; id = 372089};
               level = 100000000; scope = 0; id = 372090},
              {Types.desc =
                Types.Tlink
                 {Types.desc =
                   Types.Tconstr (Path.Pdot (Path.Pident <abstr>, "t"),
                    [], {contents = Types.Mnil});
                  level = 100000000; scope = 0; id = 372089};
               level = 100000000; scope = 0; id = 372085},
              Types.Cok);
            level = 100000000; scope = 0; id = 372084};
          val_kind = Types.Val_reg;
          val_loc =
           {Location.loc_start =
             {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
              pos_cnum = 33};
            loc_end =
             {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 29;
              pos_cnum = 48};
            loc_ghost = false};
          val_attributes = []},
         Types.Exported);
        Types.Sig_type (<abstr>,
         {Types.type_params = []; type_arity = 0;
          type_kind = Types.Type_abstract; type_private = Asttypes.Public;
          type_manifest = None; type_variance = []; type_is_newtype = false;
          type_expansion_scope = 0;
          type_loc =
           {Location.loc_start =
             {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
              pos_cnum = 137};
            loc_end =
             {Lexing.pos_fname = ""; pos_lnum = 9; pos_bol = 133;
              pos_cnum = 143};
            loc_ghost = false};
          type_attributes = []; type_immediate = false;
          type_unboxed = {Types.unboxed = false; default = false}},
         Types.Trec_first, Types.Exported);
        Types.Sig_value (<abstr>,
         {Types.val_type =
           {Types.desc =
             Types.Tarrow (Asttypes.Nolabel,
              {Types.desc =
                Types.Tconstr (Path.Pident <abstr>, [],
                 {contents = Types.Mnil});
               level = 100000000; scope = 0; id = 372083},
              {Types.desc =
                Types.Tconstr (Path.Pident <abstr>, [],
                 {contents = Types.Mnil});
               level = 100000000; scope = 0; id = 372082},
              Types.Cok);
            level = 100000000; scope = 0; id = 372081};
          val_kind = Types.Val_reg;
          val_loc =
           {Location.loc_start =
             {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
              pos_cnum = 148};
            loc_end =
             {Lexing.pos_fname = ""; pos_lnum = 10; pos_bol = 144;
              pos_cnum = 170};
            loc_ghost = false};
          val_attributes = []},
         Types.Exported)];
     md_attributes = [];
     md_loc =
      {Location.loc_start =
        {Lexing.pos_fname = ""; pos_lnum = 7; pos_bol = 60; pos_cnum = 62};
       loc_end =
        {Lexing.pos_fname = ""; pos_lnum = 11; pos_bol = 171; pos_cnum = 176};
       loc_ghost = false}},
    Types.Trec_not, Types.Exported)];
 sig_final_env = <abstr>}
```
