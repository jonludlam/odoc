open Model.Names
type sexp = Sexplib.Sexp.t =
  | Atom of string
  | List of sexp list



module Root_to_sexp =
struct
  module Root = Model.Root

  let odoc_file : Root.Odoc_file.t -> sexp = function
    | Page p ->
      List [Atom "page"; Atom p]
    | Compilation_unit {name; hidden} ->
      let hidden = if hidden then [Atom "hidden"] else [] in
      List ((Atom "compilation_unit")::(Atom name)::hidden)

  let root : Root.t -> sexp = fun {package; file; digest} ->
    List [Atom package; odoc_file file; Atom (Digest.to_hex digest)]
end



module Identifier_to_sexp =
struct
  module Identifier = Model.Paths.Identifier

  let identifier : Identifier.t -> sexp =
    let rec traverse : sexp list -> Identifier.t -> sexp =
        fun acc -> function
      | `Root (root, s) ->
        List ((List [Atom "root"; Root_to_sexp.root root; Atom (UnitName.to_string s)])::acc)
      | `Page (root, s) ->
        List ((List [Atom "root"; Root_to_sexp.root root; Atom (PageName.to_string s)])::acc)
      | `Module (parent, s) ->
        traverse ((List [Atom "module"; Atom (ModuleName.to_string s)])::acc) (parent :> Identifier.t)
      | `Argument (parent, i, s) ->
        traverse
          ((List [Atom "argument"; Atom (string_of_int i); Atom (ArgumentName.to_string s)])::acc) (parent :> Identifier.t)
      | `ModuleType (parent, s) ->
        traverse ((List [Atom "module_type"; Atom (ModuleTypeName.to_string s)])::acc) (parent :> Identifier.t)
      | `Type (parent, s) ->
        traverse ((List [Atom "type"; Atom (TypeName.to_string s)])::acc) (parent :> Identifier.t)
      | `CoreType s ->
        List ((List [Atom "core_type"; Atom (TypeName.to_string s)])::acc)
      | `Constructor (parent, s) ->
        traverse ((List [Atom "constructor"; Atom (ConstructorName.to_string s)])::acc) (parent :> Identifier.t)
      | `Field (parent, s) ->
        traverse ((List [Atom "field"; Atom (FieldName.to_string s)])::acc) (parent :> Identifier.t)
      | `Extension (parent, s) ->
        traverse ((List [Atom "extension"; Atom (ExtensionName.to_string s)])::acc) (parent :> Identifier.t)
      | `Exception (parent, s) ->
        traverse ((List [Atom "exception"; Atom (ExceptionName.to_string s)])::acc) (parent :> Identifier.t)
      | `CoreException s ->
        List ((List [Atom "core_exception"; Atom (ExceptionName.to_string s)])::acc)
      | `Value (parent, s) ->
        traverse ((List [Atom "value"; Atom (ValueName.to_string s)])::acc) (parent :> Identifier.t)
      | `Class (parent, s) ->
        traverse ((List [Atom "class"; Atom (ClassName.to_string s)])::acc) (parent :> Identifier.t)
      | `ClassType (parent, s) ->
        traverse ((List [Atom "class_type"; Atom (ClassTypeName.to_string s)])::acc) (parent :> Identifier.t)
      | `Method (parent, s) ->
        traverse ((List [Atom "method"; Atom (MethodName.to_string s)])::acc) (parent :> Identifier.t)
      | `InstanceVariable (parent, s) ->
        traverse ((List [Atom "instance_variable"; Atom (InstanceVariableName.to_string s)])::acc) (parent :> Identifier.t)
      | `Label (parent, s) ->
        traverse ((List [Atom "label"; Atom (LabelName.to_string s)])::acc) (parent :> Identifier.t)
    in
    fun path ->
      traverse [] path
end



module Path_to_sexp =
struct
  module Path = Model.Paths.Path
  module Resolved = Model.Paths.Path.Resolved

  let rec path : Path.t -> sexp = function
    | `Resolved parent ->
      List [Atom "resolved"; resolved parent]
    | `Root s ->
      List [Atom "root"; Atom s]
    | `Forward s ->
      List [Atom "forward"; Atom s]
    | `Dot (parent, s) ->
      List [Atom "dot"; Atom s; path (parent :> Path.t)]
    | `Apply (m, m') ->
      List [Atom "apply"; path (m :> Path.t); path (m' :> Path.t)]

  and resolved : Resolved.t -> sexp = function
    |`Identifier i ->
      List [Atom "identifier"; Identifier_to_sexp.identifier i]
    |`Subst (mt, m) ->
      List [Atom "subst"; resolved (mt :> Resolved.t); resolved (m :> Resolved.t)]
    |`SubstAlias (m, m') ->
      List [Atom "subst_alias"; resolved (m :> Resolved.t); resolved (m' :> Resolved.t)]
    |`Hidden m ->
      List [Atom "hidden"; resolved (m :> Resolved.t)]
    |`Module (m, s) ->
      List [Atom "module"; Atom (ModuleName.to_string s); resolved (m :> Resolved.t)]
    |`Canonical (m, p) ->
      List [Atom "canonical"; resolved (m :> Resolved.t); path (p :> Path.t)]
    |`Apply (m, p) ->
      List [Atom "apply"; resolved (m :> Resolved.t); path (p :> Path.t)]
    |`ModuleType (m, s) ->
      List [Atom "module_type"; Atom (ModuleTypeName.to_string s); resolved (m :> Resolved.t)]
    |`Type (m, s) ->
      List [Atom "type"; Atom (TypeName.to_string s); resolved (m :> Resolved.t)]
    |`Class (m, s) ->
      List [Atom "class"; Atom (ClassName.to_string s); resolved (m :> Resolved.t)]
    |`ClassType (m, s) ->
      List [Atom "class_type"; Atom (ClassTypeName.to_string s); resolved (m :> Resolved.t)]
end


module Fragment_to_sexp =
struct
  module Fragment = Model.Paths.Fragment
  module Resolved = Model.Paths.Fragment.Resolved

  let rec fragment : Fragment.t -> sexp = function
    | `Resolved f -> List [ Atom "resolved"; resolved f ]
    | `Dot (s, str) -> List [ Atom "dot"; fragment (s :> Fragment.t); Atom str]

  and resolved : Resolved.t -> sexp = function
    | `Root -> Atom "root";
    | `Subst (mty, m) -> List
      [ Atom "subst";
        Path_to_sexp.resolved (mty :> Model.Paths.Path.Resolved.t);
        resolved (m :> Resolved.t) ]
    | `SubstAlias (rm, m) -> List
      [ Atom "subst_alias";
        Path_to_sexp.resolved (rm :> Model.Paths.Path.Resolved.t);
        resolved (m :> Resolved.t)
      ]
    | `Module (s,name) -> List
      [ Atom "module";
        resolved (s :> Resolved.t);
        Atom (ModuleName.to_string name) ]
    | `Type (s,name) -> List
      [ Atom "type";
        resolved (s :> Resolved.t);
        Atom (TypeName.to_string name) ]
    | `Class (s,name) -> List
      [ Atom "class";
        resolved (s :> Resolved.t);
        Atom (ClassName.to_string name) ]
    | `ClassType (s, name) -> List
      [ Atom "class_type";
        resolved (s :> Resolved.t);
        Atom (ClassTypeName.to_string name) ]
end


module Reference_to_sexp =
struct
  module Reference = Model.Paths.Reference
  module Resolved = Model.Paths.Reference.Resolved

  let tag : Model.Paths_types.Reference.tag_any -> sexp = function
    | `TUnknown -> Atom "unknown"
    | `TModule -> Atom "module"
    | `TModuleType -> Atom "module_type"
    | `TType -> Atom "type"
    | `TConstructor -> Atom "constructor"
    | `TField -> Atom "field"
    | `TExtension -> Atom "extension"
    | `TException -> Atom "exception"
    | `TValue -> Atom "value"
    | `TClass -> Atom "class"
    | `TClassType -> Atom "class_type"
    | `TMethod -> Atom "method"
    | `TInstanceVariable -> Atom "instance_variable"
    | `TLabel -> Atom "label"
    | `TPage -> Atom "page"

  let rec reference : Reference.t -> sexp = function
    | `Resolved parent ->
      List [Atom "resolved"; resolved (parent :> Reference.Resolved.t)]
    | `Root (s, k) ->
      List [Atom "root"; Atom (UnitName.to_string s); tag k]
    | `Dot (parent, s) ->
      List [Atom "dot"; Atom (s); reference (parent :> Reference.t)]
    | `Module (parent, s) ->
      List [Atom "module"; Atom (ModuleName.to_string s); reference (parent :> Reference.t)]
    | `ModuleType (parent, s) ->
      List [Atom "module_type"; Atom (ModuleTypeName.to_string s); reference (parent :> Reference.t)]
    | `Type (parent, s) ->
      List [Atom "type"; Atom (TypeName.to_string s); reference (parent :> Reference.t)]
    | `Constructor (parent, s) ->
      List [Atom "constructor"; Atom (ConstructorName.to_string s); reference (parent :> Reference.t)]
    | `Field (parent, s) ->
      List [Atom "field"; Atom (FieldName.to_string s); reference (parent :> Reference.t)]
    | `Extension (parent, s) ->
      List [Atom "extension"; Atom (ExtensionName.to_string s); reference (parent :> Reference.t)]
    | `Exception (parent, s) ->
      List [Atom "exception"; Atom (ExceptionName.to_string s); reference (parent :> Reference.t)]
    | `Value (parent, s) ->
      List [Atom "value"; Atom (ValueName.to_string s); reference (parent :> Reference.t)]
    | `Class (parent, s) ->
      List [Atom "class"; Atom (ClassName.to_string s); reference (parent :> Reference.t)]
    | `ClassType (parent, s) ->
      List [Atom "class_type"; Atom (ClassTypeName.to_string s); reference (parent :> Reference.t)]
    | `Method (parent, s) ->
      List [Atom "method"; Atom (MethodName.to_string s); reference (parent :> Reference.t)]
    | `InstanceVariable (parent, s) ->
      List [Atom "instance_variable"; Atom (InstanceVariableName.to_string s); reference (parent :> Reference.t)]
    | `Label (parent, s) ->
      List [Atom "label"; Atom (LabelName.to_string s); reference (parent :> Reference.t)]

  and resolved : Resolved.t -> sexp = function
    | `Identifier parent ->
      List [Atom "identifier"; Identifier_to_sexp.identifier parent]
    | `SubstAlias (m, m') ->
      List [Atom "subst_alias"; Path_to_sexp.resolved (m :> Model.Paths.Path.Resolved.t); resolved (m' :> Resolved.t)]
    | `Module (parent, s) ->
      List [Atom "module"; Atom (ModuleName.to_string s); resolved (parent :> Resolved.t)]
    | `Canonical (m, m') ->
      List [Atom "canonical"; resolved (m :> Resolved.t); reference (m' :> Reference.t)]
    | `ModuleType (parent, s) ->
      List [Atom "module_type"; Atom (ModuleTypeName.to_string s); resolved (parent :> Resolved.t)]
    | `Type (parent, s) ->
      List [Atom "type"; Atom (TypeName.to_string s); resolved (parent :> Resolved.t)]
    | `Constructor (parent, s) ->
      List [Atom "constructor"; Atom (ConstructorName.to_string s); resolved (parent :> Resolved.t)]
    | `Field (parent, s) ->
      List [Atom "field"; Atom (FieldName.to_string s); resolved (parent :> Resolved.t)]
    | `Extension (parent, s) ->
      List [Atom "extension"; Atom (ExtensionName.to_string s); resolved (parent :> Resolved.t)]
    | `Exception (parent, s) ->
      List [Atom "exception"; Atom (ExceptionName.to_string s); resolved (parent :> Resolved.t)]
    | `Value (parent, s) ->
      List [Atom "value"; Atom (ValueName.to_string s); resolved (parent :> Resolved.t)]
    | `Class (parent, s) ->
      List [Atom "class"; Atom (ClassName.to_string s); resolved (parent :> Resolved.t)]
    | `ClassType (parent, s) ->
      List [Atom "class_type"; Atom (ClassTypeName.to_string s); resolved (parent :> Resolved.t)]
    | `Method (parent, s) ->
      List [Atom "method"; Atom (MethodName.to_string s); resolved (parent :> Resolved.t)]
    | `InstanceVariable (parent, s) ->
      List [Atom "instance_variable"; Atom (InstanceVariableName.to_string s); resolved (parent :> Resolved.t)]
    | `Label (parent, s) ->
      List [Atom "label"; Atom (LabelName.to_string s); resolved (parent :> Resolved.t)]
end



module Location_to_sexp =
struct
  module Location_ = Model.Location_

  let point : Location_.point -> sexp = fun {line; column} ->
    List [Atom (string_of_int line); Atom (string_of_int column)]

  let span : Location_.span -> sexp = fun {file; start; end_} ->
    List [Atom file; point start; point end_]

  let at : ('a -> sexp) -> 'a Location_.with_location -> sexp =
      fun f {location; value} ->
    List [span location; f value]
end



module Comment_to_sexp =
struct
  module Comment = Model.Comment
  let at = Location_to_sexp.at

  let style : Comment.style -> sexp = function
    | `Bold -> Atom "bold"
    | `Italic -> Atom "italic"
    | `Emphasis -> Atom "emphasis"
    | `Superscript -> Atom "superscript"
    | `Subscript -> Atom "subscript"

  let leaf_inline_element : Comment.leaf_inline_element -> sexp =
    function
    | `Space -> Atom "space"
    | `Word w -> List [Atom "word"; Atom w]
    | `Code_span c -> List [Atom "code_span"; Atom c]
    | `Raw_markup (`Html, s) -> List [Atom "raw_markup"; Atom "html"; Atom s]

  let rec non_link_inline_element : Comment.non_link_inline_element -> sexp =
    function
    | #Comment.leaf_inline_element as e ->
      leaf_inline_element e
    | `Styled (s, es) ->
      List [style s; List (List.map (at non_link_inline_element) es)]

  let rec inline_element : Comment.inline_element -> sexp = function
    | #Comment.leaf_inline_element as e ->
      leaf_inline_element e
    | `Styled (s, es) ->
      List [style s; List (List.map (at inline_element) es)]
    | `Reference (r, es) ->
      List [
        Atom "reference";
        Reference_to_sexp.reference r;
        List (List.map (at non_link_inline_element) es)
      ]
    | `Link (u, es) ->
      List [
        Atom "link";
        Atom u;
        List (List.map (at non_link_inline_element) es)
      ]

  let rec nestable_block_element
      : Comment.nestable_block_element -> sexp =
    function
    | `Paragraph es ->
      List [Atom "paragraph"; List (List.map (at inline_element) es)]
    | `Code_block c -> List [Atom "code_block"; Atom c]
    | `Verbatim t -> List [Atom "verbatim"; Atom t]
    | `Modules ps ->
      List [Atom "modules"; List (List.map Reference_to_sexp.reference (ps :> Model.Paths.Reference.t list))]
    | `List (kind, items) ->
      let kind =
        match kind with
        | `Unordered -> "unordered"
        | `Ordered -> "ordered"
      in
      let items =
        items
        |> List.map (fun item ->
          List (List.map (at nestable_block_element) item))
        |> fun items -> List items
      in
      List [Atom kind; items]

  let tag : Comment.tag -> sexp = function
    | `Author s ->
      List [Atom "@author"; Atom s]
    | `Deprecated es ->
      List ((Atom "@deprecated")::(List.map (at nestable_block_element) es))
    | `Param (s, es) ->
      List ([Atom "@param"; Atom s] @ (List.map (at nestable_block_element) es))
    | `Raise (s, es) ->
      List ([Atom "@raise"; Atom s] @ (List.map (at nestable_block_element) es))
    | `Return es ->
      List ((Atom "@return")::(List.map (at nestable_block_element) es))
    | `See (kind, s, es) ->
      let kind =
        match kind with
        | `Url -> "url"
        | `File -> "file"
        | `Document -> "document"
      in
      List
        ([Atom "@see"; Atom kind; Atom s] @
          (List.map (at nestable_block_element) es))
    | `Since s -> List [Atom "@since"; Atom s]
    | `Before (s, es) ->
      List ([Atom "@before"; Atom s] @
        (List.map (at nestable_block_element) es))
    | `Version s -> List [Atom "@version"; Atom s]
    | `Canonical (p, r) ->
      List
        [Atom "@canonical"; Path_to_sexp.path (p :> Model.Paths.Path.t); Reference_to_sexp.reference (r :> Model.Paths.Reference.t)]
    | `Inline ->
      Atom "@inline"
    | `Open ->
      Atom "@open"
    | `Closed ->
      Atom "@closed"

  let block_element : Comment.block_element -> sexp = function
    | #Comment.nestable_block_element as e -> nestable_block_element e
    | `Heading (level, label, es) ->
      let label = List [Atom "label"; Identifier_to_sexp.identifier (label :> Model.Paths.Identifier.t)] in
      let level =
        match level with
        | `Title -> "0"
        | `Section -> "1"
        | `Subsection -> "2"
        | `Subsubsection -> "3"
        | `Paragraph -> "4"
        | `Subparagraph -> "5"
      in
      List [Atom level; label; List (List.map (at non_link_inline_element) es)]
    | `Tag t -> tag t

  let comment : Comment.docs -> sexp = fun comment ->
    List (List.map (at block_element) comment)

  let docs_or_stop : Comment.docs_or_stop -> sexp = function
    | `Docs docs -> List [Atom "Docs"; comment docs]
    | `Stop -> Atom "Stop"
end



module Error_to_sexp =
struct
  let error : Model.Error.t -> sexp = fun error ->
    Atom (Model.Error.to_string error)
end

module Lang =
struct

  let list conv x = List (List.map conv x)
  let opt conv x = match x with | Some x -> List [conv x] | None -> List []
  let pair conv1 conv2 (x1,x2) = List [conv1 x1; conv2 x2]

  open Model.Lang

  let rec sexp_of_module_expansion =
    let open Module in function
    | AlreadyASig -> Atom "AlreadyASig"
    | Signature s -> List [Atom "Signature"; sexp_of_signature_t s]
    | Functor (args,s) -> List [Atom "Functor"; list (opt sexp_of_functorargument_t) args; sexp_of_signature_t s]

  and sexp_of_module_decl =
    let open Module in function
    | Alias m -> List [Atom "Alias"; Path_to_sexp.path (m :> Model.Paths.Path.t)]
    | ModuleType m -> List [Atom "ModuleType"; sexp_of_module_type_expr m]

  and sexp_of_module_t m =
    let open Module in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (m.id :> Model.Paths.Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment m.doc];
      List [Atom "type_"; sexp_of_module_decl m.type_];
      List [Atom "canonical"; opt (fun (x,y) ->
        List [
          Path_to_sexp.path (x : Model.Paths_types.Path.module_ :> Model.Paths.Path.t);
          Reference_to_sexp.reference (y : Model.Paths.Reference.Module.t :> Model.Paths.Reference.t)])
        m.canonical ];
      List [Atom "hidden"; Sexplib.Std.sexp_of_bool m.hidden];
      List [Atom "display_type"; opt sexp_of_module_decl m.display_type];
      List [Atom "expansion"; opt sexp_of_module_expansion m.expansion]
    ]

  and sexp_of_module_equation_t e = sexp_of_module_decl e

  and sexp_of_functorargument_t s =
    let open FunctorArgument in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (s.id : Identifier.Module.t :> Identifier.t)];
      List [Atom "expr"; sexp_of_module_type_expr s.expr];
      List [Atom "expansion"; opt sexp_of_module_expansion s.expansion ]
      ]
  
  and sexp_of_module_type_substitution =
    let open ModuleType in
    let open Model.Paths in
    function
    | ModuleEq (frag, eqn) -> List [Atom "ModuleEq"; Fragment_to_sexp.fragment (frag :> Fragment.t); sexp_of_module_equation_t eqn]
    | TypeEq (frag, eqn) -> List [Atom "TypeEq"; Fragment_to_sexp.fragment (frag :> Fragment.t); sexp_of_type_decl_equation_t eqn]
    | ModuleSubst (frag, path) -> List [Atom "ModuleSubst"; Fragment_to_sexp.fragment (frag :> Fragment.t); Path_to_sexp.path (path :> Path.t)]
    | TypeSubst (frag, eqn) -> List [Atom "TypeSubst"; Fragment_to_sexp.fragment (frag :> Fragment.t); sexp_of_type_decl_equation_t eqn]
  
  and sexp_of_module_type_expr =
    let open ModuleType in
    let open Model.Paths in
    function
    | Path p -> List [Atom "Path"; Path_to_sexp.path (p :> Path.t)]
    | Signature s -> List [Atom "Signature"; sexp_of_signature_t s]
    | Functor (f, expr) -> List [Atom "Functor"; opt sexp_of_functorargument_t f; sexp_of_module_type_expr expr]
    | With (e, sub_list) -> List [Atom "With"; sexp_of_module_type_expr e; list sexp_of_module_type_substitution sub_list]
    | TypeOf d -> List [Atom "TypeOf"; sexp_of_module_decl d]

  and sexp_of_module_type_t t =
    let open ModuleType in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (t.id :> Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment t.doc];
      List [Atom "expr"; opt sexp_of_module_type_expr t.expr];
      List [Atom "expansion"; opt sexp_of_module_expansion t.expansion];
    ]

  and sexp_of_signature_recursive =
    let open Signature in
    function
    | Ordinary -> Atom "Ordinary"
    | And -> Atom "And"
    | Nonrec -> Atom "Nonrec"
    | Rec -> Atom "Rec"
  
  and sexp_of_signature_item =
    let open Signature in
    function
    | Module (r, m) -> List [Atom "Module"; sexp_of_signature_recursive r; sexp_of_module_t m]
    | ModuleType t -> List [Atom "ModuleType"; sexp_of_module_type_t t]
    | Type (r, t) -> List [Atom "Type"; sexp_of_signature_recursive r; sexp_of_type_decl_t t]
    | TypExt e -> List [Atom "TypExt"; sexp_of_extension_t e]
    | Exception e -> List [Atom "Exception"; sexp_of_exception_t e]
    | Value v -> List [Atom "Value"; sexp_of_value_t v]
    | External e -> List [Atom "External"; sexp_of_external_t e]
    | Class (r, c) -> List [Atom "Class"; sexp_of_signature_recursive r; sexp_of_class_t c]
    | ClassType (r, c) -> List [Atom "ClassType"; sexp_of_signature_recursive r; sexp_of_class_type_t c]
    | Include i -> List [Atom "Include"; sexp_of_include_t i]
    | Comment c -> List [Atom "Comment"; Comment_to_sexp.docs_or_stop c]
  
  and sexp_of_signature_t t =
    list sexp_of_signature_item t

  and sexp_of_include_expansion e =
    let open Include in
    List [
      List [Atom "resolved"; Sexplib.Std.sexp_of_bool e.resolved];
      List [Atom "content"; sexp_of_signature_t e.content]
    ]
  
  and sexp_of_include_t i =
    let open Include in
    let open Model.Paths in
    List [
      List [Atom "parent"; Identifier_to_sexp.identifier (i.parent :> Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment i.doc];
      List [Atom "decl"; sexp_of_module_decl i.decl];
      List [Atom "expansion"; sexp_of_include_expansion i.expansion];
    ]

  and sexp_of_type_decl_field_t f =
    let open TypeDecl.Field in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (f.id :> Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment f.doc];
      List [Atom "mutable_"; Sexplib.Std.sexp_of_bool f.mutable_];
      List [Atom "type_"; sexp_of_type_expr_t f.type_]
    ]

  and sexp_of_type_decl_constructor_argument =
    let open TypeDecl.Constructor in
    function
    | Tuple ts -> list sexp_of_type_expr_t ts
    | Record fs -> list sexp_of_type_decl_field_t fs
  
  and sexp_of_type_decl_constructor_t c =
    let open TypeDecl.Constructor in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (c.id :> Identifier.t) ];
      List [Atom "doc"; Comment_to_sexp.comment c.doc];
      List [Atom "args"; sexp_of_type_decl_constructor_argument c.args ];
      List [Atom "res"; opt sexp_of_type_expr_t c.res];
    ]

  and sexp_of_type_decl_representation_t =
    let open TypeDecl.Representation in
    function
    | Variant cs -> list sexp_of_type_decl_constructor_t cs
    | Record fs -> list sexp_of_type_decl_field_t fs
    | Extensible -> Atom "Extensible"
  
  and sexp_of_type_decl_variance =
    let open TypeDecl in
    function
    | Pos -> Atom "Pos"
    | Neg -> Atom "Neg"
  
  and sexp_of_type_decl_param_desc =
    let open TypeDecl in
    function
    | Any -> Atom "Any"
    | Var x -> List [Atom "Var"; Atom x]
  
  and sexp_of_type_decl_param (desc,var) =
    List [sexp_of_type_decl_param_desc desc; opt sexp_of_type_decl_variance var]
  
  and sexp_of_type_decl_equation_t e =
    let open TypeDecl.Equation in
    List [
      List [Atom "params"; list sexp_of_type_decl_param e.params];
      List [Atom "private_"; Sexplib.Std.sexp_of_bool e.private_];
      List [Atom "manifest"; opt sexp_of_type_expr_t e.manifest];
      List [Atom "constraints"; list (pair sexp_of_type_expr_t sexp_of_type_expr_t) e.constraints]
    ]
  
  and sexp_of_type_decl_t d =
    let open TypeDecl in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (d.id :> Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment d.doc];
      List [Atom "equation"; sexp_of_type_decl_equation_t d.equation];
      List [Atom "representation"; opt sexp_of_type_decl_representation_t d.representation]
    ]

  and sexp_of_extension_constructor_t c =
    let open Extension.Constructor in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (c.id :> Identifier.t) ];
      List [Atom "doc"; Comment_to_sexp.comment c.doc];
      List [Atom "args"; sexp_of_type_decl_constructor_argument c.args ];
      List [Atom "res"; opt sexp_of_type_expr_t c.res];
    ]

  and sexp_of_extension_t e =
    let open Extension in
    let open Model.Paths in
    List [
      List [Atom "type_path"; Path_to_sexp.path (e.type_path :> Path.t)];
      List [Atom "doc"; Comment_to_sexp.comment e.doc];
      List [Atom "type_params"; list sexp_of_type_decl_param e.type_params];
      List [Atom "private_"; Sexplib.Std.sexp_of_bool e.private_];
      List [Atom "constructors"; list sexp_of_extension_constructor_t e.constructors]
    ]

  and sexp_of_exception_t e =
    let open Exception in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (e.id :> Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment e.doc];
      List [Atom "args"; sexp_of_type_decl_constructor_argument e.args];
      List [Atom "res"; opt sexp_of_type_expr_t e.res]
    ]

  and sexp_of_value_t v =
    let open Value in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (v.id :> Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment v.doc];
      List [Atom "type_"; sexp_of_type_expr_t v.type_];
    ]
  
  and sexp_of_external_t ext =
    let open External in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (ext.id :> Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment ext.doc];
      List [Atom "type_"; sexp_of_type_expr_t ext.type_];
      List [Atom "primitives"; list Sexplib.Std.sexp_of_string ext.primitives]
    ]

  and sexp_of_class_decl =
    let open Class in
    function
    | ClassType e -> List [Atom "ClassType"; sexp_of_class_type_expr e];
    | Arrow (l_opt, e, d) -> List [Atom "Arrow"; opt sexp_of_type_expr_label l_opt; sexp_of_type_expr_t e; sexp_of_class_decl d]

  and sexp_of_class_t c =
    let open Class in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (c.id :> Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment c.doc];
      List [Atom "virtual_"; Sexplib.Std.sexp_of_bool c.virtual_];
      List [Atom "params"; list sexp_of_type_decl_param c.params];
      List [Atom "type_"; sexp_of_class_decl c.type_];
      List [Atom "expansion"; opt sexp_of_class_signature_t c.expansion]
    ]

  and sexp_of_class_type_expr =
    let open ClassType in
    let open Model.Paths in
    function
    | Constr (p, es) -> List [Atom "Constr"; Path_to_sexp.path (p :> Path.t); list sexp_of_type_expr_t es]
    | Signature s -> List [Atom "Signature"; sexp_of_class_signature_t s]

  and sexp_of_class_type_t c =
    let open ClassType in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (c.id :> Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment c.doc];
      List [Atom "virtual_"; Sexplib.Std.sexp_of_bool c.virtual_];
      List [Atom "params"; list sexp_of_type_decl_param c.params];
      List [Atom "expr"; sexp_of_class_type_expr c.expr];
      List [Atom "expansion"; opt sexp_of_class_signature_t c.expansion]
    ]

  and sexp_of_class_signature_item =
    let open ClassSignature in
    function
    | Method m -> List [Atom "Method"; sexp_of_method_t m]
    | InstanceVariable i -> List [Atom "InstanceVariable"; sexp_of_instance_variable_t i]
    | Constraint (t1,t2) -> List [Atom "Constraint"; sexp_of_type_expr_t t1; sexp_of_type_expr_t t2]
    | Inherit e -> List [Atom "Inherit"; sexp_of_class_type_expr e]
    | Comment c -> List [Atom "Comment"; Comment_to_sexp.docs_or_stop c]

  and sexp_of_class_signature_t c =
    let open ClassSignature in
    List [
      List [Atom "self"; opt sexp_of_type_expr_t c.self];
      List [Atom "items"; list sexp_of_class_signature_item c.items];
    ]

  and sexp_of_method_t m =
    let open Method in
    let open Model.Paths in
     List [
      List [Atom "id"; Identifier_to_sexp.identifier (m.id :> Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment m.doc];
      List [Atom "private_"; Sexplib.Std.sexp_of_bool m.private_];
      List [Atom "virtual_"; Sexplib.Std.sexp_of_bool m.virtual_];
      List [Atom "type_"; sexp_of_type_expr_t m.type_]
     ]

  and sexp_of_instance_variable_t i =
    let open InstanceVariable in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (i.id :> Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment i.doc];
      List [Atom "mutable_"; Sexplib.Std.sexp_of_bool i.mutable_];
      List [Atom "virtual_"; Sexplib.Std.sexp_of_bool i.virtual_];
      List [Atom "type_"; sexp_of_type_expr_t i.type_]
    ]

  and sexp_of_type_expr_polyvar_kind =
    let open TypeExpr.Polymorphic_variant in
    function
    | Fixed -> Atom "Fixed"
    | Closed l -> List [Atom "Closed"; list Sexplib.Std.sexp_of_string l]
    | Open -> Atom "Open"

  and sexp_of_type_expr_polyvar_constructor_t c =
    let open TypeExpr.Polymorphic_variant.Constructor in
    List [
      List [Atom "name"; Sexplib.Std.sexp_of_string c.name];
      List [Atom "constant"; Sexplib.Std.sexp_of_bool c.constant];
      List [Atom "arguments"; list sexp_of_type_expr_t c.arguments];
      List [Atom "doc"; Comment_to_sexp.comment c.doc];
    ]

  and sexp_of_type_expr_polyvar_element =
    let open TypeExpr.Polymorphic_variant in
    function
    | Type t -> List [Atom "Type"; sexp_of_type_expr_t t];
    | Constructor c -> List [Atom "Constructor"; sexp_of_type_expr_polyvar_constructor_t c]
  
  and sexp_of_type_expr_polyvar_t t =
    let open TypeExpr.Polymorphic_variant in
    List [
      List [Atom "kind"; sexp_of_type_expr_polyvar_kind t.kind];
      List [Atom "elements"; list sexp_of_type_expr_polyvar_element t.elements]
    ]

  and sexp_of_type_expr_object_method m =
    let open TypeExpr.Object in
    List [
      List [Atom "name"; Sexplib.Std.sexp_of_string m.name];
      List [Atom "type_"; sexp_of_type_expr_t m.type_];
    ]
  
  and sexp_of_type_expr_object_field =
    let open TypeExpr.Object in
    function
    | Method m -> List [Atom "Method"; sexp_of_type_expr_object_method m]
    | Inherit i -> List [Atom "Inherit"; sexp_of_type_expr_t i]
  
  and sexp_of_type_expr_object_t o =
    let open TypeExpr.Object in
    List [
      List [Atom "fields"; list sexp_of_type_expr_object_field o.fields];
      List [Atom "open_"; Sexplib.Std.sexp_of_bool o.open_]
    ]

  and sexp_of_type_expr_package_substitution (frag,tyexpr) =
    List [
      Fragment_to_sexp.fragment (frag :> Model.Paths.Fragment.t);
      sexp_of_type_expr_t tyexpr
    ]
  
  and sexp_of_type_expr_package_t t =
    let open TypeExpr.Package in
    let open Model.Paths in
    List [
      List [Atom "path"; Path_to_sexp.path (t.path :> Path.t)];
      List [Atom "substitutions"; list sexp_of_type_expr_package_substitution t.substitutions];
    ] 

  and sexp_of_type_expr_label =
    let open TypeExpr in
    function
    | Label s -> List [Atom "Label"; Sexplib.Std.sexp_of_string s]
    | Optional s -> List [Atom "Optional"; Sexplib.Std.sexp_of_string s]
    
  and sexp_of_type_expr_t =
    let open TypeExpr in
    let open Model.Paths in
    function
    | Var s -> List [Atom "Var"; Sexplib.Std.sexp_of_string s]
    | Any -> Atom "Any"
    | Alias (t, s) -> List [Atom "Alias"; sexp_of_type_expr_t t; Sexplib.Std.sexp_of_string s]
    | Arrow (lbl_opt, t1, t2) -> List [Atom "Arrow"; opt sexp_of_type_expr_label lbl_opt; sexp_of_type_expr_t t1; sexp_of_type_expr_t t2]
    | Tuple ts -> List [Atom "Tuple"; list sexp_of_type_expr_t ts]
    | Constr (p, ts) -> List [Atom "Constr"; Path_to_sexp.path (p :> Path.t); list sexp_of_type_expr_t ts]
    | Polymorphic_variant v -> List [Atom "Polymorphic_variant"; sexp_of_type_expr_polyvar_t v]
    | Object o -> List [Atom "Object"; sexp_of_type_expr_object_t o]
    | Class (p, ts) -> List [Atom "Class"; Path_to_sexp.path (p :> Path.t); list sexp_of_type_expr_t ts]
    | Poly (strs, t) -> List [Atom "Poly"; list Sexplib.Std.sexp_of_string strs; sexp_of_type_expr_t t]
    | Package p -> List [Atom "Package"; sexp_of_type_expr_package_t p]

  let sexp_of_compilation_unit_import_t =
    let open Compilation_unit.Import in
    function
    | Unresolved (str, digest_opt) -> List [Atom "Unresolved"; Sexplib.Std.sexp_of_string str; opt Sexplib.Std.sexp_of_string digest_opt]
    | Resolved root -> List [Atom "Resolved"; Root_to_sexp.root root]
  
  let sexp_of_compilation_unit_source_t t =
    let open Compilation_unit.Source in
    List [
      List [Atom "file"; Sexplib.Std.sexp_of_string t.file];
      List [Atom "build_dir"; Sexplib.Std.sexp_of_string t.build_dir];
      List [Atom "digest"; Sexplib.Std.sexp_of_string t.digest];
    ]
  
  let sexp_of_compilation_unit_packed_item i =
    let open Compilation_unit.Packed in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (i.id :> Identifier.t)];
      List [Atom "path"; Path_to_sexp.path (i.path :> Path.t)];
    ]

  let sexp_of_compilation_unit_packed_t ts =
    list sexp_of_compilation_unit_packed_item ts

  let sexp_of_compilation_unit_content =
    let open Compilation_unit in
    function
    | Module m -> List [Atom "Module"; sexp_of_signature_t m]
    | Pack p -> List [Atom "Packed"; sexp_of_compilation_unit_packed_t p]
  
  let sexp_of_compilation_unit_t c =
    let open Compilation_unit in
    let open Model.Paths in
    List [
      List [Atom "id"; Identifier_to_sexp.identifier (c.id :> Identifier.t)];
      List [Atom "doc"; Comment_to_sexp.comment c.doc];
      List [Atom "digest"; Sexplib.Std.sexp_of_string c.digest];
      List [Atom "imports"; list sexp_of_compilation_unit_import_t c.imports];
      List [Atom "source"; opt sexp_of_compilation_unit_source_t c.source];
      List [Atom "interface"; Sexplib.Std.sexp_of_bool c.interface];
      List [Atom "hidden"; Sexplib.Std.sexp_of_bool c.hidden];
      List [Atom "content"; sexp_of_compilation_unit_content c.content];
      List [Atom "expansion"; opt sexp_of_signature_t c.expansion];
    ]
end

let parser_output formatter {Model.Error.value; warnings} =
  let value = Comment_to_sexp.comment value in
  let warnings = List (List.map Error_to_sexp.error warnings) in
  let output =
    List [
      List [Atom "output"; value];
      List [Atom "warnings"; warnings];
    ]
  in
  Sexplib.Sexp.pp_hum formatter output;
  Format.pp_print_newline formatter ();
  Format.pp_print_flush formatter ()
