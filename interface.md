# Odoc interface guarantees

Odoc has several 'public facing' parts, with varying levels of support guarantees.
This document describes what those interfaces are and what the support levels are
now and what we aim for in the future.

## Documentation comments

The first and most important is the syntax of the documentation comments present in source code.
This is relevant to everyone who is writing code intended to be documented by odoc, and hence is applies to the widest set of people.

The canonical description of the markup that odoc understands is in [section](https://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#s%3Aocamldoc-comments)
of the OCaml reference manual. The eventual aim is to support the in-code markup
in its entirety, although right now there are some gaps. There are also some
extensions where odoc goes beyond what is officially supported.

### Changes 

The following describes the changes between what odoc understands and what is in the OCaml manual.

#### Deficiencies WRT ocamldoc documentation in the OCaml manual
- Comments describing class inheritance are not rendered (bug)
- Odoc does not currently handle ambiguously associated doc comments (bug)
- Odoc does not ignore tags where they don't make sense (e.g. `@param` tags on instance variables are rendered) (bug)
- [Alignment elements](https://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#ss:ocamldoc-formatting) are not handled (`{C text}`. `{L text}` and `{R text}`)
- Odoc does not recognise [html tags embedded in comments](https://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sss:ocamldoc-html-tags)

#### Improvements over ocamldoc documentation in the OCaml manual
- Odoc has a better mechanism for disambiguating references in comments. See 'reference syntax' later in this document.
- Support for standalone 'mld' files - these are documents using the OCamldoc markup, but rendered as distinct pages.
- A few extra tags are supported:
  + @returns is a synonym for @return
  + @raises is a synonym for @raise
  + @open and @closed and @inline are hints for how 'included' signatures should be rendered
  + @canonical allows a definition of a module to be marked as canonically elsewhere. 

### Reference syntax
Odoc has a far more powerful reference resolution mechanism than ocamldoc. While it supports the mechanism in ocamldoc used for disambiguating between different types of references,
it offers a more powerful alternative. The new mechanism allows for disambiguation of each part in a dotted reference rather than just the final part. For example, 
where in the reference manual it suggests the syntax `{!type:Foo.Bar.t}` to designate a type, and `{!val:Foo.Bar.t}` a value of the same name, the new Odoc syntax for these
comments would be `{!Foo.Bar.type-t}` and `{!Foo.Bar.val-t}`. This allows odoc to disambiguate when there are other ambiguous elements within the path. For example, we can
distinguish between a type or value t within a module or module type with the same name: `{!module-Foo.module-type-Bar.type-t}` or {!module-type-Foo.module-Bar.val-t}`.

Additionally we support extra annotations:
- `module-type` is a replacement for `modtype`
- `class-type` is a replacement for `classtype`
- `exn` is recognised as `exception`
- `extension` refers to a type extension
- `field` is a replacement for `recfield`
- `instance-variable` refers to instance variables
- `label` refers to labels introduced in anchors
- `page` refers to `mld` pages as outlined above
- `value` is recognised as `val`

This will be described more completely in a separate document.

## CLI interface

The way in which the odoc CLI is invoked is not trivial, and requires careful
ordering and correct arguments to produce correctly linked documentation. It is not expected that
end-users will invoke odoc by hand, but rather it will be driven by a separate tool. As a consequence of
this it is important that we preserve the ability of these tools to create good documentation with
each release of odoc, and thus we will ensure backward compatibility of the CLI as much as possible.
There are currently 3 tools that 'drive' odoc that are considered 'first class' in that we will not make
releases of odoc whilst knowingly breaking these tools. These are:

- odig
- dune
- ocaml

Ocaml here refers to the newly merged configure option to build the standard library documentation with
odoc. If the recommended way of invoking odoc changes we will work with the maintainers of these projects
to ensure they are updated correspondingly.

Additionally there will be a reference implementation of a tool to build Odoc's documentation which should
serve as a guide for anyone building other 'drivers' of odoc.

## Output formats

Odoc currently outputs HTML files, man pages and latex documents. In a similar vein to the CLI interface,
we will try to ensure that the three tools described above will will not be broken by any changes to the
outputs - that is, that they will succeed and produce documentation that is 'correct'. We do not make any
guarantees about the internal structure of the output documents - for example, the exact nesting of
tags or sequence of latex commands may not be preserved. We will attempt to ensure that the anchors in
the HTML are preserved though, implying also that the filenames will also be preserved.

## Libraries

Odoc has several internal libraries. The only one of these for which we currently expect external users is
the comment parser. This will soon be removed into an external package to link with separately and will
have its own lifecycle and support statement.



