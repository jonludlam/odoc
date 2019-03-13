How odoc resolves references and paths
======================================

Walkthroughs

Let's resolve this, in a file called `a.ml`

```ocaml
module M : sig module type S = sig type t end module T : S end

type v = M.T.t
```

We're interested here in how `type v` gets resolved. 

When this is compiled, we get a cmti containing a typedtree, which can be seen by dumping the typedtree during
compilation via the `-dtypedtree` flag:

```
[
  signature_item (xref/lib/a.mli[6,122+0]..xref/lib/a.mli[6,122+62])
    Tsig_module "M/1002"
    module_type (xref/lib/a.mli[6,122+11]..xref/lib/a.mli[6,122+62])
      Tmty_signature
      [
        signature_item (xref/lib/a.mli[6,122+15]..xref/lib/a.mli[6,122+45])
          Tsig_modtype "S/1004"
            module_type (xref/lib/a.mli[6,122+31]..xref/lib/a.mli[6,122+45])
              Tmty_signature
              [
                signature_item (xref/lib/a.mli[6,122+35]..xref/lib/a.mli[6,122+41])
                  Tsig_type Rec
                  [
                    type_declaration t/1003 (xref/lib/a.mli[6,122+35]..xref/lib/a.mli[6,122+41])
                      ptype_params =
                        []
                      ptype_cstrs =
                        []
                      ptype_kind =
                        Ttype_abstract
                      ptype_private = Public
                      ptype_manifest =
                        None
                  ]
              ]
        signature_item (xref/lib/a.mli[6,122+46]..xref/lib/a.mli[6,122+58])
          Tsig_module "T/1005"
          module_type (xref/lib/a.mli[6,122+57]..xref/lib/a.mli[6,122+58])
            Tmty_ident "S/1004"
      ]
  signature_item (xref/lib/a.mli[10,209+0]..xref/lib/a.mli[10,209+14])
    Tsig_type Rec
    [
      type_declaration v/1006 (xref/lib/a.mli[10,209+0]..xref/lib/a.mli[10,209+14])
        ptype_params =
          []
        ptype_cstrs =
          []
        ptype_kind =
          Ttype_abstract
        ptype_private = Public
        ptype_manifest =
          Some
            core_type (xref/lib/a.mli[10,209+9]..xref/lib/a.mli[10,209+14])
              Ttyp_constr "M/1002.T.t"
              []
    ]
]
```

So we can see from this that module M is `M/1002` and within that, `S` is `S/1003` and `T` is `T/1005`. Our type `v`
that we're interested in has a manifest of core_type `Ttyp_constr "M/1002.T.t"` - so we can see that only the outer
`M` has been exactly specified.

Odoc reads that in the `cmti.ml` module [here](https://github.com/ocaml/odoc/blob/e714a19c715a31a97bcf4fca5339bc1f69d1d392/src/loader/cmti.ml#L74-L77),
creating a value:

```
(Constr
  (dot t
    (dot T
      (resolved
        (identifier
          ((root
            (lib (compilation_unit Lib__A hidden)
               fba2b9ecd9f8d91722c8dc26a0c4d640)
                Lib__A)
            (module M))))))
())
```

here we can see we've got an unresolved path that has a resolved base of `M`.

The result of the Cmti.load is a Compilation_unit.t (which is a Model.Lang.Compilation_unit.t) which is:

```ocaml
type t =
    { id: Identifier.Module.t;
      doc: Comment.docs;
      digest: Digest.t;
      imports: Import.t list;
      source: Source.t option;
      interface: bool;
      hidden: bool;
      content: content;
      expansion: Signature.t option; }
```

of which we're interested now in `content`, which is (indirectly) a `Signature.t`, which is a list of
