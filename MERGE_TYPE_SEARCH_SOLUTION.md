# Minimal Solution for Type Search in Merged Databases

## The Problem

Currently, merged databases don't support type search because:
1. We only have string representations of types (e.g., `"int -> string"`)
2. Type search requires **type polarity analysis** which needs the structured AST
3. Polarity analysis computes which types appear in "positive" (output) vs "negative" (input) positions
4. This data is used to build specialized indices (`db_pos_types`, `db_neg_types`)

## The Key Insight

We don't need the full Odoc AST - we just need the **computed polarity data**!

When indexing, `Type_polarity.of_typ` produces:
```ocaml
(string * int * Sign.t) Seq.t
```

This is: `(type_path, count, polarity)` tuples

For example, `List.map : ('a -> 'b) -> 'a list -> 'b list` produces:
- `("list", 1, Neg)` - list in input position
- `("list", 1, Pos)` - list in output position

This is **much smaller** than the full AST!

## Minimal Solution

### 1. Store Type Polarities in Entries

Extend `Db.Entry.t`:
```ocaml
type t =
  { name : string
  ; rhs : string option
  ; url : string
  ; kind : Kind.t
  ; cost : int
  ; doc_html : string
  ; pkg : Package.t
  ; type_polarities : (string * int * Type_polarity.Sign.t) list option  (* NEW *)
  }
```

### 2. Capture Polarities During Indexing

In `load_doc.ml`, when registering:
```ocaml
let register_type_expr ~db elt typ =
  let type_polarities = Db.Type_polarity.of_typ ~any_is_poly:true typ in
  (* Store in database indices *)
  Db_writer.store_type_polarities db elt type_polarities;
  (* Also store in the entry itself for later merging *)
  let polarities_list = List.of_seq type_polarities in
  { elt with type_polarities = Some polarities_list }
```

### 3. Use Polarities During Merge

In `merge.ml`:
```ocaml
Entry_set.iter
  (fun (entry : Entry.t) ->
    (* Register the name *)
    let name = String.lowercase_ascii entry.name in
    Db_writer.store_word db name entry;

    (* Register type polarities if present *)
    match entry.type_polarities with
    | None -> ()
    | Some polarities ->
        let polarities_seq = List.to_seq polarities in
        Db_writer.store_type_polarities db entry polarities_seq
  )
  all_entries
```

## Size Analysis

### Per Entry Overhead

For a typical entry with a type signature:
- Function with 2-3 type constructors: ~3-6 tuples
- Each tuple: ~20-50 bytes (string + int + sign)
- Total: **~100-300 bytes per typed entry**

For comparison:
- Full Odoc AST: hundreds to thousands of bytes
- String representation already stored: similar size

### Database Size Impact

For a typical codebase with 10,000 entries:
- ~50% have types: 5,000 entries
- Average polarity data: ~200 bytes
- Total overhead: **~1 MB**

This is negligible compared to the suffix tree indices themselves.

## Implementation Steps

1. **Add field to Entry.t** (`db/entry.ml`)
   - Add `type_polarities : (string * int * Type_polarity.Sign.t) list option`
   - Update `compare`, `equal`, `pp` functions
   - Update `v` constructor

2. **Capture polarities during indexing** (`index/load_doc.ml`)
   - Modify `register_type_expr` to return updated entry
   - Thread updated entry through registration functions

3. **Use polarities during merge** (`index/merge.ml`)
   - Extract `type_polarities` from entries
   - Call `Db_writer.store_type_polarities` with extracted data

4. **Update tests** (`test/cram/merge.t`)
   - Remove or update the limitation test
   - Add positive test showing type search works on merged databases

## Advantages

✓ **Minimal**: Only stores computed polarity data, not full AST
✓ **Compact**: ~100-300 bytes per entry with types
✓ **Complete**: Enables full type search on merged databases
✓ **Backward compatible**: Optional field, old databases still work

## Alternative: Store Serialized Typexpr

Instead of polarities, store `Typexpr.t` (sherlodoc's simplified type representation):
- Already a simplified form (not full Odoc AST)
- Can recompute polarities on demand
- Slightly larger but more flexible
- Type: `type_expr : Typexpr.t option`

This might be cleaner since `Typexpr.t` is already used in `Kind.t` variants like `Val of Typexpr.t`.

## Recommendation

**Store `Typexpr.t` instead of polarities**:

```ocaml
type t =
  { name : string
  ; rhs : string option
  ; url : string
  ; kind : Kind.t
  ; cost : int
  ; doc_html : string
  ; pkg : Package.t
  ; type_expr : Typexpr.t option  (* For merge support *)
  }
```

Then in merge:
```ocaml
match entry.type_expr with
| Some typ ->
    let polarities = Type_polarity.of_typ ~any_is_poly:true typ in
    Db_writer.store_type_polarities db entry polarities
| None -> ()
```

This is:
- **Cleaner**: Uses existing `Typexpr.t` type
- **Flexible**: Can recompute polarities with different parameters
- **Consistent**: `Typexpr.t` already appears in `Kind.t`
- **Size**: Similar overhead, still compact
