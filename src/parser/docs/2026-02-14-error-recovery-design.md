# Error Recovery Design: Explicit Catch-All Token Rules

## Problem

The menhir parser uses menhir's `error` token for error recovery in containers
(tables, lists) and inline contexts (styles, refs, links). This is too blunt:

- Tables/lists spill content outside when menhir discards tokens including `}`
- Styles lose their content when the error recovery discards valid inline tokens
- Warnings are generic ("Illegal character or syntax") instead of contextual

## Approach

Replace `error` token rules with explicit "catch-all" token productions that
enumerate the tokens that can appear in each context. This gives full control
over what gets consumed, what warnings are emitted, and how recovery proceeds.

## Design

### Principle: Two recovery strategies

1. **Containers (tables, lists):** Consume invalid tokens one-by-one, warn,
   keep looping until `}` or END. Content stays inside the container.

2. **Inline contexts (styles):** The existing explicit variants for RIGHT_BRACE,
   END, and RIGHT_CODE_DELIMITER already handle the main cases. Replace the
   `error` fallback with explicit matches for block-level tokens that can
   intrude into inline context.

### Pattern for containers

Define a "junk token" inline rule that matches any token not expected in the
container. Use it as an alternative in the container's item list:

```
table_heavy_item :=
  | row_heavy                           (* valid row *)
  | junk = located(table_junk); whitespace*;  (* invalid token *)
    { warn("not allowed in table"); return nothing }

table_heavy_grid := list(table_heavy_item)
```

The `list()` combinator naturally loops, so the junk alternative makes the
parser consume-and-continue until RIGHT_BRACE or END.

### Junk token rules

Each container context needs its own junk rule listing tokens that are invalid
in that context. The odoc token set is ~35 tokens, so this is manageable.

For tables, junk is anything that isn't `TABLE_ROW`, `RIGHT_BRACE`, or `END`:
- `Word`, `Space`, `MINUS`, `PLUS`, `BAR`, `Style`, `Code_span`, etc.
- Block-level tokens: `Code_block`, `Verbatim`, `List`, `Section_heading`, etc.

For lists, junk is anything that isn't a list item (`LI`, `DASH`),
`RIGHT_BRACE`, or `END`.

### Warning messages

Replace `Parse_error.illegal` with `Parse_error.not_allowed` everywhere.
Include `~in_what` context and `~suggestion` where helpful:

- Tables: "Move outside of {table ...}, or inside {tr ...}"
- Lists: "Move into a list item, '{li ...}' or '{- ...}'"

### Scope

Priority order:
1. Heavy tables (most visible error recovery difference)
2. Heavy lists
3. Style/inline error rules (replace remaining `error` uses)
4. Light tables and light lists

Start with heavy tables, validate the approach works, then extend.
