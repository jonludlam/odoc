# Menhir Parser: Plan to Reach Mergeable State

This document describes the remaining work to bring the menhir parser branch
to a state suitable for merging into ocaml/odoc master.

## Current State (post-rebase)

The menhir parser has been squash-rebased onto current master (commit
`c3f0f46ee`). The parser library, tests, and test driver all **compile
successfully**. However:

- **114 test cases** produce different output from the old parser
- **1 test case** (`complex_table`) causes a hard crash (`MenhirBasics.Error`)
- Several master features (code block tag parsing) are not yet implemented in
  the menhir lexer

The work is divided into three phases: critical fixes, feature parity, and
polish. Each phase has concrete deliverables and should be PR-able independently.

---

## Phase 1: Critical Fixes

These must be resolved before any merge. They represent crashes, correctness
bugs, and missing features that would break existing users.

### 1.1 Fix `complex_table` crash

**Problem:** The `complex_table` test in `test_tables.ml` causes an unhandled
`MenhirBasics.Error` exception. The input is a valid (if complex) heavy table
with nested content including light tables, lists, and styled text.

**Root cause:** The menhir grammar likely lacks a production or error-recovery
rule for some token sequence that occurs inside nested table constructs.

**Approach:**
- Reproduce the crash with the tester tool
- Identify which token sequence triggers the error (use the tester's token
  trace output)
- Add appropriate productions or `error` recovery rules to `parser.mly`
- Verify the AST output is reasonable (doesn't need to match old parser
  exactly, but must not crash)

**Files:** `parser.mly`

### 1.2 Fix negative column numbers

**Problem:** The `offset_location` test produces `(2 -7)` as a column number.
This occurs when the parser is invoked with a non-zero starting position
(simulating a doc comment embedded in a source file at column 20).

**Root cause:** The `lex_curr_p` initialization in `odoc_parser.ml` may
interact incorrectly with the menhir-generated position tracking, or the
location conversion in `offset_to_location` has an arithmetic error when
combined with menhir's `$loc` positions.

**Approach:**
- Trace through the position computation for the failing test
- Compare how the old parser (`Syntax.parse`) tracked positions vs how
  `Parser.main` + `$loc` works in menhir
- Fix the arithmetic in either `odoc_parser.ml` or `lexer.mll`'s position
  emission

**Files:** `odoc_parser.ml`, possibly `lexer.mll`

### 1.3 Port code block tag parsing from master

**Problem:** Master added structured code block metadata parsing (key=value
bindings, quoted strings with escape sequences). The menhir lexer still treats
everything after the language tag as a single raw string.

Master supports inputs like:
```
{@ocaml env=dev file="my file.ml"[
  let x = 1
]}
```

The menhir branch would treat `env=dev file="my file.ml"` as a single tag
string rather than parsing it into structured `[`Binding ("env", "dev"); `Tag
"file=\"my file.ml\""]`.

**What needs to be ported from master's lexer:**

1. Helper functions: `digit_value`, `num_value`, `char_for_decimal_code`
2. `string_buffer : Buffer.t` field added to the `input` record type (in both
   `lexer.mll` and `lexer.mli`)
3. Character class definitions: `tag_escape`, `tag_quoted_char`,
   `tag_quoted_atom`, `tag_unquoted_char`, `tag_unquoted_atom`
4. The `string` lexer rule (handles quoted strings with escape sequences
   including `\ddd` decimal codes)
5. The `code_block_metadata_atom` rule
6. Rewritten `code_block_metadata_tail` rule producing structured tag list
7. Update `meta` type in `tokens.ml`: change `tags : string Loc.with_location
   option` to `tags : Ast.code_block_tags`
8. Remove the bridge code in `parser.mly` lines 801/815 that converts
   `Some t -> [`Tag t]`
9. Add `should_not_be_escaped` warnings in the `string` rule
10. Add `invalid_char_code` warnings for bad `\ddd` sequences

**Files:** `lexer.mll`, `lexer.mli`, `tokens.ml`, `parser.mly`

### 1.4 Fix light table alignment/header detection

**Problem:** The `light_table_new_lines` test shows that the alignment
separator row (`--- | --- | ---`) is parsed as a data row containing
`(word ---)` nodes instead of being interpreted as alignment markers. This
completely breaks light table header/alignment detection.

**Root cause:** The light table grammar rules in `parser.mly` (around line
558+) don't correctly identify and handle the alignment separator row.

**Approach:**
- Compare how master's `Syntax.parse` handles the alignment row (it's done
  in the parser, checking the first row after the header for alignment markers)
- Either handle alignment detection in the menhir grammar or add a post-parse
  pass in `parser_aux.ml`
- The latter may be easier since LR grammars can't easily do lookahead-based
  row reinterpretation

**Files:** `parser.mly`, possibly `parser_aux.ml`

---

## Phase 2: Error Recovery Parity

These issues don't cause crashes but produce worse diagnostics or different
parse trees on malformed input. They should be fixed to maintain the quality of
odoc's error reporting.

### 2.1 Restore heavy table error containment

**Problem:** When invalid content appears inside a `{table}` or `{tr}`, the
old parser kept the content within the table structure and emitted "X is not
allowed in Y" warnings with actionable suggestions. The menhir parser closes
the table early and spills content outside as orphaned paragraph/word nodes.

**Examples:**
```
Input:  {table absurd content}
Old:    (table ...) with warnings "not allowed in table, move outside or inside {tr}"
New:    (table ...) (paragraph (word content)) (paragraph (word }))
```

**Approach:**
- Improve `error` recovery productions in `table_heavy` and `row_heavy` rules
- Instead of closing the construct on `error`, consume tokens until
  `RIGHT_BRACE` and emit warnings
- May need to use menhir's `%on_error_reduce` directives more aggressively

**Files:** `parser.mly`

### 2.2 Restore markup tag error recovery (bold, italic, etc.)

**Problem:** Invalid content in `{b}`, `{i}`, etc. causes the menhir parser to
close the tag empty and orphan the remaining content (including `}` as a word
node).

**Example:**
```
Input:  {b - foo}
Old:    (bold ((word -) (word foo)))
New:    (bold ()) (word })
```

**Approach:**
- Add error recovery productions to the style/inline element rules
- Ensure `}` is consumed as the closing delimiter rather than being emitted as
  a word

**Files:** `parser.mly`

### 2.3 Restore "should begin on its own line" warnings

**Problem:** ~13 test cases show that the menhir parser silently accepts
constructs that the old parser warned about with "X should begin on its own
line". This affects `@deprecated`, `@param`, `@see`, `-` list items, and
paragraphs appearing after tags on the same line.

**Approach:**
- These warnings were emitted by the old parser during token-stream processing
- In the menhir grammar, they can be emitted via `Writer.Warning` when the
  relevant production matches tokens that don't start at column 0 / start of
  line
- Check `$startpos` in relevant productions and emit warnings when the
  construct doesn't begin at the expected position

**Files:** `parser.mly`

### 2.4 Restore "should not be empty" warnings

**Problem:** ~18 test cases show missing emptiness warnings for `@since`,
`@version`, `@canonical`, media elements, empty link targets, etc.

**Approach:**
- Add `Writer.ensure` checks (similar to the existing code block emptiness
  check at line 804 of `parser.mly`) to the relevant productions
- Use `Parse_error.should_not_be_empty` with the appropriate `~what` parameter

**Files:** `parser.mly`

### 2.5 Restore "should be followed by whitespace" warnings

**Problem:** 4 test cases show that `{b`, `{li` etc. without following
whitespace are silently accepted.

**Approach:**
- Either handle in the lexer (check if the character after the tag is
  whitespace and emit a warning if not) or in the grammar
- The lexer approach is likely simpler since the lexer already inspects the
  character after the tag to decide which token to emit

**Files:** `lexer.mll` or `parser.mly`

### 2.6 Improve warning messages

**Problem:** Several warning messages are less informative than the old
parser's. Specifically:
- "'}': bad markup." should be "Unpaired '}' (end of markup). Suggestion: try
  '\\}'." (9 occurrences)
- "Illegal character or syntax 'X' in Y" should include suggestion text like
  "Move outside of Y, or inside Z" where applicable

**Approach:**
- Update the `error` recovery actions in `parser.mly` to use the more specific
  `Parse_error` constructors (`not_allowed` with `~suggestion`, `unclosed_bracket`,
  etc.) instead of the generic `illegal` constructor
- In some cases, the `error` token erases context needed for good messages;
  these may require restructuring the grammar or using post-parse validation

**Files:** `parser.mly`, `parser_aux.ml`

---

## Phase 3: Polish and Cleanup

These are refinements that improve quality but are not strictly necessary for
correctness.

### 3.1 Fix whitespace/space node splitting

**Problem:** ~111 test expectations differ because the menhir parser emits
separate space nodes where the old parser merged them (e.g., newline + leading
indentation becomes two space nodes instead of one).

**Assessment:** This is arguably more accurate (each space node maps to its
exact source range) but changes downstream behaviour. Need to determine:
- Does odoc's rendering pipeline handle consecutive space nodes correctly?
- Do downstream consumers (e.g., `odoc_model`) normalize whitespace?

**Approach:** If consecutive spaces cause rendering issues, merge adjacent
space tokens either in the lexer or in a post-parse normalization pass. If
rendering is unaffected, update the test expectations.

**Files:** `lexer.mll` or `parser.mly` or test expectations

### 3.2 Fix location span differences

**Problem:** ~50 test cases show different location spans. Most are minor
(cell spans including delimiter characters, end-of-line positions off by one).

**Approach:**
- Audit the location span differences systematically
- Fix any that are clearly wrong (e.g., spans including delimiter chars that
  shouldn't be part of the content)
- For others, decide case-by-case whether the new or old span is more correct
- Update test expectations for acceptable differences

**Files:** `lexer.mll`, `parser.mly`, `parser_aux.ml`, test expectations

### 3.3 Resolve menhir grammar conflicts

**Problem:** The parser compiles with 52 shift/reduce and 5 reduce/reduce
conflict states (236 S/R and 41 R/R conflicts resolved arbitrarily).

**Approach:**
- Generate the `.conflicts` file with `menhir --explain` (already configured
  in dune)
- Review each conflict state to determine if the arbitrary resolution is
  correct
- Add explicit `%left`, `%right`, `%nonassoc` precedence declarations or
  refactor rules to eliminate unintended ambiguities
- Some conflicts may be intentional (e.g., the `tag_with_content` duplication
  noted in TODO.md)
- Target: reduce to < 10 conflict states

**Files:** `parser.mly`

### 3.4 Fix `_none_` file locations in empty cells

**Problem:** Empty table cells produce AST nodes with `_none_` as the file
name in their location span.

**Approach:**
- Track down where the `_none_` file name originates (likely a default
  `Loc.span` construction without a file name)
- Propagate the correct file name from the enclosing table's location

**Files:** `parser.mly` or `parser_aux.ml`

### 3.5 Fix module list whitespace tokens

**Problem:** `{!modules: Foo Bar}` produces module nodes including whitespace
strings like `" "` between module names.

**Approach:**
- Filter whitespace tokens out of the module list in the parser rule or in a
  post-parse step

**Files:** `parser.mly`

### 3.6 Clean up test expectations

After all the above fixes, update the remaining test expectations to match the
new parser's output. Any remaining differences should be documented as
intentional changes.

**Approach:**
- Run `dune promote` after verifying each change is acceptable
- Add comments to tests where the output intentionally differs from what the
  old parser would have produced
- Ensure the tester tool (`test_driver/tester.ml`) runs without failures

**Files:** `test/test.ml`, `test/test_tables.ml`

### 3.7 Remove or gate test_driver

**Problem:** `test_driver/tester.ml` uses `Lexing.set_filename` (OCaml 4.11+)
and `List.partition_map` (OCaml 4.12+), but the parser library supports OCaml
>= 4.08.

**Approach:** Either:
- Gate the `test_driver` with `(enabled_if (>= %{ocaml_version} 4.12))` in its
  dune file, or
- Replace `set_filename` and `partition_map` with compatible alternatives

**Files:** `test_driver/dune` or `test_driver/tester.ml`

### 3.8 Review `tester.ml` public_name

**Problem:** The test driver has `(public_name tester)` which would install a
binary called `tester` globally. This is inappropriate for a development tool.

**Approach:** Remove `(public_name tester)` from `test_driver/dune`.

**Files:** `test_driver/dune`

---

## Phase 4: Integration and Review

### 4.1 Run full odoc test suite

Build the full `odoc` project (not just the parser) and run its complete test
suite to catch any downstream breakage from AST changes (space node splitting,
location differences, etc.).

```
dune build @all
dune runtest
```

### 4.2 Run benchmarks

The `ocaml-benchmarks` CI job was failing on the original PR. Run the parser
benchmarks to verify performance is acceptable. Menhir `--table` mode may be
slightly slower than the hand-written recursive descent parser.

### 4.3 Code review

- Remove the `TODO` comment in `ast.ml` about refactoring to nominal types
  (lines 3-7) if it's no longer relevant, or move it to this plan document
- Review `parser_aux.ml` for clarity and documentation
- Ensure all public functions in `odoc_parser.mli` have adequate docstrings
- Consider whether the `Tester` module should be exposed in the public `.mli`
  or moved to test-only code

### 4.4 CI verification

Push and verify all CI matrix entries pass:
- OCaml 4.08 through 5.4 (or whatever the current CI matrix is)
- Linux and macOS
- ocaml-benchmarks

---

## Dependency Summary

The menhir parser adds two new runtime dependencies to `odoc-parser`:
- `menhirLib` (runtime library for `--table` mode parsers)
- `menhir` (build-time dependency for generating the parser)

Both are well-maintained, widely-used OCaml packages. `menhirLib` has no
transitive dependencies beyond the OCaml standard library.

---

## Priority Order

If time is limited, do work in this order:

1. **1.1** Fix crash (blocks all testing)
2. **1.3** Port code block tags (feature parity with master)
3. **1.2** Fix negative columns (correctness bug)
4. **1.4** Fix light table alignment (correctness bug)
5. **2.1-2.2** Error recovery parity (quality)
6. **3.1-3.2** Whitespace/location audit (decide accept vs fix)
7. **3.6** Update test expectations
8. **2.3-2.6** Restore warnings (quality)
9. **3.3** Resolve grammar conflicts (maintenance)
10. **3.7-3.8** Cleanup (polish)
11. **4.1-4.4** Integration testing
