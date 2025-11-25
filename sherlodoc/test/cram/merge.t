Test the merge command for combining multiple sherlodoc databases

Setup: Find some odocl files to work with
  $ ODOCLS=$(find ../docs/odoc/tyxml/ -name '*.odocl' | grep -v "__" | head -20)
  $ echo "$ODOCLS" | wc -l | tr -d ' '
  16

Create three separate databases from different subsets of files
  $ DB1_FILES=$(echo "$ODOCLS" | head -6)
  $ DB2_FILES=$(echo "$ODOCLS" | head -12 | tail -6)
  $ DB3_FILES=$(echo "$ODOCLS" | tail -8)

  $ sherlodoc index --format marshal -o db1.marshal --index-docstring=false $DB1_FILES
  $ sherlodoc index --format marshal -o db2.marshal --index-docstring=false $DB2_FILES
  $ sherlodoc index --format marshal -o db3.marshal --index-docstring=false $DB3_FILES

  $ test -f db1.marshal && echo "db1.marshal created"
  db1.marshal created
  $ test -f db2.marshal && echo "db2.marshal created"
  db2.marshal created
  $ test -f db3.marshal && echo "db3.marshal created"
  db3.marshal created

Merge the three databases
  $ sherlodoc merge --format marshal -o merged.marshal db1.marshal db2.marshal db3.marshal
  Loaded 1 shard(s) from db1.marshal
  Loaded 1 shard(s) from db2.marshal
  Loaded 1 shard(s) from db3.marshal
  Total shards to merge: 3
  Merging databases...
  Writing merged database to merged.marshal...
  Merge complete!

  $ test -f merged.marshal && echo "merged.marshal created"
  merged.marshal created

Create a reference database with all files
  $ sherlodoc index --format marshal -o reference.marshal --index-docstring=false $ODOCLS

Search in the merged database - it should contain entries from all three sources
  $ export SHERLODOC_DB=merged.marshal
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc search --limit 5 --no-rhs "attrib"
  type Html_types.b_attrib
  type Html_types.i_attrib
  type Html_types.p_attrib
  type Html_types.u_attrib
  type Html_types.h6_attrib

Search for something that should be in one of the databases
  $ sherlodoc search --limit 3 --no-rhs "Tyxml_html"
  type Tyxml_html.uri
  mod Tyxml_html
  type 'a Tyxml_html.wrap

Compare merged database with traditional (all-at-once) database
The reference database was built with all files at once
  $ export SHERLODOC_DB=reference.marshal
  $ sherlodoc search --limit 5 --no-rhs "attrib" > reference_attrib.txt
  $ export SHERLODOC_DB=merged.marshal
  $ sherlodoc search --limit 5 --no-rhs "attrib" > merged_attrib.txt
  $ diff reference_attrib.txt merged_attrib.txt

Search results for "Tyxml_html" should be identical
  $ export SHERLODOC_DB=reference.marshal
  $ sherlodoc search --limit 10 --no-rhs "Tyxml_html" > reference_tyxml.txt
  $ export SHERLODOC_DB=merged.marshal
  $ sherlodoc search --limit 10 --no-rhs "Tyxml_html" > merged_tyxml.txt
  $ diff reference_tyxml.txt merged_tyxml.txt

Search results for "string_attrib" should be identical
  $ export SHERLODOC_DB=reference.marshal
  $ sherlodoc search --limit 8 --no-rhs "string_attrib" > reference_string.txt
  $ export SHERLODOC_DB=merged.marshal
  $ sherlodoc search --limit 8 --no-rhs "string_attrib" > merged_string.txt
  $ diff reference_string.txt merged_string.txt

Test partial name search consistency
  $ export SHERLODOC_DB=reference.marshal
  $ sherlodoc search --limit 6 --no-rhs "tring" > reference_tring.txt
  $ export SHERLODOC_DB=merged.marshal
  $ sherlodoc search --limit 6 --no-rhs "tring" > merged_tring.txt
  $ diff reference_tring.txt merged_tring.txt

Negative test: incomplete merge should omit entries from excluded databases
Create an incomplete merge with only db1 and db2 (excluding db3)
  $ sherlodoc merge --format marshal -o incomplete.marshal db1.marshal db2.marshal
  Loaded 1 shard(s) from db1.marshal
  Loaded 1 shard(s) from db2.marshal
  Total shards to merge: 2
  Merging databases...
  Writing merged database to incomplete.marshal...
  Merge complete!

First, identify what entries are in db3 only
  $ export SHERLODOC_DB=db3.marshal
  $ sherlodoc search --limit 5 --no-rhs "wrap" > db3_wrap.txt

These entries should be in the complete merged database
  $ export SHERLODOC_DB=merged.marshal
  $ sherlodoc search --limit 5 --no-rhs "wrap" > merged_wrap.txt
  $ diff db3_wrap.txt merged_wrap.txt

These entries should also be in the reference database
  $ export SHERLODOC_DB=reference.marshal
  $ sherlodoc search --limit 5 --no-rhs "wrap" > reference_wrap.txt
  $ diff db3_wrap.txt reference_wrap.txt

But these entries should NOT all be in the incomplete merge (only db1+db2)
The diff shows entries from db3 that are missing in the incomplete merge:
  $ export SHERLODOC_DB=incomplete.marshal
  $ sherlodoc search --limit 5 --no-rhs "wrap" > incomplete_wrap.txt
  $ diff db3_wrap.txt incomplete_wrap.txt
  1,2d0
  < type 'a Tyxml_xml.wrap
  < type 'a Tyxml_svg.wrap
  4c2,3
  < type 'a Tyxml_svg.Xml.wrap
  ---
  > mod Xml_wrap
  > type 'a Svg_f.Make.wrap
  5a5
  > type 'a Html_f.Make.wrap
  [1]

The incomplete merge correctly omits entries that were only in db3
Specifically, Tyxml_xml.wrap and Tyxml_svg.wrap are missing, demonstrating
that the merge only includes what was in the input databases (db1+db2)

================================================================================
COMPREHENSIVE COMPARISON: MERGED vs TRADITIONAL DATABASE
================================================================================

The following tests comprehensively verify that merged databases produce
identical results to databases built directly from .odocl files.

Helper function to compare search results:
  $ compare_search() {
  >   local query="$1"
  >   local limit="${2:-20}"
  >   export SHERLODOC_DB=reference.marshal
  >   sherlodoc search --limit "$limit" --no-rhs "$query" > /tmp/ref_result.txt
  >   export SHERLODOC_DB=merged.marshal
  >   sherlodoc search --limit "$limit" --no-rhs "$query" > /tmp/merged_result.txt
  >   if diff -q /tmp/ref_result.txt /tmp/merged_result.txt > /dev/null; then
  >     echo "PASS: '$query' (limit $limit)"
  >   else
  >     echo "FAIL: '$query' (limit $limit)"
  >     diff /tmp/ref_result.txt /tmp/merged_result.txt
  >   fi
  > }

--------------------------------------------------------------------------------
NAME-BASED SEARCH TESTS
--------------------------------------------------------------------------------

Exact name matches:
  $ compare_search "Tyxml_html"
  PASS: 'Tyxml_html' (limit 20)
  $ compare_search "Tyxml_svg"
  PASS: 'Tyxml_svg' (limit 20)
  $ compare_search "Html_types"
  PASS: 'Html_types' (limit 20)
  $ compare_search "Svg_types"
  PASS: 'Svg_types' (limit 20)

Partial name matches (prefix):
  $ compare_search "attrib"
  PASS: 'attrib' (limit 20)
  $ compare_search "string_"
  PASS: 'string_' (limit 20)
  $ compare_search "event_"
  PASS: 'event_' (limit 20)

Partial name matches (suffix/substring):
  $ compare_search "tring"
  PASS: 'tring' (limit 20)
  $ compare_search "_handler"
  PASS: '_handler' (limit 20)
  $ compare_search "wrap"
  PASS: 'wrap' (limit 20)

Multi-word queries:
  $ compare_search "string attrib"
  PASS: 'string attrib' (limit 20)
  $ compare_search "event handler"
  PASS: 'event handler' (limit 20)
  $ compare_search "Svg Make"
  PASS: 'Svg Make' (limit 20)

Short queries:
  $ compare_search "uri"
  PASS: 'uri' (limit 20)
  $ compare_search "elt"
  PASS: 'elt' (limit 20)
  $ compare_search "xml"
  PASS: 'xml' (limit 20)

Long queries:
  $ compare_search "Make_with_wrapped_functions"
  PASS: 'Make_with_wrapped_functions' (limit 20)
  $ compare_search "keyboard_event_handler"
  PASS: 'keyboard_event_handler' (limit 20)

--------------------------------------------------------------------------------
TYPE-BASED SEARCH TESTS
--------------------------------------------------------------------------------

Simple type searches:
  $ compare_search ": list"
  PASS: ': list' (limit 20)
  $ compare_search ": string"
  PASS: ': string' (limit 20)
  $ compare_search ": attrib"
  PASS: ': attrib' (limit 20)

Arrow type searches:
  $ compare_search ": _ -> _"
  PASS: ': _ -> _' (limit 20)
  $ compare_search ": string -> _"
  PASS: ': string -> _' (limit 20)
  $ compare_search ": _ -> attrib"
  PASS: ': _ -> attrib' (limit 20)

Polymorphic type searches:
  $ compare_search ": 'a"
  PASS: ': 'a' (limit 20)
  $ compare_search ": 'a -> 'a"
  PASS: ': 'a -> 'a' (limit 20)
  $ compare_search ": 'a list"
  PASS: ': 'a list' (limit 20)

Complex type searches:
  $ compare_search ": 'a elt"
  PASS: ': 'a elt' (limit 20)
  $ compare_search ": _ wrap"
  PASS: ': _ wrap' (limit 20)
  $ compare_search ": string -> attrib"
  PASS: ': string -> attrib' (limit 20)

--------------------------------------------------------------------------------
COMBINED NAME + TYPE SEARCH TESTS
--------------------------------------------------------------------------------

  $ compare_search "unsafe : string"
  PASS: 'unsafe : string' (limit 20)
  $ compare_search "attrib : string"
  PASS: 'attrib : string' (limit 20)
  $ compare_search "of_seq : list"
  PASS: 'of_seq : list' (limit 20)

--------------------------------------------------------------------------------
RESULT ORDERING AND COST TESTS
--------------------------------------------------------------------------------

Results should have identical ordering (costs):
  $ export SHERLODOC_DB=reference.marshal
  $ sherlodoc search --limit 15 --print-cost --no-rhs "attrib" > /tmp/ref_cost.txt
  $ export SHERLODOC_DB=merged.marshal
  $ sherlodoc search --limit 15 --print-cost --no-rhs "attrib" > /tmp/merged_cost.txt
  $ diff /tmp/ref_cost.txt /tmp/merged_cost.txt && echo "Costs match"
  Costs match

Type search costs should also match:
  $ export SHERLODOC_DB=reference.marshal
  $ sherlodoc search --limit 15 --print-cost --no-rhs ": list" > /tmp/ref_type_cost.txt
  $ export SHERLODOC_DB=merged.marshal
  $ sherlodoc search --limit 15 --print-cost --no-rhs ": list" > /tmp/merged_type_cost.txt
  $ diff /tmp/ref_type_cost.txt /tmp/merged_type_cost.txt && echo "Type search costs match"
  Type search costs match

--------------------------------------------------------------------------------
LARGE RESULT SET TESTS
--------------------------------------------------------------------------------

Test with larger result limits to catch any ordering differences:
  $ compare_search "attrib" 50
  PASS: 'attrib' (limit 50)
  $ compare_search "type" 50
  PASS: 'type' (limit 50)
  $ compare_search ": _" 50
  PASS: ': _' (limit 50)

--------------------------------------------------------------------------------
STATIC SORT TESTS
--------------------------------------------------------------------------------

Static sort (without query-based ranking) should also match:
  $ export SHERLODOC_DB=reference.marshal
  $ sherlodoc search --limit 20 --static-sort --no-rhs "handler" > /tmp/ref_static.txt
  $ export SHERLODOC_DB=merged.marshal
  $ sherlodoc search --limit 20 --static-sort --no-rhs "handler" > /tmp/merged_static.txt
  $ diff /tmp/ref_static.txt /tmp/merged_static.txt && echo "Static sort matches"
  Static sort matches

================================================================================
EDGE CASES AND ERROR HANDLING
================================================================================

Test merging with just one database (edge case)
  $ sherlodoc merge --format marshal -o single.marshal db1.marshal
  Loaded 1 shard(s) from db1.marshal
  Total shards to merge: 1
  Merging databases...
  Writing merged database to single.marshal...
  Merge complete!

  $ test -f single.marshal && echo "single.marshal created"
  single.marshal created

  $ export SHERLODOC_DB=single.marshal
  $ sherlodoc search --limit 3 --no-rhs "attrib"
  type +'a Html_f.Make.attrib
  type Html_f.Make.Xml.attrib
  type +'a Html_f.Make_with_wrapped_functions.attrib

Test error handling: merge with no input files should fail
  $ sherlodoc merge --format marshal -o empty.marshal 2>&1
  sherlodoc: required argument INPUT_DB is missing
  Usage: sherlodoc merge [--format=DB_FORMAT] [--db=DB] [OPTION]… INPUT_DB…
  Try 'sherlodoc merge --help' or 'sherlodoc --help' for more information.
  [124]

Test merging non-existent files - cmdliner validates file existence
  $ sherlodoc merge --format marshal -o fail.marshal nonexistent1.marshal nonexistent2.marshal 2>&1
  sherlodoc: INPUT_DB… arguments: no 'nonexistent1.marshal' file or directory
  Usage: sherlodoc merge [--format=DB_FORMAT] [--db=DB] [OPTION]… INPUT_DB…
  Try 'sherlodoc merge --help' or 'sherlodoc --help' for more information.
  [124]

Test that help message is available
  $ sherlodoc merge --help | head -5
  SHERLODOC-MERGE(1)             Sherlodoc Manual             SHERLODOC-MERGE(1)
  
  NNAAMMEE
         sherlodoc-merge - Merge multiple sherlodoc databases into one
  


