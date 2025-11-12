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
  NAME
         sherlodoc-merge - Merge multiple sherlodoc databases into one
  
  SYNOPSIS
         sherlodoc merge [--format=DB_FORMAT] [--db=DB] [OPTION]… INPUT_DB…
