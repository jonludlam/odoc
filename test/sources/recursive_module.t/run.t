Checking that source links exists inside recursive modules.

  $ odoc compile -c module-main -c src-source root.mld

  $ printf "main.ml" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map

  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile --source-name main.ml --source-parent-file src-source.odoc -I . main.cmt
  Shape: {<Main>
          "A"[module] -> A/268<Main.0>;
          "B"[module] -> B/269<Main.1>;
          }
  
  Struct
  Adding a 'Def' for 'def-0' at loc (0,43)
  Adding a 'Def' for 'def-2' at loc (21,33)
  Adding a 'Def' for 'def-6' at loc (59,79)
  Adding a 'Def' for 'def-5' at loc (21,33)
  Adding a 'Def' for 'def-1' at loc (45,89)
  Adding a 'Def' for 'def-3' at loc (59,79)
  uids (2 calculated vs 6 expected): [module-B,module-A]
  $ odoc link -I . main.odoc
  Found shape: A/268<Main.0> . "t"[type]
  
  Found shape: A/268<Main.0>
  
  Found shape: B/269<Main.1> . "t"[type]
  
  Found shape: B/269<Main.1>
  
  $ odoc html-generate --source main.ml --indent -o html main.odocl

Both modules should contain source links

  $ grep source_link html/Main/A/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.A</span></code>
      <a href="../../root/source/main.ml.html#def-0" class="source_link">Source
      </a>
     </h1>

  $ grep source_link html/Main/B/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.B</span></code>
      <a href="../../root/source/main.ml.html#def-1" class="source_link">Source
      </a>
     </h1>
