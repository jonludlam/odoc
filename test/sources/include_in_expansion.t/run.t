Checking that source parents are kept, using include.

  $ odoc compile -c module-a -c src-source root.mld

  $ printf "a.ml\nb.ml\nmain.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map

  $ ocamlc -c -o b.cmo b.ml -bin-annot -I .
  $ ocamlc -c -o main__A.cmo a.ml -bin-annot -I .
  $ ocamlc -c main.ml -bin-annot -I .

  $ odoc compile --source-name b.ml --source-parent-file src-source.odoc -I . b.cmt
  Shape: {<B>
          "y"[value] -> <B.0>;
          }
  
  Struct
  Adding a 'Def' for 'def-0' at loc (4,5)
  uids (1 calculated vs 1 expected): [val-y]Adding a 'Def' for 'y_268' at loc (4,5)
  $ odoc compile --source-name a.ml --source-parent-file src-source.odoc -I . main__A.cmt
  Shape: {<Main__A>
          "x"[value] -> <Main__A.0>;
          "y"[value] -> CU B . "y"[value];
          }
  
  Struct
  Adding a 'Def' for 'def-0' at loc (14,15)
  uids (1 calculated vs 1 expected): [val-x]Adding a 'Def' for 'x_270' at loc (14,15)
  $ odoc compile --source-name main.ml --source-parent-file src-source.odoc -I . main.cmt
  Shape: {<Main>
          "A"[module] -> CU Main__A;
          "y"[value] -> CU B . "y"[value];
          }
  
  Struct
  Adding a 'Def' for 'def-0' at loc (0,18)
  uids (0 calculated vs 1 expected): []

  $ odoc link -I . main.odoc
  Found shape: <B.0>
  
  Found shape: <Main__A.0>
  
  Found shape: {<Main__A>
                "x"[value] -> <Main__A.0>;
                "y"[value] -> <B.0>;
                }
  
  Found shape: <B.0>
  
  $ odoc link -I . main__A.odoc

  $ odoc html-generate --source main.ml --indent -o html main.odocl
  $ odoc html-generate --source a.ml --hidden --indent -o html main__A.odocl

In Main.A, the source parent of value x should be to Main__A, while the
source parent of value y should be left to B.

  $ grep source_link html/Main/A/index.html -C 1
     <h1>Module <code><span>Main.A</span></code>
      <a href="../../root/source/a.ml.html" class="source_link">Source</a>
     </h1>
  --
         <a href="#val-y" class="anchor"></a>
         <a href="../../root/source/b.ml.html#def-0" class="source_link">Source
         </a><code><span><span class="keyword">val</span> y : int</span></code>
  --
       <a href="#val-x" class="anchor"></a>
       <a href="../../root/source/a.ml.html#def-0" class="source_link">Source
       </a><code><span><span class="keyword">val</span> x : int</span></code>
