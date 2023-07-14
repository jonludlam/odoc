Files containing some values:

  $ cat a.ml
  type t = string
  
  let x = 2
  let y = x + 1
  let z a = if x = 1 || true then x + y else 0
  
  module A = struct
    let b = 1
  end
  module B = A
  
  let a =
    ignore(A.b);
    let open A in
    b
  
  module type T = sig end
  module type U = T
  
  type ext = ..
  type ext += Foo
  
  exception Exn
  
  class cls = object end
  class cls' = cls
  class type ct = object end

Source pages require a parent:

  $ odoc compile -c module-a -c src-source root.mld

Compile the modules:

  $ ocamlc -c a.ml -bin-annot

Compile the pages without --source:

  $ odoc compile a.cmt
  Shape: {<A>
          "#cls"[type] -> <A.14>;
          "#cls'"[type] -> <A.16>;
          "#ct"[type] -> <A.17>;
          "A"[module] -> {<A.6>
                          "b"[value] -> <A.5>;
                          };
          "B"[module] -> {<A.6>
                          "b"[value] -> <A.5>;
                          };
          "Exn"[extension constructor] -> <A.13>;
          "Foo"[extension constructor] -> <A.12>;
          "T"[module type] -> <A.9>;
          "U"[module type] -> <A.10>;
          "a"[value] -> <A.8>;
          "cls"[type] -> <A.14>;
          "cls"[class] -> <A.14>;
          "cls"[class type] -> <A.14>;
          "cls'"[type] -> <A.16>;
          "cls'"[class] -> <A.16>;
          "cls'"[class type] -> <A.16>;
          "ct"[type] -> <A.17>;
          "ct"[class type] -> <A.17>;
          "ext"[type] -> <A.11>;
          "t"[type] -> <A.0>;
          "x"[value] -> <A.1>;
          "y"[value] -> <A.2>;
          "z"[value] -> <A.3>;
          }
  
  Struct
  Adding a 'Def' for 'def-11' at loc (222,235)
  Adding a 'Def' for 'def-2' at loc (31,32)
  Adding a 'Def' for 'def-9' at loc (179,202)
  Adding a 'Def' for 'def-17' at loc (319,321)
  Adding a 'Def' for 'def-14' at loc (274,277)
  Adding a 'Def' for 'def-0' at loc (0,15)
  Adding a 'Def' for 'def-10' at loc (203,220)
  Adding a 'Def' for 'def-6' at loc (87,120)
  Adding a 'Def' for 'def-13' at loc (253,266)
  Adding a 'Def' for 'def-16' at loc (297,301)
  Adding a 'Def' for 'def-8' at loc (139,140)
  Adding a 'Def' for 'def-1' at loc (21,22)
  Adding a 'Def' for 'def-5' at loc (111,112)
  Adding a 'Def' for 'def-7' at loc (121,133)
  Adding a 'Def' for 'def-3' at loc (45,46)
  Adding a 'Def' for 'def-12' at loc (248,251)
  uids (25 calculated vs 16 expected): [val-z,val-y,val-x,type-t,type-ext,class-type-ct,type-ct,class-type-cls',class-cls',type-cls',class-type-cls,class-cls,type-cls,val-a,module-type-U,module-type-T,ext-Foo,ext-Exn,module-B.val-b,module-B,module-A.val-b,module-A,type-#ct,type-#cls',type-#cls]Adding a 'Def' for 'x_269' at loc (21,22)
  Adding a 'Def' for 'y_270' at loc (31,32)
  Adding a local occurrence for x_269 (pos (35,36))
  Adding a global occurrence for def-1 (name x) (pos (35,36))
  Adding a 'Def' for 'z_271' at loc (45,46)
  Adding a 'Def' for 'a_273' at loc (47,48)
  Adding a local occurrence for x_269 (pos (54,55))
  Adding a global occurrence for def-1 (name x) (pos (54,55))
  Adding a local occurrence for x_269 (pos (73,74))
  Adding a global occurrence for def-1 (name x) (pos (73,74))
  Adding a local occurrence for y_270 (pos (77,78))
  Adding a global occurrence for def-2 (name y) (pos (77,78))
  Adding a 'Def' for 'b_274' at loc (111,112)
  Adding a 'Def' for 'a_277' at loc (139,140)
  Adding a global occurrence for def-5 (name b) (pos (151,156))
  Adding a global occurrence for def-5 (name b) (pos (176,177))
  $ odoc link -I . a.odoc
  Found shape: <A.0>
  
  Found shape: <A.1>
  
  Found shape: <A.2>
  
  Found shape: <A.3>
  
  Found shape: <A.5>
  
  Found shape: {<A.6>
                "b"[value] -> <A.5>;
                }
  
  Found shape: {<A.6>
                "b"[value] -> <A.5>;
                }
  
  Found shape: <A.8>
  
  Found shape: <A.9>
  
  Found shape: <A.10>
  
  Found shape: <A.11>
  
  Found shape: <A.12>
  
  Found shape: <A.13>
  
  Found shape: <A.14>
  
  Found shape: <A.16>
  
  Found shape: <A.17>
  
  $ odoc html-generate --indent -o html a.odocl

No source links are generated in the documentation:

  $ ! grep source_link html/A/index.html -B 2

Now, compile the pages with the --source option:

  $ printf "a.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map

  $ odoc compile -I . --source-name a.ml --source-parent-file src-source.odoc a.cmt
  Shape: {<A>
          "#cls"[type] -> <A.14>;
          "#cls'"[type] -> <A.16>;
          "#ct"[type] -> <A.17>;
          "A"[module] -> {<A.6>
                          "b"[value] -> <A.5>;
                          };
          "B"[module] -> {<A.6>
                          "b"[value] -> <A.5>;
                          };
          "Exn"[extension constructor] -> <A.13>;
          "Foo"[extension constructor] -> <A.12>;
          "T"[module type] -> <A.9>;
          "U"[module type] -> <A.10>;
          "a"[value] -> <A.8>;
          "cls"[type] -> <A.14>;
          "cls"[class] -> <A.14>;
          "cls"[class type] -> <A.14>;
          "cls'"[type] -> <A.16>;
          "cls'"[class] -> <A.16>;
          "cls'"[class type] -> <A.16>;
          "ct"[type] -> <A.17>;
          "ct"[class type] -> <A.17>;
          "ext"[type] -> <A.11>;
          "t"[type] -> <A.0>;
          "x"[value] -> <A.1>;
          "y"[value] -> <A.2>;
          "z"[value] -> <A.3>;
          }
  
  Struct
  Adding a 'Def' for 'def-11' at loc (222,235)
  Adding a 'Def' for 'def-2' at loc (31,32)
  Adding a 'Def' for 'def-9' at loc (179,202)
  Adding a 'Def' for 'def-17' at loc (319,321)
  Adding a 'Def' for 'def-14' at loc (274,277)
  Adding a 'Def' for 'def-0' at loc (0,15)
  Adding a 'Def' for 'def-10' at loc (203,220)
  Adding a 'Def' for 'def-6' at loc (87,120)
  Adding a 'Def' for 'def-13' at loc (253,266)
  Adding a 'Def' for 'def-16' at loc (297,301)
  Adding a 'Def' for 'def-8' at loc (139,140)
  Adding a 'Def' for 'def-1' at loc (21,22)
  Adding a 'Def' for 'def-5' at loc (111,112)
  Adding a 'Def' for 'def-7' at loc (121,133)
  Adding a 'Def' for 'def-3' at loc (45,46)
  Adding a 'Def' for 'def-12' at loc (248,251)
  uids (25 calculated vs 16 expected): [val-z,val-y,val-x,type-t,type-ext,class-type-ct,type-ct,class-type-cls',class-cls',type-cls',class-type-cls,class-cls,type-cls,val-a,module-type-U,module-type-T,ext-Foo,ext-Exn,module-B.val-b,module-B,module-A.val-b,module-A,type-#ct,type-#cls',type-#cls]Adding a 'Def' for 'x_269' at loc (21,22)
  Adding a 'Def' for 'y_270' at loc (31,32)
  Adding a local occurrence for x_269 (pos (35,36))
  Adding a global occurrence for def-1 (name x) (pos (35,36))
  Adding a 'Def' for 'z_271' at loc (45,46)
  Adding a 'Def' for 'a_273' at loc (47,48)
  Adding a local occurrence for x_269 (pos (54,55))
  Adding a global occurrence for def-1 (name x) (pos (54,55))
  Adding a local occurrence for x_269 (pos (73,74))
  Adding a global occurrence for def-1 (name x) (pos (73,74))
  Adding a local occurrence for y_270 (pos (77,78))
  Adding a global occurrence for def-2 (name y) (pos (77,78))
  Adding a 'Def' for 'b_274' at loc (111,112)
  Adding a 'Def' for 'a_277' at loc (139,140)
  Adding a global occurrence for def-5 (name b) (pos (151,156))
  Adding a global occurrence for def-5 (name b) (pos (176,177))
  $ odoc link -I . a.odoc
  Found shape: <A.0>
  
  Found shape: <A.1>
  
  Found shape: <A.2>
  
  Found shape: <A.3>
  
  Found shape: <A.5>
  
  Found shape: {<A.6>
                "b"[value] -> <A.5>;
                }
  
  Found shape: {<A.6>
                "b"[value] -> <A.5>;
                }
  
  Found shape: <A.8>
  
  Found shape: <A.9>
  
  Found shape: <A.10>
  
  Found shape: <A.11>
  
  Found shape: <A.12>
  
  Found shape: <A.13>
  
  Found shape: <A.14>
  
  Found shape: <A.16>
  
  Found shape: <A.17>
  
  $ odoc html-generate --source a.ml --indent -o html a.odocl

Source links generated in the documentation:

  $ grep source_link html/A/index.html -B 2
    <header class="odoc-preamble">
     <h1>Module <code><span>A</span></code>
      <a href="../root/source/a.ml.html" class="source_link">Source</a>
  --
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-0" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-x">
       <a href="#val-x" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-1" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-y">
       <a href="#val-y" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-2" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-z">
       <a href="#val-z" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-3" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-A">
       <a href="#module-A" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-6" class="source_link">Source</a>
  --
      <div class="spec module anchored" id="module-B">
       <a href="#module-B" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-6" class="source_link">Source</a>
  --
      <div class="spec value anchored" id="val-a">
       <a href="#val-a" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-8" class="source_link">Source</a>
  --
      <div class="spec module-type anchored" id="module-type-T">
       <a href="#module-type-T" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-9" class="source_link">Source</a>
  --
      <div class="spec module-type anchored" id="module-type-U">
       <a href="#module-type-U" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-10" class="source_link">Source</a>
  --
      <div class="spec type anchored" id="type-ext">
       <a href="#type-ext" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-11" class="source_link">Source</a>
  --
      <div class="spec type extension anchored" id="extension-decl-Foo">
       <a href="#extension-decl-Foo" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-12" class="source_link">Source</a>
  --
      <div class="spec exception anchored" id="exception-Exn">
       <a href="#exception-Exn" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-13" class="source_link">Source</a>
  --
      <div class="spec class anchored" id="class-cls">
       <a href="#class-cls" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-14" class="source_link">Source</a>
  --
      <div class="spec class anchored" id="class-cls'">
       <a href="#class-cls'" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-16" class="source_link">Source</a>
  --
      <div class="spec class-type anchored" id="class-type-ct">
       <a href="#class-type-ct" class="anchor"></a>
       <a href="../root/source/a.ml.html#def-17" class="source_link">Source</a>

Ids generated in the source code:

  $ cat html/root/source/a.ml.html | tr '> ' '\n\n' | grep '^id'
  id="L1"
  id="L2"
  id="L3"
  id="L4"
  id="L5"
  id="L6"
  id="L7"
  id="L8"
  id="L9"
  id="L10"
  id="L11"
  id="L12"
  id="L13"
  id="L14"
  id="L15"
  id="L16"
  id="L17"
  id="L18"
  id="L19"
  id="L20"
  id="L21"
  id="L22"
  id="L23"
  id="L24"
  id="L25"
  id="L26"
  id="L27"
  id="def-0"
  id="def-1"
  id="x_269"
  id="def-2"
  id="y_270"
  id="def-3"
  id="z_271"
  id="a_273"
  id="def-6"
  id="def-5"
  id="b_274"
  id="def-7"
  id="def-8"
  id="a_277"
  id="def-9"
  id="def-10"
  id="def-11"
  id="def-12"
  id="def-13"
  id="def-14"
  id="def-16"
  id="def-17"
