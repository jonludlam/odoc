  $ ocamlc -bin-annot -w -49 -c -impl sexplib0__.ml-gen -no-alias-deps
  $ ocamlc -bin-annot -c sexplib0__Sexp.mli
  $ ocamlc -bin-annot -c sexplib0.ml -open Sexplib0__

  $ odoc compile sexplib0__.cmt -I .
  $ odoc compile sexplib0__Sexp.cmti -I . 
  $ odoc compile sexplib0.cmt -I . 

  $ odoc link sexplib0.odoc -I .
  $ odoc_print sexplib0.odocl
  $ odoc html-generate -o html sexplib0.odocl --indent

  $ odoc support-files -o html

  $ cat html/Sexplib0/Sexp/Private/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>Private (Sexplib0.Sexp.Private)</title>
    <link rel="stylesheet" href="../../../odoc.css"/><meta charset="utf-8"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../../index.html">Sexplib0</a> &#x00BB; 
     <a href="../index.html">Sexp</a> &#x00BB; Private
    </nav>
    <header class="odoc-preamble">
     <h1>Module <code><span>Sexp.Private</span></code></h1>
    </header>
    <div class="odoc-content">
     <div class="odoc-spec">
      <div class="spec value" id="val-x" class="anchored">
       <a href="#val-x" class="anchor"></a>
       <code>
        <span><span class="keyword">val</span> x : 
         <a href="../index.html#type-t">t</a>
        </span>
       </code>
      </div>
     </div>
    </div>
   </body>
  </html>

