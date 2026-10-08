An odoc built with an extension renders the code blocks in the extension's
language with the extension, and the others as usual.

  $ cat > index.mld << EOF
  > {0 Extensions}
  > {@shout[hello world]}
  > {@shout end=![hello again]}
  > {@shout[ALREADY SHOUTING]}
  > {@ocaml[let x = 1]}
  > EOF
  $ odoc_shout compile --parent-id pkg --output-dir _odoc index.mld
  $ odoc_shout link _odoc/pkg/page-index.odoc
  $ odoc_shout html-generate --indent -o html _odoc/pkg/page-index.odocl
  $ sed -n '/odoc-preamble/,/<\/header>/p' html/pkg/index.html
    <header class="odoc-preamble">
     <h1 id="extensions"><a href="#extensions" class="anchor"></a>Extensions
     </h1><p class="shout">HELLO WORLD</p><p class="shout">HELLO AGAIN!</p>
     <pre class="language-shout"><code>ALREADY SHOUTING</code></pre>
     <pre class="language-ocaml"><code>let x = 1</code></pre>
    </header><div class="odoc-content"></div>

The standard odoc does not know the extension, and renders every code block as
code.

  $ odoc html-generate --indent -o html _odoc/pkg/page-index.odocl
  $ sed -n '/odoc-preamble/,/<\/header>/p' html/pkg/index.html
    <header class="odoc-preamble">
     <h1 id="extensions"><a href="#extensions" class="anchor"></a>Extensions
     </h1><pre class="language-shout"><code>hello world</code></pre>
     <pre class="language-shout"><code>hello again</code></pre>
     <pre class="language-shout"><code>ALREADY SHOUTING</code></pre>
     <pre class="language-ocaml"><code>let x = 1</code></pre>
    </header><div class="odoc-content"></div>
