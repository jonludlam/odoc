An odoc built with an extension renders the code blocks in the extension's
language with the extension, and the others as usual. The extension renders the
output of a code block too: this one leaves it out.

  $ cat > index.mld << EOF
  > {0 Extensions}
  > {@shout[hello world]}
  > {@shout end=![hello again]}
  > {@shout[ALREADY SHOUTING]}
  > {@ocaml[let x = 1]}
  > {x@shout[with output]x[the output]}
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
     <p class="shout">WITH OUTPUT</p>
    </header><div class="odoc-content"></div>
  $ grep -o '<link[^>]*shout.css"/>' html/pkg/index.html
  <link rel="stylesheet" href="../shout/shout.css"/>

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
     <pre class="language-shout"><code>with output</code></pre>
     <p>the output</p>
    </header><div class="odoc-content"></div>

The extension adds its style sheet only to the pages that have a shout.

  $ cat > other.mld << EOF
  > {0 No shouting}
  > {@shout[NOT SHOUTING]}
  > EOF
  $ odoc_shout compile --parent-id pkg --output-dir _odoc other.mld
  $ odoc_shout link _odoc/pkg/page-other.odoc
  $ odoc_shout html-generate --indent -o html _odoc/pkg/page-other.odocl
  $ grep -c 'shout.css' html/pkg/other.html
  0
  [1]

The extension's style sheet is a support file, which the standard odoc does
not write.

  $ odoc_shout support-files -o support
  $ cat support/shout/shout.css
  .shout { font-weight: bold }
  $ odoc_shout support-files-targets -o support | grep shout
  support/shout/shout.css
  $ odoc support-files -o stock-support
  $ test -e stock-support/shout || echo 'no shout support files'
  no shout support files
