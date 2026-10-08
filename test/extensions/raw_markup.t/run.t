An odoc built with an extension renders the raw markup for the extension's
target with the extension, inline and on its own. The extension leaves the raw
markup already in capitals to the default rendering, which drops it.

  $ cat > index.mld << EOF
  > {0 Raw markup}
  > Say {%shout:hello%} to everyone, {%shout:LOUDLY%}.
  > 
  > {%SHOUT:goodbye%}
  > 
  > {%html:<em>html</em>%}{%other:dropped%}
  > EOF
  $ odoc_shout compile --parent-id pkg --output-dir _odoc index.mld
  $ odoc_shout link _odoc/pkg/page-index.odoc
  $ odoc_shout html-generate --indent -o html _odoc/pkg/page-index.odocl
  $ sed -n '/odoc-preamble/,/<\/header>/p' html/pkg/index.html
    <header class="odoc-preamble">
     <h1 id="raw-markup"><a href="#raw-markup" class="anchor"></a>Raw markup
     </h1><p>Say <strong>HELLO</strong> to everyone, .</p>
     <strong>GOODBYE</strong><p><em>html</em></p>
    </header><div class="odoc-content"></div>

The standard odoc drops the raw markup for targets it does not know.

  $ odoc html-generate --indent -o html _odoc/pkg/page-index.odocl
  $ sed -n '/odoc-preamble/,/<\/header>/p' html/pkg/index.html
    <header class="odoc-preamble">
     <h1 id="raw-markup"><a href="#raw-markup" class="anchor"></a>Raw markup
     </h1><p>Say  to everyone, .</p><p><em>html</em></p>
    </header><div class="odoc-content"></div>
