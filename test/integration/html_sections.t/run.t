With --sections, a heading and what follows it, up to the next heading of the
same or a higher level, are wrapped in a <section>.

  $ cat > index.mld << EOF
  > {0 Sections}
  > Introduction.
  > {1 First}
  > One.
  > {2 Inner}
  > Two.
  > {1 Second}
  > Three.
  > EOF
  $ odoc compile --parent-id pkg --output-dir _odoc index.mld
  $ odoc link _odoc/pkg/page-index.odoc

Without it, there are no sections:

  $ odoc html-generate -o html _odoc/pkg/page-index.odocl
  $ grep -c '<section' html/pkg/index.html
  0
  [1]

  $ odoc html-generate --sections --indent -o html _odoc/pkg/page-index.odocl
  $ sed -n '/<div class="odoc-content">/,/^ *<\/div>/p' html/pkg/index.html
    <div class="odoc-content">
     <section><h2 id="first"><a href="#first" class="anchor"></a>First</h2>
      <p>One.</p>
      <section><h3 id="inner"><a href="#inner" class="anchor"></a>Inner</h3>
       <p>Two.</p>
      </section>
     </section>
     <section><h2 id="second"><a href="#second" class="anchor"></a>Second</h2>
      <p>Three.</p>
     </section>
    </div>
