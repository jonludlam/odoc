An odoc built with an extension parses the extension's tags as custom tags,
and renders them with the extension. References in them are resolved.

  $ cat > m.mli << EOF
  > (** @shout Use {!y}. *)
  > val x : int
  > 
  > (** @shout.quietly Use {!x}. *)
  > val y : int
  > 
  > (** @whisper Not a custom tag. *)
  > val z : int
  > EOF
  $ ocamlc -bin-annot -c m.mli
  $ odoc_shout compile --parent-id pkg --output-dir _odoc m.cmti
  File "m.mli", line 7, characters 4-12:
  Warning: Unknown tag '@whisper'.
  $ odoc_shout link _odoc/pkg/m.odoc
  $ odoc_shout html-generate --indent -o html _odoc/pkg/m.odocl
  $ grep -A3 'class="spec-doc"' html/pkg/M/index.html
      <div class="spec-doc"><div class="shout" id="shout-1">
       <p>Use <a href="#val-y"><code>y</code></a>.</p></div>
      </div>
     </div>
  --
      <div class="spec-doc">
       <ul class="at-tags">
        <li class="shout.quietly"><span class="at-tag">shout.quietly</span>
          <p>Use <a href="#val-x"><code>x</code></a>.</p>
  --
      </div><div class="spec-doc"><p>@whisper Not a custom tag.</p></div>
     </div>
    </div>
   </body>

The standard odoc renders custom tags as other tags.

  $ odoc html-generate --indent -o html _odoc/pkg/m.odocl
  $ grep -A3 'class="spec-doc"' html/pkg/M/index.html
      <div class="spec-doc">
       <ul class="at-tags">
        <li class="shout"><span class="at-tag">shout</span> 
         <p>Use <a href="#val-y"><code>y</code></a>.</p>
  --
      <div class="spec-doc">
       <ul class="at-tags">
        <li class="shout.quietly"><span class="at-tag">shout.quietly</span>
          <p>Use <a href="#val-x"><code>x</code></a>.</p>
  --
      </div><div class="spec-doc"><p>@whisper Not a custom tag.</p></div>
     </div>
    </div>
   </body>

Custom tags are allowed in pages.

  $ cat > index.mld << EOF
  > {0 Page}
  > @shout In a page.
  > 
  > @shout{Over
  > 
  > two paragraphs.}
  > EOF
  $ odoc_shout compile --parent-id pkg --output-dir _odoc index.mld
  $ odoc_shout link _odoc/pkg/page-index.odoc
  $ odoc_shout html-generate --indent -o html _odoc/pkg/page-index.odocl
  $ sed -n '/odoc-preamble/,/<\/header>/p' html/pkg/index.html
    <header class="odoc-preamble">
     <h1 id="page"><a href="#page" class="anchor"></a>Page</h1>
     <div class="shout" id="shout-1"><p>In a page.</p></div>
     <div class="shout" id="shout-2"><p>Over</p><p>two paragraphs.</p></div>
    </header><div class="odoc-content"></div>
