{br} is a line break, in a paragraph and in a table cell.

  $ cat > m.mli << EOF
  > (** First line{br}second line.
  > 
  >     {t | one{br}two | three |} *)
  > val x : int
  > EOF
  $ ocamlc -bin-annot -c m.mli
  $ odoc compile --parent-id pkg --output-dir _odoc m.cmti
  $ odoc link _odoc/pkg/m.odoc
  $ odoc html-generate -o html _odoc/pkg/m.odocl
  $ grep -o 'First line.*second line.' html/pkg/M/index.html
  First line<br/>second line.
  $ grep -o '<td>.*</td>' html/pkg/M/index.html
  <td><p>one<br/>two</p></td><td><p>three</p></td>
  $ odoc latex-generate -o latex _odoc/pkg/m.odocl
  $ grep -o 'First line.*' latex/pkg/M.tex
  First line\\
  $ odoc man-generate -o man _odoc/pkg/m.odocl
  $ grep -A2 'First line' man/pkg/M.3o
  First line
  .br 
  .ti +2
