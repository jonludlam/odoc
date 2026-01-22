(lang dune 3.18)

(using dune_site 0.1)

(name odoc)

(documentation "https://ocaml.org/p/odoc")

(source
 (github ocaml/odoc))

(license ISC)

(authors
 "Anton Bachin <antonbachin@yahoo.com>"
 "Daniel B\195\188nzli <daniel.buenzli@erratique.ch>"
 "David Sheets <sheets@alum.mit.edu>"
 "Jon Ludlam <jon@recoil.org>"
 "Jules Aguillon <juloo.dsi@gmail.com>"
 "Leo White <leo@lpw25.net>"
 "Lubega Simon <lubegasimon73@gmail.com>"
 "Paul-Elliot Angl\195\168s d'Auriac <paul-elliot@tarides.com>"
 "Thomas Refis <trefis@janestreet.com>"
 "Arthur Wendling <art.wendling@gmail.com>"
 "Emile Trotignon <emile.trotignon@gmail.com>")

(maintainers
 "Daniel B\195\188nzli <daniel.buenzli@erratique.ch>"
 "Jon Ludlam <jon@recoil.org>"
 "Jules Aguillon <juloo.dsi@gmail.com>"
 "Paul-Elliot Angl\195\168s d'Auriac <paul-elliot@tarides.com>")

(cram enable)

(package
 (name odoc)
 (sites (lib extensions)))

(package (name odoc-parser))
(package (name odoc-md))
(package (name odoc-driver))
(package (name odoc-bench))
(package (name sherlodoc))

; Example extension packages
(package
 (name odoc-dot-extension)
 (synopsis "Graphviz/DOT diagram support for odoc documentation")
 (description "Renders {@dot[...]} code blocks as SVG diagrams using Graphviz.
Supports width, height, and layout engine options.")
 (depends
  (ocaml (>= 4.14))
  odoc))

(package
 (name odoc-mermaid-extension)
 (synopsis "Mermaid diagram support for odoc documentation")
 (description "Renders {@mermaid[...]} code blocks as interactive diagrams.
Supports flowcharts, sequence diagrams, class diagrams, and more.")
 (depends
  (ocaml (>= 4.14))
  odoc))

(package
 (name odoc-msc-extension)
 (synopsis "Message Sequence Chart support for odoc documentation")
 (description "Renders {@msc[...]} code blocks as message sequence charts.
Uses the MscGen syntax for defining sequence diagrams.")
 (depends
  (ocaml (>= 4.14))
  odoc))

(using mdx 0.3)

; Sherlodoc
(using menhir 2.1)
(using directory-targets 0.1)
