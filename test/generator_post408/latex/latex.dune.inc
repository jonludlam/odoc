(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../labels.odocl})
   (with-stdout-to
    Labels.tex.gen
    (progn
     (system "cat 'latex.gen/test/Labels.tex'")))
   (with-stdout-to
    Labels.c.tex.gen
    (progn
     (system "cat 'latex.gen/test/Labels.c.tex'")))))
 (enabled_if
  (< %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Labels.tex Labels.tex.gen))
 (enabled_if
  (< %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Labels.c.tex Labels.c.tex.gen))
 (enabled_if
  (< %{ocaml_version} 4.08)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../recent.odocl})
   (with-stdout-to
    Recent.tex.gen
    (progn
     (system "cat 'latex.gen/test/Recent.tex'")))))
 (enabled_if
  (< %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent.tex Recent.tex.gen))
 (enabled_if
  (< %{ocaml_version} 4.08)))

(rule
 (action
  (progn
   (run odoc latex-generate -o latex.gen %{dep:../recent_impl.odocl})
   (with-stdout-to
    Recent_impl.tex.gen
    (progn
     (system "cat 'latex.gen/test/Recent_impl.tex'")))
   (with-stdout-to
    Recent_impl.B.tex.gen
    (progn
     (system "cat 'latex.gen/test/Recent_impl.B.tex'")))))
 (enabled_if
  (< %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent_impl.tex Recent_impl.tex.gen))
 (enabled_if
  (< %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff Recent_impl.B.tex Recent_impl.B.tex.gen))
 (enabled_if
  (< %{ocaml_version} 4.08)))