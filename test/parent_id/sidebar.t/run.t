  $ ocamlc -c -bin-annot unit.ml

  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc/dir1 dir1/my_page.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc/dir1 dir1/index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc file.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/lib/libname unit.cmt

  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/page-file.odoc
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/dir1/page-my_page.odoc
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/dir1/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/lib/libname/unit.odoc

  $ odoc compile-index -P pkg:_odoc/pkg/doc/ -L libname:_odoc/pkg/lib/libname -o sidebar.odoc-index
  $ odoc sidebar-generate sidebar.odoc-index
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/doc/page-file.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/doc/dir1/page-my_page.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/doc/dir1/page-index.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/doc/page-index.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/lib/libname/unit.odocl

  $ odoc sidebar-generate --json sidebar.odoc-index
  $ cat sidebar.json | jq
  {
    "pages": [
      {
        "name": "pkg",
        "pages": {
          "node": {
            "url": "pkg/doc/index.html",
            "kind": "leaf-page",
            "content": "<a href=\"pkg/doc/index.html\">Package <code>pkg</code></a>"
          },
          "children": [
            {
              "node": {
                "url": "pkg/doc/dir1/index.html",
                "kind": "leaf-page",
                "content": "<a href=\"pkg/doc/dir1/index.html\">A directory</a>"
              },
              "children": [
                {
                  "node": {
                    "url": "pkg/doc/dir1/my_page.html",
                    "kind": "leaf-page",
                    "content": "<a href=\"pkg/doc/dir1/my_page.html\">My page</a>"
                  },
                  "children": []
                }
              ]
            },
            {
              "node": {
                "url": "pkg/doc/file.html",
                "kind": "leaf-page",
                "content": "<a href=\"pkg/doc/file.html\">File</a>"
              },
              "children": []
            }
          ]
        }
      }
    ],
    "libraries": [
      {
        "name": "libname",
        "pages": [
          {
            "node": {
              "url": "pkg/lib/libname/Unit/index.html",
              "kind": "module",
              "content": "<a href=\"pkg/lib/libname/Unit/index.html\">Unit</a>"
            },
            "children": [
              {
                "node": {
                  "url": "pkg/lib/libname/Unit/X/index.html",
                  "kind": "module",
                  "content": "<a href=\"pkg/lib/libname/Unit/X/index.html\">X</a>"
                },
                "children": []
              },
              {
                "node": {
                  "url": "pkg/lib/libname/Unit/module-type-Y/index.html",
                  "kind": "module-type",
                  "content": "<a href=\"pkg/lib/libname/Unit/module-type-Y/index.html\">Y</a>"
                },
                "children": []
              }
            ]
          }
        ]
      }
    ]
  }

  $ cat html/pkg/doc/index.html | grep odoc-global-toc -A 15
     <nav class="odoc-toc odoc-global-toc">
      <ul class="odoc-modules">
       <li><b>Library <code>libname</code></b>
        <ul><li><a href="../lib/libname/Unit/index.html">Unit</a></li></ul>
       </li>
      </ul><b>Documentation</b>
      <ul class="odoc-pages">
       <li><a href="#" class="current_unit">Package <code>pkg</code></a>
        <ul>
         <li><a href="dir1/index.html">A directory</a>
          <ul><li><a href="dir1/my_page.html">My page</a></li></ul>
         </li><li><a href="file.html">File</a></li>
        </ul>
       </li>
      </ul>
     </nav>

  $ cat html/pkg/lib/libname/Unit/index.html | grep odoc-global-toc -A 15
     <nav class="odoc-toc odoc-global-toc">
      <ul class="odoc-modules">
       <li><b>Library <code>libname</code></b>
        <ul>
         <li><a href="#" class="current_unit">Unit</a>
          <ul><li><a href="X/index.html">X</a></li>
           <li><a href="module-type-Y/index.html">Y</a></li>
          </ul>
         </li>
        </ul>
       </li>
      </ul><b>Documentation</b>
      <ul class="odoc-pages">
       <li><a href="../../../doc/index.html">Package <code>pkg</code></a>
        <ul>
         <li><a href="../../../doc/dir1/index.html">A directory</a>

  $ odoc support-files -o html
$ cp -r html /tmp/html
