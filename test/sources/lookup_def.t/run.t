Compile the modules:

  $ odoc compile -c module-a -c src-source root.mld

  $ printf "a.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map

  $ ocamlc -c a.mli a.ml -bin-annot
  $ odoc compile --source-name a.ml --source-parent-file src-source.odoc -I . a.cmti
  Shape: {<A>
          "#cls"[type] -> <A.12>;
          "#clst"[type] -> <A.14>;
          "Exn"[extension constructor] -> <A.9>;
          "Ext"[extension constructor] -> <A.11>;
          "M"[module] -> {<A.0>
                          };
          "N"[module] ->
              {<A.5>
               "S"[module type] -> <A.2>;
               "T"[module] -> {<A.4>
                               "x"[value] -> <A.3>;
                               };
               };
          "a"[value] -> <A.7>;
          "cls"[type] -> <A.12>;
          "cls"[class] -> <A.12>;
          "cls"[class type] -> <A.12>;
          "clst"[type] -> <A.14>;
          "clst"[class type] -> <A.14>;
          "ext"[type] -> <A.10>;
          "t"[type] -> <A.6>;
          }
  
  Struct
  Adding a 'Def' for 'def-11' at loc (228,231)
  Adding a 'Def' for 'def-2' at loc (43,84)
  Adding a 'Def' for 'def-9' at loc (186,199)
  Adding a 'Def' for 'def-14' at loc (268,272)
  Adding a 'Def' for 'def-0' at loc (0,21)
  Adding a 'Def' for 'def-10' at loc (201,214)
  Adding a 'Def' for 'def-6' at loc (135,141)
  Adding a 'Def' for 'def-4' at loc (88,129)
  Adding a 'Def' for 'def-8' at loc (178,180)
  Adding a 'Def' for 'def-1' at loc (67,78)
  Adding a 'Def' for 'def-5' at loc (23,133)
  Adding a 'Def' for 'def-7' at loc (147,148)
  Adding a 'Def' for 'def-3' at loc (118,119)
  Adding a 'Def' for 'def-12' at loc (239,242)
  uids (17 calculated vs 14 expected): [type-t,type-ext,class-type-clst,type-clst,class-type-cls,class-cls,type-cls,val-a,module-N.module-T.val-x,module-N.module-T,module-N.module-type-S,module-N,module-M,ext-Ext,ext-Exn,type-#clst,type-#cls]Adding a 'Def' for 'x_271' at loc (118,119)
  Adding a 'Def' for 'a_275' at loc (147,148)
  Adding a 'Def' for 'a'_276' at loc (178,180)
  $ odoc link a.odoc
  Found shape: {<A.0>
                }
  
  Found shape: <A.2>
  
  Found shape: <A.2>
  
  Found shape: <A.3>
  
  Found shape: {<A.4>
                "x"[value] -> <A.3>;
                }
  
  Found shape: {<A.5>
                "S"[module type] -> <A.2>;
                "T"[module] -> {<A.4>
                                "x"[value] -> <A.3>;
                                };
                }
  
  Found shape: <A.6>
  
  Found shape: <A.7>
  
  Found shape: <A.9>
  
  Found shape: <A.10>
  
  Found shape: <A.11>
  
  Found shape: <A.12>
  
  Found shape: <A.14>
  

Show the locations:

  $ odoc_print a.odocl | jq -c '.. | select(.locs?) | [ .id, .locs ]'
  [{"`Module":[{"`Root":["None","A"]},"M"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-0"]}}]
  [{"`Module":[{"`Root":["None","A"]},"N"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-5"]}}]
  [{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-2"]}}]
  [{"`Value":[{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},"x"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-2"]}}]
  [{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-4"]}}]
  [{"`Value":[{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},"x"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-3"]}}]
  [{"`Type":[{"`Root":["None","A"]},"t"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-6"]}}]
  [{"`Value":[{"`Root":["None","A"]},"a"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-7"]}}]
  [{"`Exception":[{"`Root":["None","A"]},"Exn"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-9"]}}]
  [{"`Type":[{"`Root":["None","A"]},"ext"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-10"]}}]
  [{"`Extension":[{"`Root":["None","A"]},"Ext"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-11"]}}]
  [{"`Class":[{"`Root":["None","A"]},"cls"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-12"]}}]
  [{"`ClassType":[{"`Root":["None","A"]},"clst"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-14"]}}]
