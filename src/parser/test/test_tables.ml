open Test

[@@@ocaml.warning "-32"]

let%expect_test _ =
  let module Heavy = struct
    let empty_table_heavy =
      test "{table }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (table (syntax heavy) (header ()) (data ()) (align ())))))
         (warnings ())) |}]

    let empty_row =
      test "{table {tr } }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 14))
            (table (syntax heavy) (header ()) (data ()) (align ())))))
         (warnings ()))|}]

    let no_header =
      test "{table {tr {td}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 17))
            (table (syntax heavy) (header ()) (data ((row ((cell ()))))) (align ())))))
         (warnings ())) |}]

    let no_data =
      test "{table {tr {th}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 17))
            (table (syntax heavy) (header ((cell ()))) (data ()) (align (center))))))
         (warnings ())) |}]

    let multiple_headers =
      test "{table {tr {th}} {tr {th}} {tr {td}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 37))
            (table (syntax heavy) (header ((cell ())))
             (data ((row ((cell ()))) (row ((cell ()))))) (align (center))))))
         (warnings ())) |}]

    let complex_table =
      test
        {|
        {table
          {tr
            {th xxx}
            {th yyy}
          }
          {tr
            {td aaaa bbb ccc {i ddd}
            }
            {td
               {table {tr {td}}}
            }
          }
          {tr
            {td
               - aaa
               - bbb
               - ccc
            }
            {td
              {t
                 x | y | z
                 --|---|--
                 1 | 2 | 3
              }
            }
          }
        }
        |};
      [%expect
        {|
        ((output
          (((f.ml (2 8) (28 9))
            (table (syntax heavy)
             (header
              ((cell
                (((f.ml (4 16) (4 19))
                  (paragraph (((f.ml (4 16) (4 19)) (word xxx)))))))
               (cell
                (((f.ml (5 16) (5 19))
                  (paragraph (((f.ml (5 16) (5 19)) (word yyy)))))))))
             (data
              ((row
                ((cell
                  (((f.ml (8 16) (8 36))
                    (paragraph
                     (((f.ml (8 16) (8 20)) (word aaaa)) ((f.ml (8 20) (8 21)) space)
                      ((f.ml (8 21) (8 24)) (word bbb)) ((f.ml (8 24) (8 25)) space)
                      ((f.ml (8 25) (8 28)) (word ccc)) ((f.ml (8 28) (8 29)) space)
                      ((f.ml (8 29) (8 36))
                       (italic (((f.ml (8 32) (8 35)) (word ddd))))))))))
                 (cell
                  (((f.ml (11 15) (11 32))
                    (table (syntax heavy) (header ()) (data ((row ((cell ())))))
                     (align ())))))))
               (row
                ((cell
                  (((f.ml (16 15) (18 20))
                    (unordered light
                     ((((f.ml (16 17) (16 20))
                        (paragraph (((f.ml (16 17) (16 20)) (word aaa))))))
                      (((f.ml (17 17) (17 20))
                        (paragraph (((f.ml (17 17) (17 20)) (word bbb))))))
                      (((f.ml (18 17) (18 20))
                        (paragraph (((f.ml (18 17) (18 20)) (word ccc)))))))))))
                 (cell
                  (((f.ml (21 14) (25 15))
                    (table (syntax light)
                     (header
                      ((cell (((f.ml (22 17) (22 18)) (word x))))
                       (cell (((f.ml (22 21) (22 22)) (word y))))
                       (cell (((f.ml (22 25) (22 26)) (word z))))))
                     (data
                      ((row
                        ((cell (((f.ml (24 17) (24 18)) (word 1))))
                         (cell (((f.ml (24 21) (24 22)) (word 2))))
                         (cell (((f.ml (24 25) (24 26)) (word 3))))))))
                     (align (center center center))))))))))
             (align (center center))))))
         (warnings ())) |}]

    let align =
      test
        {|
        {table
          {tr
            {th {L a}}
            {th {C b}}
            {th {R c}}
          }
        }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 8) (8 9))
            (table (syntax heavy)
             (header
              ((cell
                (((f.ml (4 19) (4 20)) (paragraph (((f.ml (4 19) (4 20)) (word a)))))))
               (cell
                (((f.ml (5 19) (5 20)) (paragraph (((f.ml (5 19) (5 20)) (word b)))))))
               (cell
                (((f.ml (6 19) (6 20)) (paragraph (((f.ml (6 19) (6 20)) (word c)))))))))
             (data ()) (align (left center right))))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Light = struct
    let empty_table_light =
      test "{t }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4))
            (table (syntax light) (header ()) (data ()) (align ())))))
         (warnings ())) |}]

    let simple =
      test {|
        {t
          | a |
        }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 8) (4 9))
            (table (syntax light) (header ())
             (data ((row ((cell (((f.ml (3 12) (3 13)) (word a)))))))) (align ())))))
         (warnings ())) |}]

    let stars =
      test
        {|
        {t
          |a|   *b*|
          |*c| d* |
        }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 8) (5 9))
            (table (syntax light) (header ())
             (data
              ((row
                ((cell (((f.ml (3 11) (3 12)) (word a))))
                 (cell (((f.ml (3 16) (3 19)) (word *b*))))))
               (row
                ((cell (((f.ml (4 11) (4 13)) (word *c))))
                 (cell (((f.ml (4 15) (4 17)) (word d*))))))))
             (align ())))))
         (warnings ())) |}]

    let backquotes =
      test {|
      {t
         | `a |`
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (4 7))
            (table (syntax light) (header ())
             (data
              ((row
                ((cell (((f.ml (3 11) (3 13)) (word `a))))
                 (cell (((f.ml (3 15) (3 16)) (word `))))))))
             (align ())))))
         (warnings ())) |}]

    let no_header =
      test {|
      {t
       |---|---|
       | x | y |
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (5 7))
            (table (syntax light) (header ())
             (data
              ((row
                ((cell (((f.ml (4 9) (4 10)) (word x))))
                 (cell (((f.ml (4 13) (4 14)) (word y))))))))
             (align (center center))))))
         (warnings ())) |}]

    let no_align =
      test {|
      {t
       | x | y |
       | x | y |
      }
      |};
      [%expect
        {|
          ((output
            (((f.ml (2 6) (5 7))
              (table (syntax light) (header ())
               (data
                ((row
                  ((cell (((f.ml (3 9) (3 10)) (word x))))
                   (cell (((f.ml (3 13) (3 14)) (word y))))))
                 (row
                  ((cell (((f.ml (4 9) (4 10)) (word x))))
                   (cell (((f.ml (4 13) (4 14)) (word y))))))))
               (align ())))))
           (warnings ())) |}]

    let only_align =
      test {|
      {t
        |--|--|
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (4 7))
            (table (syntax light) (header ()) (data ()) (align (center center))))))
         (warnings ())) |}]

    let no_data =
      test {|
      {t
       | x | y |
       |---|---|
      }
      |};
      [%expect
        {|
          ((output
            (((f.ml (2 6) (5 7))
              (table (syntax light)
               (header
                ((cell (((f.ml (3 9) (3 10)) (word x))))
                 (cell (((f.ml (3 13) (3 14)) (word y))))))
               (data ()) (align (center center))))))
           (warnings ())) |}]

    let alignment =
      test
        {|
      {t
       | a | b | c | d |
       |---|:--|--:|:-:|
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (5 7))
            (table (syntax light)
             (header
              ((cell (((f.ml (3 9) (3 10)) (word a))))
               (cell (((f.ml (3 13) (3 14)) (word b))))
               (cell (((f.ml (3 17) (3 18)) (word c))))
               (cell (((f.ml (3 21) (3 22)) (word d))))))
             (data ()) (align (center left right center))))))
         (warnings ())) |}]

    let no_bars =
      test
        {|
      {t
        a | b | c | d
       ---|:--|--:|:-:
        a | b | c | d
      }
      |};
      [%expect
        {|
          ((output
            (((f.ml (2 6) (6 7))
              (table (syntax light)
               (header
                ((cell (((f.ml (3 8) (3 9)) (word a))))
                 (cell (((f.ml (3 12) (3 13)) (word b))))
                 (cell (((f.ml (3 16) (3 17)) (word c))))
                 (cell (((f.ml (3 20) (3 21)) (word d))))))
               (data
                ((row
                  ((cell (((f.ml (5 8) (5 9)) (word a))))
                   (cell (((f.ml (5 12) (5 13)) (word b))))
                   (cell (((f.ml (5 16) (5 17)) (word c))))
                   (cell (((f.ml (5 20) (5 21)) (word d))))))))
               (align (center left right center))))))
           (warnings ())) |}]

    let light_table_new_lines =
      test
        {|
      {t

       | a | b | c | d |

       |---|---|---|---|

       | a | b | c | d |

      }
      |};
      [%expect
        {|
          ((output
            (((f.ml (2 6) (10 7))
              (table (syntax light)
               (header
                ((cell (((f.ml (4 9) (4 10)) (word a))))
                 (cell (((f.ml (4 13) (4 14)) (word b))))
                 (cell (((f.ml (4 17) (4 18)) (word c))))
                 (cell (((f.ml (4 21) (4 22)) (word d))))))
               (data
                ((row
                  ((cell (((f.ml (8 9) (8 10)) (word a))))
                   (cell (((f.ml (8 13) (8 14)) (word b))))
                   (cell (((f.ml (8 17) (8 18)) (word c))))
                   (cell (((f.ml (8 21) (8 22)) (word d))))))))
               (align (center center center center))))))
           (warnings ())) |}]

    let light_table_markup =
      test
        {|
      {t
       | {i a} {:google.com} \t | | {m b} {e c} {% xyz %} | {b d} [foo] |
       |---|---|---|---|
      }
      |};
      [%expect
        {|
          ((output
            (((f.ml (2 6) (5 7))
              (table (syntax light)
               (header
                ((cell
                  (((f.ml (3 9) (3 14)) (italic (((f.ml (3 12) (3 13)) (word a)))))
                   ((f.ml (3 15) (3 28)) (google.com ()))
                   ((f.ml (3 29) (3 31)) (word "\\t"))))
                 (cell ())
                 (cell
                  (((f.ml (3 36) (3 41)) (math_span b))
                   ((f.ml (3 42) (3 47)) (emphasis (((f.ml (3 45) (3 46)) (word c)))))
                   ((f.ml (3 48) (3 57)) (raw_markup () " xyz "))))
                 (cell
                  (((f.ml (3 60) (3 65)) (bold (((f.ml (3 63) (3 64)) (word d)))))
                   ((f.ml (3 66) (3 71)) (code_span foo))))))
               (data ()) (align (center center center center))))))
           (warnings ())) |}]

    let no_space =
      test
        {|
       {t
         | a | b |c| d |
         |---|--:|:--|:-:|
       }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 7) (5 8))
            (table (syntax light)
             (header
              ((cell (((f.ml (3 11) (3 12)) (word a))))
               (cell (((f.ml (3 15) (3 16)) (word b))))
               (cell (((f.ml (3 18) (3 19)) (word c))))
               (cell (((f.ml (3 21) (3 22)) (word d))))))
             (data ()) (align (center right left center))))))
         (warnings ())) |}]

    let multiple_headers =
      test
        {|
      {t
       ||a|b|
       |:-|---:|
       |c|d|
       |cc|dd|
       |-:|:-:|
       |e|f|
       |g|h||
      }
      |};
      [%expect
        {|
        ((output
          (((f.ml (2 6) (10 7))
            (table (syntax light)
             (header
              ((cell ()) (cell (((f.ml (3 9) (3 10)) (word a))))
               (cell (((f.ml (3 11) (3 12)) (word b))))))
             (data
              ((row
                ((cell (((f.ml (5 8) (5 9)) (word c))))
                 (cell (((f.ml (5 10) (5 11)) (word d))))))
               (row
                ((cell (((f.ml (6 8) (6 10)) (word cc))))
                 (cell (((f.ml (6 11) (6 13)) (word dd))))))
               (row
                ((cell (((f.ml (7 8) (7 10)) (word -:))))
                 (cell (((f.ml (7 11) (7 14)) (word :-:))))))
               (row
                ((cell (((f.ml (8 8) (8 9)) (word e))))
                 (cell (((f.ml (8 10) (8 11)) (word f))))))
               (row
                ((cell (((f.ml (9 8) (9 9)) (word g))))
                 (cell (((f.ml (9 10) (9 11)) (word h)))) (cell ())))))
             (align (left right))))))
         (warnings ())) |}]

    let block_element_in_cell =
      test
        {|
           {t
           | {[ a ]} | b |
           |---|---|
           }
          |};
      [%expect
        {|
        ((output
          (((f.ml (2 11) (5 12))
            (table (syntax light)
             (header ((cell ()) (cell (((f.ml (3 23) (3 24)) (word b)))))) (data ())
             (align (center center))))))
         (warnings
          ( "File \"f.ml\", line 3, characters 13-20:\
           \n'{[...]}' (code block) is not allowed in '{t ...}' (table)."))) |}]

    let block_element_in_row =
      test
        {|
           {t
           {[ a ]}
           | a | b |
           |---|---|
           }
          |};
      [%expect
        {|
        ((output
          (((f.ml (2 11) (6 12))
            (table (syntax light)
             (header
              ((cell (((f.ml (4 13) (4 14)) (word a))))
               (cell (((f.ml (4 17) (4 18)) (word b))))))
             (data ()) (align (center center))))))
         (warnings
          ( "File \"f.ml\", line 3, characters 11-18:\
           \n'{[...]}' (code block) is not allowed in '{t ...}' (table)."))) |}]
  end in
  ()