;; -*- lexical-binding: t -*-

;; `el-patch-unit-tests' - unit tests using Buttercup.
;;
;; Setup originally stolen from Apheleia and modified.

(require 'el-patch)
(require 'buttercup)

(require 'cl-lib)

(describe "el-patch--resolve"
  (cl-macrolet ((testcases
                 (description &rest specs)
                 `(it ,description
                    ,@(mapcan
                       (lambda (spec)
                         (cl-destructuring-bind (input old new) spec
                           `((expect (el-patch--resolve ',input nil)
                                     :to-equal '(,old))
                             (expect (el-patch--resolve ',input t)
                                     :to-equal '(,new)))))
                       specs))))
    (testcases
     "does no-ops when no patch directives used"

     ((foo bar baz)
      (foo bar baz)
      (foo bar baz))

     ([foo bar baz]
      [foo bar baz]
      [foo bar baz])

     ((oh my how . improper)
      (oh my how . improper)
      (oh my how . improper))

     )
    (testcases
     "handles el-patch-add and el-patch-remove"

     ((foo (el-patch-add bar) baz)
      (foo baz)
      (foo bar baz))

     ([foo (el-patch-add bar) baz]
      [foo baz]
      [foo bar baz])

     ((foo (el-patch-add bar baz) quux)
      (foo quux)
      (foo bar baz quux))

     (((el-patch-add foo) bar baz)
      (bar baz)
      (foo bar baz))

     ((foo bar (el-patch-add baz))
      (foo bar)
      (foo bar baz))

     (((el-patch-add foo) bar (el-patch-add baz))
      (bar)
      (foo bar baz))

     (((el-patch-add foo))
      ()
      (foo))

     (((el-patch-add foo) bar (el-patch-remove baz))
      (bar baz)
      (foo bar))

     )
    (testcases
     "handles el-patch-concat"

     ((el-patch-concat "foo" "bar")
      "foobar"
      "foobar")

     ((el-patch-concat "foo" (el-patch-add "bar") "baz")
      "foobaz"
      "foobarbaz")

     ((foo (el-patch-concat "test" (el-patch-swap "1" "2")) bar)
      (foo "test1" bar)
      (foo "test2" bar))

     )))
