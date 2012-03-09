;; Unit tests for elemental.el
;;
;; These tests use the ERT framework which ships with Emacs 24.
;;
;; To run the tests, eval this buffer and run the ert function. For
;; more information see the ERT info page.

(require 'ert)
(require 'elemental)

(require 'cc-mode)

(defun elem-test (before func-to-test after &optional mode-func)
  "Run an elemental unit test

  Sets up a temporary buffer with the 'before' text, runs func-to-test
  and checks that the buffer looks like the 'after' text. 'mode-func'
  is an optional function to call to set the mode of the buffer."
  (with-temp-buffer
    (insert before)
    (beginning-of-buffer)
    (search-forward "|")
    (backward-char)
    (delete-char 1)
    (when mode-func
      (funcall mode-func)
      (font-lock-fontify-buffer))
    (if (functionp func-to-test)
        (funcall func-to-test)
      (eval func-to-test))
    (insert "|")
    (should (string=
             (buffer-substring-no-properties (point-min) (point-max))
             after))))

(defmacro elem-deftest (name before test-func after)
  (declare (indent 4))
  `(ert-deftest ,(intern (format "elem-test-%s" name)) ()
    (elem-test ,before (quote ,test-func) ,after)))


;; elem-forward-one

(elem-deftest move-to-end-of-arg
    "foo(|abc, def)"
    elem-forward-one
    "foo(abc|, def)")

(ert-deftest test-move-to-next-arg ()
  (elem-test "foo(abc|, def)"
             'elem-forward-one
             "foo(abc, def|)"))

(ert-deftest test-attempt-to-move-past-end ()
  (elem-test "foo(abc, def|)"
             'elem-forward-one
             "foo(abc, def|)"))

(ert-deftest test-move-to-next-arg-when-starting-in-string ()
  (elem-test "foo(\"th|at, thing\", 123)"
             'elem-forward-one
             "foo(\"that, thing\"|, 123)"))

(ert-deftest test-move-to-next-arg-with-trailing-space ()
  (elem-test "foo(abc|, def )"
             'elem-forward-one
             "foo(abc, def| )"))

(ert-deftest test-move-to-next-arg-inside-brackets ()
  (elem-test "[abc|, 123, haha ]"
             'elem-forward-one
             "[abc, 123|, haha ]"))

(ert-deftest test-move-to-next-arg-inside-braces ()
  (elem-test "{abc|, 123 , haha}"
             'elem-forward-one
             "{abc, 123| , haha}"))

(ert-deftest test-move-to-end-of-arg-complex ()
  (elem-test "foo(123, |yyy =\"a,b\")"
             'elem-forward-one
             "foo(123, yyy =\"a,b\"|)"))

(ert-deftest test-move-to-next-arg-complex ()
  (elem-test "foo(123|, yyy=\"a,b\")"
             'elem-forward-one
             "foo(123, yyy=\"a,b\"|)"))

(ert-deftest test-move-to-next-arg-complex ()
  (elem-test "foo(123|, yyy =\"a,b\")"
             'elem-forward-one
             "foo(123, yyy =\"a,b\"|)"))

(ert-deftest test-when-inside-buffer ()
  (elem-test "foo(|abc, def)"
             'elem-forward-one
             "foo(abc|, def)"))

(ert-deftest test-move-forward-when-not-in-parens ()
  "Nothing should happen when outside parens/brackets/braces"
  (elem-test "f|oo bah"
             'elem-forward-one
             "f|oo bah"))

(ert-deftest test-move-forward-multi-line-0 ()
  (elem-test "[|foo,\n   \"second, actually\", \n123  ]"
             'elem-forward-one
             "[foo|,\n   \"second, actually\", \n123  ]"))

(ert-deftest test-move-forward-multi-line-1 ()
  (elem-test "[foo|,\n   \"second, actually\", \n123  ]"
             'elem-forward-one
             "[foo,\n   \"second, actually\"|, \n123  ]"))

(ert-deftest test-move-forward-multi-line-2 ()
  (elem-test "[foo,\n   \"second, actually\"|, \n123  ]"
             'elem-forward-one
             "[foo,\n   \"second, actually\", \n123|  ]"))

(ert-deftest test-c++-move-forward-with-comments ()
  (elem-test
   "
   x = [foo|,                    /* another, comment */
        \"second, actually\",   // comment
        123 ];
   "
   'elem-forward-one
   "
   x = [foo,                    /* another, comment */
        \"second, actually\"|,   // comment
        123 ];
   "
   'c++-mode))

(ert-deftest test-c++-move-forward-with-comments-2 ()
  (elem-test
   "
   x = [foo,                    /* another, comment */
        \"second, actually\"|,   // comment
        123 ];
   "
   'elem-forward-one
   "
   x = [foo,                    /* another, comment */
        \"second, actually\",   // comment
        123| ];
   "
   'c++-mode))

;; elem-backward-one

(ert-deftest test-move-back-to-start-of-arg ()
  (elem-test "foo(abc, def|)"
             'elem-backward-one
             "foo(abc, |def)"))

(ert-deftest test-move-to-previous-arg ()
  (elem-test "foo(abc, |def)"
             'elem-backward-one
             "foo(|abc, def)"))

(ert-deftest test-move-to-previous-arg-with-leading-space ()
  (elem-test "foo( abc, |def)"
             'elem-backward-one
             "foo( |abc, def)"))

(ert-deftest test-move-to-start-of-arg-when-starting-in-string ()
  (elem-test "foo(321, \"th|at, thing\", 123)"
             'elem-backward-one
             "foo(321, |\"that, thing\", 123)"))

(ert-deftest test-move-to-start-of-arg-when-starting-in-string-with-kwarg ()
  (elem-test "foo(meh=\"th|at, thing\", 123)"
             'elem-backward-one
             "foo(|meh=\"that, thing\", 123)"))

(ert-deftest test-move-to-previous-arg-inside-brackets ()
  (elem-test "[abc, |123, haha ]"
             'elem-backward-one
             "[|abc, 123, haha ]"))

(ert-deftest test-move-to-previous-arg-inside-braces ()
  (elem-test "{abc, |123 , haha}"
             'elem-backward-one
             "{|abc, 123 , haha}"))

(ert-deftest test-move-to-beginning-of-arg-complex ()
  (elem-test "foo(123, yyy =\"a,b\"|)"
             'elem-backward-one
             "foo(123, |yyy =\"a,b\")"))

(ert-deftest test-move-to-previous-arg-complex ()
  (elem-test "foo(123, |yyy =\"a,b\")"
             'elem-backward-one
             "foo(|123, yyy =\"a,b\")"))

(ert-deftest test-move-backward-when-not-in-parens ()
  "Nothing should happen when outside parens/brackets/braces"
  (elem-test "foo b|ah"
             'elem-backward-one
             "foo b|ah"))

(ert-deftest test-move-backward-multi-line-0 ()
  (elem-test "[foo,\n   \"second, actually\", \n123|  ]"
             'elem-backward-one
             "[foo,\n   \"second, actually\", \n|123  ]"))

(ert-deftest test-move-backward-multi-line-1 ()
  (elem-test "[foo,\n   \"second, actually\", \n|123  ]"
             'elem-backward-one
             "[foo,\n   |\"second, actually\", \n123  ]"))

(ert-deftest test-move-backward-multi-line-2 ()
  (elem-test "[foo,\n   |\"second, actually\", \n123  ]"
             'elem-backward-one
             "[|foo,\n   \"second, actually\", \n123  ]"))

(ert-deftest test-c++-move-backwards-with-comments ()
  (elem-test
   "
   x = [foo,                    /* another, comment */
        \"second, actually\",   // comment
        |123 ];
   "
   'elem-backward-one
   "
   x = [foo,                    /* another, comment */
        |\"second, actually\",   // comment
        123 ];
   "
   'c++-mode))

(ert-deftest test-c++-move-backwards-with-comments-2 ()
  (elem-test
   "
   x = [foo,                    /* another, comment */
        |\"second, actually\",   // comment
        123 ];
   "
   'elem-backward-one
   "
   x = [|foo,                    /* another, comment */
        \"second, actually\",   // comment
        123 ];
   "
   'c++-mode))

;; elem-forward

(ert-deftest test-elem-forward-one ()
  (elem-test "foo(|abc, def)"
             '(lambda () (elem-forward 1))
             "foo(abc|, def)"))

(ert-deftest test-elem-forward-two ()
  (elem-test "foo(|abc, def)"
             '(lambda () (elem-forward 2))
             "foo(abc, def|)"))

(ert-deftest test-elem-forward-too-many ()
  (elem-test "foo(|abc, def)"
             '(lambda () (elem-forward 4))
             "foo(abc, def|)"))

(ert-deftest test-elem-forward-negative ()
  (elem-test "foo(abc, def|)"
             '(lambda () (elem-forward -2))
             "foo(|abc, def)"))

(ert-deftest test-elem-forward-negative-too-many ()
  (elem-test "foo(abc, def|)"
             '(lambda () (elem-forward -3))
             "foo(|abc, def)"))

;; elem-transpose

(ert-deftest test-transpose-simple-forward ()
  (elem-test "foo(|abc, def)"
             '(lambda () (elem-transpose 1))
             "foo(def, abc|)"))

(ert-deftest test-transpose-simple-forward-2 ()
  (elem-test "foo(a|bc, def)"
             '(lambda () (elem-transpose 1))
             "foo(def, abc|)"))

(ert-deftest test-transpose-forward-with-ws ()
  (elem-test "foo(  |aaa, bbb  ,  ccc,ddd  )"
             '(lambda () (elem-transpose 1))
             "foo(  bbb, aaa|  ,  ccc,ddd  )"))

(ert-deftest test-transpose-forward-with-ws-2 ()
  (elem-test "foo(  bbb, aaa|  ,  ccc,ddd  )"
             '(lambda () (elem-transpose 1))
             "foo(  bbb, ccc  ,  aaa|,ddd  )"))

(ert-deftest test-transpose-simple-backward ()
  (elem-test "foo(abc, def|)"
             '(lambda () (elem-transpose -1))
             "foo(def|, abc)"))

;; FIXME: doesn't work as expected but transpose-words doesn't work
;; well in this situation either
;; (ert-deftest test-transpose-simple-backward-2 ()
;;   (elem-test "foo(abc, |def)"
;;              '(lambda () (elem-transpose -1))
;;              "foo(|def, abc)"))

(ert-deftest test-transpose-nested-inner ()
  (elem-test "xxx(yyy(|123, zzz), 66)"
             '(lambda () (elem-transpose 1))
             "xxx(yyy(zzz, 123|), 66)"))

(ert-deftest test-transpose-nested-outer ()
  (elem-test "xxx(y|yy(123, zzz), 66)"
             '(lambda () (elem-transpose 1))
             "xxx(66, yyy(123, zzz)|)"))

(ert-deftest test-transpose-inside-string-with-comma ()
  (elem-test "foo(abc, \"de|f, smell\", thing=12, xxx)"
             '(lambda () (elem-transpose 1))
             "foo(abc, thing=12, \"def, smell\"|, xxx)"))

(ert-deftest test-c++-mode-transpose-0 ()
  (elem-test
   "
   x = [foo|,    /* another, comment */
        \"second, actually\",   // comment
        123 ];
   "
   '(lambda () (elem-transpose 1))
   "
   x = [\"second, actually\",    /* another, comment */
        foo|,   // comment
        123 ];
   "
   'c++-mode))

(ert-deftest test-c++-mode-transpose-1 ()
  (elem-test
   "
   x = [foo,    /* another, comment */
        \"seco|nd, actually\",   // comment
        123 ];
   "
   '(lambda () (elem-transpose 1))
   "
   x = [foo,    /* another, comment */
        123,   // comment
        \"second, actually\"| ];
   "
   'c++-mode))
