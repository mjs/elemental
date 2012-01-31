(require 'ert)
(require 'elemental)


;; elem-forward-one

(ert-deftest test-move-to-end-of-arg ()
  (elem-test "foo(|abc, def)"
             'elem-forward-one
             "foo(abc|, def)"))

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

(defun elem-test (before func-to-test after)
  (with-temp-buffer
    (insert before)
    (beginning-of-buffer)
    (search-forward "|")
    (backward-char)
    (delete-char 1)
    (funcall func-to-test)
    (insert "|")
    (should (string=
             (buffer-substring-no-properties (point-min) (point-max))
             after))))
