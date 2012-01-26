(require 'ert)
(require 'transpose-funcargs)

; XXX multi-line tests

;; forward-one-funcarg

(ert-deftest test-move-to-end-of-arg ()
  (test-in-buffer "foo(|abc, def)"
                  'forward-one-funcarg
                  "foo(abc|, def)"))

(ert-deftest test-move-to-next-arg ()
  (test-in-buffer "foo(abc|, def)"
                  'forward-one-funcarg
                  "foo(abc, def|)"))

(ert-deftest test-attempt-to-move-past-end ()
  (test-in-buffer "foo(abc, def|)"
                  'forward-one-funcarg
                  "foo(abc, def|)"))

(ert-deftest test-move-to-next-arg-when-starting-in-string ()
  (test-in-buffer "foo(\"th|at, thing\", 123)"
                  'forward-one-funcarg
                  "foo(\"that, thing\"|, 123)"))

(ert-deftest test-move-to-next-arg-with-trailing-space ()
  (test-in-buffer "foo(abc|, def )"
                  'forward-one-funcarg
                  "foo(abc, def| )"))

(ert-deftest test-move-to-next-arg-inside-brackets ()
  (test-in-buffer "[abc|, 123, haha ]"
                  'forward-one-funcarg
                  "[abc, 123|, haha ]"))

(ert-deftest test-move-to-next-arg-inside-braces ()
  (test-in-buffer "{abc|, 123 , haha}"
                  'forward-one-funcarg
                  "{abc, 123| , haha}"))

(ert-deftest test-move-to-end-of-arg-complex ()
  (test-in-buffer "foo(123, |yyy =\"a,b\")"
                  'forward-one-funcarg
                  "foo(123, yyy =\"a,b\"|)"))

(ert-deftest test-move-to-next-arg-complex ()
  (test-in-buffer "foo(123|, yyy=\"a,b\")"
                  'forward-one-funcarg
                  "foo(123, yyy=\"a,b\"|)"))

(ert-deftest test-move-to-next-arg-complex ()
  (test-in-buffer "foo(123|, yyy =\"a,b\")"
                  'forward-one-funcarg
                  "foo(123, yyy =\"a,b\"|)"))

(ert-deftest test-when-inside-buffer ()
  (test-in-buffer "foo(|abc, def)"
                  'forward-one-funcarg
                  "foo(abc|, def)"))

(ert-deftest test-move-forward-when-not-in-parens ()
  "Nothing should happen when outside parens/brackets/braces"
  (test-in-buffer "f|oo bah"
                  'forward-one-funcarg
                  "f|oo bah"))


;; backward-one-funcarg

(ert-deftest test-move-back-to-start-of-arg ()
  (test-in-buffer "foo(abc, def|)"
                  'backward-one-funcarg
                  "foo(abc, |def)"))

(ert-deftest test-move-to-previous-arg ()
  (test-in-buffer "foo(abc, |def)"
                  'backward-one-funcarg
                  "foo(|abc, def)"))

(ert-deftest test-move-to-previous-arg-with-leading-space ()
  (test-in-buffer "foo( abc, |def)"
                  'backward-one-funcarg
                  "foo( |abc, def)"))

(ert-deftest test-move-to-start-of-arg-when-starting-in-string ()
  (test-in-buffer "foo(321, \"th|at, thing\", 123)"
                  'backward-one-funcarg
                  "foo(321, |\"that, thing\", 123)"))

(ert-deftest test-move-to-start-of-arg-when-starting-in-string-with-kwarg ()
  (test-in-buffer "foo(meh=\"th|at, thing\", 123)"
                  'backward-one-funcarg
                  "foo(|meh=\"that, thing\", 123)"))

(ert-deftest test-move-to-previous-arg-inside-brackets ()
  (test-in-buffer "[abc, |123, haha ]"
                  'backward-one-funcarg
                  "[|abc, 123, haha ]"))

(ert-deftest test-move-to-previous-arg-inside-braces ()
  (test-in-buffer "{abc, |123 , haha}"
                  'backward-one-funcarg
                  "{|abc, 123 , haha}"))

(ert-deftest test-move-to-beginning-of-arg-complex ()
  (test-in-buffer "foo(123, yyy =\"a,b\"|)"
                  'backward-one-funcarg
                  "foo(123, |yyy =\"a,b\")"))

(ert-deftest test-move-to-previous-arg-complex ()
  (test-in-buffer "foo(123, |yyy =\"a,b\")"
                  'backward-one-funcarg
                  "foo(|123, yyy =\"a,b\")"))

(ert-deftest test-move-backward-when-not-in-parens ()
  "Nothing should happen when outside parens/brackets/braces"
  (test-in-buffer "foo b|ah"
                  'backward-one-funcarg
                  "foo b|ah"))


;; forward-funcarg

(ert-deftest test-forward-funcarg-one ()
  (test-in-buffer "foo(|abc, def)"
                  '(lambda () (forward-funcarg 1))
                  "foo(abc|, def)"))

(ert-deftest test-forward-funcarg-two ()
  (test-in-buffer "foo(|abc, def)"
                  '(lambda () (forward-funcarg 2))
                  "foo(abc, def|)"))

(ert-deftest test-forward-funcarg-too-many ()
  (test-in-buffer "foo(|abc, def)"
                  '(lambda () (forward-funcarg 4))
                  "foo(abc, def|)"))

(ert-deftest test-forward-funcarg-negative ()
  (test-in-buffer "foo(abc, def|)"
                  '(lambda () (forward-funcarg -2))
                  "foo(|abc, def)"))

(ert-deftest test-forward-funcarg-negative-too-many ()
  (test-in-buffer "foo(abc, def|)"
                  '(lambda () (forward-funcarg -3))
                  "foo(|abc, def)"))

;; transpose-funcarg

(ert-deftest test-transpose-simple-forward ()
  (test-in-buffer "foo(|abc, def)"
                  '(lambda () (transpose-funcarg 1))
                  "foo(def, abc|)"))

(ert-deftest test-transpose-simple-forward-2 ()
  (test-in-buffer "foo(a|bc, def)"
                  '(lambda () (transpose-funcarg 1))
                  "foo(def, abc|)"))

(ert-deftest test-transpose-forward-with-ws ()
  (test-in-buffer "foo(  |aaa, bbb  ,  ccc,ddd  )"
                  '(lambda () (transpose-funcarg 1))
                  "foo(  bbb, aaa|  ,  ccc,ddd  )"))

(ert-deftest test-transpose-forward-with-ws-2 ()
  (test-in-buffer "foo(  bbb, aaa|  ,  ccc,ddd  )"
                  '(lambda () (transpose-funcarg 1))
                  "foo(  bbb, ccc  ,  aaa|,ddd  )"))

(ert-deftest test-transpose-simple-backward ()
  (test-in-buffer "foo(abc, def|)"
                  '(lambda () (transpose-funcarg -1))
                  "foo(def|, abc)"))

(ert-deftest test-transpose-simple-backward-2 ()
  (test-in-buffer "foo(abc, |def)"
                  '(lambda () (transpose-funcarg -1))
                  "foo(|def, abc)"))

(ert-deftest test-transpose-nested-inner ()
  (test-in-buffer "xxx(yyy(|123, zzz), 66)"
                  '(lambda () (transpose-funcarg 1))
                  "xxx(yyy(zzz, 123|), 66)"))

(ert-deftest test-transpose-nested-outer ()
  (test-in-buffer "xxx(y|yy(123, zzz), 66)"
                  '(lambda () (transpose-funcarg 1))
                  "xxx(66, yyy(123, zzz)|)"))

(ert-deftest test-transpose-inside-string-with-comma ()
  (test-in-buffer "foo(abc, \"de|f, smell\", thing=12, xxx)"
                  '(lambda () (transpose-funcarg 1))
                  "foo(abc, thing=12, \"def, smell\"|, xxx)"))

(defun test-in-buffer (before func-to-test after)
  (with-temp-buffer
    (insert before)
    (beginning-of-line)
    (search-forward "|")
    (backward-char)
    (delete-char 1)
    (funcall func-to-test)
    (insert "|")
    (should (string=
             (buffer-substring-no-properties (point-min) (point-max))
             after))))
