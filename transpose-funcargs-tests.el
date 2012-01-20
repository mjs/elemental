(require 'ert)
(require 'transpose-funcargs)

; XXX test function calls inside function calls, e.g. xxx(zzz(123, yyy), 66)
; XXX test with crazy whitespace like: something(  aaa, bbb  ,  ccc,ddd  )
; XXX test when not inside parens
; XXX per mode tests (macro to help?)
; XXX single quotes don't work in fundamental mode (but should in Python mode)
; XXX multi-line tests
; XXX more tests at limits

;; XXX Python test data to use
;;
;; foo(abc, "def, smell", thing=12, xxx)
;; foo( abc, "def, smell", thing=12, xxx )
k

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

(ert-deftest test-move-to-end-of-arg-complex ()
  (test-in-buffer "foo(123, |yyy=\"a,b\")"
                  'forward-one-funcarg
                  "foo(123, yyy=\"a,b\"|)"))

(ert-deftest test-move-to-next-arg-complex ()
  (test-in-buffer "foo(123|, yyy=\"a,b\")"
                  'forward-one-funcarg
                  "foo(123, yyy=\"a,b\"|)"))

(ert-deftest test-move-to-next-arg-complex ()
  (test-in-buffer "foo(123|, yyy=\"a,b\")"
                  'forward-one-funcarg
                  "foo(123, yyy=\"a,b\"|)"))


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
  (test-in-buffer "foo(\"th|at, thing\", 123)"
                  'backward-one-funcarg
                  "foo(|\"that, thing\", 123)"))

(ert-deftest test-move-to-start-of-arg-when-starting-in-string-with-kwarg ()
  (test-in-buffer "foo(meh=\"th|at, thing\", 123)"
                  'backward-one-funcarg
                  "foo(|meh=\"that, thing\", 123)"))

(ert-deftest test-move-to-beginning-of-arg-complex ()
  (test-in-buffer "foo(123, yyy=\"a,b\"|)"
                  'backward-one-funcarg
                  "foo(123, |yyy=\"a,b\")"))

(ert-deftest test-move-to-previous-arg-complex ()
  (test-in-buffer "foo(123, |yyy=\"a,b\")"
                  'backward-one-funcarg
                  "foo(|123, yyy=\"a,b\")"))


;; forward-funcarg

(ert-deftest test-forward-funcarg-one ()
  (test-in-buffer "foo(|abc, def)"
                  '(lambda () (forward-funcarg 1))
                  "foo(abc|, def)"))

(ert-deftest test-forward-funcarg-two ()
  (test-in-buffer "foo(|abc, def)"
                  '(lambda () (forward-funcarg 2))
                  "foo(abc, def|)"))

(ert-deftest test-forward-funcarg-negative ()
  (test-in-buffer "foo(abc, def|)"
                  '(lambda () (forward-funcarg -2))
                  "foo(|abc, def)"))

; XXX test when arg is larger than number of function params to move through

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
  (test-in-buffer "foo(a|bc, def  )"
                  '(lambda () (transpose-funcarg 1))
                  "foo(def, abc|  )"))

(ert-deftest test-transpose-simple-backward ()
  (test-in-buffer "foo(abc, def|)"
                  '(lambda () (transpose-funcarg -1))
                  "foo(def|, abc)"))

(ert-deftest test-transpose-simple-backward-2 ()
  (test-in-buffer "foo(abc, |def)"
                  '(lambda () (transpose-funcarg -1))
                  "foo(|def, abc)"))


(defun test-in-buffer (before func-to-test after)
  (with-temp-buffer
    ;;XXX ideally we could test in various modes but the hooks get in the way, kill them first
    ;; (python-mode)
    (insert before)
    (beginning-of-line)
    (search-forward "|")
    (backward-char)
    (delete-char 1)
    (funcall func-to-test)
    (insert "|")
    (should (string= (plain-thing-at-point 'line) after))))

(defun plain-thing-at-point (thing-type)
  "Like thing-at-point, but strip out any text properties"
  (let ((thing (thing-at-point thing-type)))
      (set-text-properties 0 (length thing) nil thing)
      thing))

; XXX - hack - there must be a better way of doing this
(defun run-tests-in-this-file ()
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert t))
