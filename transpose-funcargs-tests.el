(require 'ert)
(require 'transpose-funcargs)

; XXX with point inside an arg
; XXX with point inside a string arg
; XXX nested parens
; XXX single quotes don't work in fundamental mode (but should in Python mode)
; XXX multi-line tests
; XXX more tests at limits
; XXX test when not inside parens

;; forward-one-funcarg

(ert-deftest test-move-to-end-of-arg ()
  (test-in-buffer "foo(|abc, def)"
                  'forward-one-funcarg
                  "foo(abc|, def)"))

(ert-deftest test-move-to-next-arg ()
  (test-in-buffer "foo(abc|, def)"
                  'forward-one-funcarg
                  "foo(abc, def|)"))

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

; XXX - hack
(defun run-tests-in-this-file ()
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert t))
