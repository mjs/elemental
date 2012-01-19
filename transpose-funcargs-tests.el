(defun plain-thing-at-point (thing-type)
  "Like thing-at-point, but strip out any text properties"
  (let ((thing (thing-at-point thing-type)))
      (set-text-properties 0 (length thing) nil thing)
      thing))

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

(ert-deftest test1 ()
  (test-in-buffer "foo(|abc, def)"
                  'forward-one-funcarg
                  "foo(abc|, def)"))
