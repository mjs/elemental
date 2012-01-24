(defun forward-one-funcarg ()
  (interactive)
  (unless (outside-parens?)
    (move-out-of-string-if-required)
    (condition-case nil
        (progn
          (forward-sexp)
          (while (not (looking-forward-ignoring-ws "[,)]"))
            (forward-sexp)))
      (scan-error nil))))

(defun backward-one-funcarg ()
  (interactive)
  (unless (outside-parens?)
    (condition-case nil
        (progn
          (unless (move-out-of-string-if-required)
            (backward-sexp))
          (while (not (looking-back-ignoring-ws "[,(]"))
            (backward-sexp)))
      (scan-error nil))))

(defun looking-forward-ignoring-ws (regex)
  (save-excursion
    (skip-ws 'char-after 'forward-char)
    (looking-at regex)))

(defun looking-back-ignoring-ws (regex)
  (save-excursion
    (skip-ws 'char-before 'backward-char)
    (looking-back regex)))

(defun skip-ws (look-func move-func)
  (while (= (char-syntax (funcall look-func)) 32)
    (funcall move-func)))

(defun outside-parens? ()
  (<= (car (syntax-ppss)) 0))

(defun move-out-of-string-if-required ()
  (let ((pstate (syntax-ppss)))
    (if (nth 3 pstate)
        (goto-char (nth 8 pstate))    ; in a string, move to start
      nil)))

(defun forward-funcarg (arg)
  (interactive "p*")
  (if (>= arg 0)
      (dotimes (_ arg) (forward-one-funcarg))
    (dotimes (_ (abs arg)) (backward-one-funcarg))))

(defun transpose-funcarg (arg)
  (interactive "*p")
  (transpose-subr 'forward-funcarg arg))

(defun transpose-previous-funcarg (arg)
  (interactive "*p")
  (transpose-subr 'forward-funcarg (- arg)))

(provide 'transpose-funcargs)
