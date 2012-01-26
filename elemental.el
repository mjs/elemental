(defun forward-one-element ()
  (interactive)
  (unless (outside-parens?)
    (move-out-of-string-if-required)
    (condition-case nil
        (progn
          (forward-sexp)
          (while (not (looking-forward-ignoring-ws ","))
            (forward-sexp)))
      (scan-error nil))))

(defun backward-one-element ()
  (interactive)
  (unless (outside-parens?)
    (condition-case nil
        (progn
          (unless (move-out-of-string-if-required)
            (backward-sexp))
          (while (not (looking-back-ignoring-ws ","))
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
    (when (nth 3 pstate)
        (goto-char (nth 8 pstate)))))    ; in a string, move to start

(defun forward-element (arg)
  (interactive "p*")
  (if (>= arg 0)
      (dotimes (_ arg) (forward-one-element))
    (dotimes (_ (abs arg)) (backward-one-element))))

(defun transpose-element (arg)
  (interactive "*p")
  (transpose-subr 'forward-element arg))

(defun transpose-previous-element (arg)
  (interactive "*p")
  (transpose-subr 'forward-element (- arg)))

(provide 'elemental)
