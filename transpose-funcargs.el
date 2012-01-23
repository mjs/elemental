; Useful things:
; char-after
; char-syntax
; skip-syntax-forward / backward

(defun forward-one-funcarg ()
  (interactive)
  (unless (outside-parens?)
    (move-out-of-string-if-required)
    (condition-case nil
        (progn
          (forward-sexp)
          (while (not (looking-at "[,) ]"))
            (forward-sexp)))
      (scan-error nil))))

(defun backward-one-funcarg ()
  (interactive)
  (unless (outside-parens?)
    (condition-case nil
        (progn
          (unless (move-out-of-string-if-required)
            (backward-sexp))
          (while (not (looking-back "[,( ]"))
            (backward-sexp)))
      (scan-error nil))))

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
