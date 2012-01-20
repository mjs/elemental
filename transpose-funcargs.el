; Useful things:
; char-after
; char-syntax
; skip-syntax-forward / backward

(defun forward-one-funcarg ()
  (interactive)
  (move-one-funcarg 'forward-sexp (lambda () (looking-at "[,) ]"))))

(defun backward-one-funcarg ()
  (interactive)
  (move-one-funcarg 'backward-sexp (lambda () (looking-back "[,( ]"))))

(defun move-one-funcarg (move-func looking-func)
  (unless (outside-parens?)
    (condition-case nil
        (progn
          (funcall move-func)
          (while (not (funcall looking-func))
            (funcall move-func)))
      (scan-error nil))))

(defun outside-parens? ()
  (eq (car (syntax-ppss)) 0))

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
