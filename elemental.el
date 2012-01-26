;; elemental.el -- functions for intelligently jumping between and
;; transposing list/tuple/dictionary/function-parameter
;; elements. These functions are mainly useful when editing software
;; source code.
;;
;; Copyright (C) 2012 Menno Smits
;;
;; Author: Menno Smits <menno@freshfoo.com>
;; Version: 0.1
;; URL: https://bitbucket.org/mjs0/elemental/
;;
;; This file is not part of GNU Emacs.

;; Description:
;;
;; It is difficult to efficiently navigate and manipulate list
;; elements and function parameter lists, especially when string
;; literals and nested function calls are involved. The functions in
;; this module make this process easier.
;;
;; The main functions of interest are: forward-one-element,
;; backward-one-element, forward-element and transpose-element.
;;
;; Given a source code line, here's how the point moves with success
;; calls to forward-element:
;;     something(|123, "some, string", fn(a, b))
;;     something(123|, "some, string", fn(a, b))
;;     something(123, "some, string"|, fn(a, b))
;;     something(123, "some, string", fn(a, b)|)

;; XXX finish

;; Installation:
;;



;; Limitations:


;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.


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
