;;; expatpt.el --- Get arithmetic expression at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Package-Requires: ((parsec "0.1"))
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Junpeng Qiu


;; Table of Contents
;; _________________

;; 1 expatpt
;; 2 Command: `expatpt'
;; 3 Function: `expatpt-grab'
;; 4 The Supported Arithmetic Expression Syntax
;; 5 TODO TODO
;; 6 Dependencies


;; 1 expatpt
;; =========

;;   expatpt: arithmetic EXPression AT PoinT

;;   Grab the arithmetic expression around the cursor.

;;   This package makes use of [parsec.el].


;; [parsec.el] https://github.com/cute-jumper/parsec.el


;; 2 Command: `expatpt'
;; ====================

;;   This command grabs a *valid* arithmetic expression around the cursor,
;;   evaluates the expression, puts the result in the `kill-ring' and shows
;;   it in the echo area.

;;   Example ( `|' indicates the cursor position):
;;   ,----
;;   | -+2|-4
;;   `----

;;   After invoking `expatpt', the echo area will show `+2-4 => -2', and
;;   the result `-2' will be put into the `kill-ring' so you can paste it
;;   whenever you want.

;;   If `expatpt' is called with a prefix argument, it will also replace
;;   the grabbed expression with its evaluation result in the buffer. The
;;   expression itself will be put into the `kill-ring' so that it now
;;   becomes the first item in the `kill-ring', followed by the evaluation
;;   result.


;; 3 Function: `expatpt-grab'
;; ==========================

;;   This function is not intended for interactive usage. It is used to
;;   grab the *valid* arithmetic expression around the cursor. The return
;;   value is a list containing three values. The first one is the string
;;   of the expression, and the second and the third one are beginning and
;;   ending postions of the expression in the current buffer. This
;;   *thing-at-point* function can be used as a utility function to define
;;   your own commands/functions.


;; 4 The Supported Arithmetic Expression Syntax
;; ============================================

;;   For the time being, it is very simple. A number can be `+2', `2.6', or
;;   `-2.0e2', and only addition(+), subtraction(-), multiplication(*),
;;   division(/) and power(^) are supported.


;; 5 TODO TODO
;; ===========

;;   I would like to make the arithmetic expression syntax mode-aware. For
;;   example, it would be great if we add a parser for arithmetic
;;   expression in \LaTeX{}.


;; 6 Dependencies
;; ==============

;;   - [parsec.el]: parser combinator library for Emacs Lisp.
;;   - calc: built-in library for arithmetic calculation

;;   Side note: since we have the parser for the arithmetic expression, we
;;   actually don't need `calc'. But for convenience's sake, we use `calc'
;;   to evaluate the arithmetic expression.


;; [parsec.el] https://github.com/cute-jumper/parsec.el

;;; Code:

(require 'parsec)
(require 'calc)

(defvar expatpt-regexp "[0-9*+-/^ .()eE]+")

(defun expatpt-find-beginning ()
  (save-excursion
    (while
        (looking-back expatpt-regexp nil t)
      (goto-char (match-beginning 0)))
    (while (looking-at "[ ]+")
      (goto-char (match-end 0)))
    (point)))

(defun expatpt-find-ending ()
  (save-excursion
    (while
        (looking-at expatpt-regexp)
      (goto-char (match-end 0)))
    (point)))

;; ----------- ;;
;; parser part ;;
;; ----------- ;;

(defun expatpt-int ()
  (parsec-collect-s
   (parsec-optional
    (parsec-one-of ?+ ?-))
   (parsec-many1-s (parsec-digit))))

(defun expatpt-decimal ()
  (parsec-collect-s
   (expatpt-int)
   (parsec-optional
    (parsec-collect-s
     (parsec-ch ?.)
     (expatpt-int)))))

(defun expatpt-num ()
  (parsec-collect-s
   (expatpt-decimal)
   (parsec-optional
    (parsec-collect-s
     (parsec-ch ?e)
     (expatpt-int)))))

(defun expatpt-operator ()
  (parsec-collect-s
   (parsec-many-s (parsec-ch ? ))
   (parsec-one-of ?+ ?- ?* ?/ ?^)
   (parsec-many-s (parsec-ch ? ))))

(defun expatpt-open-paren ()
  (parsec-collect-s
   (parsec-many-s (parsec-ch ? ))
   (parsec-ch ?\()
   (parsec-many-s (parsec-ch ? ))))

(defun expatpt-close-paren ()
  (parsec-collect-s
   (parsec-many-s (parsec-ch ? ))
   (parsec-ch ?\))
   (parsec-many-s (parsec-ch ? ))))

(defun expatpt-exp ()
  (parsec-or
   (parsec-collect-s
    (expatpt-open-paren)
    (expatpt-parse)
    (expatpt-close-paren))
   (expatpt-num)))

(defun expatpt-parse ()
  (parsec-collect-s
   (expatpt-exp)
   (parsec-many-s
    (parsec-try
     (parsec-collect-s
      (expatpt-operator)
      (expatpt-exp))))))

(defun expatpt--internal (beg end pt)
  (when (< beg end)
    (let* ((exp (buffer-substring-no-properties
                 beg
                 end))
           (res (parsec-with-input exp
                  (expatpt-parse)))
           valid-exp-end)
      (if (parsec-error-p res)
          (expatpt--internal (1+ beg) end pt)
        (if (< (setq valid-exp-end (+ beg (length res))) pt)
            (expatpt--internal (1+ valid-exp-end) end pt)
          (list res beg (+ beg (length res))))))))

(defun expatpt--eval-with (func &optional replace)
  (let* ((exp-list (funcall func))
         (exp (car exp-list))
         result)
    (when exp-list
      (kill-new (setq result (calc-eval exp)))
      (message "%s => %s" exp result)
      (when replace
        (kill-region (nth 1 exp-list) (nth 2 exp-list))
        (insert result))
      result)))

;;;###autoload
(defun expatpt-grab ()
  (let* ((beg (expatpt-find-beginning))
         (end (expatpt-find-ending)))
    (expatpt--internal beg end (point))))

;;;###autoload
(defun expatpt (&optional prefix)
  (interactive "P")
  (expatpt--eval-with #'expatpt-grab prefix))

(provide 'expatpt)
;;; expatpt.el ends here
