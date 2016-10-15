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

;; 1 expatpt (WIP)
;; 2 Functions
;; .. 2.1 `expatpt-around'
;; 3 Dependencies


;; 1 expatpt (WIP)
;; ===============

;;   expatpt: arithmetic EXPression AT PoinT

;;   Grab the arithmetic expression around the cursor.

;;   This package makes use of [parsec.el] to demonstrate a real-world use
;;   case of this parser combinator library.

;;   Note: This work is still *incomplete*.


;; [parsec.el] https://github.com/cute-jumper/parsec.el


;; 2 Functions
;; ===========

;; 2.1 `expatpt-around'
;; ~~~~~~~~~~~~~~~~~~~~

;;   `expatpt-around' would grab a valid arithmetic expression starting
;;   somewhere before the cursor, and the ending of the expression may be
;;   after or before the cursor since we will stop parsing the expression
;;   as soon as it becomes invalid. So it may not behave exactly like a
;;   traditional *thing-at-point* function. Let's see an example ( |
;;   indicates the cursor position):

;;   ,----
;;   | This is text: 100+2e2-(8^2-|2) -
;;   `----

;;   Calling `expatpt-around' will return `100+2e2-(8^2-2)' since the
;;   trailing `-' is not valid.

;;   If the cursor is after `-' :

;;   ,----
;;   | This is text: 100+2e2-(8^2-2) -|
;;   `----

;;   it will still return the same result.

;;   So what this function does is to parse a valid arithmetic expression
;;   from a string, and this string forms by gathering all the digits and
;;   arithmetic operators around the cursor. The cursor itself, may or may
;;   not be inside the resulting valid arithmetic expression.

;;   For convenience, two variants are provided:
;;   - `expatpt-around-eval': return the result of the arithmetic
;;     expression
;;   - `expatpt-around-eval-and-replace': get the result of the arithmetic
;;     expression and replace the expression with the result


;; 3 Dependencies
;; ==============

;;   - [parsec.el]: parser combinator library for Emacs Lisp.
;;   - calc: built-in library for arithmetic calculation


;; [parsec.el] https://github.com/cute-jumper/parsec.el

;;; Code:

(require 'parsec)

(defvar expatpt-regexp "[0-9*+-/^ .()eE]+")

(defun expatpt-find-beginning ()
  (save-excursion
    (while
        (looking-back expatpt-regexp nil t)
      (goto-char (match-beginning 0)))
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
  (parsec-many (parsec-ch ? ))
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

(defun expatpt--around-internal ()
  (let* ((beg (expatpt-find-beginning))
         (exp (buffer-substring-no-properties
               beg
               (expatpt-find-ending)))
         (res (parsec-with-input exp
                (expatpt-parse))))
    (unless (parsec-error-p res)
      (list res beg (+ beg (length res))))))

;;;###autoload
(defun expatpt-around ()
  (interactive)
  (car (expatpt--around-internal)))

;;;###autoload
(defun expatpt-around-eval ()
  (interactive)
  (let ((exp (expatpt-around))
        result)
    (when exp
      (kill-new (setq result (calc-eval exp)))
      (message "%s" result)
      result)))

;;;###autoload
(defun expatpt-around-eval-and-replace ()
  (interactive)
  (let ((exp-list (expatpt--around-internal))
        result)
    (when exp-list
      (kill-region (nth 1 exp-list) (nth 2 exp-list))
      (insert (setq result (calc-eval (car exp-list))))
      result)))

(provide 'expatpt)
;;; expatpt.el ends here
