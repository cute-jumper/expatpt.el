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

;;

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
  (parsec-collect-as-string
   (parsec-optional
    (parsec-one-of ?+ ?-))
   (parsec-many1-as-string (parsec-re "[0-9]"))))

(defun expatpt-decimal ()
  (parsec-collect-as-string
   (expatpt-int)
   (parsec-optional
    (parsec-collect-as-string
     (parsec-ch ?.)
     (expatpt-int)))))

(defun expatpt-num ()
  (parsec-collect-as-string
   (expatpt-decimal)
   (parsec-optional
    (parsec-collect-as-string
     (parsec-ch ?e)
     (expatpt-int)))))

(defun expatpt-operator ()
  (parsec-collect-as-string
   (parsec-many-as-string (parsec-ch ? ))
   (parsec-one-of ?+ ?- ?* ?/ ?^)
   (parsec-many-as-string (parsec-ch ? ))))

(defun expatpt-open-paren ()
  (parsec-collect-as-string
   (parsec-many-as-string (parsec-ch ? ))
   (parsec-ch ?\()
   (parsec-many-as-string (parsec-ch ? ))))

(defun expatpt-close-paren ()
  (parsec-collect-as-string
   (parsec-many-as-string (parsec-ch ? ))
   (parsec-ch ?\))
   (parsec-many-as-string (parsec-ch ? ))))

(defun expatpt-exp ()
  (parsec-or
   (parsec-collect-as-string
    (expatpt-open-paren)
    (expatpt-expression)
    (expatpt-close-paren))
   (expatpt-num)))

(defun expatpt-expression ()
  (parsec-collect-as-string
   (expatpt-exp)
   (parsec-many-as-string
    (parsec-try
     (parsec-collect-as-string
      (expatpt-operator)
      (expatpt-exp))))))

;;;###autoload
(defun expatpt-around ()
  (interactive)
  (let* ((exp (buffer-substring-no-properties
               (expatpt-find-beginning)
               (expatpt-find-ending)))
         (res (parsec-with-input exp
                (parsec-return (expatpt-expression)
                  (parsec-eol-or-eof)))))
    (unless (parsec-error-p res)
      res)))

(provide 'expatpt)
;;; expatpt.el ends here
