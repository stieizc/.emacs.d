;;; peg-mode.el --- emacs mode for editing PEG grammar files

;; Originally found at
;; https://github.com/stevej/emacs/blob/master/vendor/peg-mode/peg-mode.el
;; Modified by Wenxin Wang, because the original version seems broken

;; Copyright (C) 2008  Utz-Uwe Haus <lisp@uuhaus.de>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; This file provides a major mode for editing PEG grammar files.
;; It includes font-lock definitions and commands for
;; controlling indentation, re-indenting by subdivisions.
;;
;; To use, put the following in your .emacs:
;;
;; (autoload 'peg-mode "peg-mode" "Mode for editing PEG grammar files" t)
;;
;; You may also want something like:
;;
;; (setq auto-mode-alist
;;       (append '(("\\.peg$"    . peg-mode))
;;               auto-mode-alist))

;;

;;; Code:
(defconst peg-version "0"
  "`peg-mode' version number.")

(require 'custom)

(defgroup peg nil
  "Support for editing PEG grammar files."
  :group 'languages)

(defcustom peg-mode-hook nil
  "*Hook to be run when `peg-mode' is entered."
  :type 'hook
  :group 'peg)

;; faces and font-locking
(defcustom peg-font-lock-keywords
  `(("^\\sw*" . font-lock-variable-name-face)
    ;; ("\\[\\([^]\\[]\\|\\\\[\\|\\\]\\)*\\]" . font-lock-string-face)
    ("\\_<<-\\_>" . font-lock-function-name-face))
  "Identifiers treated as reserved keywords in PEG."
  :group 'peg)

;; major-mode stuff
(defvar peg-mode-abbrev-table
  (make-abbrev-table))

(defvar peg-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?#  "<"   table)
    (modify-syntax-entry ?\n ">"   table)
    ;;  quoted strings
    (modify-syntax-entry ?\' "\""  table)
    (modify-syntax-entry ?\" "\""  table)
    ;; operators
    (modify-syntax-entry ?&  "_"  table)
    (modify-syntax-entry ?!  "_"  table)
    (modify-syntax-entry ?+  "_"  table)
    (modify-syntax-entry ?*  "_"  table)
    (modify-syntax-entry ?/  "_"  table)
    (modify-syntax-entry ?{  "(}"  table)
    (modify-syntax-entry ?}  "){"  table)
    (modify-syntax-entry ?\\ "\\" table)
    ;;
    (modify-syntax-entry ?\(  "()"   table)
    (modify-syntax-entry ?\)  ")("   table)
    (modify-syntax-entry ?\[  "(]"   table)
    (modify-syntax-entry ?\]  ")["   table)
    ;; (modify-syntax-entry ?'  "|"   table)
    table)
  "Syntax table used in `peg-mode' buffers.")

;; comments: starting at ', ending at end of line

;;;###autoload
(define-derived-mode peg-mode prog-mode "PEG"
  "Major mode for editing PEG programs."
  :group 'peg
  :syntax-table peg-mode-syntax-table
  :abbrev-table peg-mode-abbrev-table
  (when (version< emacs-version "24.3")
    (error "peg-mode requires at least Emacs 24.3"))
  ;; (kill-all-local-variables)
  ;; (use-local-map peg-mode-map)
  (make-local-variable 'electric-indent-functions)
  (make-local-variable 'indent-line-function)
  (setq font-lock-defaults
	'(peg-font-lock-keywords)
	electric-indent-inhibit t
	electric-indent-functions 'no-indent
	indent-line-function #'ignore)
  (local-set-key "\t" 'self-insert-command)

  ; ;; local variables
  ; (make-local-variable 'parse-sexp-ignore-comments)
  ; (make-local-variable 'comment-start-skip)
  ; (make-local-variable 'comment-start)
  ; (make-local-variable 'comment-end)
  ; (make-local-variable 'paragraph-start)
  ; (make-local-variable 'paragraph-separate)
  ; (make-local-variable 'paragraph-ignore-fill-prefix)
  ; (make-local-variable 'indent-region-function)
  ; 					; now set their values
  ; (setq parse-sexp-ignore-comments t
  ; 	comment-start-skip "![ \t]*"
  ; 	comment-start "! "
  ; 	comment-end "")
  ; (setq ; indent-region-function 'peg-indent-region
  ;  paragraph-ignore-fill-prefix t
  ;  paragraph-start (concat "^[ \t]*$\\|^[ \t]*[!]\\|" page-delimiter)
  ;  paragraph-separate paragraph-start)
  (run-hooks 'peg-mode-hook))

(provide 'peg-mode)
;;; peg-mode.el ends here
