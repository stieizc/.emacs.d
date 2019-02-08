;;; core --- emacs core components initialization

;;; Commentary:
;;; Initialization for:
;;; 1. straight.el and use-package
;;; 2. basic settings

;;; Code:

;;; 1. Bootstrap straight.el & use-package

(let ((bootstrap-file
        (expand-file-name
          "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
        (url-retrieve-synchronously
          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
          nil 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Done bootstrapping straight.el

;;; Setup use-package
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ;; ("marmalade" . "https://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa.org/packages/")))

;;; Done setting up use-package

;;; 2. Basic settings

;; https://framagit.org/steckerhalter/steckemacs.el/blob/master/steckemacs.el
;; personal variables
(defvar my:todo "~/Orgzly/todo.org")
(defvar my:tmpdir (expand-file-name "emacs-auto-save" temporary-file-directory))
(make-directory my:tmpdir t)

;; global flags
(setq
  inhibit-startup-message t
  backup-directory-alist `((".*" . ,my:tmpdir)) ;don't clutter my fs and put backups into tmp
  auto-save-file-name-transforms `((".*" ,my:tmpdir t))
  require-final-newline t                ;auto add newline at the end of file
  column-number-mode t                   ;show the column number
  default-major-mode 'text-mode          ;use text mode per default
  mouse-yank-at-point t                  ;middle click with the mouse yanks at point
  history-length 250                     ;default is 30
  locale-coding-system 'utf-8            ;utf-8 is default
  tab-always-indent 'complete            ;try to complete before identing
  confirm-nonexistent-file-or-buffer nil ;don't ask to create a buffer
  vc-follow-symlinks t                   ;follow symlinks automatically
  recentf-max-saved-items 5000           ;save up to 5000 recent files
  eval-expression-print-length nil       ;do not truncate printed expressions
  eval-expression-print-level nil        ;print nested expressions
  send-mail-function 'sendmail-send-it
  kill-ring-max 5000                     ;truncate kill ring after 5000 entries
  mark-ring-max 5000                     ;truncate mark ring after 5000 entries
  mouse-autoselect-window -.1            ;window focus follows the mouse pointer
  mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))) ;make mouse scrolling smooth
  indicate-buffer-boundaries 'left       ;fringe markers (on the left side)
  split-height-threshold 110             ;more readily split horziontally
  enable-recursive-minibuffers t         ;whatever...
  show-paren-delay 0                     ;show the paren immediately
  load-prefer-newer t                    ;prefer newer .el instead of the .elc
  split-width-threshold 160              ;split horizontally only if less than 160 columns
  gc-cons-percentage 0.3                 ;increase garbage collection limit
  safe-local-variable-values '((engine . django))
  switch-to-buffer-preserve-window-point t ;this allows operating on the same buffer in diff. positions
  custom-file "/tmp/custom-file.el" ;don't pollute the init file and don't `load' the customs but keep them for reference...
  initial-buffer-choice my:todo)

;; default flags
(setq-default
  ;; tab-width 4
  ;; indent-tabs-mode nil                   ;use spaces instead of tabs
  ;; c-basic-offset 4                       ;"tab" with in c-related modes
  c-hungry-delete-key t)                 ;delete more than one space

;; disable full `yes' or `no' answers, `y' and `n' suffices
(defalias 'yes-or-no-p 'y-or-n-p)

;; display the time in the mode-line
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(setq display-time-use-mail-icon t)
(display-time)

;; encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; disable some global modes
(blink-cursor-mode -1)       ;no cursor blinking
(tool-bar-mode -1)           ;disable the awful toolbar
(menu-bar-mode -1)           ;no menu, you can toggle it with C-c m
(scroll-bar-mode -1)         ;disable the sroll bar

;; narrow to region should be enabled by default
(put 'narrow-to-region 'disabled nil)

;; don't ask to kill buffers
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; default font
;; (defvar my-font-attributes '(default nil :family "fixed" :width semi-condensed :height 120))
;; (defvar my-font-attributes '(default nil :family "DejaVu Sans Mono" :height 90))
;; (defvar my-font-attributes '(default nil :family "Anonymous Pro" :height 90))
;; (apply 'set-face-attribute  my-font-attributes)

;;; core.el ends here
