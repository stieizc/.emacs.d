;;; -*- lexical-binding: t; -*-

;;; Section: UI settings.
;;; TODO: tidy things up.

(require 'init-packaging)

;;; - Parens matching
(setq
 column-number-mode t
 ;; show the paren immediately
 show-paren-delay 0)

(show-paren-mode t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

;;; - Window management
(setq
 ;; less likely to split vertically
 split-height-threshold 110
 ;; split horizontally only if less than 160 columns
 ;; split-width-threshold 160
 ;; fringe markers (on the left side)
 indicate-buffer-boundaries 'left
 enable-recursive-minibuffers t
 ;; this allows operating on the same buffer in diff. positions
 switch-to-buffer-preserve-window-point t)

(minibuffer-depth-indicate-mode t)

;;; - Eye candy

;; line-numbers
;; https://github.com/noctuid/evil-guide#how-can-i-have-relative-line-numbers
;; https://www.emacswiki.org/emacs/LineNumbers
(setq-default
 display-line-numbers t
 display-line-numbers-widen t
 ;; this is the default
 display-line-numbers-current-absolute t)

(defun my:line-numbers-relative ()
  (setq-local display-line-numbers 'visual))

(defun my:line-numbers-absolute ()
  (setq-local display-line-numbers t))

(add-hook 'evil-insert-state-entry-hook #'my:line-numbers-relative)
(add-hook 'evil-insert-state-exit-hook #'my:line-numbers-absolute)

(setq-default
 show-trailing-whitespace t)

;; display the time in the mode-line
(setq
 display-time-24hr-format t
 display-time-default-load-average nil
 display-time-use-mail-icon t)

(display-time)

;; disable some global modes
(blink-cursor-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; scrollbar
;; (set-scroll-bar-mode 'right)
(scroll-bar-mode -1)

;; default font
;; (defvar my-font-attributes '(default nil :family "fixed" :width semi-condensed :height 120))
;; (defvar my-font-attributes '(default nil :family "DejaVu Sans Mono" :height 90))
;; (defvar my-font-attributes '(default nil :family "Anonymous Pro" :height 90))
;; (apply 'set-face-attribute  my-font-attributes)

(provide 'init-ui)
