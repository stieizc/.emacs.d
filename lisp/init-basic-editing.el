;;; -*- lexical-binding: t; -*-

;;; - Autosave
(use-package savehist
  :init
  :config
  (savehist-mode 1)
  (setq
    ;; savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
    savehist-additional-variables '(search-ring regexp-search-ring)))

;;; - Basic editing habit
(setq-default
  indent-tabs-mode nil
  ;; auto add newline at the end of file
  require-final-newline t)

(setq
  column-number-mode t
  mouse-yank-at-point t
  ;; default-major-mode 'text-mode
  ;; try to complete before identing
  tab-always-indent 'complete
  ;; don't ask to create a buffer
  confirm-nonexistent-file-or-buffer nil
  ;; do not truncate printed expressions
  eval-expression-print-length nil
  ;; print nested expressions
  eval-expression-print-level nil
  ;; make mouse scrolling smooth
  mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control)))
  ;; show the paren immediately
  show-paren-delay 0)

;;; global modes
(show-paren-mode t)
(save-place-mode t)

;; narrow to region should be enabled by default
(put 'narrow-to-region 'disabled nil)

;; don't ask to kill buffers
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
    kill-buffer-query-functions))

;;; - History
(setq
  ;; default is 30
  history-length 250
  ;; save up to 5000 recent files
  recentf-max-saved-items 500
  ;; truncate kill ring after 5000 entries
  kill-ring-max 500
  ;; truncate mark ring after 5000 entries
  mark-ring-max 500)

;;; - Encoding

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system        'chinese-gb18030)
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(set-language-environment "UTF-8")

;;; - Version control
(setq
  ;; follow symlinks automatically
  vc-follow-symlinks t)

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

;; disable full `yes' or `no' answers, `y' and `n' suffices
(defalias 'yes-or-no-p 'y-or-n-p)

;; default font
;; (defvar my-font-attributes '(default nil :family "fixed" :width semi-condensed :height 120))
;; (defvar my-font-attributes '(default nil :family "DejaVu Sans Mono" :height 90))
;; (defvar my-font-attributes '(default nil :family "Anonymous Pro" :height 90))
;; (apply 'set-face-attribute  my-font-attributes)

;;; - Other global flags
(setq
 send-mail-function 'sendmail-send-it)

;;; - Builtin modes

(use-package autorevert
  :straight nil
  :diminish auto-revert-mode)

(use-package abbrev
  :straight nil
  :diminish abbrev-mode)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package string-inflection)

(provide 'init-basic-editing)
