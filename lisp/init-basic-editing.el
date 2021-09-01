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
  mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))

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

(save-place-mode t)

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
