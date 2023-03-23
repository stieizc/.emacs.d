;;; -*- lexical-binding: t; -*-

;;; Section: On editing.

(require 'init-packaging)

;;; - Autosave

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight nil
  :hook ((emacs-startup . savehist-mode))
  :diminish savehist-mode
  :custom
  ;; savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
  (savehist-additional-variables '(search-ring regexp-search-ring)))

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

(use-package saveplace
  :straight nil
  :hook ((emacs-startup . save-place-mode))
  :diminish save-place-mode)

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
  :hook ((emacs-startup . auto-revert-mode))
  :diminish auto-revert-mode)

(use-package abbrev
  :straight nil
  :hook ((emacs-startup . abbrev-mode))
  :diminish abbrev-mode)

(use-package exec-path-from-shell
  :defer t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package with-editor
  :hook ((emacs-startup . shell-command-with-editor-mode)))

(use-package sudo-edit
  :general
  (my:space-file-leader-def
   "E" '(sudo-edit :which-key "open a file sudo")))

;; TODO(wenxin): set key bindings
(use-package string-inflection
  :defer t)

(use-package minimap
  :defer t)

(provide 'init-editing)
