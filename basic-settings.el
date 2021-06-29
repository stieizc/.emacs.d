;;; basic-settings --- emacs basic-settings components initialization

;;; Commentary:

;;; Code:

;; Borrow a lot from
;; https://framagit.org/steckerhalter/steckemacs.el/blob/master/steckemacs.el
;; https://github.com/hlissner/doom-emacs
;; https://github.com/syl20bnr/spacemacs/

;;; - Performance
(setq
  idle-update-delay 2                    ; update ui less often
  gc-cons-percentage 0.3)                ; increase garbage collection limit

;;; - Startup

(defvar my:scratchdir (expand-file-name "~/zscratchpad/"))
(defvar my:todo "~/zscratchpad/todo.org")
(setq
 inhibit-startup-message t
 load-prefer-newer t                    ;prefer newer .el instead of the .elc
 custom-file "/tmp/custom-file.el" ;don't pollute the init file and don't `load' the customs but keep them for reference...
 debug-on-error (and (not noninteractive) my:debug-mode)
 initial-buffer-choice my:todo)

;;; - Autosave
(use-package savehist
  :init
  (savehist-mode 1)
  :config
  (setq
    ;; savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
    savehist-additional-variables '(search-ring regexp-search-ring)))

;;; - Basic editing habit
(setq
;; tab-width 4
;; indent-tabs-mode nil                   ;use spaces instead of tabs
;; c-basic-offset 4                       ;"tab" with in c-related modes
  require-final-newline t                ;auto add newline at the end of file
  column-number-mode t                   ;show the column number
  default-major-mode 'text-mode          ;use text mode per default
  mouse-yank-at-point t                  ;middle click with the mouse yanks at point
  tab-always-indent 'complete            ;try to complete before identing
  confirm-nonexistent-file-or-buffer nil ;don't ask to create a buffer
  eval-expression-print-length nil       ;do not truncate printed expressions
  eval-expression-print-level nil        ;print nested expressions
  mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))) ;make mouse scrolling smooth
  show-paren-delay 0                     ;show the paren immediately
  c-hungry-delete-key t)                 ;delete more than one space

(show-paren-mode t)

; (setq save-place-file (expand-file-name "places" my:cache-dir))
(save-place-mode t)

;; narrow to region should be enabled by default
(put 'narrow-to-region 'disabled nil)

;; don't ask to kill buffers
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;; - History
(setq
 history-length 250                     ;default is 30
 recentf-max-saved-items 5000           ;save up to 5000 recent files
 kill-ring-max 5000                     ;truncate kill ring after 5000 entries
 mark-ring-max 5000)                    ;truncate mark ring after 5000 entries

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
 vc-follow-symlinks t)                  ;follow symlinks automatically

;;; - Window management
(setq
 split-height-threshold 110             ;more readily split horziontally
 split-width-threshold 160              ;split horizontally only if less than 160 columns
 indicate-buffer-boundaries 'left       ;fringe markers (on the left side)
 enable-recursive-minibuffers t
 switch-to-buffer-preserve-window-point t) ;this allows operating on the same buffer in diff. positions

;;; - Eye candy

;; line-numbers
;; https://github.com/noctuid/evil-guide#how-can-i-have-relative-line-numbers
;; https://www.emacswiki.org/emacs/LineNumbers
(when (version<= "26.0.50" emacs-version)
  (setq-default display-line-numbers t
              display-line-numbers-widen t
              ;; this is the default
              display-line-numbers-current-absolute t))

(setq-default show-trailing-whitespace t)

;; display the time in the mode-line
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(setq display-time-use-mail-icon t)
(display-time)

;; disable some global modes
(blink-cursor-mode -1)       ;no cursor blinking
(tool-bar-mode -1)
(menu-bar-mode -1)           ;no menu, you can toggle it with C-c m
(scroll-bar-mode -1)         ;disable the sroll bar

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

(use-package exec-path-from-shell)

(use-package string-inflection)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; basic-settings.el ends here
