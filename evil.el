;;; evil --- evil initialization

;;; Commentary:

;;; Code:

;; evil

(use-package undo-tree)

(use-package evil
  :init
  (setq evil-want-integration t ;; This is optional since it's already set to t by default.
	evil-want-keybinding nil)
  (setq-default evil-auto-indent nil)
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree)
  (evil-mode t))

;; evil-leader
;; jiege!
;; https://github.com/jiegec/emacs.d/blob/master/lisp/init-evil.el

(use-package evil-leader
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "<SPC>" #'save-buffer
   "bb" #'switch-to-buffer
   "bk" #'kill-buffer
   "qq" #'save-buffers-kill-terminal
   "zd" #'toggle-debug-on-error
   "z-" #'text-scale-adjust
   "z+" #'text-scale-adjust
   "z0" #'text-scale-adjust))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'Info-mode-map
    "h" #'evil-backward-char
    "l" #'evil-forward-char))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t))

(use-package evil-matchit
 :config
 (global-evil-matchit-mode 1))

(use-package hydra)

(use-package ranger)

;;; evil.el ends here
