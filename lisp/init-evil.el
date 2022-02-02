;;; -*- lexical-binding: t; -*-

(use-package undo-tree
  :init
  (setq undo-tree-auto-save-history t)
  :commands (undo-tree undo-tree-undo)
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package evil
  :ensure t
  :init
  (setq
   ;; for evil-collection
   evil-want-integration t
   ;; for evil-collection
   evil-want-keybinding nil
   ;; hybrid-mode from spacemacs, use emacs bindings in insert-mode
   evil-disable-insert-state-bindings t)
  (setq-default
   evil-auto-indent nil)
  :config
  (evil-set-undo-system 'undo-tree)
  (evil-mode t))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'Info-mode-map
    "h" #'evil-backward-char
    "l" #'evil-forward-char))

(use-package general)

;; (use-package evil-leader
;;   :after evil
;;   :config
;;   (global-evil-leader-mode t)
;;   (evil-mode t)
;;   (evil-leader/set-leader "<SPC>")
;;   (evil-leader/set-key
;;     "<SPC>" #'save-buffer
;;     "bb" #'switch-to-buffer
;;     "bk" #'kill-buffer
;;     "qq" #'save-buffers-kill-terminal
;;     "zd" #'toggle-debug-on-error
;;     "z-" #'text-scale-adjust
;;     "z+" #'text-scale-adjust
;;     "z0" #'text-scale-adjust))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package hydra)

;; (use-package ranger)

(provide 'init-evil)
