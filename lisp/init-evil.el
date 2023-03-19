;;; -*- lexical-binding: t; -*-

;;; Section: Vi keybindings.
;; Evil, that is.

(require 'init-packaging)
(eval-when-compile
  (require 'lib-keybinding))

;; Required by evil as a undo system. Useful anyways.
;; https://www.emacswiki.org/emacs/UndoTree
;; https://www.dr-qubit.org/undo-tree/undo-tree.txt
(use-package undo-tree
  :hook ((emacs-startup . global-undo-tree-mode))
  :init
  (setq undo-tree-auto-save-history t)
  :diminish undo-tree-mode)

;; https://github.com/emacs-evil/evil
;; https://github.com/noctuid/evil-guide
(use-package evil
  :hook ((emacs-startup . evil-mode))
  :config
  (my:space-file-leader-def
    "f" '(find-file :which-key "open a file"))
  (my:space-exit-leader-def
    "q" '(save-buffers-kill-terminal :which-key "kill current window"))
  (my:space-zmisc-leader-def
    "d" #'toggle-debug-on-error
    "-" #'text-scale-adjust
    "=" #'text-scale-adjust
    "+" #'text-scale-adjust
    "0" #'text-scale-adjust)
  (my:space-leader-def
    "TAB" '(switch-to-other-buffer :which-key "prev buffer")
    "SPC" '(save-buffer :which-key "save buffer")
    "b" '(my:space-buffer-leader-command :which-key "buffer commands")
    "f" '(my:space-file-leader-command :which-key "file commands")
    "g" '(my:space-vc-leader-command :which-key "version control commands")
    "h" '(:keymap help-map :which-key "help")
    "q" '(my:space-exit-leader-command :which-key "exit commands")
    "z" '(my:space-zmisc-leader-command :which-key "zmisc commands"))
  :custom
  ;; For evil-collection.
  (evil-want-integration t)
  (evil-want-keybinding nil)
  ;; Use emacs bindings in insert-mode.
  (evil-disable-insert-state-bindings t)
  ;; (setq-default
  ;;  evil-auto-indent nil)
  ;; For jumping
  (evil-symbol-word-search t)
  :config
  (add-to-list 'evil-emacs-state-modes 'exwm-mode)
  (evil-set-undo-system 'undo-tree))

;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  ;;:hook ((emacs-startup . evil-collection-init))
  :after (evil)
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'Info-mode-map
    "h" #'evil-backward-char
    "l" #'evil-forward-char))

(use-package evil-matchit
  :hook ((emacs-startup . global-evil-matchit-mode)))

(use-package hydra
  :defer t)

(provide 'init-evil)
