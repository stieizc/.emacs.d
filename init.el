;;; -*- lexical-binding: t; -*-

;;; Section: Acknowledgement.
;; Lots of code and ideas are borrowed from
;; https://d12frosted.io/posts/2021-04-09-emacs-d.html
;; https://github.com/d12frosted/environment/blob/master/emacs/init.el
;; Thank you!
;;
;; I have since decided that I don't want that fancy directory layout
;; and just leave things in one big file. This makes it easier to see
;; the big picture. I do need a way to generate table of contents
;; without using org-babel as an early dependency.

;;; Section: Important constants.
;; These value are important to my workflow.

(defconst my:scratchdir (expand-file-name "~/zscratchpad/"))
(defconst my:todo (expand-file-name "todo.org" my:scratchdir))

;;; Section: Important init setup.

(setq
 inhibit-startup-message t
 ;; prefer newer .el instead of the .elc
 load-prefer-newer t
 initial-buffer-choice my:todo)

;;; Section: Package management base.
;; Setup straight.el & use-package for package management.

;; https://github.com/radian-software/straight.el
(setq
 straight-vc-git-default-clone-depth 1
 straight-repository-branch "develop"
 straight-check-for-modifications nil
 straight-use-package-by-default t)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; https://github.com/jwiegley/use-package
(straight-use-package 'use-package)
;; https://github.com/myrjola/diminish.el
(use-package diminish)
;; https://github.com/radian-software/el-patch
(use-package el-patch)
;; Setup no-littering as early as possible.
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :config
  (setq custom-file (expand-file-name "custom.el" no-littering-etc-directory)))

;;; Section: Vi keybindings
;; Evil, that is.

;; Required by evil as a undo system. Useful anyways.
;; https://www.emacswiki.org/emacs/UndoTree
;; https://www.dr-qubit.org/undo-tree/undo-tree.txt
(use-package undo-tree
  :init
  (setq undo-tree-auto-save-history t)
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; https://github.com/emacs-evil/evil
;; https://github.com/noctuid/evil-guide
(use-package evil
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

;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'Info-mode-map
    "h" #'evil-backward-char
    "l" #'evil-forward-char))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package general
  :config
  (general-create-definer my:space-file-leader-def
    :prefix-command 'my:space-file-leader-command
    :prefix-map 'my:space-file-leader-map)
  (my:space-file-leader-def
   "f" '(find-file :which-key "open a file"))
  (general-create-definer my:space-buffer-leader-def
    :prefix-command 'my:space-buffer-leader-command
    :prefix-map 'my:space-buffer-leader-map)
  (my:space-buffer-leader-def
   "b" '(list-buffers :which-key "list buffers"))
  (general-create-definer my:space-leader-def
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :prefix-map 'my:space-leader-map)
  (my:space-leader-def
    ;; simple command
    "TAB" '(switch-to-other-buffer :which-key "prev buffer")
    "SPC" '(save-buffer :which-key "save buffer")
    "f" '(my:space-file-leader-command :which-key "file commands")
    "b" '(my:space-buffer-leader-command :which-key "buffer commands")))

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

(use-package hydra)

;; (use-package ranger)
;;; Section: Debug utilities.

(use-package esup
  :commands (esup))

;; (require 'init-basic-editing)
;; (require 'init-ui)

;;; Make emacs more responsive.

(setq
 ;; update ui less often
 idle-update-delay 2)

;; (require 'init-fonts)
;; (require 'init-orgmode)

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
