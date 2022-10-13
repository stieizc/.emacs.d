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

;; TODO: benchmark init.
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-benchmarking.el

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
  (general-create-definer my:space-buffer-leader-def
    :prefix-command 'my:space-buffer-leader-command
    :prefix-map 'my:space-buffer-leader-map)
  (general-create-definer my:space-exit-leader-def
    :prefix-command 'my:space-exit-leader-command
    :prefix-map 'my:space-exit-leader-map)
  (general-create-definer my:space-vc-leader-def
    :prefix-command 'my:space-vc-leader-command
    :prefix-map 'my:space-vc-leader-map)
  (general-create-definer my:space-leader-def
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :prefix-map 'my:space-leader-map)
  (my:space-file-leader-def
   "f" '(find-file :which-key "open a file"))
  (my:space-exit-leader-def
   "q" '(save-buffers-kill-terminal :which-key "kill current window"))
  (my:space-leader-def
    ;; simple command
    "TAB" '(switch-to-other-buffer :which-key "prev buffer")
    "SPC" '(save-buffer :which-key "save buffer")
    "f" '(my:space-file-leader-command :which-key "file commands")
    "b" '(my:space-buffer-leader-command :which-key "buffer commands")
    "g" '(my:space-vc-leader-command :which-key "version control commands")
    "q" '(my:space-exit-leader-command :which-key "exit commands")))

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

;;; Section: Completion base.

;; https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  (recentf-mode)
  (setq
   vertico-resize t
   vertico-cycle t))

;; https://github.com/minad/consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  (my:space-buffer-leader-def
   "b" '(consult-buffer :which-key "list buffers"))

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; https://github.com/minad/corfu
(use-package corfu
  :ensure t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :config
  (global-corfu-mode))

;; https://github.com/oantolin/orderless
;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; https://github.com/oantolin/embark
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;;; Section: Version control.

(use-package magit
  :general
  (my:space-vc-leader-def "g" 'magit-status)
  (my:space-vc-leader-def "b" 'magit-blame))

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
