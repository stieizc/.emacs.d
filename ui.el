;;; ui --- ui initialization

;;; Commentary:

;;; Code:

;; which-key
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons)

;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :ensure t
  ;; :init
  ;; (with-eval-after-load 'winum
  ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  (evil-leader/set-key
    "tt" #'treemacs-select-window
    "tT" #'treemacs
    "tf" #'treemacs-find-file))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :ensure t)

;; https://github.com/hlissner/emacs-doom-themes/
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
    doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; https://github.com/TheBB/spaceline
(use-package spaceline)

;; https://github.com/domtronn/spaceline-all-the-icons.el
(use-package spaceline-all-the-icons
  :after spaceline
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-all-the-icons--setup-neotree))

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((recents . 10)
                           (bookmarks . 5)
                           (projects . 5)
                           (agenda . 5)
                           (registers . 5)))
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;;; ui.el ends here
