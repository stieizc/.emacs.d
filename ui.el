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

;; https://github.com/jaypei/emacs-neotree
(use-package neotree
  :init
  ;; (setq neo-autorefresh nil)
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (defun neotree-project-dir-toggle ()
    "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
    (interactive)
    (if (neo-global--window-exists-p)
      (neotree-hide)
      (progn
        (ignore-errors
          (neotree-dir (projectile-project-root)))
        (neotree-show))))
  (defun neotree-project-dir-toggle-no-focus ()
    "neotree-project-dir-toggle without loosing focus on current buffer"
    (interactive)
    (save-selected-window (neotree-project-dir-toggle)))
  ;; projectile support
  (defun neotree-refresh (&optional is-auto-refresh)
    "Refresh the NeoTree buffer."
    (interactive)
    (if (eq (current-buffer) (neo-global--get-buffer))
      (neo-buffer--refresh t)
      (save-excursion
        (let ((cw (selected-window)))  ;; save current window
          (if is-auto-refresh
            (let ((origin-buffer-file-name (buffer-file-name)))
              (when (and (fboundp 'projectile-project-p)
                      (projectile-project-p)
                      (fboundp 'projectile-project-root))
                (neo-global--open-dir (projectile-project-root))
                (neotree-find (projectile-project-root)))
              (neotree-find origin-buffer-file-name))
            (neo-buffer--refresh t t))
          (recenter)
          (when (or is-auto-refresh neo-toggle-window-keep-p)
            (select-window cw))))))
  (evil-leader/set-key
    "tt" #'neotree-project-dir-toggle-no-focus
    "to" #'neotree-project-dir-toggle))

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

;;; ui.el ends here
