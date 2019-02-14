;;; projectile --- projectile and friends

;;; Commentary:

;;; Code:

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-known-projects-file
	(expand-file-name "projectile-bookmarks.eld" my:cache-dir)))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode t)
  (evil-leader/set-key
    "p" 'projectile-command-map))

;;; projectile.el ends here
