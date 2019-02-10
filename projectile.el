;;; projectile --- projectile and friends

;;; Commentary:

;;; Code:

(use-package counsel-projectile
  :config
  (counsel-projectile-mode t)
  (evil-leader/set-key
    "p" 'projectile-command-map))

;;; projectile.el ends here
