;;; projectile --- projectile and friends

;;; Commentary:

;;; Code:

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode t)
  (evil-leader/set-key
    "p" 'projectile-command-map))

;;; projectile.el ends here
