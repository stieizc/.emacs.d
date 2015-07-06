;;; init-modes  --- initialize modes

;;; Commentary:
;;; All global modes goes here

;;; Code:

(add-hook
 'python-mode-hook
 (lambda ()
   (add-to-list
    (make-local-variable 'company-backends)
    'company-jedi)))
(global-company-mode)
(global-flycheck-mode)

(helm-mode 1)
(helm-autoresize-mode 1)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(show-paren-mode 1)
(setq show-paren-delay 0)

(electric-pair-mode)

(provide 'init-modes)

;;; init-modes.el ends here
