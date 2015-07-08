;;; init-modes  --- initialize modes

;;; Commentary:
;;; All global modes goes here

;;; Code:

;; Python
(add-hook
 'python-mode-hook
 (lambda ()
   (add-to-list
    (make-local-variable 'company-backends)
    'company-jedi)))

;; CoffeeScript Mode
(custom-set-variables '(coffee-tab-width 2))

;; Completion
(global-company-mode)

;; Flycheck
(global-flycheck-mode)

;; Helm
(helm-mode 1)
(helm-autoresize-mode 1)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Parens
(show-paren-mode 1)
(setq show-paren-delay 0)
(electric-pair-mode)

(provide 'init-modes)

;;; init-modes.el ends here
