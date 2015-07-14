;;; init-modes  --- initialize modes

;;; Commentary:
;;; All global modes goes here

;;; Code:

(setq-default fill-column 72)
(setq-default indent-tabs-mode nil)

;; Python
(add-hook
 'python-mode-hook
 (lambda ()
   (add-to-list
    (make-local-variable 'company-backends)
    'company-jedi)))

;; CoffeeScript Mode
(custom-set-variables '(coffee-tab-width 2))

;; Apiary
(add-to-list 'auto-mode-alist '("\\.apib$" . markdown-mode))

;; Markdown
(add-hook 'markdown-mode-hook 'outline-evil-keys)

;; Outline
(defun outline-evil-keys()
  (progn
    (define-key evil-normal-state-map (kbd "C-n") 'outline-next-visible-heading)
    (define-key evil-normal-state-map (kbd "C-p") 'outline-previous-visible-heading)
    (define-key evil-normal-state-map (kbd "C-f") 'outline-forward-same-level)
    (define-key evil-normal-state-map (kbd "C-b") 'outline-backward-same-level)
    (define-key evil-normal-state-map (kbd "C-u") 'outline-up-heading)))

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
