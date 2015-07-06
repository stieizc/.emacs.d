;;; init-evil --- evil initialization

;;; Commentary:
;;; All Evil Things here

;;; Code:

(global-evil-leader-mode)
(evil-mode 1)

(evil-leader/set-key
 "w" 'save-buffer
 "q" 'save-buffers-kill-terminal
 "ee" (lambda () (interactive) (find-file "~/.emacs.d"))
 "ek" 'kill-emacs)

(define-key evil-insert-state-map (kbd "TAB") 'company-complete)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-command-map)

(provide 'init-evil)

;;; init-evil.el ends here
