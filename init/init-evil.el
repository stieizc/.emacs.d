;;; init-evil --- evil initialization

;;; Commentary:
;;; All Evil Things here

;;; Code:

(global-evil-leader-mode)
(custom-set-variables
 '(evil-auto-indent nil))
(evil-mode 1)

(evil-leader/set-key
 "w" 'save-buffer
 "q" 'save-buffers-kill-terminal
 "ee" (lambda () (interactive) (find-file "~/.emacs.d"))
 "ek" (lambda () (interactive)
	(progn
	  (save-buffers-kill-terminal)
	  (kill-emacs))))

(define-key evil-insert-state-map (kbd "TAB")
  (lambda() (interactive)
    (if (or (bolp) (string-match "[\s-]" (string (char-before))))
	(indent-according-to-mode)
      (company-complete-common))))

(define-key evil-normal-state-map (kbd "C-p") 'projectile-command-map)

(provide 'init-evil)

;;; init-evil.el ends here
