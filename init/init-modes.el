;;; init-modes  --- initialize modes

;;; Commentary:
;;; All global modes goes here

;;; Code:

(add-hook 'python-mode-hook
	(lambda ()
	 (add-to-list
	  (make-local-variable 'company-backends)
	  'company-jedi)))
(global-company-mode)
(global-flycheck-mode)

(helm-mode 1)
(helm-autoresize-mode 1)

(provide 'init-modes)

;;; init-modes.el ends here
