;;; init-key-bindings  --- initialize key bindings

;;; Commentary:
;;; All global key bindings goes here

;;; Code:

(global-set-key (kbd "<f12>") 'magit-status)
(global-set-key (kbd "M-x") 'helm-M-x)
;(global-set-key (kbd "<tab>") 'company-complete)

(provide 'init-key-bindings)

;;; init-key-bindings.el ends here
