;;; init-evil --- evil initialization

;;; Commentary:
;;; All Evil Things here

;;; Code:

(evil-mode 1)

(define-key evil-insert-state-map (kbd "TAB") 'company-complete)

(provide 'init-evil)

;;; init-evil.el ends here
