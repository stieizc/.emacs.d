;;; life --- life tools

;;; Commentary:

;;; Code:

;;; hledger

(use-package beancount
  :straight nil
  :load-path ("/usr/elisp/")
  :mode ("\\.beancount\\'" . beancount-mode))

;; (use-package hledger-mode
;;   :mode ("\\.hledger\\'")
;;   :init
;;   (setq hledger-jfile
;;         (expand-file-name "~/Personal/Finance/main.hledger"))
;;   :config
;;   (add-hook 'hledger-mode-hook
;;             (lambda ()
;; 	      (make-local-variable 'company-backends)
;; 	      (add-to-list 'company-backends 'hledger-company))))

;;; life.el ends here
