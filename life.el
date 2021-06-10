;;; life --- life tools

;;; Commentary:

;;; Code:

;;; hledger

(use-package beancount
  :straight (beancount
              :type git :repo "https://github.com/beancount/beancount-mode")
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

(use-package anki-editor)

;;; life.el ends here
