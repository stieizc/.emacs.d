;;; init --- emacs initialization

;;; Commentary:
;;; Initialization

;;; Code:
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))

(add-hook 'after-init-hook
    (lambda ()
      (mapc 'require
	    '(init-theme
	      init-key-bindings
	      init-modes
	      init-evil))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" default)))
 '(safe-local-variable-values
   (quote
    ((eval progn
	   (setq jedi:environment-root "python3")
	   (setq jedi:server-args
		 (\`
		  ("--virtual-env"
		   (\,
		    (concat
		     (projectile-project-root)
		     "venv")))))
	   (setq python-environment-virtualenv
		 (append python-environment-virtualenv
			 (quote
			  ("--python" "python3"))))
	   (jedi:install-server))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

;;; init.el ends here
