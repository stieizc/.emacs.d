;;; ivy --- ivy and friends

;;; Commentary:

;;; Code:

(use-package counsel
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
	ivy-display-style 'fancy
	swiper-action-recenter t)
  (evil-leader/set-key
    "ff" 'counsel-find-file
    "fe" #'(lambda () (interactive) (find-file "~/.emacs.d/init.el"))))

;;; ivy.el ends here
