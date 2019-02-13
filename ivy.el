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

(use-package swiper
  :config
  (define-key evil-normal-state-map (kbd "*")
    ;; (format "\\<%s\\>" (thing-at-point 'symbol))
    (lambda () (interactive) (swiper (thing-at-point 'symbol))))
  (define-key evil-normal-state-map (kbd "#")
    (lambda () (interactive) (swiper (thing-at-point 'symbol)))))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode t))

;;; ivy.el ends here
