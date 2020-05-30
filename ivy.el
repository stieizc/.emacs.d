;;; ivy --- ivy and friends

;;; Commentary:

;;; Code:

(use-package sudo-edit
  :functions sudo-edit)

(use-package counsel
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
    ivy-display-style 'fancy
    ivy-height 20
    swiper-action-recenter t)
  (evil-leader/set-key
    "ff" 'counsel-find-file
    "fE" #'sudo-edit
    "fe" #'(lambda () (interactive) (find-file "~/.emacs.d/init.el"))))

(use-package swiper
  :diminish
  :config
  (define-key evil-normal-state-map (kbd "*")
    ;; (format "\\<%s\\>" (thing-at-point 'symbol))
    (lambda () (interactive) (swiper (thing-at-point 'symbol))))
  (define-key evil-normal-state-map (kbd "#")
    (lambda () (interactive) (swiper (thing-at-point 'symbol)))))

(use-package prescient
  :init
  (setq prescient-save-file (expand-file-name "var/prescient-save.el" my:cache-dir))
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode t))

;; (use-package ivy-rich
;;   :config
;;   (ivy-rich-mode 1)
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;;; ivy.el ends here
