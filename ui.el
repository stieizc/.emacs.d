;;; ui --- ui initialization

;;; Commentary:

;;; Code:

;; which-key

(use-package which-key
  :config
  (which-key-mode))

(use-package all-the-icons)

(use-package neotree
  :config
  (evil-leader/set-key
    "pt" 'neotree-toggle))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-city-lights t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;;; ui.el ends here
