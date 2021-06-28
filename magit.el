;;; magit --- magit initialization

;;; Commentary:

;;; Code:

;; magit

(use-package transient
  :straight (transient :host github :repo "magit/transient"))

(use-package magit
  :config
  (evil-leader/set-key
   "gg" 'magit-status
   "gS"  'magit-stage-file
   "gU"  'magit-unstage-file))


;;; magit.el ends here
