;;; magit --- magit initialization

;;; Commentary:

;;; Code:

;; magit

(use-package magit)
(use-package evil-magit
  :config
  (setq-default evil-magit-use-y-for-yank t)
  (evil-leader/set-key
   "gg" 'magit-status
   "gS"  'magit-stage-file
   "gU"  'magit-unstage-file))

;;; magit.el ends here
