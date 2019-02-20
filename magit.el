;;; magit --- magit initialization

;;; Commentary:

;;; Code:

;; magit

(use-package transient
  :straight (transient :type git
		       :repo "https://github.com/magit/transient")
  :init
  (setq transient-levels-file (expand-file-name "levels.el" my:cache-dir)
	transient-values-file (expand-file-name "values.el" my:cache-dir)
	transient-history-file (expand-file-name "history.el" my:cache-dir)))
(use-package magit)
(use-package evil-magit
  :config
  (setq-default evil-magit-use-y-for-yank t)
  (evil-leader/set-key
   "gg" 'magit-status
   "gS"  'magit-stage-file
   "gU"  'magit-unstage-file))

;;; magit.el ends here
