;;; evil --- evil initialization

;;; Commentary:

;;; Code:

;; evil-leader
;; jiege!
;; https://github.com/jiegec/emacs.d/blob/master/lisp/init-evil.el

(use-package evil-leader
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>"))

;; evil

(use-package evil
  :config
  (setq-default evil-auto-indent nil)
  (evil-mode t)
  (evil-leader/set-key
   "<SPC>" 'save-buffer
   "bb" 'switch-to-buffer
   "bk" 'kill-buffer
   "qq" 'save-buffers-kill-terminal
   "z-" 'text-scale-adjust
   "z+" 'text-scale-adjust
   "z0" 'text-scale-adjust))

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t
	undo-tree-history-directory-alist
	`((".*" . ,(expand-file-name "undo-tree-history" my:cache-dir))))
  :diminish undo-tree-mode)

;;; evil.el ends here
