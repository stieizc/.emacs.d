;;; init-evil --- evil initialization

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
   "<SPC>" 'save-buffer))

;;; init-evil.el ends here
