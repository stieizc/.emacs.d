;;; -*- lexical-binding: t; -*-

;;; Lots of code and ideas are borrowed from
;;; https://d12frosted.io/posts/2021-04-09-emacs-d.html
;;; https://github.com/d12frosted/environment/blob/master/emacs/init.el
;;; Thank you!

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq
 ;; update ui less often
 idle-update-delay 2)

(require 'init-package-management)
(require 'init-start-up)
(require 'init-debug)
(require 'init-evil)
;; (require 'init-basic-editing)
;; (require 'init-ui)
;; (require 'init-fonts)
;; (require 'init-orgmode)

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
