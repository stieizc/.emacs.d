;;; -*- lexical-binding: t; -*-

;;; Lots of code and ideas are borrowed from
;;; https://github.com/d12frosted/environment/blob/master/emacs/init.el
;;; Thank you!

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
       (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold
                 normal-gc-cons-threshold))))

(setq
  ;; update ui less often
  idle-update-delay 2)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-start-up)
(require 'init-package-management)
(require 'init-debug)
(require 'init-evil)
(require 'init-basic-editing)
(require 'init-ui)
(require 'init-fonts)
(require 'init-orgmode)

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
