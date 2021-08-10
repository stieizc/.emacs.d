;;; -*- lexical-binding: t; -*-

;;; Lots of code are borrowed from
;;; https://github.com/d12frosted/environment/blob/master/emacs/init.el
;;; Thank you!

(message "Initializing emacs: %s on %s" user-init-file window-system)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-performance-tune)
(require 'init-start-up)
(require 'init-package-management)
(require 'init-debug)
(require 'init-evil)
(require 'init-basic-editing)

;; (my:load-config-when-exists "priv-config")
;; ;; load org-mode settings as early as possible
;; ;; see link in org.el
;; (my:load-config "org-straight")
;; (my:load-config "magit")
;; (my:load-config "ivy")
;; (my:load-config "projectile")
;; (my:load-config "ui")
;; (my:load-config "font")
;; (my:load-config "lang")
;; (my:load-config "pdf")
;; (my:load-config-when-exists "priv")
;; (my:load-config "work")
;; (my:load-config "life")
;; (my:load-config "utils")
