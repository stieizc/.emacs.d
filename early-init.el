;;; -*- lexical-binding: t; -*-

;;; Section: Emacs 27+ pre-initialisation config
;;; Copied from https://github.com/purcell/emacs.d/blob/master/early-init.el

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold
                             normal-gc-cons-threshold))))

(setq
 package-enable-at-startup nil
 inhibit-startup-message t
 ;; prefer newer .el instead of the .elc
 load-prefer-newer t)
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
;; (add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; So we can detect this having been loaded
(provide 'early-init)
