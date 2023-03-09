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

;; Start-up setup.
(setq
 package-enable-at-startup nil
 inhibit-startup-message t
 ;; prefer newer .el instead of the .elc
 load-prefer-newer t)

;;; Section: Acknowledgement.
;; Lots of code and ideas are borrowed from
;; https://d12frosted.io/posts/2021-04-09-emacs-d.html
;; https://github.com/d12frosted/environment/blob/master/emacs/init.el
;; Thank you!

;;; Bootstrap straight.el
(load (expand-file-name "lisp/init-bootstrap-straight" user-emacs-directory))

;;; Load my init files.
;; With this I can:
;; - Byte-compile my init files;
;; - Use autoloads with my library files;
;; - Specify dependencies between sections of my init, so I can split sections
;; into files, and clarify the dependencies between them.
(if (bound-and-true-p my-init:no-byte-compile)
    ;; When byte-compilation step in my way, load the *.el files for debugging purpose.
    (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (straight-use-package
   `(0init :type nil :local-repo ,(concat user-emacs-directory "lisp"))))

(require '0init)

(provide 'early-init)
