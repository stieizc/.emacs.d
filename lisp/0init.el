;;; -*- lexical-binding: t; -*-

;;; init.el --- True init file.

;; This file is prefixed with '0' so that it is byte-compiled first.
;; This ensures that before compilation, all packages are installed
;; by forcing use-package evaluation with require.

;; TODO: benchmark init.
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-benchmarking.el
(require 'init-packaging)
(require 'init-perf)
(require 'init-evil)
(require 'init-completion)
(require 'init-version-control)
(require 'init-editing)
(require 'init-workspace)
(require 'init-ui)
(require 'init-fonts)
(require 'init-knowledge)
(require 'init-wm)

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(add-hook 'emacs-startup-hook #'server-start)

(provide '0init)
