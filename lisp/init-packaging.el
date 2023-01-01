;;; -*- lexical-binding: t; -*-

;;; Section: More packaging.

(require 'init-bootstrap-straight)

;;; Install use-package
(straight-use-package `use-package)
(setq
 use-package-always-defer t
 ;; If use-package-use-theme is set to t, it will put all the :custom definitions
 ;; under use-package-theme, which for some unknown reason is NOT enabled by default
 ;; under my settings.
 use-package-use-theme nil)
(eval-when-compile
  (require 'use-package)
  (custom-set-variables
   '(use-package-verbose t)))

;; Setup no-littering as early as possible.
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :demand t
  :config
  (setq custom-file (expand-file-name "custom.el" no-littering-etc-directory)))

;; https://github.com/myrjola/diminish.el
(use-package diminish
  :commands diminish)

;; https://github.com/noctuid/general.el
(use-package general
  :init
  (eval-when-compile
    (require 'general)
    (require 'lib-keybinding))
  :custom
  (general-use-package-emit-autoloads nil))

;; If we realy need :bind in use-package calls, we
;; need the following blocks
;; (use-package bind-key
;;   :demand t)

;; https://github.com/radian-software/el-patch
(use-package el-patch
  :defer t)

(provide 'init-packaging)
