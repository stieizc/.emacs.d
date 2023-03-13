;;; -*- lexical-binding: t; -*-

;;; Section: More packaging.

(require 'init-bootstrap-straight)

;;; Install use-package
(straight-use-package `use-package)
(setq
 ;; If use-package-use-theme is set to t, it will put all the :custom definitions
 ;; under use-package-theme, which for some unknown reason is NOT enabled by default
 ;; under my settings.
 use-package-use-theme nil
 use-package-verbose t)
(eval-when-compile
  (require 'use-package))

;; Setup no-littering as early as possible.
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :config
  (setq custom-file (expand-file-name "custom.el" no-littering-etc-directory)))

;; https://github.com/myrjola/diminish.el
(use-package diminish
  :commands diminish)

;; https://github.com/noctuid/general.el
(eval-when-compile
  (setq general-use-package-emit-autoloads nil))
(use-package general)

;;(use-package bind-key)

;; https://github.com/radian-software/el-patch
(use-package el-patch
  :defer t)

(use-package which-key
  :hook ((emacs-startup . which-key-mode)))

(provide 'init-packaging)
