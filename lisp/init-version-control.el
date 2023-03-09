;;; -*- lexical-binding: t; -*-

;;; Section: Version control.

(require 'init-packaging)

;;; Section: Version control.

(use-package magit
  :init
  (eval-when-compile
    (require 'lib-keybinding))
  :general
  (my:space-vc-leader-def "g" 'magit-status)
  (my:space-vc-leader-def "b" 'magit-blame))

(provide 'init-version-control)
