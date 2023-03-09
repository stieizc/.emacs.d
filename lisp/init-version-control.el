;;; -*- lexical-binding: t; -*-

;;; Section: Version control.

(require 'init-packaging)
(eval-when-compile
    (require 'lib-keybinding))

;;; Section: Version control.

(use-package magit
  :defer t
  :general
  (my:space-vc-leader-def "g" 'magit-status)
  (my:space-vc-leader-def "b" 'magit-blame))

(provide 'init-version-control)
