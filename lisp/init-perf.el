;;; -*- lexical-binding: t; -*-

;;; Section: Debug utilities.

(require 'init-packaging)

(setq
 ;; update ui less often
 idle-update-delay 2)

(use-package esup
  :commands (esup))

(provide 'init-perf)
