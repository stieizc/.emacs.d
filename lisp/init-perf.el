;;; -*- lexical-binding: t; -*-

;;; Section: Debug utilities.

(require 'init-bootstrap-straight)

(setq
 ;; update ui less often
 idle-update-delay 2)

(use-package esup
  :commands (esup))

(provide 'init-perf)
