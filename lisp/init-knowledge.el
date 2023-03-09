;;; -*- lexical-binding: t; -*-

;;; Section: Knowledge Base.
;; Org-mode and friends.

(require 'init-packaging)

(use-package org
  :straight '(org :type built-in)
  :defer t
  :init
  (setq
   ;; Without this org mode will add indentation
   ;; after headings.
   ;; Also, use M-return for inserting headlines!
   org-adapt-indentation nil)
  :config
  (require 'org-tempo))

(use-package org-roam
  :defer t)

(provide 'init-knowledge)
