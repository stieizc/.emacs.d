;;; -*- lexical-binding: t; -*-

(use-package org
  :straight nil
  :init
  (setq
   ;; Without this org mode will add indentation
   ;; after headings.
   ;; Also, use M-return for inserting headlines!
   org-adapt-indentation nil))

(provide 'init-orgmode)
