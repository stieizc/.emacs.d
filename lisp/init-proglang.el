;;; -*- lexical-binding: t; -*-

;;; Section: Programming languanges.

(require 'init-packaging)

(use-package clang-format
  :straight '(clang-format
              :type nil
              :local-repo "/usr/share/emacs/site-lisp/clang-format-6.0")
  :commands (clang-format clang-format-region clang-format-buffer))

(provide 'init-proglang)
