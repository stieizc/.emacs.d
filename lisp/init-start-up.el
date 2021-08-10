;;; -*- lexical-binding: t; -*-

(defvar my:scratchdir (expand-file-name "~/zscratchpad/"))
(defvar my:todo (expand-file-name "~/zscratchpad/todo.org"))

(setq
  inhibit-startup-message t
  ;; prefer newer .el instead of the .elc
  load-prefer-newer t
  initial-buffer-choice my:todo)

(provide 'init-start-up)
