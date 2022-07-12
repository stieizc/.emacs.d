;;; -*- lexical-binding: t; -*-

(require 'config-path)

(setq
 ;; update ui less often
 idle-update-delay 2
 inhibit-startup-message t
 ;; prefer newer .el instead of the .elc
 load-prefer-newer t
 initial-buffer-choice my:todo)

(provide 'init-start-up)
