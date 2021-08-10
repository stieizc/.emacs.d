;;; -*- lexical-binding: t; -*-

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
       (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold
                 normal-gc-cons-threshold))))

(setq
  ;; update ui less often
  idle-update-delay 2)

(provide 'init-performance-tune)
