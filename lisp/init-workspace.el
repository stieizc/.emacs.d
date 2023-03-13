;;; -*- lexical-binding: t; -*-

;;; Section: General workspace setup.

(require 'init-packaging)
(require 'config-path)
(eval-when-compile
  (require 'consult))
(eval-when-compile
  (require 'lib-keybinding))

(setq
 initial-buffer-choice my:todo)

(use-package perspective
  :commands (persp-mode)
  :custom
  (persp-suppress-no-prefix-key-warning t)
  (persp-state-default-file (expand-file-name "perspectives.el" no-littering-var-directory))
  :init
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (add-hook 'emacs-startup-hook
            #'(lambda ()
                (persp-mode t)
                ;; (persp-state-load persp-state-default-file)
                ))
  :config
  (my:space-leader-def
    "x" '(:keymap perspective-map :which-key "perspectives" :package perspective))

  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

(provide 'init-workspace)
