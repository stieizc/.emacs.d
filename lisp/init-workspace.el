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

(use-package bufler
  :hook ((emacs-startup . bufler-mode))
  :config
  (require 'consult)
  (defvar consult--bufler-workspace+
    `(:name "Workspace"
            :narrow ?w
            :category buffer
            :face consult-buffer
            :history  buffer-name-history
            :state    ,#'consult--buffer-state
            :enabled  ,(lambda () (frame-parameter nil 'bufler-workspace-path))
            :items
            ,(lambda ()
               (let ((bufler-vc-state nil))
                 (mapcar #'buffer-name
                         (mapcar #'cdr
                                 (bufler-buffer-alist-at
                                  (frame-parameter nil 'bufler-workspace-path)
                                  :filter-fns bufler-filter-buffer-fns))))))
    "Bufler workspace buffers source for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--bufler-workspace+ 'append))

;;(use-package perspective
;;  :commands (persp-mode)
;;  :custom
;;  (persp-suppress-no-prefix-key-warning t)
;;  (persp-state-default-file (expand-file-name "perspectives.el" no-littering-var-directory))
;;  :init
;;  (add-hook 'kill-emacs-hook #'persp-state-save)
;;  (add-hook 'emacs-startup-hook
;;            #'(lambda ()
;;                (persp-mode t)
;;                ;; (persp-state-load persp-state-default-file)
;;                ))
;;  :config
;;  (require 'consult)
;;  (my:space-leader-def
;;    "x" '(:keymap perspective-map :which-key "perspectives" :package perspective))
;;
;;  (consult-customize consult--source-buffer :hidden t :default nil)
;;  (add-to-list 'consult-buffer-sources persp-consult-source))

(provide 'init-workspace)
