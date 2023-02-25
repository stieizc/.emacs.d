;;; -*- lexical-binding: t; -*-

;;; Section: Windows Management.

(require 'init-packaging)

;;; - VTerm
(use-package vterm)
(use-package multi-vterm)

;;; - exwm
(use-package exwm
  :hook ((before-init . exwm-enable))
  :config
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (require 'exwm-randr)
  (exwm-randr-enable)
  (require 'exwm-config)
  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
  ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
  ;; are run when a new X window class name or title is available.  Here's
  ;; some advice on this topic:
  ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
  ;; + For applications with multiple windows (e.g. GIMP), the class names of
                                        ;    all windows are probably the same.  Using window titles for them makes
  ;;   more sense.
  ;; In the following example, we use class names for all windows except for
  ;; Java applications and GIMP.
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-title)))

  ;; To add a key binding only available in line-mode, simply define it in
  ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

  ;; The following example demonstrates how to use simulation keys to mimic
  ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
  ;; and DEST is what EXWM actually sends to application.  Note that both SRC
  ;; and DEST should be key sequences (vector or string).
  (setq exwm-input-simulation-keys
        '(
          ;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ;; cut/paste.
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])))
  :custom
  (exwm-workspace-show-all-buffers t)
  (exwm-layout-show-all-buffers t)
  (exwm-workspace-number 5)
  ;; Global keybindings can be defined with `exwm-input-global-keys'.
  ;; Here are a few examples:
  (exwm-input-global-keys
   `(
     ;; Bind "s-r" to exit char-mode and fullscreen mode.
     ([?\s-r] . exwm-reset)
     ;; Bind "s-w" to switch workspace interactively.
     ([?\s-w] . exwm-workspace-switch)
     ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
     ,@(mapcar (lambda (i)
                 `(,(kbd (format "s-%d" i)) .
                   (lambda ()
                     (interactive)
                     (exwm-workspace-switch-create ,i))))
               (number-sequence 0 9))
     ;; Move between windows
     (,(kbd "s-h") . windmove-left)
     ([s-left] . windmove-left)
     (,(kbd "s-l") . windmove-right)
     ([s-right] . windmove-right)
     (,(kbd "s-k") . windmove-up)
     ([s-up] . windmove-up)
     (,(kbd "s-j") . windmove-down)
     ([s-down] . windmove-down)
     ;; Bind "s-&" to launch applications ('M-&' also works if the output
     ;; buffer does not bother you).
     ([?\s-&] . (lambda (command)
                  (interactive (list (read-shell-command "$ ")))
                  (start-process-shell-command command nil command)))
     (,(kbd "s-d") . (lambda ()
                       (interactive)
                       (let ((process-connection-type nil))  ; use a pipe
                         (start-process "rofi" nil "rofi" "-show-icons" "-show" "drun"))))
     (,(kbd "s-<return>") . multi-vterm)
     (,(kbd "s-q") . kill-current-buffer)
     (,(kbd "s-<tab>") . (lambda ()
                           (interactive)
                           (let ((process-connection-type nil))  ; use a pipe
                             (start-process "rofi" nil "rofi" "-show-icons" "-show" "window"))))
     ;; Bind "s-<f2>" to "slock", a simple X display locker.
     (,(kbd "s-<f2>") . (lambda ()
                          (interactive)
                          (start-process "" nil "/usr/bin/slock")))
     (,(kbd "s-<SPC>") . my:space-leader-command)))
  (exwm-manage-configurations
   '(((member exwm-class-name '("firefox"))
      char-mode t))))

(provide 'init-wm)
