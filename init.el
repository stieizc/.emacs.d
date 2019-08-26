;;; init --- emacs initialization

;;; Commentary:
;;; Initialization

;;; Code:
(message "Initializing emacs: %s on %s" user-init-file window-system)

(defvar my:debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all config functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")

(defun my:fullpath-current-file ()
  (file-name-directory (or load-file-name buffer-file-name)))

(defun xah:fullpath-relative-to-current-file (@file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's file location.
See http://ergoemacs.org/emacs/organize_your_dot_emacs.html"
  (concat (my:fullpath-current-file) @file-relative-path))

(defun my:load-config (@file-relative-path)
  (load (concat user-emacs-directory @file-relative-path)))

(defun my:load-config-when-exists (@file-relative-path)
  (let ((path (concat user-emacs-directory @file-relative-path)))
    (when (file-exists-p (concat path ".el"))
      (load path))))

;; I know, I know. I'd like to have emacs accept custom files
;; under different path than "~/.emacs.d"
(setq user-emacs-directory (my:fullpath-current-file))

(my:load-config "package-management")
;; load org-mode settings as early as possible
;; see link in org.el
(my:load-config "org-straight")
(my:load-config "basic-settings")
(my:load-config "evil")
(my:load-config "evil-hybrid-mode")
(my:load-config "magit")
(my:load-config "ivy")
(my:load-config "projectile")
(my:load-config "ui")
(my:load-config "font")
(my:load-config "lang")
(my:load-config "pdf")
(my:load-config-when-exists "priv")
(my:load-config "work")
(my:load-config "life")
;; (my:load-config "utils")

;;; init.el ends here
