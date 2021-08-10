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

(provide 'lib-path)
