;;; -*- lexical-binding: t; -*-

;;; Section: Functions related to paths.

;;;###autoload
(defun my:fullpath-current-file ()
  (file-name-directory (or load-file-name buffer-file-name)))

;;;###autoload
(defun xah:fullpath-relative-to-current-file (@file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's file location.
See http://ergoemacs.org/emacs/organize_your_dot_emacs.html"
  (concat (my:fullpath-current-file) @file-relative-path))

(provide 'lib-path)
