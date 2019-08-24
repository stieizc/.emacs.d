;;; org --- org-mode initialization

;;; Commentary:

;;; Code:

;; Need to load org-mode as early as possible, see
;; https://github.com/raxod502/straight.el#installing-org-with-straightel

(require 'subr-x)
(use-package git)

(defun org-git-version ()
  "The Git version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                    "straight/repos/org/" user-emacs-directory)))
    (string-trim
      (git-run "describe"
               "--match=release\*"
               "--abbrev=6"
               "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                    "straight/repos/org/" user-emacs-directory)))
    (string-trim
      (string-remove-prefix
        "release_"
        (git-run "describe"
                 "--match=release\*"
                 "--abbrev=0"
                 "HEAD")))))

(provide 'org-version)

(use-package org ; or org-plus-contrib if desired
  :init
  (setq org-startup-folded 'showall
	org-adapt-indentation nil
	org-src-preserve-indentation t)
  :config
  (require 'org-tempo))

;;; org.el ends here
