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
  (defun my:todo-file-today()
    (expand-file-name
      (format-time-string "%Y-%m-%d-todo.org")
      my:scratchdir))
  (defun my:journal-file-today()
    (expand-file-name
      (format-time-string "%Y-%m-%d-journal.org")
      my:scratchdir))
  (defun my:reading-file-today()
    (expand-file-name
      (format-time-string "%Y-%m-%d-reading.org")
      my:scratchdir))
  (defun my:org-capture-todo ()
    (interactive)
    (org-capture nil "t"))
  (defun my:org-capture-journal ()
    (interactive)
    (org-capture nil "j"))
  (defun my:org-capture-reading ()
    (interactive)
    (org-capture nil "r"))
  (setq org-startup-folded 'showall
    org-adapt-indentation nil
    org-src-preserve-indentation t
    org-link-descriptive nil
    org-file-apps
    '((auto-mode . emacs)
       (directory . emacs)
       ("\\.mm\\'" . default)
       ("\\.x?html?\\'" . default)
       ("\\.pdf\\'" . "okular %s"))
    org-capture-templates
    '(("t" "Task" entry (file my:todo-file-today)
        "* TODO %?\n  %u\n  %a\n")
       ("j" "Journal" entry (file my:journal-file-today)
         "* %?\n  %u\n  %a\n")
       ("r" "Reading" entry (file my:reading-file-today)
         "* %?\n")
       ("w" "Website" entry (file my:reading-file-today)
         "* %a\n%:initial\n%?")))
  :config
  (require 'org-tempo)
  (require 'org-protocol)
  :config
  (evil-leader/set-key
    "wc" #'org-clock-in
    "wC" #'org-clock-out
    "ww" #'my:org-capture-todo
    "we" #'my:org-capture-journal
    "wr" #'my:org-capture-reading))

;;; org.el ends here
