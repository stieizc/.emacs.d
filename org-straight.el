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
  (defun my:scratch-find-file()
    (interactive)
    (with-temp-buffer
      (setq default-directory my:scratchdir)
      (counsel-find-file)))
  (defun my:scratch-find-today-title()
    (interactive)
    (with-temp-buffer
      (setq default-directory my:scratchdir)
      (counsel-find-file (format-time-string "%Y-%m-%d-"))))
  (defun my:file-today (name)
    (expand-file-name
      (format "%s-%s" (format-time-string "%Y-%m-%d") name)
      my:scratchdir))
  (defun my:inbox-file-today ()
    (my:file-today "inbox.org"))
  (defun my:todo-file-today ()
    (my:file-today "todo.org"))
  (defun my:journal-file-today()
    (my:file-today "journal.org"))
  (defun my:reading-file-today()
    (my:file-today "reading.org"))
  (defun my:org-capture-inbox ()
    (interactive)
    (org-capture nil "i"))
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
    '(("i" "Inbox" entry (file my:inbox-file-today)
        "* %?\n  %u\n  %a\n")
       ("t" "Task" entry (file my:todo-file-today)
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
    "cf" #'my:scratch-find-file
    "cF" #'my:scratch-find-today-title
    "cp" #'org-capture
    "cc" #'org-clock-in
    "cC" #'org-clock-out
    "ci" #'my:org-capture-inbox
    "cw" #'my:org-capture-todo
    "ce" #'my:org-capture-journal
    "cr" #'my:org-capture-reading))

;;; org.el ends here
