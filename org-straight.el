;;; org --- org-mode initialization

;;; Commentary:

;;; Code:

;; Need to load org-mode as early as possible, see
;; https://github.com/raxod502/straight.el#installing-org-with-straightel

(require 'subr-x)
(use-package git)

(use-package org ; or org-plus-contrib if desired
  :straight (:host github :repo "bzg/org-mode")
  :init
  (defun my:scratch-todo()
    (interactive)
    (find-file (expand-file-name "todo.org" my:scratchdir)))
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
  (setq
    org-startup-folded 'showall
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
        "* %?\n%a\n\n")
       ("t" "Task" entry (file my:todo-file-today)
         "* TODO %?\n%a\n\n")
       ("j" "Journal" entry (file my:journal-file-today)
         "* %?\n%a\n\n")
       ("r" "Reading" entry (file my:reading-file-today)
         "* %?\n%a\n\n")
       ("w" "Website" entry (file my:reading-file-today)
         "* %a\n%:initial\n%?")))
  :config
  (require 'org-tempo)
  (require 'org-protocol)
  :config
  (evil-leader/set-key
    "cF" #'my:scratch-find-today-title
    "cp" #'org-capture
    "cc" #'org-clock-in
    "cC" #'org-clock-out
    "ci" #'my:org-capture-inbox
    "ct" #'my:scratch-todo
    "cT" #'my:org-capture-todo
    "cj" #'my:org-capture-journal
    "cr" #'my:org-capture-reading))

;;; org.el ends here
