;;; work

;;; Commentary:

;;; Code:

(use-package miyuki
  :straight (miyuki
	     :local-repo "miyuki"
   	     :repo "https://github.com/wenxin-wang/miyuki"
	     :files ("miyuki.el")))

(use-package org-miyuki
  :straight (org-miyuki
	     :local-repo "miyuki"
   	     :repo "https://github.com/wenxin-wang/miyuki"
	     :files ("org-miyuki.el"))
  :functions (org-miyuki/ivy-list-keywords org-miyuki/insert-header org-miyuki/org-link-init)
  :config
  (with-eval-after-load 'org
    (org-miyuki/org-link-init))
  (evil-leader/set-key
    "wh" #'org-miyuki/insert-header
    "wK" #'org-miyuki/ivy-list-keywords))

(use-package counsel-miyuki
  :straight (counsel-miyuki
	     :local-repo "miyuki"
   	     :repo "https://github.com/wenxin-wang/miyuki"
	     :files ("counsel-miyuki.el"))
  :functions (counsel-miyuki/ag counsel-miyuki/find-file counsel-miyuki/find-all-file)
  :config
  (evil-leader/set-key
    "ww" #'counsel-miyuki/ag
    "wf" #'counsel-miyuki/find-file
    "wF" #'counsel-miyuki/find-all-file))

(use-package mu4e
  :straight nil
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :init
  (setq mu4e-maildir       "~/Maildir"   ;; top-level Maildir
	mu4e-sent-folder   "/fastmail/Sent"       ;; folder for sent messages
	mu4e-drafts-folder "/fastmail/Drafts"     ;; unfinished messages
	mu4e-trash-folder  "/fastmail/Trash"      ;; trashed messages
	mu4e-refile-folder "/fastmail/Archive"
	mu4e-get-mail-command "mbsync -V fastmail-inbox")
  :config
  (defun mu4e-headers-mark-all (MARKPAIR)
    "Put a ! \(read) mark on all visible unread messages"
    (mu4e-headers-mark-for-each-if
     MARKPAIR
     (lambda (msg param) t)))
  (defun mu4e-headers-mark-all-delete ()
    "Put a ! \(read) mark on all visible unread messages"
    (interactive)
    (mu4e-headers-mark-all '(delete)))
  (evil-leader/set-key
    "ue" #'mu4e
    "uD" #'mu4e-headers-mark-all-delete))
;;; work.el ends here
