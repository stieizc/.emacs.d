;;; work

;;; Commentary:

;;; Code:

(use-package org-simple-wiki
  :straight (org-simple-wiki
	     :repo "https://github.com/wenxin-wang/org-simple-wiki"
	     :files ("org-simple-wiki.el"))
  :config
  (evil-leader/set-key
        "wh" #'org-simple-wiki-insert-header))

;;; work.el ends here
