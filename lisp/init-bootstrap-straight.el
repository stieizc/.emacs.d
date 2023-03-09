;;; -*- lexical-binding: t; -*-

;;; Section: Package management base.
;; Setup straight.el & use-package for package management.

;;; Bootstrap straight.el & use-package
;; https://github.com/radian-software/straight.el
(setq
 straight-vc-git-default-clone-depth 1
 straight-repository-branch "develop"
 straight-check-for-modifications nil
 straight-use-package-by-default t)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (with-temp-buffer
    (insert-file bootstrap-file)
    (goto-char (point-min))
    (while (search-forward "https://git.savannah.gnu.org/git/emacs/nongnu.git" nil t)
      (beginning-of-defun)
      (kill-sexp))
    (set-visited-file-name bootstrap-file)
    (setq load-file-name nil)
    (eval-buffer)
    (set-visited-file-name nil)))
;;(load bootstrap-file nil 'nomessage))

(provide 'init-bootstrap-straight)
