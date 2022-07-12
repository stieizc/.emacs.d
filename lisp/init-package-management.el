;;; -*- lexical-binding: t; -*-

(setq straight-vc-git-default-clone-depth 1)

(setq
 straight-repository-branch "develop"
 straight-check-for-modifications nil
 straight-use-package-by-default t)

(defvar bootstrap-version)
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
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package diminish)
(use-package el-patch)

;;; 4. setup no-littering as early as possible
;; https://github.com/emacscollective/no-littering
(use-package no-littering)

(setq custom-file (concat no-littering-etc-directory "custom.el"))

(provide 'init-package-management)
