(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))

(add-hook 'after-init-hook
    (lambda ()
     (mapc 'require '(init-evil))
     ))
