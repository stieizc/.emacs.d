;;; lang --- emacs lang components initialization

;;; Commentary:

;;; Code:

;;; Editor Config

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;; Elisp setup

;; https://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/
(defun er:remove-elc-on-save ()
  "If you're saving an Emacs Lisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                (delete-file (concat buffer-file-name "c"))))
            nil
            t))
(add-hook 'emacs-lisp-mode-hook 'er:remove-elc-on-save)

;;; lang.el ends here
