;;; lang --- emacs lang components initialization

;;; Commentary:

;;; Code:

;;; Editor Config

;; I think editor config just change some basic indent settings
;; It doesn't change indent logic
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;; Clang-format

(defvar-local my:c-common-use-clang-format t
  "use clang-format for regiion indent, default to t")

(defun my:c-common-hook ()
  (when my:c-common-use-clang-format
    (make-local-variable 'indent-region-function)
    (setq indent-region-function #'clang-format-region)))

(use-package clang-format
  :config
  (add-hook 'c-mode-common-hook #'my:c-common-hook))

;;; Elisp setup

;; https://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/
(defun er:remove-elc-on-save ()
  "If you're saving an Emacs Lisp file, likely the .elc is no longer valid."
  (add-hook
   'after-save-hook
   (lambda ()
     (if (file-exists-p (concat buffer-file-name "c"))
	 (delete-file (concat buffer-file-name "c"))))
   nil
   t))
(add-hook 'emacs-lisp-mode-hook 'er:remove-elc-on-save)

;;; Haskell setup

(use-package haskell-mode
  :defer
  :config
  ;; (setq-default haskell-tags-on-save nil
  ;; 		haskell-process-suggest-remove-import-lines t
  ;; 		haskell-process-auto-import-loaded-modules t
  ;; 		haskell-process-type 'stack-ghci)
  ;; (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
  ;; (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  )

;;; PEG

(use-package peg-mode
  :straight nil
  :load-path "third-party/peg-mode.el/"
  :mode ("\\.peg\\'" . peg-mode))

;;; lang.el ends here
