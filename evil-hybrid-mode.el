;;; evil-hybrid-mode --- evil hybrid mode from spacemacs

;;; Commentary:

;;; Code:

;; hybrid mode, like spacemacs
;; https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Bdistributions/spacemacs-base/local/hybrid-mode/hybrid-mode.el

;; When I use code from spacemacs, C-k/C-y still gets shadowed by evil-insert-state-map
;; So I use code from here
;; https://stackoverflow.com/questions/25542097/emacs-evil-mode-how-to-change-insert-state-to-emacs-state-automatically/28985130#28985130

(defconst evil-insert-state-map-backup evil-insert-state-map
  "Backup of `evil-insert-state-map'.")

(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

;;; evil-hybrid-mode.el ends here
