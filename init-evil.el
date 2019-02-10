;;; init-evil --- evil initialization

;;; Commentary:

;;; Code:

;; evil-leader
;; jiege!
;; https://github.com/jiegec/emacs.d/blob/master/lisp/init-evil.el

(use-package evil-leader
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>"))

;; evil

(use-package evil
  :config
  (setq-default evil-auto-indent nil)
  (evil-mode t)
  (evil-leader/set-key
   "<SPC>" 'save-buffer))

;; hybrid mode borrow from spacemacs
;; https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Bdistributions/spacemacs-base/local/hybrid-mode/hybrid-mode.el

(defcustom hybrid-mode-enable-evilified-state t
  "If non nil then evilified states is enabled in buffer supporting it."
  :group 'spacemacs
  :type 'boolean)

(defvar hybrid-mode-default-state-backup evil-default-state
  "Backup of `evil-default-state'.")

(defadvice evil-insert-state (around hybrid-insert-to-hybrid-state disable)
  "Forces Hybrid state."
  (evil-hybrid-state))

(defadvice evil-evilified-state (around hybrid-evilified-to-hybrid-state disable)
  "Forces Hybrid state."
  (if (equal -1 (ad-get-arg 0))
      ad-do-it
    (if hybrid-mode-enable-evilified-state
        ad-do-it
      ;; seems better to set the emacs state instead of hybrid for evilified
      ;; buffers
      (evil-emacs-state))))

;;;###autoload
(define-minor-mode hybrid-mode
  "Global minor mode to replace insert state by hybrid state."
  :global t
  :lighter " hybrid"
  :group 'spacemacs
  (if hybrid-mode
      (enable-hybrid-editing-style)
    (disable-hybrid-editing-style)))

(defun enable-hybrid-editing-style ()
  "Enable the hybrid editing style."
  (setq hybrid-mode-default-state-backup evil-default-state
        evil-default-state hybrid-mode-default-state)
  ;; replace evil states by `hybrid state'
  (ad-enable-advice 'evil-insert-state
                    'around 'hybrid-insert-to-hybrid-state)
  (ad-enable-advice 'evil-evilified-state
                    'around 'hybrid-evilified-to-hybrid-state)
  (ad-activate 'evil-insert-state)
  (ad-activate 'evil-evilified-state)
  ;; key bindings hooks for dynamic switching of editing styles
  ;; (run-hook-with-args 'spacemacs-editing-style-hook 'hybrid)
  ;; initiate `hybrid state'
  (hybrid-mode//update-states-for-current-buffers 'hybrid))

(defun disable-hybrid-editing-style ()
  "Disable the hybrid editing style (reverting to 'vim style)."
  (setq evil-default-state hybrid-mode-default-state-backup)
  ;; restore evil states
  (ad-disable-advice 'evil-insert-state
                     'around 'hybrid-insert-to-hybrid-state)
  (ad-disable-advice 'evil-evilified-state
                     'around 'hybrid-evilified-to-hybrid-state)
  (ad-activate 'evil-insert-state)
  (ad-activate 'evil-evilified-state)
  ;; restore key bindings
  ;; (run-hook-with-args 'spacemacs-editing-style-hook 'vim)
  ;; restore the states
  (hybrid-mode//update-states-for-current-buffers 'vim))

;; This code is from evil insert state definition, any change upstream
;; should be reflected here
;; see https://github.com/emacs-evil/evil/blob/56e92f7cb4e04e665670460093b41f58446b7a2b/evil-states.el#L108
(evil-define-state hybrid
  "Hybrid state for hybrid mode."
  :tag " <H> "
  :cursor (bar . 2)
  :message "-- HYBRID --"
  :entry-hook (evil-start-track-last-insertion)
  :exit-hook (evil-cleanup-insert-state evil-stop-track-last-insertion)
  :input-method t
  (cond
   ((evil-hybrid-state-p)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
    (add-hook 'pre-command-hook #'evil-insert-repeat-hook)
    (setq evil-maybe-remove-spaces t)
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step)))
   (t
    (remove-hook 'post-command-hook #'evil-maybe-remove-spaces)
    (remove-hook 'pre-command-hook #'evil-insert-repeat-hook)
    (evil-maybe-remove-spaces t)
    (setq evil-insert-repeat-info evil-repeat-info)
    (evil-set-marker ?^ nil t)
    (unless (eq evil-want-fine-undo t)
      (evil-end-undo-step))
    (when evil-move-cursor-back
      (when (or (evil-normal-state-p evil-next-state)
                (evil-motion-state-p evil-next-state))
        (evil-move-cursor-back))))))

(define-key evil-hybrid-state-map [escape] 'evil-normal-state)

;; Override stock evil function `evil-insert-state-p'
(defun evil-insert-state-p (&optional state)
  "Whether the current state is insert."
  (and evil-local-mode
       (memq (or state evil-state) '(insert hybrid))))

(defun hybrid-mode//update-states-for-current-buffers (style)
  "Update the active state in all current buffers given current STYLE."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond
       ((eq 'hybrid style)
        (if (memq major-mode evil-evilified-state-modes)
            (evil-evilified-state)
          (funcall (intern (format "evil-%S-state"
                                   hybrid-mode-default-state)))))
       ((and (eq 'vim style)
             (memq evil-state '(hybrid emacs)))
        (cond
         ((memq major-mode evil-evilified-state-modes) (evil-evilified-state))
         ((memq major-mode evil-motion-state-modes) (evil-motion-state))
         (t (evil-normal-state))))))))

;;; init-evil.el ends here
