;;; -*- lexical-binding: t; -*-

;;; Section: Functions related to windows managing.

;;;###autoload
(defun exwm-all-buffers ()
  (seq-filter
   (lambda (buffer)
     (eq 'exwm-mode (buffer-local-value 'major-mode buffer)))
   (buffer-list)))

;;;###autoload
(defun consult-buffer-state-filter ()
  "Buffer state function that doesn't preview X/Tramp buffers."
  (let ((orig-state (consult--buffer-state))
        (filter (lambda (action cand)
                  (if (and cand
                           (or (eq action 'return)
                               (let ((buffer (get-buffer cand)))
                                 (and buffer
                                      (not (eq 'exwm-mode (buffer-local-value 'major-mode buffer)))
                                      (not (file-remote-p (buffer-local-value 'default-directory buffer)))))))
                      cand
                    nil))))
    (lambda (action cand)
      (funcall orig-state action (funcall filter action cand)))))

;;;###autoload
(defun exwm-move-window-to-workspace (workspace-number)
  (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
        (id (exwm--buffer->id (window-buffer))))
    (exwm-workspace-move-window frame id)))

;;;###autoload
(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(provide 'lib-wm)
