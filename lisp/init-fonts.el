;;; -*- lexical-binding: t; -*-

;;; Section: Fonts.
;;; Fonts and friends.

;; (set-face-attribute 'default nil
;;                     :family "Source Code Pro"
;;                     :height 110
;;                     :weight 'normal
;;                     :width 'normal)

;; https://github.com/zhangjunphy/breeze/blob/master/breeze-ui.el
(defun breeze--get-display-dpi ()
  "Get DPI of the display."
  (if (display-graphic-p)
      (let* ((attrs (frame-monitor-attributes))
             (mm-height (car (cddr (assoc 'mm-size attrs))))
             (pixel-height (nth 3 (cdr (assoc 'geometry attrs)))))
        (round (/ pixel-height (/ mm-height 25.4))))
    (error "Attempt to calculate the dpi of a non-graphic display")))

(defun breeze--set-font-and-size
    (latin-family latin-size cjk-family cjk-size)
  "Font settings.
LATIN-FAMILY: font family for latin characters.
LATIN-SIZE: font size for latin characters.
CJK-FAMILY: font family for CJK characters.
CJK-SIZE: font size for CJK characters."
  (set-face-attribute
   'default nil
   :font (font-spec :family latin-family :size latin-size))
  ;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
  ;;   (set-fontset-font
  ;;    t charset (font-spec :family cjk-family :size cjk-size)))
  )

(defun breeze--set-font-and-scale (family scale)
  "Font settings.
FAMILY: font family.
SCALE: font scale."
  (let ((spec '(font-spec :family family)))
    (set-face-attribute
     'default nil
     :font spec)
    (assq-delete-all family face-font-rescale-alist)
    (add-to-list 'face-font-rescale-alist `(,family . ,scale))))

(defun breeze-refresh-frame (&optional frame)
  "Refresh frame."
  (interactive)
  (with-selected-frame (or frame (selected-frame))
    (if (display-graphic-p)
        (if (>= (breeze--get-display-dpi) 109)
            (breeze--set-font-and-size "Sarasa Mono SC" 24 "Sarasa Mono SC" 24)
          ;; (breeze--set-font-and-scale "Sarasa Mono SC" 5)
          (breeze--set-font-and-size "Sarasa Mono SC" 19 "Sarasa Mono SC" 19)
          ;; (breeze--set-font-and-scale "Sarasa Mono SC" 1)
          ))))
;; (load-theme 'doom-solarized-light t)
;; (spaceline-compile)))

(add-hook 'after-make-frame-functions #'breeze-refresh-frame)
(add-hook 'window-size-change-functions #'breeze-refresh-frame)

(provide 'init-fonts)
;;; font.el ends here
