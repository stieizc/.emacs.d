;;; font --- font and friends

;;; Commentary:

;;; Code:

;; (set-face-attribute 'default nil
;;                     :family "Source Code Pro"
;;                     :height 110
;;                     :weight 'normal
;;                     :width 'normal)

;; https://github.com/zhangjunphy/breeze/blob/master/breeze-ui.el

(defun breeze--get-display-dpi ()
  "Get DPI of the display."
  (if (display-graphic-p)
      (round (/ (display-pixel-height)
    (/ (display-mm-height) 25.4)))
    (error "Attempt to calculate the dpi of a non-graphic display")))

(defun breeze--set-font-and-size (latin-family
                  latin-size
                  cjk-family
                  cjk-size)
  "Font settings.
LATIN-FAMILY: font family for latin characters.
LATIN-SIZE: font size for latin characters.
CJK-FAMILY: font family for CJK characters.
CJK-SIZE: font size for CJK characters."
  (set-face-attribute
   'default nil
   :font (font-spec :family latin-family :size latin-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     t charset (font-spec :family cjk-family :size cjk-size))))

(defun breeze-refresh-frame (&optional frame)
  "Refresh frame."
  (interactive)
  (with-selected-frame (or frame (selected-frame))
                       (if (display-graphic-p)
                           (if (>= (breeze--get-display-dpi) 150)
                               (breeze--set-font-and-size "Sarasa Mono SC" 25 "Sarasa Mono SC" 25)
                               (breeze--set-font-and-size "Sarasa Mono SC" 20 "Sarasa Mono SC" 20)))))
		       ;; (load-theme 'doom-solarized-light t)
		       ;; (spaceline-compile)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'breeze-refresh-frame))

;;; font.el ends here
