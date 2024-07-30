;;; init-emoji.el --- Emoji support -*- lexical-binding: t; -*-

;; (when (member "Segoe UI Emoji" (font-family-list))
;;   (set-fontset-font
;;     t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))

(defun mt/frame/emoji (frame)
  "Configure FRAME based on its display type."
  (with-selected-frame frame
    (when (display-graphic-p)
      (message "Setting up emoji font for frame %s" frame)
      (cond
       ((eq system-type 'darwin)
	(set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji") nil 'prepend))
       ((eq system-type 'gnu/linux)
	(set-fontset-font t 'unicode (font-spec :family "Noto Color Emoji") nil 'prepend))))))

;; Add the hook for existing frames
(if (daemonp)
    (add-hook 'after-make-frame-functions #'mt/frame/emoji)
  ;; For non-daemon mode, setup for the initial frame
  (mt/frame/emoji (selected-frame)))

(provide 'init-emoji)

;;; init-emoji.el ends here
