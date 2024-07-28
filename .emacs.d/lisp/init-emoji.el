;;; init-emoji.el --- Emoji support -*- lexical-binding: t; -*-

;; (when (member "Segoe UI Emoji" (font-family-list))
;;   (set-fontset-font
;;     t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))

(cond
 ((eq system-type 'darwin)
  (set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji") nil 'prepend))
 ((eq system-type 'gnu/linux)
  (set-fontset-font t 'unicode (font-spec :family "Noto Color Emoji") nil 'prepend)))

(provide 'init-emoji)

;;; init-emoji.el ends here
