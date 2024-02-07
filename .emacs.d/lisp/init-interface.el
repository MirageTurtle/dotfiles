;;; init-interface.el

;; For font
;; References: https://github.com/lujun9972/emacs-document/blob/master/org-mode/%E7%BE%8E%E5%8C%96%20Org%20mode.org
(when (member "Symbola" (font-family-list))
  (set-fontset-font "fontset-default" nil
                    (font-spec :size 20 :name "Symbola")))
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(use-package cnfonts
  :ensure t
  :init
  (cnfonts-mode 1)
  :config
  (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-decrease-fontsize)
  (define-key cnfonts-mode-map (kbd "C-+") #'cnfonts-increase-fontsize))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dashboard
 :ensure t
 :config
 (setq dashboard-banner-logo-title "Welcome to Emacs!")
 (setq dashboard-projects-backend 'projectile)
 (setq dashboard-startup-banner 'official)
 (setq dashboard-items '((recents  . 10)
		  (bookmarks . 5)
		  (projects . 5)))
 (dashboard-setup-startup-hook))

(provide 'init-interface)

;;; init-interface.el ends here.
