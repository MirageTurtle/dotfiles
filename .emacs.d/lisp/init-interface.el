;;; init-interface.el

(use-package cnfonts
  :ensure t
  :init
  (cnfonts-mode 1)
  :config
  (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
  (define-key cnfonts-mode-map (kbd "C-+") #'cnfonts-increase-fontsize)
  (setq cnfonts-profiles '("Program"))
  (setq cnfonts-personal-fontnames '(("JetBrains Mono"))))

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
