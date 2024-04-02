;;; init-interface.el
;;; including some font-/unicode-related

(when (member "Symbola" (font-family-list))
  (set-fontset-font "fontset-default" nil
                    (font-spec :size 20 :name "Symbola")))
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(use-package cnfonts
  :ensure t
  :init
  (cnfonts-mode 1)
  :config
  (setq use-default-font-for-symbols nil)
  ;; (setq cnfonts-use-face-font-rescale t)
  (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
  (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)
  (setq cnfonts-profiles '("Program" "Document"))
  (setq cnfonts-personal-fontnames '(("JetBrains Mono" "LXGW WenKai Mono") ("LXGW WenKai Mono"))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dashboard
 :ensure t
 :config
 (setq dashboard-banner-logo-title "Enjoy Hacking!")
 (setq dashboard-projects-backend 'projectile)
 (setq dashboard-startup-banner 'logo)
 ;; (setq dashboard-startup-banner nil)
 (setq dashboard-items '((recents  . 10)
		  (bookmarks . 5)
		  (projects . 5)))
 (dashboard-setup-startup-hook))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-responsive 'top))

(provide 'init-interface)

;;; init-interface.el ends here
