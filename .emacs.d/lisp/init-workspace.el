;;; init-workspace.el

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically t))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode)
  (treemacs-follow-mode)
  (treemacs-filewatch-mode)
  ;; (treemacs-git-mode 'deferred)
  (treemacs-icons-dired-mode)
  (setf treemacs-select-when-already-in-treemacs 'stay)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ;; ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
	("/" . treemacs-advanced-helpful-hydra)
	("n" . treemacs-next-neighbour)
	("p" . treemacs-previous-neighbour)
	("r" . treemacs-rename-file)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))


(provide 'init-workspace)

;;; init-workspace.el ends here.
