;;; init-theme.el

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-nerd-fonts
  :ensure t)

(unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
  (nerd-icons-install-fonts))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

(use-package awesome-tray
  :straight (awesome-tray :type git :host github :repo "manateelazycat/awesome-tray")
  :ensure t
  :config
  (awesome-tray-mode 1)
  (setq awesome-tray-active-modules '("location" "file-path" "buffer-name" "mode-name" "git" "battery" "date"))
  (setq awesome-tray-buffer-name-buffer-changed t)
  (setq awesome-tray-date-format "%Y-%m-%d %H:%M %a")
  (defun awesome-tray-git-command-update-cache ()
    "Remove the space between the branch name and the status symbol, based on the original function."
    (if (file-exists-p (format "%s" (buffer-file-name)))
	(let* ((filename (buffer-file-name))
               (status (vc-git-state filename))
               (branch (car (vc-git-branches))))

          (pcase status
            ('up-to-date (setq status ""))
            ('edited (setq status "!"))
            ('needs-update (setq status "⇣"))
            ('needs-merge (setq status "⇡"))
            ('unlocked-changes (setq status ""))
            ('added (setq status "+"))
            ('removed (setq status "-"))
            ('conflict (setq status "="))
            ('missing (setq status "?"))
            ('ignored (setq status ""))
            ('unregistered (setq status "?"))
            (_ (setq status "")))
          (if (not branch) (setq branch ""))

          (setq awesome-tray-git-buffer-filename filename)

          (setq awesome-tray-git-command-cache (if awesome-tray-git-show-status
                                                   (format awesome-tray-git-format (string-trim (concat branch "" status)))
						 (format awesome-tray-git-format branch))))
      (setq awesome-tray-git-buffer-filename nil
            awesome-tray-git-command-cache ""))))

(provide 'init-theme)

;;; init-theme.el ends here.
