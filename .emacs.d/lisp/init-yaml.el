;;; init-yaml.el

(use-package yaml-mode
  :ensure t)

(use-package yaml-pro
  :ensure t
  ;; :after (yaml-mode)
  :config
  (add-hook 'yaml-mode-hook #'yaml-pro-mode))

(provide 'init-yaml)

;;; init-yaml.el ends here.
