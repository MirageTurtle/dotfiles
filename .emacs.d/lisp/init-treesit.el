;;; init-treesit.el -*- lexical-binding: t; -*-

(use-package treesit
  :init
  (setq treesit-language-source-alist
	'((elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
	  (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
	  (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
	  (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
	  (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("go\\.mod\\'" . gomod-ts-mode))
  ;; (add-hook 'go-ts-mode-hook #'(lambda () (treesit-parser-create 'go)))
  :config
  (setq treesit-font-lock-level 4))


(provide 'init-treesit)

;;; init-treesit.el ends here
