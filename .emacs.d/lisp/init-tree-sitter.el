;; init-tree-sitter.el -*- lexical-binding: t; -*-
;; This elisp file is for setting up tree-sitter,
;; instead of using the Emacs 29 treesit.
(use-package tree-sitter
  :ensure t
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  (emacs-lisp-mode . tree-sitter-hl-mode)
  (python-mode . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode)
  (setq tree-sitter-load-paths "~/.emacs.d/tree-sitter/")
  ;; for major mode language register
  (setq tree-sitter-major-mode-language-alist
	'((emacs-lisp-mode . elisp)
	  (python-ts-mode . python)
	  (bash-ts-mode . bash)
	  (rust-ts-mode . rust)
	  (go-ts-mode . go)
	  (lisp-data-mode . elisp))))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package ts-fold
  :ensure t
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :config
  (add-hook 'tree-sitter-after-on-hook #'ts-fold-mode))

(provide 'init-tree-sitter)
;; init-tree-sitter.el ends here
