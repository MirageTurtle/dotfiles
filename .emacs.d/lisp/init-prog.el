;;; init-prog.el -*- lexical-binding: t; -*-

(use-package reformatter
  :ensure t)

(use-package ts-fold
  :ensure t
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :hook (prog-mode . ts-fold-mode))

;; debug
(use-package dape
  ;; require jsonrpc >= 1.0.24, I use 1.0.25
  :config
  (setq dape-buffer-window-arrangement 'right))

(provide 'init-prog)

;;; init-prog.el ends here
