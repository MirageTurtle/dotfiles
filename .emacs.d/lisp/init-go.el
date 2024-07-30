;;; init-go.el --- Go configuration -*- lexical-binding: t; -*-

(use-package go-ts-mode
  :hook
  (go-ts-mode . lsp-deferred)
  (go-ts-mode . go-format-on-save-mode)
  :init
  ;;; if go/gomod is not in treesit-language-source-alist, add it
  (dolist (lang '(go gomod))
    (unless (assoc lang treesit-language-source-alist)
      (push `(,lang . ,(treesit-get-language lang)) treesit-language-source-alist)))
  :config
  (reformatter-define go-format
    :program "gofmt"
    :args '("-s")))

(provide 'init-go)

;;; init-go.el ends here.
