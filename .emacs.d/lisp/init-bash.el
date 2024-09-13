;; init-bash.el

(unless (assoc 'bash treesit-language-source-alist)
    (push '(bash . ("https://github.com/tree-sitter/tree-sitter-bash")) treesit-language-source-alist))

(reformatter-define bash-format
    :program "shfmt"
    :args '("-i" "4" "-ci" "-sr"))

(add-hook 'bash-ts-mode 'bash-format-on-save-mode)
(add-hook 'sh-mode 'bash-format-on-save-mode)

(provide 'init-bash)

;; init-bash.el ends here
