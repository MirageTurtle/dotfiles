;;; init-rust.el --- Rust configuration

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  :custom
  (rust-indent-where-clause t)
  (rust-format-on-save t)
  (rust-format-show-buffer nil))

(provide 'init-rust)

;;; init-rust.el ends here
