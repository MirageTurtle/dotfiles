;;; init-rust.el --- Rust configuration

(let ((rustup-bin "/opt/homebrew/opt/rustup/bin"))
  (when (file-exists-p rustup-bin)
    (setenv "PATH" (concat rustup-bin ":" (getenv "PATH")))
    (add-to-list 'exec-path rustup-bin)))

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
