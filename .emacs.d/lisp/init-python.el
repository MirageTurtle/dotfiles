;;; init-python.el

(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" (expand-file-name "~/Documents/venv"))
  ;; (setq python-shell-interpreter "python3")
  (pyvenv-mode t)
  (pyvenv-activate (expand-file-name "~/Documents/venv/base")))

;; for lsp-bridge
(defun local/lsp-bridge-get-single-lang-server-by-project (project-path filepath)
  (let* ((json-object-type 'plist)
         (custom-dir (expand-file-name ".cache/lsp-bridge/ruff" user-emacs-directory))
         (custom-config (expand-file-name "ruff.json" custom-dir))
         (default-config (json-read-file (expand-file-name "straight/build/lsp-bridge/langserver/ruff.json" user-emacs-directory)))
         (settings (plist-get default-config :settings))
         )

    (plist-put settings :pythonPath (executable-find "python3"))

    (make-directory (file-name-directory custom-config) t)

    (with-temp-file custom-config
      (insert (json-encode default-config)))

    custom-config))

(add-hook 'python-mode-hook (lambda () (setq-local lsp-bridge-get-single-lang-server-by-project 'local/lsp-bridge-get-single-lang-server-by-project)))

(add-hook 'pyvenv-post-activate-hooks
          (lambda ()
            (lsp-bridge-restart-process)))

(use-package ein
  :ensure t)

;; for black, a formatter
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(provide 'init-python)

;;; init-python.el ends here.
