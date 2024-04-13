;;; init-lsp-bridge.el

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package markdown-mode
  :ensure t)

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :ensure t
  :init
  (global-lsp-bridge-mode)
  :hook
  ((prog-mode . lsp-bridge-mode)
   ((latex-mode LaTeX-mode) . lsp-bridge-mode)
   (markdown-mode . lsp-bridge-mode))
  :config
  (setq lsp-bridge-python-lsp-server 'ruff)
  (setq lsp-bridge-tex-lsp-server 'texlab)
  ;; (setq lsp-bridge-enable-log t)
  ;; (setq lsp-bridge-enable-debug t)
  (setq lsp-bridge-python-command (expand-file-name "~/Documents/venv/lsp-bridge/bin/python3"))
  ;; (setq acm-enable-copilot t)
  )

;; Some Issue
;; https://github.com/manateelazycat/lsp-bridge/blob/master/README.md
(defun enable-lsp-bridge()
  (when-let* ((project (project-current))
              (project-root (nth 2 project)))
    (setq-local lsp-bridge-user-langserver-dir project-root
                lsp-bridge-user-multiserver-dir project-root))
  (lsp-bridge-mode))


(provide 'init-lsp-bridge)

;;; init-lsp-bridge.el ends here.
