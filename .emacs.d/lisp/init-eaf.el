;;; init-eaf.el

(defun mt/eaf-install-deps(app-dir)
  "Install deps from dependencies.json for eaf."
  (let* ((deps-dict (with-temp-buffer
                      (insert-file-contents (expand-file-name "dependencies.json" app-dir))
                      (json-parse-string (buffer-string))))
         ;; (pip-deps (gethash "win32" (or (gethash "pip" deps-dict) (make-hash-table))))
	 (pip-deps (gethash (cond ((and (boundp '*is-a-mac*) *is-a-mac*) "darwin")
				  ((and (boundp '*is-a-window*) *is-a-window*) "win32")
				  (t "linux")) (or (gethash "pip" deps-dict) (make-hash-table))))
         (vue-install (gethash "vue_install" deps-dict))
         (npm-install (gethash "npm_install" deps-dict))
         (npm-rebuild (gethash "npm_rebuild" deps-dict))
         (npm-cmd (if (memq system-type '(cygwin windows-nt ms-dos)) "npm.cmd" "npm")))
    (when pip-deps
      (dolist (pkg (append pip-deps nil))
        (message "%s" (shell-command-to-string (format "pip install %s" pkg)))))
    (when vue-install
      (let ((default-directory app-dir))
        (message "%s" (shell-command-to-string (format "%s install" npm-cmd)))
        (message "%s" (shell-command-to-string (format "%s run build" npm-cmd)))))
    (when npm-install
      (let ((default-directory app-dir))
        (message "%s" (shell-command-to-string (format "%s install" npm-cmd)))))
    (when npm-rebuild
      (let ((default-directory app-dir))
        (message "%s" (shell-command-to-string (format "%s rebuild" npm-cmd)))))))

(use-package eaf
  :straight (eaf :type git :host github :repo "emacs-eaf/emacs-application-framework"
                 :files ("*")
                 :post-build ("python" "install-eaf.py" "--install-core-deps"))
  ;; :ensure t
  :config
  (when (and (boundp '*is-a-window*) *is-a-window*)
    (setq eaf-python-command "D:\\Applications\\Scoop\\apps\\python\\current\\python.exe")
    (setq eaf-wm-name "windows"))
  (setq eaf-proxy-type "http")
  (setq eaf-proxy-host "127.0.0.1")
  (setq eaf-proxy-port "7890"))

(use-package eaf-demo
  ;; :ensure t
  :after (eaf)
  :straight (eaf-demo :type git :host github :repo "emacs-eaf/eaf-demo" :files ("*")))

(use-package eaf-browser
  ;; :ensure t
  :after (eaf)
  :straight (eaf-browser :type git :host github :repo "emacs-eaf/eaf-browser" :files ("*")
                          :post-build (mt/eaf-install-deps (straight--build-dir "eaf-browser")))
  :config
  ;; (setq eaf-browser-auto-import-chrome-cookies t)
  (setq eaf-browser-default-search-engine "google")
  (setq eaf-browser-enable-adblocker t)
  (when (and (boundp '*is-a-mac*) *is-a-mac*)
    (add-to-list 'eaf-browser-keybinding '("s-]" . "history_forward"))
    (add-to-list 'eaf-browser-keybinding '("s-[" . "history_backward"))))

(use-package eaf-pdf-viewer
  ;; :ensure t
  :after (eaf)
  :straight (eaf-pdf-viewer :type git :host github :repo "emacs-eaf/eaf-pdf-viewer" :files ("*")
                            :post-build (mt/eaf-install-deps (straight--build-dir "eaf-pdf-viewer"))))

(use-package eaf-markdown-previewer
  ;; :ensure t
  :after (eaf)
  :straight (eaf-markdown-previewer :type git :host github :repo "emacs-eaf/eaf-markdown-previewer" :files ("*")
                                    :post-build (mt/eaf-install-deps (straight--build-dir "eaf-markdown-previewer"))))

(provide 'init-eaf)

;;; init-eaf.el ends here
