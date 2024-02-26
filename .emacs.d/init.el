;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of other files.

;;; Code:
(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq confirm-kill-emacs #'yes-or-no-p) ; confirm if kill emacs
(electric-pair-mode t) ; auto electric pair((){}[] .ete)
(add-hook 'prog-mode-hook #'show-paren-mode) ; highlight electric pair in program mode
(column-number-mode t) ; show column number in mode line
(global-auto-revert-mode t) ; emacs fresh buffer automatically if the file is edited in another palce
(delete-selection-mode t) ; replace text in selection mode
(setq inhibit-startup-message t) ; no emacs hello face
(setq make-backup-files nil) ; no backup files
(add-hook 'prog-mode-hook #'hs-minor-mode) ; fold code block in program mode
(global-display-line-numbers-mode 1) ; show column number in window
(tool-bar-mode -1) ; no tool bar
(menu-bar-mode -1) ; no menu bar
(tab-bar-mode 1) ; use tab bar
(when (display-graphic-p) (toggle-scroll-bar -1)) ; no scroll bar in graphic window
(savehist-mode 1) ; save buffer history
(setq-default cursor-type 'bar)

;; Global Key bind
(global-set-key (kbd "RET") 'newline-and-indent)
;; (global-set-key (kbd "C-c '") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c ;") 'comment-line)

;; (global-set-key (kbd "C-c SPC") 'set-mark-command)
;; (global-set-key (kbd "M-SPC") 'rectangle-mark-mode)
(global-set-key (kbd "M-h") 'set-mark-command)
;; (global-set-key (kbd "C--") 'undo)

;; macOS preferences
(if *is-a-mac*
    (progn
      (global-set-key (kbd "s-a") 'mark-whole-buffer)
      (global-set-key (kbd "s-c") 'kill-ring-save)
      (global-set-key (kbd "s-s") 'save-buffer)
      (global-set-key (kbd "s-v") 'yank)
      (global-set-key (kbd "s-z") 'undo)
      (global-set-key (kbd "s-x") 'kill-region))
  ())


;; (setq package-install-upgrade-built-in t)

;; package
;; (require 'package)
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
(setq package-check-signature nil)
;; (setq package-check-signature 'allow-unsigned)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (package-initialize)
;; (package-refresh-contents)

;; proxy
(defun clash()
  "Use clash as proxy."
  (interactive)
  (setq url-proxy-services '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
			     ("http" . "127.0.0.1:7890")
			     ("https" . "127.0.0.1:7890"))))
(defun noproxy()
  "No proxy."
  (interactive)
  (setq url-proxy-services nil))
(clash)

;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
;; (straight-pull-all)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-bridge yasnippet copilot cnfonts org-bullets org-roam eglot lua-mode pyvenv lsp-pyright lsp-ui yaml-pro json-mode multiple-cursors smart-tabs-mode wgrep lsp-treemacs lsp-ivy lsp-mode flycheck company treemacs-projectile treemacs counsel-projectile projectile undo-tree google-this rainbow-delimiters dashboard mwim counsel ivy use-package gnu-elpa-keyring-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(eval-when-compile
  (require 'use-package))

;; ;; windmove
;; (windmove-default-keybindings)
;; (setq windmove-wrap-around t)


;; Program for all
;; (use-package company
;;   :ensure t
;;   :init (global-company-mode)
;;   :config
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-idle-delay 0.0)
;;   (setq company-show-numbers t)
;;   (setq company-selection-wrap-around t)
;;   (setq company-transformers '(company-sort-by-occurrence)))

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (setq truncate-lines nil)
;;   ;; :hook
;;   ;; (prog-mode . flycheck-mode)
;; )

;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l"
;; 	lsp-file-watch-threshold 500)
;;   :hook 
;;   (lsp-mode . lsp-enable-which-key-integration) ; which-key integration
;;   :commands (lsp lsp-deferred)
;;   :config
;;   (setq lsp-completion-provider :none)
;;   (setq lsp-headerline-breadcrumb-enable t)
;;   :bind
;;   ("C-c l s" . lsp-ivy-workspace-symbol))

;; (use-package lsp-ui
;;   :ensure t
;;   :after (lsp)
;;   :config
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;   (setq lsp-ui-doc-position 'top))

;; (use-package lsp-ivy
;;   :ensure t
;;   :after (lsp-mode))

;; (use-package eglot
;;   :ensure t
;;   :after (lsp)
;;   :hook
;;   ('c-mode-hook . 'eglot-ensure)
;;   ('c++-mode-hook . 'eglot-ensure))

;; (use-package lsp-treemacs
;;   :ensure t
;;   :after (treemacs lsp))

;; (use-package smart-tabs-mode
;;   :ensure t
;;   :config
;;   (smart-tabs-insinuate 'python 'c 'c++))

;; (use-package multiple-cursors
;;   :ensure t
;;   :bind (("C->"           . mc/mark-next-like-this)
;;          ("C-<"           . mc/mark-previous-like-this)
;;          ("C-M->"         . mc/skip-to-next-like-this)
;;          ("C-M-<"         . mc/skip-to-previous-like-this)
;;          ("C-c C-<"       . mc/mark-all-like-this)
;;          ("C-S-<mouse-1>" . mc/add-cursor-on-click)
;;          :map mc/keymap
;;          ("C-|" . mc/vertical-align-with-space))
;;   :config
;;   (setq mc/insert-numbers-default 1))


;; accept completion from copilot and fallback to company
;;; dependencies
;; (use-package editorconfig
;;   :ensure t)
;; (use-package jsonrpc
;;   :ensure t)
;;; copilot
;; (use-package copilot
;;   :quelpa (copilot :fetcher github
;;                    :repo "copilot-emacs/copilot.el"
;;                    :branch "main"
;;                    :files ("dist" "*.el"))
;;   :ensure t
;;   :after (editorconfig jsonrpc)
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word)))


;; (use-package lsp-pyright
;;   :ensure t
;;   :config
;;   (setq lsp-pyright-use-library-code-for-types t)
;;   (setq lsp-pyright-stub-path "~/Documents/repo/python-type-stubs")
;;   (setq lsp-pyright-typechecking-mode nil)
;;   :hook
;;   (python-mode . (lambda ()
;; 		  (require 'lsp-pyright)
;; 		  (lsp-deferred))))


;; For C/C++
;; (use-package clang-format
  ;; :ensure t)

(require 'init-utils)

(require 'init-efficiency)
(require 'init-theme)
(require 'init-interface)

(require 'init-ivy)
(require 'init-workspace)
(require 'init-lsp-bridge)

(require 'init-python)
(require 'init-org)
(require 'init-lua)
(require 'init-json)
(require 'init-yaml)


(provide 'init)

;;; init.el ends here.
