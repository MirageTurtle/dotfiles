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
; (tool-bar-mode -1) ; no tool bar
(when (display-graphic-p) (toggle-scroll-bar -1)) ; no scroll bar in graphic window
(savehist-mode 1) ; save buffer history

;; Global Key bind
(global-set-key (kbd "RET") 'newline-and-indent)
;; (global-set-key (kbd "C-c '") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c ;") 'comment-line)
;; Didn't work
;; (global-set-key (kbd "C-h") 'view-order-manuals)
;; (global-set-key (kbd "C-SPC") 'set-mark-command)

;; package
(require 'package)
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
(setq package-check-signature nil)
;; (setq package-check-signature 'allow-unsigned)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

;; proxy
;; TODO: FIX clash()
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

(provide 'init)

;;; init.el ends here.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-pro json-mode multiple-cursors smart-tabs-mode wgrep company-jedi lsp-treemacs lsp-ivy lsp-mode flycheck company treemacs-projectile treemacs counsel-projectile projectile undo-tree google-this rainbow-delimiters dashboard mwim counsel ivy use-package gnu-elpa-keyring-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(eval-when-compile
  (require 'use-package))

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring)
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

;; windmove
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; Program for all
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)))

(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines nil)
  :hook
  (prog-mode . flycheck-mode))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
	lsp-file-watch-threshold 500)
  :hook 
  (lsp-mode . lsp-enable-which-key-integration) ; which-key integration
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-completion-provider :none)
  (setq lsp-headerline-breadcrumb-enable t)
  :bind
  ("C-c l s" . lsp-ivy-workspace-symbol))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
	("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))

;; (use-package smart-tabs-mode
;;   :ensure t)
;; (smart-tabs-insinuate 'python)

(use-package multiple-cursors
  :ensure t
  :bind (("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space))
  :config
  (setq mc/insert-numbers-default 1))

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package yaml-pro
  :ensure t
  ;; :after (yaml-mode)
  :config
  (add-hook 'yaml-mode-hook #'yaml-pro-mode))
  

;; Program for Python
(use-package company-jedi
  :ensure t
  :after (company-mode))

;; Others
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package dashboard
 :ensure t
 :config
 (setq dashboard-banner-logo-title "Welcome to Emacs!")
 ;; (setq dashboard-projects-backend 'projectile)
 (setq dashboard-startup-banner 'official)
 (setq dashboard-items '((recents  . 5)
		  (bookmarks . 5)
		  (projects . 10)))
 (dashboard-setup-startup-hook))
