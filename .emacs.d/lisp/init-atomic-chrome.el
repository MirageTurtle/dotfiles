;;; atomic-chrome.el --- Edit Chrome text area with Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Need to install [[GhostText][https://ghosttext.fregante.com/welcome]] in the browser end.

(use-package atomic-chrome
  :ensure t
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
	'(("overleaf\\.com" . latex-mode)))
  )
