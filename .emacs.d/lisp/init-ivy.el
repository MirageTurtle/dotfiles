;;; init-ivy.el

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :after counsel
  :init
  :config
  (counsel-mode 1)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   ("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ;; ("C-x C-@" . 'counsel-mark-ring)
   ("C-x SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)
   ;; the ~ivy-immediate-done~ is binded to C-M-j
))

(provide 'init-ivy)

;;; init-ivy.el ends here.
