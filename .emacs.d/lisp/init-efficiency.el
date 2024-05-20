;;; init-efficiency.el

(global-hl-line-mode 1) ;; highlight current line

(use-package mwim
  ;; :straight t
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; customed split window function
(defun split-window-right-new ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (call-interactively 'switch-to-buffer))
(defun split-window-below-new ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (call-interactively 'switch-to-buffer))
(global-set-key (kbd "C-x 2") 'split-window-below-new)
(global-set-key (kbd "C-x 3") 'split-window-right-new)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; multi-cursor
(use-package multiple-cursors
  :ensure t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(provide 'init-efficiency)

;;; init-efficiency.el ends here.
