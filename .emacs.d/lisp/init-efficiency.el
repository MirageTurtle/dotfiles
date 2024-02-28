;;; init-efficiency.el

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
(global-set-key (kbd "C-x 3") 'split-window-right-new)

(provide 'init-efficiency)

;;; init-efficiency.el ends here.
