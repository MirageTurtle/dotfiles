;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :config
;;   (treemacs-tag-follow-mode)
;;   (treemacs-follow-mode)
;;   (treemacs-filewatch-mode)
;;   ;; (treemacs-git-mode 'deferred)
;;   (use-package treemacs-icons-dired
;;     :ensure t
;;     :after (treemacs dired)
;;     :config
;;     (treemacs-icons-dired-mode))
;;   (setf treemacs-select-when-already-in-treemacs 'stay)
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ;; ("C-x t C-t" . treemacs-find-file)
;;         ;; ("C-x t M-t" . treemacs-find-tag)
;; 	)
;;   (:map treemacs-mode-map
;; 	("/" . treemacs-advanced-helpful-hydra)
;; 	("n" . treemacs-next-neighbour)
;; 	("p" . treemacs-previous-neighbour)
;; 	("r" . treemacs-rename-file)))

;; (use-package treemacs-projectile
;;   :ensure t
;;   :after (treemacs projectile))

;; custom other-window function
;; https://stackoverflow.com/questions/4941960/how-do-i-make-emacs-other-window-command-ignore-terminal-windows
;; TODO:
;; + [BUG] when there is only one window, it will not work
(defvar ignore-windows-regexps
  "Treemacs-Scoped-Buffer")
(require 'cl)
(defun my-other-window ()
  "Similar to 'other-window, only try to avoid windows whose buffers match ignore-window-regexp"
  (interactive)
  (let* ((window-list (delq (selected-window) (window-list)))
         (filtered-window-list (remove-if
                                (lambda (w)
                                  (string-match-p ignore-windows-regexps (buffer-name (window-buffer w))))
                                window-list)))
    (if filtered-window-list
        (select-window (car filtered-window-list))
      (and window-list
           (select-window (car window-list))))))
(global-set-key (kbd "C-x o") 'my-other-window)
