;;; init-org.el
; Some references
; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
; https://github.com/lujun9972/emacs-document/blob/master/org-mode/%E7%BE%8E%E5%8C%96%20Org%20mode.org


; org
;; basic
(setq org-directory (file-truename "~/Documents/org/"))
;; (setq org-startup-indented t)
;; (setq org-startup-numerated t)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'org-mode-hook
          (lambda ()
            (variable-pitch-mode 1)
            visual-line-mode))

(setq org-hide-leading-stars t)  ;; hide the leading star
(setq org-pretty-entities t)  ;; Using UTF8 for pretty entities
;; (setq org-odd-levels-only t) ;; only odd level
;; part from references but not familiar with
;; (setq org-hide-emphasis-markers t)
;; (setq org-fontify-done-headline t)

;; pretty leading stars of headings
(use-package org-bullets
  :ensure t
  :after org
  :custom
  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

;; leading stars of table
(font-lock-add-keywords 'org-mode
                        '(("^ *\([-]\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\([+]\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

; For org-roam
(use-package org-roam
   :ensure t
   :after org
   :init
   (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
   :config
   (org-roam-setup)
   :custom
   (org-roam-directory (concat org-directory "roam/")) ; set org-roam directory
   :bind
   (("C-c n f" . org-roam-node-find)
    (:map org-mode-map
	  (("C-c n i" . org-roam-node-insert)))))

; For org-agenda
(setq org-agenda-files '("~/Documents/org/"))
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
	 ((agenda "")
	  (alltodo "")))))


(provide 'init-org)

;;; init-org.el ends here
