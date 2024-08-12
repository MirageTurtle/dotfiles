;;; init-org.el
; Some references
; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
; https://github.com/lujun9972/emacs-document/blob/master/org-mode/%E7%BE%8E%E5%8C%96%20Org%20mode.org

;; fonts
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; org
(require 'org-indent)
;; basic
;;; mac: ~/Documents/sshfs-client/org/
;;; linux: ~/Documents/sshfs-docs-client/org/
(if (eq system-type 'darwin)
    (setq org-directory (file-truename "~/Documents/sshfs-client/org/"))
  (if (eq system-type 'gnu/linux)
      (setq org-directory (file-truename "~/Documents/sshfs-docs-client/org/"))))
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
   (org-roam-dailies-directory "daily/")
   (org-roam-dailies-capture-templates
    '(("d" "default" entry
       "* %?"
       :target (file+head "%<%Y-%m-%d>.org"
			  "#+title: %<%Y-%m-%d>\n"))))
   :bind
   (("C-c n f" . org-roam-node-find)
    (:map org-mode-map
	  (("C-c n i" . org-roam-node-insert)))))

; For org-agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files (list org-directory))
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
	 ((agenda "")
	  (alltodo "")))))
(setq org-agenda-start-on-weekday 0) ;; start on Sunday

;; for my personal todo
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "WAITING" "|" "DONE")))
;; (setq org-log-done 'time)
(setq org-log-done 'note)

(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key (kbd "C-c c") 'org-capture)


(provide 'init-org)

;;; init-org.el ends here
