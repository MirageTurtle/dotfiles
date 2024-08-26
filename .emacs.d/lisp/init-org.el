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
;; basic
;;; mac: ~/Documents/sshfs-client/org/
;;; linux: ~/Documents/sshfs-docs-client/org/
(if (eq system-type 'darwin)
    (setq org-directory (file-truename "~/Documents/sshfs-client/org/"))
  (if (eq system-type 'gnu/linux)
      (setq org-directory (file-truename "~/Documents/sshfs-docs-client/org/"))))
;; (setq org-startup-indented t)
;; (setq org-startup-numerated t)
(add-hook 'org-mode-hook
	  (lambda ()
	    (setq truncate-lines nil)
	    visual-line-mode))
(add-hook 'org-mode-hook 'org-indent-mode)

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
  :custom
  (org-roam-directory (concat org-directory "roam/")) ; set org-roam directory
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
			 "#+title: %<%Y-%m-%d>\n"))))
  ;; (org-roam-node-display-template (concat "${type:15} ${doom-hierarchy:80} " (propertize "${tags:*}" 'face 'org-tag)))
  (org-roam-node-display-template (concat "${doom-hierarchy:80} " (propertize "${tags:*}" 'face 'org-tag)))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  ;; Codes blow are used to general a hierachy for title nodes that under a file
  (cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
    "Return the value of \"#+title:\" (if any) from file that NODE resides in.
      If there's no file-level title in the file, return empty string."
    (or (if (= (org-roam-node-level node) 0)
            (org-roam-node-title node)
          (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
        ""))
  (cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
    "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
        If some elements are missing, they will be stripped out."
    (let ((title     (org-roam-node-title node))
          (olp       (org-roam-node-olp   node))
          (level     (org-roam-node-level node))
          (filetitle (org-roam-node-doom-filetitle node))
          (separator (propertize " > " 'face 'shadow)))
      (cl-case level
        ;; node is a top-level file
        (0 filetitle)
        ;; node is a level 1 heading
        (1 (concat (propertize filetitle 'face '(shadow italic))
                   separator title))
        ;; node is a heading with an arbitrary outline path
        (t (concat (propertize filetitle 'face '(shadow italic))
                   separator (propertize (string-join olp " > ") 'face '(shadow italic))
                   separator title)))))
  ;; (setq org-roam-node-display-template (concat "${type:15} ${doom-hierarchy:80} " (propertize "${tags:*}" 'face 'org-tag)))
  :bind
  (("C-c n f" . org-roam-node-find)
   (:map org-mode-map
	 (("C-c n i" . org-roam-node-insert)))))

; For org-agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files (list org-directory (concat org-directory "tasks/") (concat org-roam-directory "daily/"))) ; hard-code `org-roam-dailies-directory'
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
	 ((agenda "")
	  (alltodo "")))))
(setq org-agenda-start-on-weekday 0) ;; start on Sunday

;; for my personal todo
;; (setq org-todo-keywords
;;       '((sequence "TODO" "DOING" "WAITING" "|" "DONE")))
;; (setq org-log-done 'time)
(setq org-log-done 'note)

(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key (kbd "C-c c") 'org-capture)


(provide 'init-org)

;;; init-org.el ends here
