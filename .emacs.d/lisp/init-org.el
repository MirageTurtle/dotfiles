; Some references
; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html


; For org
(setq org-directory (file-truename "~/Documents/org/"))
(setq org-startup-indented t)
(setq org-startup-numerated t)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

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
