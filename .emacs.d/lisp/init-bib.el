;; init-bib.el
;; https://www.bilibili.com/read/cv23440624/

(eval-after-load 'org-roam
  '(progn
     (setq zot_bib '("~/Documents/jianguoyun-papers/Research.bib") ; Zotero .bib file exported by Better BibTeX
	    zot_pdf "~/Documents/jianguoyun-papers/" ; Zotero ZotFile directory
	    org_refs (concat org-directory "roam/ref/"))))


(use-package helm-bibtex
  :ensure t
  :after org-roam
  :custom
  (bibtex-completion-notes-path org_refs)
  (bibtex-completion-bibliography zot_bib)
  (bibtex-completion-library-path zot_pdf))

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)i
  :bind (("C-c n k" . orb-insert-link)
         ("C-c n a" . orb-note-actions))
  :custom
  (orb-insert-interface 'helm-bibtex)
  (orb-insert-link-description 'citekey)
  (orb-preformat-keywords
   '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf"))
  :config
  (setq my/ref-template
        (concat "#+FILETAGS: reading research \n"
                "- tags :: %^{keywords} \n"
                "* %^{title}\n"
                ":PROPERTIES:\n"
                ":Custom_ID: %^{citekey}\n"
                ":URL: %^{url}\n"
                ":AUTHOR: %^{author-or-editor}\n"
                ":NOTER_DOCUMENT: \"" zot_pdf "%^{citekey}.pdf\"\n"
                ":NOTER_PAGE:\n"
                ":END:"))
  (setq org-roam-capture-templates
	(append org-roam-capture-templates
               `(("r" "Zotero Reference" plain
                 ,my/ref-template
                 :target
                 (file+head "ref/${citekey}.org" "#+title: ${title}\n"))))))

(use-package org-noter
  :ensure t
  :after org-roam
  :custom
  (org-noter-notes-search-path '((concat org-directory "roam/ref/")))
  (org-noter-auto-save-last-location t)
  (org-noter-max-short-selected-text-length 20) ;; default value is 80
  (org-noter-default-heading-title "note for page $p$")
  :bind
  (("C-c n n" . org-noter)
   :map org-noter-doc-mode-map
   ("e" . org-noter-insert-note)
   ("M-e" . org-noter-insert-precise-note)))

(provide 'init-bib)

;; init-bib.el ends here
