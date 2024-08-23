;;; init-dirvish.el --- -*- lexical-binding: t -*-

(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("e" "~/.emacs.d"                  "Emacs User Directory")
     ("d" "~/Downloads/"                "Downloads")
     ("r" "~/Documents/repo"            "Repository")))
  :config
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls"))
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side)
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(vc-state nerd-icons file-time file-size collapse subtree-state git-msg))
  (setq dirvish-subtree-state-style 'nerd)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; integrating with awesome-tray
  (setq dirvish-use-header-line nil)     ; hide header line (show the classic dired header)
  (setq dirvish-use-mode-line nil)       ; hide mode line
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

;; add `org-directory' to `dirvish-quick-access-entries', if it is defined, and the directory is valid
(when (and (boundp 'org-directory)
           (file-directory-p org-directory)
           (not (assoc "o" dirvish-quick-access-entries)))
  (customize-set-variable 'dirvish-quick-access-entries
        (append dirvish-quick-access-entries
                `(("o" ,(expand-file-name org-directory) "Org Directory")))))

(provide 'init-dirvish)

;;; init-dirvish ends here.
