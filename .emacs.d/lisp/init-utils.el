;;; init-utils.el

(defun print-elements-of-list (list)
  ;; Or maybe use cl-prettyprint?
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

;;; https://emacs.stackexchange.com/questions/32150/how-to-add-a-timestamp-to-each-entry-in-emacs-messages-buffer
(defun mt/ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.
   Activate this advice with:
   (advice-add 'message :before 'mt/ad-timestamp-message)
   Deactivate this advice with:
   (advice-remove 'message 'mt/ad-timestamp-message)"
  (if message-log-max
      (let ((deactivate-mark nil)
            (inhibit-read-only t))
	(with-current-buffer "*Messages*"
	  ;; Goto the end of buffer
	  (goto-char (point-max))
	  (if (not (bolp))
	      (newline))
	  ;; insert the timestamp
	  (insert (format-time-string "[%Y-%m-%d %T.%3N] "))))))

(advice-add 'message :before 'mt/ad-timestamp-message)

(provide 'init-utils)

;;; init-utils.el ends here.
