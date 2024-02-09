;;; init-utils.el

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(provide 'init-utils)

;;; init-utils.el ends here.
