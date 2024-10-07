;;; init-deep.el -*- lexical-binding: t; -*-

(require 'url)
(require 'json)

;; TODO:
;; - Add error handling
(defun deeplx-translate (text source-lang target-lang)
  "Translate TEXT from SOURCE-LANG to TARGET-LANG using DeepL API, and copy the result to kill-ring."
  (let* ((url "https://api.deeplx.org/BY8mVtDIYSW1rPtp2_nohLP1sCyqS-k5CZXLhkfJT4A/translate")
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json; charset=utf-8")))
         (url-request-data (encode-coding-string
                            (json-encode `(("text" . ,text)
                                           ("source_lang" . ,source-lang)
                                           ("target_lang" . ,target-lang))) 'utf-8))
         (buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (goto-char url-http-end-of-headers)
      (let* ((json-object-type 'hash-table)
             (response (json-read-from-string
                        (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8)))
             (translated-text (gethash "data" response))) ;; get the translated text from the response
        (when translated-text
          (kill-new translated-text) ;; put the translated text into the kill-ring
          (message "Translated text: %s" translated-text)) ;; show the translated text in the minibuffer
        translated-text))))

(defun deeplx-translate-region (beg end &optional prefix)
  "Translate the region from BEG to END using DeepL API.
The source language is auto-detected.
By default, the target language is English.
With a PREFIX argument, the target language is Chinese."
  (interactive "r\nP") ;; r for region, P for prefix
  (let* ((text (buffer-substring-no-properties beg end))
	 (source-lang "auto")
	 (target-lang (if prefix "ZH" "EN")))
    (deeplx-translate text source-lang target-lang)))

(global-set-key (kbd "C-c t") #'deeplx-translate-region)

(provide 'init-deepl)

;;; init-deep.el ends here
