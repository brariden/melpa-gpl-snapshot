;;; go-playground-fmt.el --- go fmt using Go Playground  -*- lexical-binding: t; -*-

;; NOTE: this library is under development

;;; Code:

(require 'json)
(require 'url)
(require 'f)
(require 's)
(require 'let-alist)

;; (url-handler-mode +1)

(defvar go-playground-fmt-url
  "https://play.golang.org/fmt")

(defun go-playground-fmt-request (code)
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (url-build-query-string
                           `(("body" ,code)))))
    (f-read go-playground-fmt-url)))

(defun go-playground-fmt ()
  "Format current go program."
  (interactive "*")
  (let-alist (json-read-from-string
              (go-playground-fmt-request (buffer-string)))
    (if (s-present? .Error)
        (error (s-chomp .Error))
      (let ((point (point)))
        (erase-buffer)
        (insert .Body)
        (goto-char point)))))

(provide 'go-playground-fmt)

;;; go-playground-fmt.el ends here
