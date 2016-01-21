;; Required testing libraries
(require 'cl)
(require 'el-mock)

(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'cryptsy-public-api)

(setq test-directory (file-name-directory (directory-file-name (file-name-directory load-file-name))))

(defun read-fixture (file)
  
  ;; Get the file path
  (let* ((file-path (concat test-directory "/fixtures/" file))
         (file-contents (with-temp-buffer
                          (insert-file-contents file-path)
                          (buffer-string))))
    (json-read-from-string file-contents)))

;; Mock Helpers

(defmacro mock-request (query-vars fixture)
  (let ((uri (cryptsy-public-api--create-endpoint query-vars)))
    `(mock (cryptsy-public-api--get-uri ,uri) => (read-fixture ,fixture))))
