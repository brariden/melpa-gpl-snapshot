;;; signature-backend.el --- Basic wireing

;;; Commentary:

;; Reading of files, determining which parser to use, picking matcher
;; for current source line and building signature using the available
;; machinery. this is where it comes to gether.

;;; Code:

(cl-defmacro signature--with-source-file ((file) &body body)
 "Executes BODY as lines from FILE with the variable LINES bound to the lines of the file."
 `(with-temp-buffer
   (insert-file-contents ,file)
   (let ((lines (split-string (buffer-string) "\n")))
    ,@body)))

(cl-defmacro signature--with-source-lines ((line) &body body)
 "Iterate over LINES binding each one to LINE and executing BODY."
 `(dolist (,line lines)
   ,@body))

(defun signature--parser-for-file (file)
 "Determine parser (language implementation) by matching extension of FILE with the languages defined extension."
 (cl-find-if
  (lambda (language)
   (with-slots (extension) language
    (s-suffix-p extension file)))
  signature--languages))

(defun signature--match (parser line)
 "Given a language PARSER and source code LINE, return a matcher."
 (cl-find-if
  (lambda (matcher)
   (with-slots (regexp) matcher
    (string-match-p regexp line)))
  (slot-value parser 'source-line-matchers)))

(defun signature--indentation-level (line)
 "Determines the indentation-level of a LINE."
 (length (car (s-split "[^\s]+" line))))

(defun signature--parse-file (file)
 "Parse a FILE, returning a list of statistics and an ascii signature."
 (let* ((parser (signature--parser-for-file file))
        (indentation 0)
        (class-count 0)
        (method-count 0)
        (line-count 0)
        (current-line 0)
        (stack nil)
        (indentation-stack nil))

  (signature--with-source-file (file)
   (with-temp-buffer
    (signature--with-source-lines (line)

     (incf current-line)
     (setq identation (signature--indentation-level line))

     (let ((matcher (signature--match parser line)))
      (when matcher

       ;; Statistics:
       (incf line-count)
       (when (signature-match-class-child-p matcher) (incf class-count))
       (when (signature-match-method-child-p matcher) (incf method-count))

       ;; Signature production:
       (cond
        ((signature--push-state-p matcher stack)
         (insert (signature--render-signature-char (signature--marker-enter matcher) file current-line))
         (push matcher stack)
         (push indentation indentation-stack))

        ((signature--pop-state-p matcher stack)
         (when stack
          (insert (signature--render-signature-char (signature--marker-exit (pop stack)) file current-line))
          (pop indentation-stack)))

        (t (insert (signature--render-signature-char (signature--marker matcher) file current-line)))))))

    (list class-count method-count line-count (buffer-string))))))

(provide 'signature-backend)

;;; signature-backend.el ends here
