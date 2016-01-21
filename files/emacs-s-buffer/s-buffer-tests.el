;;; tests for s-buffer.el

(require 's-buffer)
(require 'ert)

(defconst s-buffer/test-content "this is just a test

some-value: ${some-value}

other-value: ${other-value}
"
  "Some test content for a buffer.")

(defun s-buffer/test-buf ()
  (let ((buf (get-buffer-create "*test*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert s-buffer/test-content)
      (goto-char (point-min)))
    buf))

(ert-deftest s-buffer-format ()
  (unwind-protect
       (progn
         (s-buffer/test-buf)
         (s-buffer-format
          (get-buffer "*test*")
          'aget '(("some-value" . "1")
                  ("other-value" . "2")))
         ;; Now do some assertions
         (with-current-buffer (get-buffer "*test*")
           (goto-char (point-min))
           (should (re-search-forward "^some-value: 1" nil t))
           (should (re-search-forward "^other-value: 2" nil t))))
    (kill-buffer (get-buffer-create "*test*"))))

(ert-deftest s-buffer-lex-format ()
  (unwind-protect
       (progn
         (s-buffer/test-buf)
         (let ((some-value 1)
               (other-value 2))
           (s-buffer-lex-format (get-buffer "*test*")))
         ;; Now do some assertions
         (with-current-buffer (get-buffer "*test*")
           (goto-char (point-min))
           (should (re-search-forward "^some-value: 1" nil t))
           (should (re-search-forward "^other-value: 2" nil t)))
         ;; Now do one with strings
         (s-buffer/test-buf)
         (let ((some-value "this is some value")
               (other-value "and another value"))
           (s-buffer-lex-format (get-buffer "*test*")))
         ;; Now do some assertions
         (with-current-buffer (get-buffer "*test*")
           (goto-char (point-min))
           (should
            (re-search-forward
             "^some-value: this is some value" nil t))
           (should
            (re-search-forward
             "^other-value: and another value" nil t))))
    (kill-buffer (get-buffer-create "*test*"))))

;;; s-buffer-tests.el ends here
