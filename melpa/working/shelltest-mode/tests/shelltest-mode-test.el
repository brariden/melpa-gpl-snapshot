;;; shelltest-mode-test.el --- Tests for shelltest-mode

(require 'shelltest-mode)

;; FIXME: allow interactive execution by properly cleaning up
;;        stray buffers, etc...
(unless noninteractive
  (error "Run only in batch mode for now"))

;; tests for shelltest-find
(ert-deftest shelltest-find-works ()
  (save-window-excursion
    (with-temp-buffer
      (set-visited-file-name "foobar")
      (let ((d (file-name-directory buffer-file-name)))
        (shelltest-find)
        (should (string= buffer-file-name (concat d "tests/foobar.test")))))))

(ert-deftest shelltest-find-file-name ()
  (save-window-excursion
    (with-temp-buffer
      (set-visited-file-name "-'foo bar")
      (let ((d (file-name-directory buffer-file-name)))
        (shelltest-find)
        (should (string= buffer-file-name (concat d "tests/-'foo bar.test")))))))

(ert-deftest shelltest-find-other-window ()
  (save-window-excursion
    (delete-other-windows)
    (let ((shelltest-other-window nil))
      (with-temp-buffer
        (set-visited-file-name "foobar")
        (shelltest-find)
        (should (= (count-windows nil) 1))))
    (let ((shelltest-other-window t))
      (with-temp-buffer
        (set-visited-file-name "foobar")
        (shelltest-find)
        (should (= (count-windows nil) 2))))))

;; tests for shelltest-run
;; TODO: write some tests

;; tests for shelltest-run-all
;; TODO: write some tests

;; tests for helper functions
(ert-deftest shelltest--command-line-does-quote ()
  (let ((file (make-temp-file "-'foo bar")))
    (unwind-protect
        (should (string-match-p (regexp-quote "-\\'foo\\ bar")
                                (shelltest--command-line file)))
      (delete-file file))))

(ert-deftest shelltest--command-line-absolute-path ()
  (let ((file (make-temp-file "foobar")))
    (unwind-protect
        (should (string-match-p (concat "^" (regexp-quote shelltest-command) " /")
                                (shelltest--command-line file)))
      (delete-file file))))

(provide 'shelltest-mode-test)

;;; shelltest-mode-test.el ends here
