(let ((org-dir (file-name-as-directory (or (getenv "ORG_DIR") "../org-mode"))))
  (mapc (lambda (dir)
          (add-to-list 'load-path (concat org-dir dir)))
        '("lisp" "testing")))

(add-to-list 'load-path ".")

(load "test-ox-pukiwiki")
(setq org-startup-folded nil)
(ert-run-tests-batch-and-exit)
