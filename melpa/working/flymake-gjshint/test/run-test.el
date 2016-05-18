;; Usage:
;;
;;   emacs -Q -l test/run-test.el           # interactive mode
;;   emacs -batch -Q -l test/run-test.el    # batch mode


;; Utils
(defun test-join-path (path &rest rest)
  "Join a list of PATHS with appropriate separator (such as /).

\(fn &rest paths)"
  (if rest
      (concat (file-name-as-directory path) (apply 'test-join-path rest))
    path))

(defvar test-dir (file-name-directory load-file-name))
(defvar root-dir (concat test-dir ".."))


;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list test-dir
            root-dir))

;; Load tests
(load "flymake-gjshint-test")


;; Run tests
(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))
