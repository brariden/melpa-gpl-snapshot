;; from shell: emacs -Q --batch -l ./launch-tests.el

(load-file "./creds.el")
(load-file "./test/creds-tests.el")

(require 'creds)
(require 'creds-tests)
(ert-run-tests-batch-and-exit)
