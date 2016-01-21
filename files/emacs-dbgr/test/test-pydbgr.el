;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../realgud/debugger/pydbgr/pydbgr.el")
(declare-function pydbgr-parse-cmd-args 'pydbgr-pdb)
(declare-function __FILE__              'load-relative)

(test-simple-start)

(note "pydbgr-parse-cmd-args")

(assert-equal '(nil ("pydbgr") ("foo") nil)
	      (pydbgr-parse-cmd-args '("pydbgr" "foo")))
(assert-equal '(nil ("pydbgr" "-n") ("foo") nil)
	      (pydbgr-parse-cmd-args '("pydbgr" "-n" "foo")))
(assert-equal '(nil ("pydbgr" "--annotate=1") ("foo") t)
	      (pydbgr-parse-cmd-args
	       '("pydbgr" "--annotate=1" "foo")))
(assert-equal '(nil ("mypydbgr" "--annotate=1") ("foo") t)
	      (pydbgr-parse-cmd-args
	       '("mypydbgr" "--annotate=1" "foo")))
(assert-equal '(("python") ("pydbgr" "--annotate") ("1" "foo") t)
	      (pydbgr-parse-cmd-args
	       '("python" "pydbgr" "--annotate" "1" "foo")))
(assert-equal '(("/usr/bin/python") ("pydbgr" "--different")
		("foo") nil)
	      (pydbgr-parse-cmd-args
	       '("/usr/bin/python" "pydbgr"
		 "--different" "foo")))
(assert-equal '(nil ("program.py") ("foo") nil)
	      (pydbgr-parse-cmd-args '("program.py" "foo")))
(assert-equal '(nil ("pydbgr") ("program.py" "foo") nil)
	      (pydbgr-parse-cmd-args
	       '("pydbgr" "program.py" "foo")))

(end-tests)
