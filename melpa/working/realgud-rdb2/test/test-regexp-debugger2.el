;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "realgud.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(require 'realgud)
(load-file "../rdb2/init.el")
(load-file "./regexp-helper.el")

(declare-function __FILE__              'load-relative)

(test-simple-start)

(setup-regexp-vars realgud:rdb2-pat-hash)

(eval-when-compile
  (defvar dbg-name)   (defvar realgud-pat-hash)   (defvar realgud-bt-hash)
  (defvar loc-pat)    (defvar prompt-pat)         (defvar s1)
  (defvar file-group) (defvar line-group)         (defvar pos)
  (defvar test-dbgr)  (defvar test-text)
)

; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "rdb2")

(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))
(setq test-dbgr (make-realgud-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (realgud-loc-pat-regexp loc-pat)
		  :file-group (realgud-loc-pat-file-group loc-pat)
		  :line-group (realgud-loc-pat-line-group loc-pat)))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.

(setq test-text "	from /usr/local/bin/irb:12:in `<main>'")
(note "traceback location matching")

;; (assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")
;; (assert-equal "/usr/local/bin/irb"
;; 	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
;; 			    test-text) "extract file name")
;; (assert-equal "12"
;; 	      (match-string (realgud-cmdbuf-info-line-group test-dbgr)
;; 			    test-text) "extract line number")

(setq test-text "Breakpoint 1 file /usr/bin/irb, line 10\n")
(assert-t (numberp (loc-match test-text helper-bps)) "basic breakpoint location")
;; (assert-equal "/usr/bin/irb"
;; 	      (match-string (realgud-loc-pat-file-group helper-bps)
;; 			    test-text) "extract breakpoint file name")
;; (assert-equal "10"
;; 	      (match-string (realgud-loc-pat-line-group helper-bps)
;; 			    test-text)   "extract breakpoint line number")

(note "prompt")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:rdb2-pat-hash))
(prompt-match "(rdb2:1) ")

(end-tests)
