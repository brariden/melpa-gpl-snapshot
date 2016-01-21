
(defmacro with-metascript-buffer (&rest body)
  "Similar to `with-temp-buffer' except the body is evaluated after `metascript-mode'."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((metascript-mode-hook nil))
       (metascript-mode)
       ,@body)))


(defmacro let-temp-buffer (symbol &rest body)
  "Create a temporary buffer, assign it to symbol, evaluate BODY like `progn', and then kill the buffer."
  (declare (indent 1) (debug t))
  `(let ((,symbol (generate-new-buffer " *temp*")))
     (unwind-protect
         (progn ,@body)
       (and (buffer-name ,symbol)
            (kill-buffer ,symbol)))))


(defun metascript-mode-test/accept-repl-output (repl)
  (accept-process-output (get-buffer-process repl) 1))


(defmacro let-temp-metascript-repl (symbol &rest body)
  "Create a temporary buffer, assign it to symbol, evaluate BODY like `progn', and then kill the buffer."
  (declare (indent 1) (debug t))
  `(let* ((,symbol (metascript-repl-make-comint))
	  (repl-process (get-buffer-process ,symbol)))
     (unwind-protect
         (progn
           (set-process-query-on-exit-flag repl-process nil)
           (metascript-mode-test/accept-repl-output ,symbol)
           ,@body)
       (progn
	 (when (process-live-p repl-process)
	   (quit-process repl-process))
         (and (buffer-name ,symbol)
              (kill-buffer ,symbol))))))


(ert-deftest metascript-mode-test/can-mark-sexp ()
  (with-sandbox
   (with-metascript-buffer
     (let ((metascript-sexp "var f = () ->\n  42\n"))
       (insert metascript-sexp metascript-sexp)
       (goto-char (point-min))
       (metascript-mark-sexp)
       (should (equal metascript-sexp (metascript-region-string)))))))


(defun metascript-mode-test/first-line-of (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (end-of-line)
    (buffer-substring-no-properties (point-min) (point))))


(defun metascript-mode-test/test-repl-eval-after (setup)
  (with-sandbox
   (let-temp-metascript-repl repl
    (with-metascript-buffer
      (insert "var a = 2 * 21")
      (funcall setup)
      (metascript-repl-eval repl)
      (metascript-mode-test/accept-repl-output repl)
      (should (equal "mjs> 42" (metascript-mode-test/first-line-of repl)))))))


(ert-deftest metascript-mode-test/repl-eval-sends-active-region-to-repl ()
  (metascript-mode-test/test-repl-eval-after
   (lambda () (set-mark 9))))


(ert-deftest metascript-mode-test/repl-eval-sends-enclosing-sexp-when-region-is-not-active ()
  "not implemented"
  :expected-result :failed
  (metascript-mode-test/test-repl-eval-after
   (lambda () (goto-char (point-max)))))
