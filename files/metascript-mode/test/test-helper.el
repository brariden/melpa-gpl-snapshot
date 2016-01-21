(require 'f)

(defvar metascript-test-path
  (f-dirname (f-this-file)))

(defvar metascript-code-path
  (f-parent metascript-test-path))

(defvar metascript-sandbox-path
  (f-expand "sandbox" metascript-test-path))

(require 'metascript-mode (f-expand "metascript-mode.el" metascript-code-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory metascript-sandbox-path))
     (when (f-dir? metascript-sandbox-path)
       (f-delete metascript-sandbox-path :force))
     (f-mkdir metascript-sandbox-path)
     ,@body))
