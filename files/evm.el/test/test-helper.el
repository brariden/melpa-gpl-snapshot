(require 'f)

(defvar evm-test/test-path
  (f-dirname (f-this-file)))

(defvar evm-test/root-path
  (f-parent evm-test/test-path))

(defvar evm-sandbox-path
  (f-expand "sandbox" evm-test/test-path))

(defmacro with-sandbox (&rest body)
  `(let ((evm-path evm-sandbox-path)
         (evm-local-path evm-sandbox-path)
         (evm-emacs (f-join evm-sandbox-path "bin" "evm-emacs"))
         (default-directory evm-sandbox-path))
     (when (f-dir? evm-sandbox-path)
       (f-delete evm-sandbox-path 'force))
     (f-mkdir evm-sandbox-path)
     (f-mkdir evm-sandbox-path "bin")
     (let ((emacs-test-path (f-expand "emacs-test" evm-sandbox-path)))
       (when (f-dir? emacs-test-path)
         (f-delete emacs-test-path 'force))
       (f-mkdir emacs-test-path)
       (f-mkdir emacs-test-path "bin")
       (f-mkdir emacs-test-path "Emacs.app" "Contents" "MacOS")
       (let ((default-directory emacs-test-path))
         ,@body))))

(require 'evm (f-expand "evm" evm-test/root-path))
