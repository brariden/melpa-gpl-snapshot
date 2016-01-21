(ert-deftest evm-find-test ()
  (with-sandbox
   (let ((bin-path (f-join emacs-test-path "bin" "emacs")))
     (f-touch bin-path)
     (f-symlink bin-path evm-emacs))
   (f-mkdir "foo" "bar")
   (f-mkdir "baz")
   (f-touch (f-join "foo" "FOO"))
   (f-touch (f-join "foo" "bar" "BAR"))
   (f-touch (f-join "baz" "BAZ"))
   (should (equal (f-join default-directory "foo" "FOO") (evm-find "FOO")))
   (should (equal (f-join default-directory "foo" "bar" "BAR") (evm-find "BAR")))
   (should (equal (f-join default-directory "baz" "BAZ") (evm-find "BAZ")))
   (should-not (evm-find "qux"))))

(ert-deftest evm-emacs-test/osx ()
  (with-sandbox
   (let ((bin-path (f-join emacs-test-path "Emacs.app" "Contents" "MacOS" "Emacs")))
     (f-touch bin-path)
     (f-symlink bin-path evm-emacs))
   (let ((system-type 'darwin))
     (should (string= (evm-emacs) (f-expand (f-join "Emacs.app" "Contents" "MacOS" "Emacs")))))))

(ert-deftest evm-emacs-test/non-osx ()
  (with-sandbox
   (let ((bin-path (f-join emacs-test-path "bin" "emacs")))
     (f-touch bin-path)
     (f-symlink bin-path evm-emacs))
   (let ((system-type 'msdos))
     (should (string= (evm-emacs) (f-expand (f-join "bin" "emacs")))))))

(ert-deftest evm-emacsclient-test/osx ()
  (with-sandbox
   (let ((bin-path (f-join emacs-test-path "Emacs.app" "Contents" "MacOS" "Emacs")))
     (f-touch bin-path)
     (f-symlink bin-path evm-emacs))
   (let ((system-type 'darwin))
     (should (string= (evm-emacsclient) (f-expand (f-join "Emacs.app" "Contents" "MacOS" "bin" "emacsclient")))))))

(ert-deftest evm-emacsclient-test/non-osx ()
  (with-sandbox
   (let ((bin-path (f-join emacs-test-path "bin" "emacs")))
     (f-touch bin-path)
     (f-symlink bin-path evm-emacs))
   (let ((system-type 'msdos))
     (should (string= (evm-emacsclient) (f-expand (f-join "bin" "emacsclient")))))))
