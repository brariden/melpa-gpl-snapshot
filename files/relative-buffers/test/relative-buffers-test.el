;;; relative-buffers-test.el --- relative-buffers test suite

;;; Commentary:

;;; Code:

(require 'relative-buffers)
(require 'ert)
(require 's)
(require 'f)

(defvar test-directory (f-dirname load-file-name)
  "Current directory.")

;; Python.

(ert-deftest test-python-package ()
  (let ((path (f-join test-directory "fixtures/python/package/subpackage/module.py")))
    (should (s-equals? (relative-buffers-python-package path)
                       "package.subpackage.module"))))

(ert-deftest test-python-package-init ()
  (let ((path (f-join test-directory "fixtures/python/package/__init__.py")))
    (should (s-equals? (relative-buffers-python-package path)
                       "package"))))

(ert-deftest test-python-script ()
  (let ((path (f-join test-directory "fixtures/python/nopackage.py")))
    (should (null (relative-buffers-python-package path)))))

;; Dired.

(ert-deftest test-dired-vc ()
  (let ((path (f-join test-directory "fixtures/vc/subdir/dir")))
    (should (s-equals? (relative-buffers-directory path)
                       "subdir/dir/"))))

(ert-deftest test-dired-vc-topdir ()
  (let ((path (f-join test-directory "fixtures/vc/subdir")))
    (should (s-equals? (relative-buffers-directory path)
                       "subdir/"))))

(ert-deftest test-dired-simple-directory ()
  (let ((path (f-join (f-root) "tmp")))
    (should (null (relative-buffers-directory path)))))

;; File.

(ert-deftest test-file-name-vc ()
  (let ((path (f-join test-directory "fixtures/vc/subdir/dir/test")))
    (should (s-equals? (relative-buffers-file-name path)
                       "subdir/dir/test"))))

(ert-deftest test-file-name-simple-file ()
  (let ((path (f-join (f-root) "tmp" "simple")))
    (should (null (relative-buffers-file-name path)))))

(ert-deftest test-file-name-without-file ()
  (should (null (relative-buffers-file-name nil))))

;; Project root.

(ert-deftest test-project-root ()
  (let ((path (f-join test-directory "fixtures/vc/subdir/dir/test")))
    (should (s-equals? (relative-buffers-project-root path)
                       (f-slash (f-join test-directory "fixtures" "vc"))))))

(ert-deftest test-not-project-root ()
  (let ((path (f-join (f-root) "tmp")))
    (should (null (relative-buffers-project-root path)))))

;; Global mode.

(ert-deftest test-open-differnt-files-with-same-name ()
  "Check if renaming work correctly for complex layout.
- each file has same name
- each file has same relative path
- each file placed in different project
README files on top of any vcs project root may cause this error."
  (unwind-protect
      (progn
        (global-relative-buffers-mode +1)
        (find-file (f-join test-directory "fixtures/same-name/a/README.rst"))
        (find-file (f-join test-directory "fixtures/same-name/b/README.rst"))
        (should (get-buffer "README.rst<2>")))
    (ignore-errors
      (global-relative-buffers-mode -1))))

(provide 'relative-buffers-test)

;;; relative-buffers-test.el ends here
