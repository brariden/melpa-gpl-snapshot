;;; edbi-database-url-test.el --- edbi-database-url test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'edbi-database-url)

(defun mock-read-string (&rest ignored)
  "Mock `read-string' variant IGNORED any args."
  "postgres://user:password@host:5678/name")

(defalias 'read-string 'mock-read-string)

;;; Read settings.

(ert-deftest test-edbi-database-url-read-url ()
  (let ((process-environment
         '("DATABASE_URL=mysql://user:password@host:5678/name")))
    (should (equal "mysql://user:password@host:5678/name"
                   (edbi-database-url-read-url)))))

(ert-deftest test-edbi-database-url-read-url-user-input ()
  (should (equal "postgres://user:password@host:5678/name"
                 (edbi-database-url-read-url))))

(ert-deftest test-edbi-database-url-read-url-region-active ()
  (with-temp-buffer
    (insert "sqlite://user:password@host:5678/name")
    (transient-mark-mode 1)
    (mark-whole-buffer)
    (should (equal "sqlite://user:password@host:5678/name"
                   (edbi-database-url-read-url)))))

;;; Parse url.

(ert-deftest test-edbi-database-url-parse-url ()
  (should (equal "localhost"
                 (url-host
                  (edbi-database-url-parse-url
                   "sqlite://user:password@localhost:5678/name")))))

(ert-deftest test-edbi-database-url-parse-url-sqlite-memory ()
  (should (equal "_:memory:"  ;; First trash character will be removed later.
                 (url-filename
                  (edbi-database-url-parse-url
                   "sqlite://:memory:")))))

;;; Generate uri.

(ert-deftest test-edbi-database-url-generate-uri ()
  (should (equal "dbi:Pg:dbname=test;host=localhost;port=5678"
                 (edbi-database-url-generate-uri
                  (edbi-database-url-parse-url
                   "postgres://user:password@localhost:5678/test")))))

(ert-deftest test-edbi-database-url-generate-uri-no-port ()
  (should (equal "dbi:SQLite:dbname=/tmp/db.sqlite3"
                 (edbi-database-url-generate-uri
                  (edbi-database-url-parse-url
                   "sqlite:////tmp/db.sqlite3")))))

(ert-deftest test-edbi-database-url-generate-uri-sqlite-memory ()
  (should (equal "dbi:SQLite:dbname=:memory:"
                 (edbi-database-url-generate-uri
                  (edbi-database-url-parse-url
                   "sqlite://:memory:")))))

(ert-deftest test-edbi-database-url-generate-uri-unknown-scheme ()
  (should-error (edbi-database-url-generate-uri
                 (edbi-database-url-parse-url
                  "hahaha:////tmp/db.sqlite3"))))

;;; Data source.

(ert-deftest test-edbi-database-url-data-source ()
  (should (equal "password"
                 (caddr
                  (edbi-database-url-data-source
                   (edbi-database-url-parse-url
                    "postgres://user:password@localhost:5678/test"))))))

(provide 'edbi-django-test)

;;; edbi-database-url-test.el ends here
