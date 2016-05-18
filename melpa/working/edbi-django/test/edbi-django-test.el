;;; edbi-django-test.el --- edbi-django test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'edbi-django)

;;; Read settings.

(ert-deftest test-read-django-settings ()
  (let ((settings (cdr (assoc "default" (edbi-django-settings)))))
    (should (s-equals? (cdr (assoc "ENGINE" settings))
                       "django.db.backends.sqlite3"))
    (should (s-equals? (cdr (assoc "NAME" settings))
                       (f-join project-directory "db.sqlite3")))))

(ert-deftest test-read-settings-error ()
  (let* ((envvar "DJANGO_SETTINGS_MODULE=project.settings")
         (process-environment (--remove (s-equals-p it envvar) process-environment)))
    (should-error (edbi-django-settings))))

(ert-deftest test-read-settings-error-message ()
  (let* ((envvar "DJANGO_SETTINGS_MODULE=project.settings")
         (process-environment (--remove (s-equals-p it envvar) process-environment)))
    (should (s-equals? "Unable to read database django settings"
                       (condition-case err
                           (edbi-django-settings)
                         (error (error-message-string err)))))))

;;; Read databases list.

(ert-deftest test-databases-list ()
  (should (equal (edbi-django-databases
                  (edbi-django-settings))
                 '("default"))))

;;; Completing system.

(ert-deftest test-completing-read-nil ()
  (should (null (edbi-django-completing-read "" nil))))

(ert-deftest test-completing-read-single ()
  (should (s-equals? "single" (edbi-django-completing-read "" '("single")))))

(ert-deftest test-completing-read-multiple ()
  (let ((completing-read-function (lambda (p c &rest i) (car c))))
    (should (s-equals? "first" (edbi-django-completing-read "" '("first" "second"))))))

;;; Database uri generation.

(ert-deftest test-get-dbi-uri ()
  (let ((database (cdr (assoc "default" (edbi-django-settings)))))
    (should (s-equals?  (edbi-django-uri database)
                        (concat "dbi:SQLite:dbname="
                                (f-join project-directory "db.sqlite3"))))))

(ert-deftest test-dbi-uri-filter-empty-params ()
  (let ((database '(("ENGINE" . "django.db.backends.postgresql_psycopg2")
                    ("HOST" . "localhost")
                    ("NAME" . "bars_web_edu")
                    ("PASSWORD" . "bars_web_edu")
                    ("PORT" . "")
                    ("USER" . "bars_web_edu"))))
    (should (-same-items?
             (s-split "[:;]" (edbi-django-uri database))
             (list "dbi" "Pg" "host=localhost" "dbname=bars_web_edu")))))

;;; Database connection.

(ert-deftest test-read-table-list ()
  (edbi-django-disconnect)
  (edbi-django-connect)
  (should (equal '("auth_group"
                   "auth_group_permissions"
                   "auth_permission"
                   "auth_user"
                   "auth_user_groups"
                   "auth_user_user_permissions"
                   "django_admin_log"
                   "django_content_type"
                   "django_session")
                 (sort (--map
                        (cadr it)
                        (edbi:sync edbi:select-all-d
                                   edbi-django-connection
                                   "SELECT * FROM sqlite_master WHERE type='table';"))
                       'string<))))

(ert-deftest test-connection-error ()
  (edbi-django-disconnect)
  (let* ((module "DJANGO_SETTINGS_MODULE=fixtures.postgres")
         (path (concat "PYTHONPATH=" (f-join root-directory "test")))
         (process-environment (append (list module path) process-environment)))
    (should-error (edbi-django-connect))))

(ert-deftest test-connection-error-string ()
  (edbi-django-disconnect)
  (let* ((module "DJANGO_SETTINGS_MODULE=fixtures.postgres")
         (path (concat "PYTHONPATH=" (f-join root-directory "test")))
         (process-environment (append (list module path) process-environment)))
    (should (s-equals? "Unable to connect to django database"
                       (condition-case err
                           (edbi-django-connect)
                         (error (error-message-string err)))))))

(provide 'edbi-django-test)

;;; edbi-django-test.el ends here
