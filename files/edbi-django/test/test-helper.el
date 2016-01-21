;;; test-helper.el --- ert-runner test helper

;;; Commentary:

;;; Code:

(require 'cask)

(defvar root-directory (locate-dominating-file load-file-name "Cask"))

(defvar project-directory (expand-file-name ".project" root-directory))

(cask-initialize root-directory)

(add-to-list 'load-path root-directory)

(undercover "*.el" (:report-file "emacs-coveralls.json") (:send-report nil))

(unless (file-exists-p project-directory)
  (let ((default-directory root-directory))
    (make-directory project-directory)
    (shell-command "django-admin.py startproject project .project")))

(setenv "DJANGO_SETTINGS_MODULE" "project.settings")

(setenv "PYTHONPATH" project-directory)

(unless (file-exists-p (f-join project-directory "db.sqlite3"))
  (shell-command "django-admin.py syncdb --noinput" "*syncdb*")
  (with-current-buffer "*syncdb*"
    (message (buffer-string))))

(provide 'test-helper)

;;; test-helper.el ends here
