;;; test-helper.el --- ert-runner test helper

;;; Commentary:

;;; Code:

(require 'cask)

(let ((root-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize root-directory)
  (add-to-list 'load-path root-directory))

(undercover "tern-django.el" (:report-file "emacs-coveralls.json") (:send-report nil))

(provide 'test-helper)

;;; test-helper.el ends here
