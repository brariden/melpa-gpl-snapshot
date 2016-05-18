;;; test-helper.el --- ert-runner test helper

;;; Commentary:

;;; Code:

(require 'cask)

(let ((root-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize root-directory)
  (add-to-list 'load-path root-directory))

(undercover "*.el")

(provide 'test-helper)

;;; test-helper.el ends here
