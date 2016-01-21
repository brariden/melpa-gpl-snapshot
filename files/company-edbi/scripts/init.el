;;; init --- configuration file

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(global-company-mode)

(add-to-list 'company-backends 'company-edbi)

(provide 'init)

;;; init.el ends here
