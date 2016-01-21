;;; test-helper.el --- Tests for restart-emacs             -*- lexical-binding: t; -*-

;;; Commentary:
;;; Helpers to write tests for restart-emacs

;;; Code:

;; Setup load-path, some of this is redundant when tests are run from the
;; command line
(let ((project-dir (locate-dominating-file (or (buffer-file-name) load-file-name)
                                           ".cask")))
  (if (not project-dir)
      (user-error "Could not locate project root")
    (let ((default-directory (expand-file-name (concat ".cask/" emacs-version) project-dir)))
      (normal-top-level-add-subdirs-to-load-path))

    (add-to-list 'load-path project-dir)))

(provide 'test-helper)
;;; test-helper.el ends here
