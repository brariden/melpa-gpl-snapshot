
;; requires and setup

(when load-file-name
  (setq pcache-directory (expand-file-name "test_output/" (file-name-directory load-file-name)))
  (setq package-enable-at-startup nil)
  (setq package-load-list '((pcache t)
                            (list-utils t)
                            (persistent-soft t)))
  (when (fboundp 'package-initialize)
    (package-initialize)))

(require 'list-utils)
(require 'persistent-soft)
(require 'ido-load-library)

;;; this is a stub - no tests defined

;;; ido-load-library-all-library-names (&optional progress regenerate)

;;; ido-load-library (library &optional regenerate)

;;; ido-load-library-find (file &optional regenerate)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;

;;; ido-load-library-test.el ends here
