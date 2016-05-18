;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq easy-after-load-root-path project-directory)
  (setq easy-after-load-util-path (expand-file-name "util" easy-after-load-root-path)))

(add-to-list 'load-path easy-after-load-root-path)
(add-to-list 'load-path (expand-file-name "espuds" easy-after-load-util-path))
(add-to-list 'load-path (expand-file-name "ert" easy-after-load-util-path))

(require 'easy-after-load)
(require 'espuds)
(require 'ert)


(Setup
 (setq easy-after-load-test-directory (expand-file-name "tmp/after-loads/" easy-after-load-root-path)))

(Teardown
 ; nothing
 )

(Before
 (mkdir easy-after-load-test-directory 'parents)
 (setq easy-after-load-directory easy-after-load-test-directory
       ; And restore defaults corrupted by tests
       easy-after-load-pattern "^after-\\(.+\\)\\.el$"
       easy-after-load-function 'easy-after-load-apply-pattern))

(After
 (delete-directory easy-after-load-test-directory 'recursive)
 (when (boundp 'easy-after-load-test-extra-dir)
   (delete-directory easy-after-load-test-extra-dir 'recursive)
   (makunbound 'easy-after-load-test-extra-dir)))
