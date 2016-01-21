(require 'ert)
(require 'el-mock)
(require 'flymake-gjshint)

;; eval the following forms, when Testing in customized emacs configuration.
;;
;; (add-to-list 'load-path (expand-file-name "."))
;; (add-to-list 'load-path (expand-file-name ".."))

(ert-deftest test:jshint-command-line ()
  (let ((flymake-gjshint:jshint-configuration-path nil))
    (should (equal flymake-gjshint:jshint-command
                   (flymake-gjshint:jshint-command-line))))
  (let* ((config (expand-file-name "~/.jshintrc"))
         (flymake-gjshint:jshint-configuration-path config))
    (should (equal (format
                    "%s --config %s "
                    flymake-gjshint:jshint-command
                    config)
                   (flymake-gjshint:jshint-command-line)))))

(ert-deftest test:gjslint-command-line ()
  (let ((flymake-gjshint:gjslint-flagfile-path nil))
    (should (equal flymake-gjshint:gjslint-command
                   (flymake-gjshint:gjslint-command-line))))
  (let* ((config (expand-file-name "~/.gjslintrc"))
         (flymake-gjshint:gjslint-flagfile-path config))
    (should (equal (format
                    "%s --flagfile %s "
                    flymake-gjshint:gjslint-command
                    config)
                   (flymake-gjshint:gjslint-command-line)))))

(defvar test:gjslint-err01
  "Line 1, E:0001: Extra space after function")

(ert-deftest test:gjslint-err-line-patterns ()
  (let((pattern (car flymake-gjshint:gjslint-err-line-patterns)))
    (should (string-match  pattern test:gjslint-err01))))
(defvar test:jshint-err01
  "src/test.js: line 1, col 5, Redefinition of 'global'.")
(defvar test:jshint-err02
  "src/test.js: line 1, col 13, 'global' is defined but never used.")

(ert-deftest test:jshint-err-line-patterns ()
  (let((pattern (car flymake-gjshint:jshint-err-line-patterns)))
    (should (string-match  pattern test:jshint-err01))
    (should (string-match  pattern test:jshint-err02))))

(ert-deftest test:setup ()
  (let ((flymake-gjshint t))
    (flymake-gjshint:setup)
    (should (local-variable-p 'flymake-allowed-file-name-masks))
    (should (local-variable-p 'flymake-err-line-patterns))
    (should (memq flymake-gjshint:allowed-file-name-masks
                  flymake-allowed-file-name-masks))
    (should (memq flymake-gjshint:gjslint-err-line-patterns
                  flymake-err-line-patterns))
    (should (memq flymake-gjshint:jshint-err-line-patterns
                  flymake-err-line-patterns))
    (should (eq flymake-mode t)))

  (let ((flymake-gjshint nil))
    (flymake-gjshint:setup)
    (mocklet ((make-local-variable not-called)
              (add-to-list not-called)
              (flymake-mode not-called)))))

(ert-deftest test:load ()
  (mocklet ((message)
            (make-local-variable not-called))
    ;; show error message, if jshint and gjslint is not found.
    (mocklet ((executable-find))
      (flymake-gjshint:load))
    (let ((flymake-gjshint nil))
      ;; flymake-gjshint:setup is not called,
      ;; if flymake:gjshint is not found.
      (mocklet ((executable-find => t))
        (flymake-gjshint:load))))
  (mocklet ((executable-find => t))
    (let ((flymake-gjshint t))
      (flymake-gjshint:load)))
  (should (local-variable-p 'hack-local-variables-hook)))

(unless noninteractive
  (defun flymake-error-list ()
    (let* ((line-no            (flymake-current-line-no))
           (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
           (menu-data          (flymake-make-err-menu-data line-no line-err-info-list)))
      (when menu-data
        (mapcar (lambda (el) (car el)) (cadr menu-data)))))

  (defun flymake-next-error-p ()
    "Go to next error in err ring."
    (interactive)
    (let ((line-no (flymake-get-next-err-line-no flymake-err-info (flymake-current-line-no))))
      (when (not line-no)
        (setq line-no (flymake-get-first-err-line-no flymake-err-info))
        (flymake-log 1 "passed end of file"))
      (if line-no t nil)))

  (defvar is-exec-from-run-test (boundp 'test-dir))


  (ert-deftest test:flymake-error ()
    (let (js-mode-hook buffer dir)
      (add-hook 'js-mode-hook 'flymake-gjshint:load)

      (setq dir (if is-exec-from-run-test test-dir (expand-file-name ".")))

      (setq buffer (find-file (format "%s/interactive/test.js" dir)))
      (sit-for 1) ;; wait for buffer-file-name set
      (with-current-buffer buffer
        (goto-char (point-min))
        (should (flymake-error-list)))

      (setq buffer (find-file (format "%s/interactive/file-local-test.js" dir)))
      (sit-for 1) ;; wait for buffer-file-name set
      (with-current-buffer buffer
        (goto-char (point-min))
        (should (not (flymake-next-error-p))))

      (setq buffer (find-file (format "%s/interactive/file-local-test-gjslint.js" dir)))
      (sit-for 1) ;; wait for buffer-file-name set
      (with-current-buffer buffer
        (goto-char (point-min))
        (forward-line 4)
        (should (flymake-error-list)))

      (setq buffer (find-file (format "%s/interactive/dir/dir-local-test.js" dir)))
      (sit-for 1) ;; wait for buffer-file-name set
      (with-current-buffer buffer
        (goto-char (point-min))
        (should (not (flymake-error-list))))
      ))

  (ert-deftest test:flymake-gjshint:init-jshint ()
    (let ((flymake-gjshint 'jshint)
          (flymake-gjshint:jshint-configuration-path "")
          js-mode-hook buffer dir)
      (add-hook 'js-mode-hook 'flymake-gjshint:load)

      (setq dir (if is-exec-from-run-test test-dir (expand-file-name ".")))

      (setq buffer (find-file (format "%s/interactive/file-local-test-jshint.js" dir)))
      (sit-for 1) ;; wait for buffer-file-name set
      (with-current-buffer buffer
        (goto-char (point-min))
        (should (flymake-error-list)))

      (let ((flymake-gjshint:jshint-configuration-path
             (format "%s/interactive/.jshintrc" dir)))
        (kill-buffer buffer)
        (setq buffer (find-file (format "%s/interactive/file-local-test-jshint.js" dir)))
        (sit-for 1) ;; wait for buffer-file-name set
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (flymake-error-list)))
        )))

  (ert-deftest test:flymake-gjshint:init-gjslint ()
    (let ((flymake-gjshint 'gjslint)
          (flymake-gjshint:gjslint-flagfile-path "")
          js-mode-hook buffer dir)
      (add-hook 'js-mode-hook 'flymake-gjshint:load)

      (setq dir (if is-exec-from-run-test test-dir (expand-file-name ".")))

      (setq buffer (find-file (format "%s/interactive/file-local-test-gjslint.js" dir)))
      (sit-for 1) ;; wait for buffer-file-name set
      (with-current-buffer buffer
        (goto-char (point-min))
        (forward-line 4)
        (should (flymake-error-list)))

      (let ((flymake-gjshint:gjslint-flagfile-path
             (format "%s/interactive/.gjslintrc" dir)))
        (kill-buffer buffer)
        (setq buffer (find-file (format "%s/interactive/file-local-test-gjslint.js" dir)))
        (sit-for 1) ;; wait for buffer-file-name set
        (with-current-buffer buffer
          (goto-char (point-min))
          (forward-line 4)
          (should (flymake-error-list)))
        )))
  )
