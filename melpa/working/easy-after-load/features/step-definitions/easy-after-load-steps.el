;; easy-after-load

(Given "^I have an easy-after-load directory$"
       (lambda ()
         ; nothing to do
         ))

(Given "^I have a second easy-after-load directory \"\\(.+\\)\"$"
       (lambda (dir)
         (setq easy-after-load-test-extra-dir (expand-file-name (concat "tmp/" dir) easy-after-load-root-path))
         (mkdir easy-after-load-test-extra-dir 'parents)))

(Given "^I have an? \"\\(.+\\)\" after-load file$"
       (lambda (filename)
         (let ((path (expand-file-name filename easy-after-load-test-directory)))
           (condition-case nil (delete-file path) (error))
           (with-temp-buffer
             (insert (concat "(message \"Loaded %s!\" \"" filename "\")\n"))
             (write-region (point-min) (point-max) path)))))

(Given "^I have an? \"\\(.+\\)\" after-load file in \"\\(.+\\)\"$"
       (lambda (filename dir)
         ; Ignoring dir, it's really just easy-after-load-test-extra-dir
         (let ((easy-after-load-test-directory easy-after-load-test-extra-dir))
           (Given (format "I have an \"%s\" after-load file" filename)))))

(Given "^I am recording every file found by easy-after-load$"
       (lambda ()
         (setq easy-after-load-test-files-seen nil
               easy-after-load-function (lambda (file)
                                          (setq easy-after-load-test-files-seen
                                                (cons file easy-after-load-test-files-seen))
                                          (easy-after-load-apply-pattern file)))))

(Given "^my easy-after-load-function returns the filename sans extension$"
       (lambda ()
         (setq easy-after-load-function 'file-name-sans-extension)))

(Given "^my easy-after-load-function just raises errors$"
       (lambda ()
         (setq easy-after-load-function (lambda (f) (error "Failure!")))))

(When "^I run easy-after-load$"
      (lambda () (easy-after-load)))

(When "^I run easy-after-load on \"\\(.+\\)\"$"
       (lambda (dir)
         ; Ignoring dir, it's really just easy-after-load-test-extra-dir
         (easy-after-load easy-after-load-test-extra-dir)))

(When "^I add a custom eval-after-load line for '\\(.+\\)$"
       (lambda (mode)
         (eval-after-load (intern mode) '(message "Custom after-load!"))))

(When "^I remove my \"\\(.+\\)\" after-load file$"
      (lambda (filename)
        (delete-file (expand-file-name filename easy-after-load-test-directory))))

(Then "^I should have seen message \"\\(.+\\)\"$"
      (lambda (msg)
        (let ((matches (-select (lambda (seen) (equal msg seen))
                                ecukes-message-log)))
          (should (not (zerop (length matches)))))))

(Then "^\\(.+\\) after-load-alist entries should be present for '\\(.+\\)$"
      (lambda (count feature)
        (let ((entry (assoc (intern feature) after-load-alist)))
          (should (= (string-to-int count) (length (cdr entry)))))))

(Then "^easy-after-load should never have seen \"\\(.+\\)\"$"
      (lambda (file)
        (should (not (member file easy-after-load-test-files-seen)))))

;; easy-auto-mode
(When "^I open a file \"\\(.+\\)\"$"
      (lambda (filename)
        (find-file filename)))

(Then "^the major mode should be '\\(.+\\)$"
      (lambda (mode)
        (should (equal major-mode (intern mode)))))
