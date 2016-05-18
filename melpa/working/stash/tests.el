(require 'ert)
(require 'ert-x)
(require 'stash)

(defmacro with-clean-cache (&rest body)
  `(let ((stash-directory (expand-file-name "./.test-caches")))
     (when (file-exists-p stash-directory)
       (delete-directory stash-directory t))
     (mkdir stash-directory)
     ,@body))

(stash-app-new test-app 60)

(ert-deftest simple ()
  (with-clean-cache

   (should
    (eq 'tmp
        (stash-new tmp "tmp.el")))

   (ert-run-idle-timers)

   (should
    (file-exists-p (stash-file 'tmp)))

   (setq tmp 123)

   (should
    (null (stash-reset 'tmp)))

   (should
    (null tmp))))

(ert-deftest simple-with-load-default ()
  (with-clean-cache
   (should
    (equal "stash test"
           (symbol-value
            (stash-new tmp "tmp.el" test-app
                       (or stashed "stash test")))))
   (ert-run-idle-timers)
   (should
    (equal "stash test"
           (stash-load 'tmp)))
   (setq tmp nil)
   (ert-run-idle-timers)
   (should (null (stash-load 'tmp)))
   (should (null tmp))))

(ert-deftest simple-with-idle-timer ()
  (with-clean-cache

   (stash-new tmp "tmp.el" test-app 'my-default)

   (should-not
    (file-exists-p (stash-file 'tmp)))

   (ert-run-idle-timers)

   (should
    (file-exists-p (stash-file 'tmp)))))
