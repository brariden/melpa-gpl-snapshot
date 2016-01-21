;;; pythonic-test.el --- pythonic test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'pythonic)

;;; Executable.

(ert-deftest test-pythonic-executable ()
  "Basic python executable."
  (should (s-equals-p "python" (pythonic-executable))))

(ert-deftest test-pythonic-executable-interpreter ()
  "Python executable on the local host."
  (let ((python-shell-interpreter "/path/to/the/python"))
    (should (s-equals-p "/path/to/the/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-interpreter-remote ()
  "Python executable on the remote host."
  (let ((python-shell-interpreter "/ssh:test@localhost:/path/to/the/python"))
    (should (s-equals-p "/path/to/the/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-virtualenv ()
  "Virtual environment python executable."
  (let ((python-shell-virtualenv-path "/home/me/env"))
    (should (s-equals-p "/home/me/env/bin/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-virtualenv-remote ()
  "Virtual environment python executable on the remote host."
  (let ((python-shell-virtualenv-path "/ssh:test@localhost:/vagrant/env"))
    (should (s-equals-p "/vagrant/env/bin/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-virtualenv-windows ()
  "Virtual environment python executable on the windows platform."
  (let ((system-type 'windows-nt)
        (python-shell-virtualenv-path "C:/env"))
    (should (s-equals-p "C:/env/Scripts/pythonw" (pythonic-executable)))))

;;; Default directory.

(ert-deftest test-pythonic-default-directory ()
  "Run processes in $HOME by default."
  (should (s-equals-p (f-full "~") (pythonic-default-directory))))

(ert-deftest test-pythonic-default-directory-localhost ()
  "Pass directory unmodified in clean environment."
  (should (s-equals-p "/me" (pythonic-default-directory "/me"))))

(ert-deftest test-pythonic-default-directory-interpreter-remote ()
  "Default directory must point to the tramp address in the case
  remote address was specified in the
  `python-shell-interpreter'."
  (unwind-protect
      (let ((python-shell-interpreter "/ssh:test@localhost:/path/to/the/python"))
        (should (s-equals-p "/ssh:test@localhost:/home/test/"
                            (pythonic-default-directory))))
    (kill-buffer "*tramp/ssh test@localhost*")
    (setq tramp-current-connection)
    (sleep-for 0.5)))

(ert-deftest test-pythonic-default-directory-virtualenv-remote ()
  "Virtual environment `default-directory' on the remote host."
  (unwind-protect
      (let ((python-shell-virtualenv-path "/ssh:test@localhost:/vagrant/env"))
        (should (s-equals-p "/ssh:test@localhost:/home/test/"
                            (pythonic-default-directory))))
    (kill-buffer "*tramp/ssh test@localhost*")
    (setq tramp-current-connection)
    (sleep-for 0.5)))

;;; Set PYTHONPATH variable.

(ert-deftest test-pythonic-set-pythonpath-variable ()
  "Set PYTHONPATH according to the `python-shell-extra-pythonpaths' variable."
  (let (process-environment
        (python-shell-extra-pythonpaths '("/home/test/modules")))
    (pythonic-set-pythonpath-variable)
    (should (equal process-environment
                   '("PYTHONPATH=/home/test/modules")))))

(ert-deftest test-pythonic-set-pythonpath-variable-exists ()
  "Set PYTHONPATH according to the `python-shell-extra-pythonpaths' variable.
Respect existing PYTHONPATH."
  (let ((process-environment '("PYTHONPATH=/home/me/modules"))
        (python-shell-extra-pythonpaths '("/home/test/modules")))
    (pythonic-set-pythonpath-variable)
    (should (equal process-environment
                   '("PYTHONPATH=/home/test/modules:/home/me/modules")))))

(ert-deftest test-pythonic-set-pythonpath-variable-few-calls ()
  "Set PYTHONPATH doesn't double entries in the variable on
successively calls."
  (let (process-environment
        (python-shell-extra-pythonpaths '("/home/test/modules")))
    (pythonic-set-pythonpath-variable)
    (pythonic-set-pythonpath-variable)
    (should (equal process-environment
                   '("PYTHONPATH=/home/test/modules")))))

(ert-deftest test-pythonic-set-pythonpath-variable-tramp ()
  "Set PYTHONPATH according to the
`python-shell-extra-pythonpaths' variable on remote host."
  (unwind-protect
      (let ((python-shell-extra-pythonpaths '("/home/test/modules"))
            (python-shell-interpreter "/ssh:test@localhost:/usr/bin/python"))
        (pythonic-set-pythonpath-variable-tramp)
        (tramp-send-command ["ssh" "test" "localhost" "" nil] "echo $PYTHONPATH")
        (should (equal "/home/test/modules\n"
                       (with-current-buffer "*tramp/ssh test@localhost*"
                         (buffer-string)))))
    (kill-buffer "*tramp/ssh test@localhost*")
    (setq tramp-current-connection)
    (sleep-for 0.5)))

(ert-deftest test-pythonic-set-pythonpath-variable-tramp-exists ()
  "Set PYTHONPATH according to the
`python-shell-extra-pythonpaths' variable on remote host.
Respect existing PYTHONPATH on the remote host."
  (unwind-protect
      (let ((python-shell-extra-pythonpaths '("/home/test/modules"))
            (python-shell-interpreter "/ssh:test@localhost:/usr/bin/python"))
        (tramp-send-command ["ssh" "test" "localhost" "" nil]
                            "export PYTHONPATH=/home/me/modules")
        (pythonic-set-pythonpath-variable-tramp)
        (tramp-send-command ["ssh" "test" "localhost" "" nil] "echo $PYTHONPATH")
        (should (equal "/home/test/modules:/home/me/modules\n"
                       (with-current-buffer "*tramp/ssh test@localhost*"
                         (buffer-string)))))
    (kill-buffer "*tramp/ssh test@localhost*")
    (setq tramp-current-connection)
    (sleep-for 0.5)))

(ert-deftest test-pythonic-set-pythonpath-variable-tramp-few-calls ()
  "Set PYTHONPATH doesn't double entries in the variable on
successively calls on the remote host."
  (unwind-protect
      (let ((python-shell-extra-pythonpaths '("/home/test/modules"))
            (python-shell-interpreter "/ssh:test@localhost:/usr/bin/python"))
        (pythonic-set-pythonpath-variable-tramp)
        (pythonic-set-pythonpath-variable-tramp)
        (tramp-send-command ["ssh" "test" "localhost" "" nil] "echo $PYTHONPATH")
        (should (equal "/home/test/modules\n"
                       (with-current-buffer "*tramp/ssh test@localhost*"
                         (buffer-string)))))
    (kill-buffer "*tramp/ssh test@localhost*")
    (setq tramp-current-connection)
    (sleep-for 0.5)))

;;; PATH variable.

(ert-deftest test-pythonic-set-path-variable ()
  "Set PATH variable from `python-shell-exec-path'."
  (let (process-environment
        (python-shell-exec-path '("/home/test/bin")))
    (pythonic-set-path-variable)
    (should (equal '("PATH=/home/test/bin")
                   process-environment))))

(ert-deftest test-pythonic-set-path-variable-tramp ()
  "Set PATH variable from `python-shell-exec-path' on remote host."
  (unwind-protect
      (let ((python-shell-exec-path '("/home/test/bin"))
            (python-shell-interpreter "/ssh:test@localhost:/usr/bin/python"))
        (pythonic-set-path-variable-tramp)
        (tramp-send-command ["ssh" "test" "localhost" "" nil] "echo $PATH")
        (should (s-starts-with-p "/home/test/bin:"
                                 (with-current-buffer "*tramp/ssh test@localhost*"
                                   (buffer-string)))))
    (kill-buffer "*tramp/ssh test@localhost*")
    (setq tramp-current-connection)
    (sleep-for 0.5)))

;;; Extra variables.

(ert-deftest test-pythonic-set-extra-variables ()
  "Set environment from `python-shell-process-environment'."
  (let (process-environment
        (python-shell-process-environment '("PING=PONG")))
    (pythonic-set-extra-variables)
    (should (equal process-environment '("PING=PONG")))))

(ert-deftest test-pythonic-set-extra-variables-tramp ()
  "Set environment form `python-shell-process-environment' on
remote host."
  (unwind-protect
      (let ((python-shell-process-environment '("PING=PONG"))
            (python-shell-interpreter "/ssh:test@localhost:/usr/bin/python"))
        (pythonic-set-extra-variables-tramp)
        (tramp-send-command ["ssh" "test" "localhost" "" nil] "echo $PING")
        (should (equal "PONG\n"
                       (with-current-buffer "*tramp/ssh test@localhost*"
                         (buffer-string)))))
    (kill-buffer "*tramp/ssh test@localhost*")
    (setq tramp-current-connection)
    (sleep-for 0.5)))

;;; Call process.

(ert-deftest test-call-pythonic ()
  "Run synchronous python process."
  (should (eq 0 (call-pythonic :buffer "*out*"
                               :args '("-V")))))

(ert-deftest test-call-pythonic-cwd ()
  "Pass current working directory to the process."
  (call-pythonic :buffer "*out1*"
                 :cwd "~"
                 :args '("-c" "from __future__ import print_function; import os; print(os.getcwd())"))
  (should (s-equals-p (s-concat (f-expand "~") "\n")
                      (with-current-buffer "*out1*"
                        (buffer-string)))))

(ert-deftest test-call-pythonic-extra-pythonpaths ()
  "Synchronous python process respect `python-shell-extra-pythonpaths'."
  (let* ((python-shell-extra-pythonpaths '("/home/test/modules")))
    (call-pythonic :buffer "*out2*"
                   :args '("-c" "from __future__ import print_function; import os; print(os.getenv('PYTHONPATH'))"))
    (should (s-equals-p "/home/test/modules\n"
                        (with-current-buffer "*out2*"
                          (buffer-string))))))

(ert-deftest test-call-pythonic-process-environment ()
  "Synchronous python process respect `python-shell-process-environment'."
  (let ((python-shell-process-environment '("PING=PONG")))
    (call-pythonic :buffer "*out3*"
                   :args '("-c" "from __future__ import print_function; import os; print(os.getenv('PING'))"))
    (should (s-equals-p "PONG\n"
                        (with-current-buffer "*out3*"
                          (buffer-string))))))

(ert-deftest test-call-pythonic-protect-process-environment ()
  "Synchronous python process doesn't affect global `process-environment'."
  (let ((python-shell-process-environment '("PING=PONG")))
    (call-pythonic :buffer (generate-new-buffer-name "*out*") :args '("-V"))
    (should-not (getenv "PING"))))

;;; Start process.

(ert-deftest test-start-pythonic ()
  "Run asynchronous python process."
  (should (equal '("python" "-V")
                 (process-command
                  (start-pythonic :process "out4"
                                  :buffer "*out4*"
                                  :args '("-V"))))))

(ert-deftest test-start-pythonic-cwd ()
  "Run asynchronous python process with working directory specified."
  (let ((process (start-pythonic :process "out5"
                                 :buffer "*out5*"
                                 :cwd "~"
                                 :args '("-c" "from __future__ import print_function; import os; print(os.getcwd())"))))
    (while (process-live-p process)
      (accept-process-output process))
    (should (s-equals-p (s-concat (f-expand "~") "\n\nProcess out5 finished\n")
                        (with-current-buffer "*out5*"
                          (buffer-string))))))

(ert-deftest test-start-pythonic-extra-pythonpaths ()
  "Asynchronous python process respect `python-shell-extra-pythonpaths'."
  (let* ((python-shell-extra-pythonpaths '("/home/test/modules"))
         (process (start-pythonic :process "out6"
                                  :buffer "*out6*"
                                  :args '("-c" "from __future__ import print_function; import os; print(os.getenv('PYTHONPATH'))"))))
    (while (process-live-p process)
      (accept-process-output process))
    (should (s-equals-p "/home/test/modules\n\nProcess out6 finished\n"
                        (with-current-buffer "*out6*"
                          (buffer-string))))))

(ert-deftest test-start-pythonic-process-environment ()
  "Asynchronous python process respect `python-shell-process-environment'."
  (let* ((python-shell-process-environment '("PING=PONG"))
         (process (start-pythonic :process "out7"
                                  :buffer "*out7*"
                                  :args '("-c" "from __future__ import print_function; import os; print(os.getenv('PING'))"))))
    (while (process-live-p process)
      (accept-process-output process))
    (should (s-equals-p "PONG\n\nProcess out7 finished\n"
                        (with-current-buffer "*out7*"
                          (buffer-string))))))

(ert-deftest test-start-pythonic-protect-process-environment ()
  "Asynchronous python process doesn't affect global `process-environment'."
  (let ((python-shell-process-environment '("PING=PONG")))
    (start-pythonic :process "out" :buffer (generate-new-buffer-name "*out*") :args '("-V"))
    (should-not (getenv "PING"))))

(ert-deftest test-start-pythonic-filter ()
  "Set filter function for asynchronous python process."
  (should (eq 'empty
              (process-filter
               (start-pythonic :process "out" :args '("-V") :filter 'empty)))))

(ert-deftest test-start-pythonic-sentinel ()
  "Set sentinel function for asynchronous python process."
  (should (eq 'empty
              (process-sentinel
               (start-pythonic :process "out" :args '("-V") :sentinel 'empty)))))

(ert-deftest test-start-pythonic-query-on-exit ()
  "Set query on exit flag for asynchronous python process."
  (should (process-query-on-exit-flag
           (start-pythonic :process "out" :args '("-V") :query-on-exit t))))

;;; Proper process environment detection.

(ert-deftest test-start-pythonic-path-property ()
  "Set `python-shell-exec-path' as `path' process property."
  (let* ((home (f-expand "~"))
         (python-shell-exec-path '("test"))
         (process-environment (list "PATH=/usr/bin" (concat "HOME" "=" home))))
    (should (equal "test:/usr/bin"
                   (plist-get
                    (process-get
                     (start-pythonic :process "out" :args '("-V"))
                     'pythonic)
                    :path)))))

(ert-deftest test-start-pythonic-path-property-tramp ()
  "Set `python-shell-exec-path' as `path' process property on remote process."
  (unwind-protect
      (let* ((python-shell-interpreter "/ssh:test@localhost:/path/to/the/python")
             (python-shell-exec-path '("/home/test/bin")))
        (should (equal "/home/test/bin:/bin:/usr/bin:/sbin:/usr/sbin:/usr/local/bin:/usr/local/sbin"
                       (plist-get
                        (process-get
                         (start-pythonic :process "out" :args '("-V"))
                         'pythonic)
                        :path))))
    (kill-buffer "*tramp/ssh test@localhost*")
    (setq tramp-current-connection)
    (sleep-for 0.5)))

(ert-deftest test-start-pythonic-pythonpath-property ()
  "Set `python-shell-extra-pythonpaths' as `pythonpath' process property."
  (let* ((home (f-expand "~"))
         (python-shell-extra-pythonpaths '("test"))
         (process-environment (list "PYTHONPATH=/home/me" (concat "HOME" "=" home))))
    (should (equal "test:/home/me"
                   (plist-get
                    (process-get
                     (start-pythonic :process "out" :args '("-V"))
                     'pythonic)
                    :pythonpath)))))

(ert-deftest test-start-pythonic-pythonpath-property-tramp ()
  "Set `python-shell-extra-pythonpaths' as `pythonpath' process property on remote host.
Respect remote PYTHONPATH value."
  (unwind-protect
      (let* ((python-shell-interpreter "/ssh:test@localhost:/path/to/the/python")
             (python-shell-extra-pythonpaths '("/home/test/modules")))
        (tramp-send-command ["ssh" "test" "localhost" "" nil]
                            "export PYTHONPATH=/home/me/modules")
        (should (equal "/home/test/modules:/home/me/modules"
                       (plist-get
                        (process-get
                         (start-pythonic :process "out" :args '("-V"))
                         'pythonic)
                        :pythonpath))))
    (kill-buffer "*tramp/ssh test@localhost*")
    (setq tramp-current-connection)
    (sleep-for 0.5)))

(ert-deftest test-start-pythonic-connection-property ()
  "Set `pythonic-tramp-connection' result as `connection' process property."
  (unwind-protect
      (let ((python-shell-interpreter "/ssh:test@localhost:/path/to/the/python"))
        (should (equal "/ssh:test@localhost:"
                       (plist-get
                        (process-get
                         (start-pythonic :process "out" :args '("-V"))
                         'pythonic)
                        :connection))))
    (kill-buffer "*tramp/ssh test@localhost*")
    (setq tramp-current-connection)
    (sleep-for 0.5)))

(ert-deftest test-start-pythonic-environment-property ()
  "Set `python-shell-process-environment' as `environment' process property."
  (let ((python-shell-process-environment '("TEST=t")))
    (should (equal '("TEST=t")
                   (plist-get
                    (process-get
                     (start-pythonic :process "out" :args '("-V"))
                     'pythonic)
                    :environment)))))

(ert-deftest test-pythonic-proper-environment-p-non-pythonic-process ()
  "Applying `pythonic-proper-environment-p' to the process
doesn't started with `start-pythonic' will cause error."
  (let ((process (start-process "out" (generate-new-buffer-name "*out*") "python" "-V")))
    (should-error (pythonic-proper-environment-p process))))

(ert-deftest test-pythonic-proper-environment-p ()
  "`pythonic-proper-environment-p' is true if environment doesn't
change since process was start."
  (let ((process (start-pythonic :process "out" :args '("-V"))))
    (should (pythonic-proper-environment-p process))))

(ert-deftest test-pythonic-proper-environment-p-change-pythonpath ()
  "`pythonic-proper-environment-p' is null if PYTHONPATH was changed."
  (let* ((process (start-pythonic :process "out" :args '("-V")))
         (python-shell-extra-pythonpaths '("/home/test/modules")))
    (should-not (pythonic-proper-environment-p process))))

(ert-deftest test-pythonic-proper-environment-p-change-path ()
  "`pythonic-proper-environment-p' is null if PATH was changed."
  (let* ((process (start-pythonic :process "out" :args '("-V")))
         (python-shell-exec-path '("/home/test/bin")))
    (should-not (pythonic-proper-environment-p process))))

(ert-deftest test-pythonic-proper-environment-p-change-environment ()
  "`pythonic-proper-environment-p' is null if
`python-shell-process-environment' was changed."
  (let* ((process (start-pythonic :process "out" :args '("-V")))
         (python-shell-process-environment '("TEST=t")))
    (should-not (pythonic-proper-environment-p process))))

(ert-deftest test-pythonic-proper-environment-p-change-interpreter ()
  "`pythonic-proper-environment-p' is null if interpreter path was changed."
  (let* ((process (start-pythonic :process "out" :args '("-V")))
         (python-shell-interpreter "/path/to/the/python"))
    (should-not (pythonic-proper-environment-p process))))

(ert-deftest test-pythonic-proper-environment-p-change-host ()
  "`pythonic-proper-environment-p' is null if tramp host was changed."
  ;; `unwind-protect' block is unnecessary here because non of tramp
  ;; connection will be opened below.  First process starts locally,
  ;; proper environment check will stop at connection strings
  ;; comparison, non of remote PATH or PYTHONPATH checks will be made.
  (let* ((process (start-pythonic :process "out" :args '("-V")))
         (python-shell-interpreter "/ssh:test@localhost:/path/to/the/python"))
    (should-not (pythonic-proper-environment-p process))))

;;; Activate/deactivate environment.

(ert-deftest test-pythonic-activate ()
  "Activate virtual environment."
  (let (python-shell-virtualenv-path)
    (pythonic-activate "/home/me/env")
    (should (s-equals-p python-shell-virtualenv-path "/home/me/env"))))

(ert-deftest test-pythonic-deactivate ()
  "Deactivate virtual environment."
  (let ((python-shell-virtualenv-path "/home/me/env"))
    (pythonic-deactivate)
    (should-not python-shell-virtualenv-path)))

(provide 'pythonic-test)

;;; pythonic-test.el ends here
