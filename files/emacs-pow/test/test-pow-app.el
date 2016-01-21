;;; test-pow-app.el --- unit test for pow-app

;; Copyright (c) 2014 yukihiro hara

;; Author: yukihiro hara <yukihr@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

(add-to-list 'load-path "./test")

(setq pow-symlink-directory "./test/.pow")

(require 'ert)
(require 'pow-app)
(require 'mocker)
(require 'cl-lib)

(defvar testpath (expand-file-name "./test/rack-app"))

;; pow-app
(ert-deftest make-pow-app ()
  (let ((app (make-pow-app :path "/home/huga/proj/hoge")))
    (should
     (equal (pow-app-name app) "hoge"))
    (should
     (equal (pow-app-path app) "/home/huga/proj/hoge"))
    (should (equal (pow-app-symlink-directory app) pow-symlink-directory))))

(ert-deftest pow-app-set-name-default ()
  (let ((app (make-pow-app :path "/home/huga/proj/hoge")))
    (pow-app-set-name-default app)
    (should (equal (pow-app-name app) "default"))))

(ert-deftest pow-app-url ()
  (let ((app (make-pow-app :path "/home/huga/proj/hoge")))
    (should (equal (pow-app-url app) "http://hoge.dev/"))))

(ert-deftest pow-app-url-for-remote ()
  (let ((app (make-pow-app :path "/home/huga/proj/hoge")))
    (mocker-let ((pow-local-ip-address-string
                  ()
                  ((:output "10.0.0.3"))))
      (should (equal (pow-app-url-for-remote app)
                     "http://hoge.10.0.0.3.xip.io/")))))

(ert-deftest pow-app-symlink-path ()
  (let ((app (make-pow-app :path testpath
                           :name "test-app")))
    (should (pow-same-file-p
             (expand-file-name "test-app" pow-symlink-directory)
             (pow-app-symlink-path app)))))

(ert-deftest pow-app-validate-with-no-args ()
  (should-error
   (let ((app (make-pow-app)))
     (pow-app-validate app))))

(ert-deftest pow-app-validate-with-invalid-path ()
  (should-error
   (let ((app (make-pow-app :path "/hoge/huga")))
     (pow-app-validate app))))

(ert-deftest pow-app-validate-with-invalid-name ()
  (should-error
   (let ((app (make-pow-app :path " foo bar ")))
     (pow-app-validate app))))

(ert-deftest pow-app-validate-with-valid-path ()
  (let ((app (make-pow-app :path testpath)))
    (pow-app-validate app)))

(ert-deftest pow-app-save ()
  (let* ((app (make-pow-app :path testpath))
         (symlink-path (expand-file-name
                        (pow-app-name app)
                        (pow-app-symlink-directory app))))
    (pow-app-save app)
    (should (file-symlink-p symlink-path))
    (delete-file symlink-path)))

(ert-deftest pow-app-delete ()
  (let ((app (make-pow-app :path testpath)))
    (pow-app-save app)
    (should (file-symlink-p (pow-app-symlink-path app)))
    (pow-app-delete app)
    (should (not (file-exists-p (pow-app-symlink-path app))))))

(ert-deftest pow-app-rename ()
  (let ((app (make-pow-app :path testpath)))
    (pow-app-save app)
    (pow-app-rename app "emacs-pow-new-name")
    (should (file-symlink-p
             (expand-file-name "emacs-pow-new-name"
                               pow-symlink-directory)))
    (pow-app-delete app)))

(ert-deftest pow-app-restart ()
  (let* ((app (make-pow-app :path testpath))
        (txt (expand-file-name "tmp/restart.txt"
                               (pow-app-path app))))
    (pow-app-restart app)
    (should (file-exists-p txt))))

(ert-deftest pow-app-log-path ()
  (let ((app (make-pow-app :path testpath)))
    (should (equal (pow-app-log-path app)
                  (expand-file-name "~/Library/Logs/Pow/apps/rack-app.log")))))

(ert-deftest pow-app-app-log-path ()
  (let ((app (make-pow-app :path testpath)))
    (should (equal (pow-app-app-log-path app 'development)
                   (expand-file-name "log/development.log"
                                     testpath)))
    (should (equal (pow-app-app-log-path app 'test)
                   (expand-file-name "log/test.log"
                                     testpath)))
    (should (equal (pow-app-app-log-path app 'production)
                   (expand-file-name "log/production.log"
                                     testpath)))))


(ert-run-tests-batch)

(makunbound 'testpath)
