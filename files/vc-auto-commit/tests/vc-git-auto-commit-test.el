;;; vc-git-auto-commit-test.el ---

;; Copyright (C) 2012-2016 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; URL: https://github.com/thisirs/vc-auto-commit.git
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar vc-auto-commit-test-repository-list ()
  "List of repositories used in the tests.")

(defun vc-auto-commit-test-shell-commands (&rest commands)
  "Execute the set of COMMANDS with `shell-command'."
  (declare (indent nil))
  (mapc (lambda (command)
          (shell-command command))
        commands))

(defmacro with-empty-repository (prefix &rest body)
  "Create an empty repository in a temporary directory prefixed
by PREFIX and execute BODY in it. Return the path of the newly
created repository to allow easy cloning."
  (declare (indent 1))
  `(let ((default-directory
           (file-name-as-directory (make-temp-file ,prefix :dir))))
     (add-to-list 'vc-auto-commit-test-repository-list default-directory)
     (shell-command "git init .")
     ,@body
     default-directory))

(defmacro with-delete-repository (&rest body)
  "Execute BODY and delete all the repositories created during
the process."
  `(let (vc-auto-commit-test-repository-list)
     (unwind-protect
         (progn ,@body)
       (mapc (lambda (dir)
               (ignore-errors (delete-directory dir :recursive)))
             vc-auto-commit-test-repository-list))))

(ert-deftest vc-auto-commit-is-auto-committed-local ()
  "Test if repository is marked as an auto-committed one."
  (with-delete-repository
   (let ((repo (with-empty-repository "check-local"
                 (shell-command "touch blah-local")
                 (with-temp-buffer
                   (princ '((nil (vc-auto-commit . t)))
                          (current-buffer))
                   (write-file ".dir-locals.el"))
                 (find-file "blah-local"))))
     (with-current-buffer "blah-local"
       (should (local-variable-p 'vc-auto-commit))
       (should vc-auto-commit))
     (should (assoc repo (vc-auto-commit--get-repositories))))))

(ert-deftest vc-auto-commit-is-auto-committed-global ()
  "Test if repository is marked as an auto-committed one."
  (with-delete-repository
   (let ((repo (with-empty-repository "check-global"
                 (shell-command "touch blah-global")
                 (find-file "blah-global"))))
     (setq vc-auto-commit-repository nil)
     (should-not (vc-auto-commit--get-repositories))
     (setq vc-auto-commit-repository (list repo))
     (should (assoc-default repo (vc-auto-commit--get-repositories)
                            'string-match)))))

;;; vc-git-auto-commit-test.el ends here
