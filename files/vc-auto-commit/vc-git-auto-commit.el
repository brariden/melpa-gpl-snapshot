;;; vc-git-auto-commit.el --- Auto-commit feature for git

;; Copyright (C) 2013-2016 Sylvain Rousseau

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; URL: http://github.com/thisirs/vc-auto-commit.git
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

;;; Commentary

;; This library implements auto-commit feature for git-controlled
;; repositories.

;;; Code:

(defvar vc-git-commit-msg-function 'vc-git-commit-msg
  "Function that returns a commit message.")

(defun vc-git-commit-msg (repo)
  "Return default commit message."
  (with-temp-buffer
    (if (= 0 (vc-git-command t nil nil
                             "diff-index" "--name-status"
                             "HEAD"))
        (concat (current-time-string) "\n\n" (buffer-string))
      "First commit")))

(defun vc-git-auto-commit (repo &optional messagep)
  "Auto-commit repository REPO and asks for a commit message if
MESSAGEP is non-nil."
  (with-temp-buffer
    (let ((default-directory repo))
      ;; changes in submodule are not commitable, so add dirty flag
      (vc-git-command t 0 nil "status" "--porcelain" "--ignore-submodules=dirty")
      (if (zerop (buffer-size (current-buffer)))
          (message "Nothing to commit in repo %s" repo)
        (vc-git-command nil 0 nil "add" "-A" ".")
        (vc-git-command nil 0 nil "commit" "-m"
                        (if messagep
                            (let ((msg (read-string "Commit message: ")))
                              (if (equal msg "")
                                  (funcall vc-git-commit-msg-function repo)
                                msg))
                          (funcall vc-git-commit-msg-function repo)))))))


(provide 'vc-git-auto-commit)

;;; vc-git-auto-commit.el ends here
