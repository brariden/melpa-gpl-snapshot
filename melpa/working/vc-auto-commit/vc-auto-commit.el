;;; vc-auto-commit.el --- Auto-committing feature for your repository

;; Copyright (C) 2012-2016 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; URL: http://github.com/thisirs/vc-auto-commit.git
;; Version: 0.1
;; Keywords: vc, convenience

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

;; This package allows you to automatically commit all the changes of
;; a repository. It is useful when you have a project that needs to be
;; put under a version control system but you don't want to write
;; any commit message.

;;; Installation:

;; Just put the following in your `.emacs`:

;; (require 'vc-auto-commit)

;; If you want to auto-commit all repositories when quitting emacs,
;; add this:

;; (vc-auto-commit-activate)

;;; Code:

(require 'vc)
(require 'vc-git-auto-commit)

(defvar vc-auto-commit-repository ()
  "List of auto-committed repository.")

;;;###autoload
(progn
  (defvar vc-auto-commit nil
    "Buffer local variable that controls whether corresponding file
is in a auto-committed repository.")
  (make-variable-buffer-local 'vc-auto-commit)
  (put 'vc-auto-commit 'safe-local-variable 'booleanp))

(defun vc-auto-commit--responsible-backend (buffer)
  "Return (ROOT . BACKEND) if the file visited by BUFFER is under
a version controlled system. Otherwise, return nil."
  (condition-case nil
      (with-current-buffer buffer
        (let* ((backend (vc-responsible-backend buffer-file-name))
               (root (vc-call-backend backend 'root default-directory)))
          (and backend root (cons root backend))))
    (error)))

(defun vc-auto-commit--get-repositories ()
  "Return repositories marked for auto-committing as a list of
conses of the form (ROOT . BACKEND) where ROOT is the path of a
repository and BACKEND its backend."
  (let (result)
    (dolist (buffer (buffer-list) result)
      (let ((root+backend (vc-auto-commit-backend buffer)))
        (when (and root+backend (not (assoc (car root+backend) result)))
          (push root+backend result))))
    result))

;;;###autoload
(defun vc-auto-commit (repository &optional arg)
  "Commit all changes of REPOSITORY. If used with
\\[prefix-argument], the user is asked for a commit message.
Otherwise, `vc-<BACKEND>-commit-msg-function' is called to
generate a commit message."
  (interactive (list default-directory current-prefix-arg))
  (let ((backend (vc-responsible-backend repository)))
    (unless backend
      (error "No backend found!"))
    (let ((commit-function
           (intern (concat "vc-" (downcase (symbol-name backend))
                           "-auto-commit"))))
      (if (not (fboundp commit-function))
          (error "Sorry, auto-committing is not implemented for %s" backend)
        (message "Auto-committing repository %s..." repository)
        (funcall commit-function repository arg)
        (message "Auto-committing repository %s...done" repository)))))

;;;###autoload
(defun vc-auto-commit-all ()
  "Auto-commit all repositories marked for auto-committing."
  (let ((repos (vc-auto-commit--get-repositories))
        repo)
    (while (and
            (setq repo (pop repos))
            (condition-case err
                (or (vc-auto-commit (car repo)) t)
              (error (yes-or-no-p
                      (format "An error occurred when auto-committing repo %s: %s; Exit anyway?"
                              (car repo) err))))))
    ;; return nil if repo not nil ie an error occurred and answer is no
    (not repo)))

(defun vc-auto-commit-backend (&optional buffer)
  "Return (ROOT . BACKEND) if BUFFER is under a version
controlled system and marked for auto-committing. If not, return
nil."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((root+backend (vc-auto-commit--responsible-backend buffer)))
    (if (and root+backend
             (or
              (and (local-variable-p 'vc-auto-commit buffer)
                   (buffer-local-value 'vc-auto-commit buffer))
              (member (abbreviate-file-name (car root+backend))
                      (mapcar #'abbreviate-file-name
                              vc-auto-commit-repository))))
        root+backend)))

;;;###autoload
(defun vc-auto-commit-activate (&optional arg)
  (interactive "P")
  (if (< (prefix-numeric-value arg) 0)
      (progn
        (remove-hook 'kill-emacs-query-functions 'vc-auto-commit-all)
        (message "Auto-committing disabled"))
    (add-hook 'kill-emacs-query-functions 'vc-auto-commit-all t)
    (message "Auto-committing enabled")))

(provide 'vc-auto-commit)

;;; vc-auto-commit.el ends here
