;;; tern-django.el --- Create tern projects for django applications.

;; Copyright (C) 2014-2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/tern-django
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (tern "0.0.1") (f "0.17.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'python)
(require 'f)

(defgroup tern-django nil
  "Create tern projects for django applications."
  :group 'programming)

(defcustom tern-django-debug nil
  "Run tern_django.py script with debug enabled."
  :group 'tern-django
  :type 'boolean)

(defvar tern-django-directory (file-name-directory load-file-name)
  "Directory contain `tern-django' package.")

(defvar tern-django-script "tern_django.py"
  "Script path to read django settings.")

(defvar tern-django-process nil
  "Currently running `tern-django' process.")

(defvar tern-django-buffer "*tern-django*"
  "Buffer for `tern-django' process output.")

(defun tern-django-p ()
  "Return t if script run inside django environment."
  (stringp (getenv "DJANGO_SETTINGS_MODULE")))

(defun tern-django-python ()
  "Detect python executable."
  (let ((python (if (eq system-type 'windows-nt) "pythonw" "python"))
        (bin (if (eq system-type 'windows-nt) "Scripts" "bin")))
    (--if-let python-shell-virtualenv-path
        (f-join it bin python)
      python)))

(defun tern-django-args ()
  "Build `tern-django' script options."
  (let (options)
    (when tern-django-debug
      (push "--debug" options))
    (push tern-django-script options)
    options))

(defun tern-django-running-p ()
  "Check if `tern-django' process is running."
  (and tern-django-process
       (process-live-p tern-django-process)))

(defun tern-django-bootstrap ()
  "Start `tern-django' python script."
  (when (tern-django-p)
    (let ((default-directory tern-django-directory))
      (with-current-buffer
          (get-buffer-create tern-django-buffer)
        (erase-buffer))
      (setq tern-django-process
            (apply 'start-process
                   "tern-django"
                   tern-django-buffer
                   (tern-django-python)
                   (tern-django-args)))
      (pop-to-buffer tern-django-buffer))))

(defun tern-django-terminate ()
  "Terminate `tern-django' python script."
  (when (tern-django-running-p)
    (set-process-query-on-exit-flag tern-django-process nil)
    (kill-process tern-django-process))
  (when (get-buffer tern-django-buffer)
    (kill-buffer tern-django-buffer))
  (setq tern-django-process nil))

;;;###autoload
(defun tern-django ()
  "Create tern projects for django applications."
  (interactive)
  (unless (tern-django-running-p)
    (tern-django-bootstrap)))

(provide 'tern-django)

;;; tern-django.el ends here
