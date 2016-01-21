;;; creds.el --- A parser credentials file library (not limited to credentials entries)

;; Copyright (C) 2013
;;   Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.0.6.1
;; Package-Requires: ((dash "2.5.0") (s "1.9.0"))
;; Keywords: credentials
;; URL: https://github.com/ardumont/emacs-creds

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A small authinfo/netrc parsing library to deal with more entries than just the default credentials (as in netrc library)
;;
;; Here is an example of .authinfo
;; machine machine0 port http login nouser password nopass
;; machine machine1 login some-login password some-pwd port 993
;; machine machine2 login some-login port 587 password some-pwd
;; machine jabber   login some-login password some-pwd
;; machine description name "my name is" blog some-blog mail some-mail
;;
;; Read the content of the file and return an alist:
;; (creds/read-lines "~/.authinfo")
;; > (("machine" "machine0" "port" "http" "login" "nouser" "password" "nopass")
;;    ("machine" "machine1" "login" "some-login" "password" "some-pwd" "port" "993")
;;    ("machine" "machine2" "login" "some-login" "port" "587" "password" "some-pwd")
;;    ("machine" "jabber" "login" "some-login" "password" "some-pwd")
;;    ("machine" "description" "name" "\"my name is\"" "blog" "some-blog" "mail" "some-mail"))

;; To retrieve the machine entry:
;; (creds/get data "machine1")
;; > ("machine" "machine1" "login" "some-login" "password" "some-pwd" "port" "993")

;; To retrieve the machine entry "machine2" with login "some-login"
;; (creds/get-with data '(("machine" . "machine2") ("login" . "some-login")))
;; > ("machine" "machine2" "login" "some-login" "port" "587" "password" "some-pwd")

;; To retrieve the value from the key in an entry line
;; (creds/get-entry '("machine" "machine2" "login" "some-login" "port" "587" "password" "some-pwd") "login")
;; > "some-login"

;;; Code:

(require 'dash)
(require 's)

(defvar *creds/protection-string-against-blank-char* "@#$~!!~$#@" "A string to replace blank space.")

(defun creds/--protect-blank-spaced-words (s)
  "Protect the string S by removing blank space and \"."
  (replace-regexp-in-string " " *creds/protection-string-against-blank-char* s))

(defun creds/--unprotect-blank-spaced-words (s)
  "Unprotectd the string S by replacing the protected characters by blank space."
  (->> s
    (replace-regexp-in-string *creds/protection-string-against-blank-char* " ")
    (replace-regexp-in-string "\"" "")))

(defun creds/--read-and-protect-content-file (filepath)
  "Given a file FILEPATH, return the contents of such file with potential blank spaced word protected."
  (with-temp-buffer
    (insert-file-contents filepath)
    (goto-char (point-min))
    (while (re-search-forward "\"[^\"]*\"" nil t)
      (let ((string-to-replace (-> (match-string-no-properties 0) creds/--protect-blank-spaced-words)))
        (replace-match string-to-replace nil t)))
    (buffer-string)))

(defun creds/read-lines (filepath)
  "Return a list of lines from a file FILEPATH."
  (->> filepath
    creds/--read-and-protect-content-file
    s-lines
    (--map (let ((protected-words (s-split " " it)))
             (mapcar #'creds/--unprotect-blank-spaced-words protected-words)))))

(defun creds/get-with (data key-value-pairs)
  "Return the DATA list for the list KEY-VALUE-PAIRS of associations."
  (when data
    (let ((entry-line-as-alist (-partition 2 (car data))))
      (if (->> key-value-pairs
            (mapcar (lambda (key-value)
                      (let ((key   (car key-value))
                            (value (cdr key-value)))
                        (string= value (car (assoc-default key entry-line-as-alist))))))
            (--all? (equal t it)))
          (--mapcat it entry-line-as-alist)
        (creds/get-with (cdr data) key-value-pairs)))))

(defun creds/get (data entry-name)
  "Return the DATA list for the line ENTRY-NAME."
  (creds/get-with data `(("machine" . ,entry-name))))

(defun creds/get-entry (data entry-key)
  "Given a DATA list, return the ENTRY-KEY value in that list."
  (when data
    (let ((data-as-alist (-partition 2 data)))
      (-when-let (value (assoc-default entry-key data-as-alist))
        (car value)))))

(provide 'creds)

;;; creds.el ends here
