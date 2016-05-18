;;; epkg-list.el --- list Epkg packages           -*- lexical-binding: t -*-

;; Copyright (C) 2016  Jonas Bernoulli

;; This file contains code from GNU Emacs, which is
;; Copyright (C) 1976-2016 Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU GPL see http://www.gnu.org/licenses.

;;; Code:

(require 'epkg)
(require 'epkg-desc)

;;; Options

(defcustom epkg-list-packages-omit-shelved t
  "Whether shelved packages are omitted when listing packages."
  :group 'epkg
  :type 'boolean)

(defcustom epkg-list-columns
  '(("Package"     25 t   nil name    epkg-list-format-name)
    ("Type"        12 t   nil class   nil)
    ("Description" 99 nil nil summary nil))
  "Slots displayed in the package menu.

The value is a list of columns.  Each element has the form
\(HEADER WIDTH SORTP PROPS SLOT FORMAT).  HEADER is the string
displayed in the header.  WIDTH is the width of the column.  If
SORTP is t, then the column can be sorted, if it is nil then it
can not.  PROPS is an alist, supported keys are `:right-align'
and `:pad-right'.  Slot is an Epkg object slot or `type'.  FORMAT
is a function.  It is called with one argument the slot value and
has to return a representation of that.  If FORMAT is nil, then
the value is inserted as-is."
  :group 'epkg
  :type `(repeat
          (list :tag "Column"
                (string  :tag "Header Label")
                (integer :tag "Column Width")
                (choice  :tag "Sort predicate"
                         (const :tag "don't sort" nil)
                         (const :tag "default" t)
                         (function))
                (repeat  :tag "Properties"
                         (list (choice :tag "Property"
                                       (const :right-align)
                                       (const :pad-right)
                                       (symbol))
                               (sexp   :tag "Value")))
                (choice  :tag "Slot symbol" ,@epkg--custom-slot-choices)
                (choice  :tag "Format value"
                         (const :tag "as is" nil)
                         (const epkg-list-format-name)
                         (function)))))

(unless (find-lisp-object-file-name 'epkg-list-mode-hook 'defvar)
  (add-hook 'epkg-list-mode-hook 'hl-line-mode))
(defcustom epkg-list-mode-hook '(hl-line-mode)
  "Hook run after entering Epkg-List mode."
  :group 'epkg
  :type 'hook
  :options '(hl-line-mode))

(defface epkg-list-name
  '((t :inherit link :underline nil))
  "Face used on package names in the package list."
  :group 'epkg)

;;; Commands

;;;###autoload
(defun epkg-list-packages (&optional all)
  "Display a list of packages.

By default shelved packages are omitted from the output.  With a
prefix argument or when `epkg-list-packages-omit-shelved' is nil,
then no packages are omitted."
  (interactive (list current-prefix-arg))
  (epkg--list-packages
   (epkgs (epkg--list-columns-vector)
          (and (not all)
               '(epkg-builtin-package-p
                 epkg-mirrored-package--eieio-childp)))))

;;;###autoload
(defun epkg-list-matching-packages (pattern &optional all)
  "Display a list of packages whose summaries match REGEXP.

By default shelved packages are omitted from the output.  With a
prefix argument or when `epkg-list-packages-omit-shelved' is nil,
then no packages are omitted."
  (interactive (list (read-string "pattern: ") current-prefix-arg))
  (epkg--list-packages
   (epkg-sql [:select $i1 :from packages
              :where (like summary $s2)
              :and class :in $v3]
             (epkg--list-columns-vector)
             (intern (if (string-match-p "%_" pattern)
                         pattern
                       (concat "%" pattern "%")))
             (epkg--list-where-class-in all))))

;;;###autoload
(defun epkg-list-keyworded-packages (keyword &optional all)
  "Display a list of packages that have KEYWORD set.

Only keywords that are members of `finder-known-keywords' are
offered as completion candidates, but you can also enter other
keywords.

By default shelved packages are omitted from the output.  With a
prefix argument or when `epkg-list-packages-omit-shelved' is nil,
then no packages are omitted."
  (interactive (list (intern (completing-read
                              "List packages with keyword: "
                              (progn (require 'finder nil t)
                                     (bound-and-true-p finder-known-keywords))))
                     current-prefix-arg))
  (epkg--list-packages
   (epkg-sql [:select $i1 :from [packages keywords]
              :where (= name package)
              :and (= keyword $s2)
              :and class :in $v3]
             (epkg--list-columns-vector)
             keyword
             (epkg--list-where-class-in all))))

;;;###autoload
(defun epkg-list-packages-by-author (author &optional all)
  "Display a list of packages authored or maintained by AUTHOR.

AUTHOR may be a name or an email address.  Packages whose
Author(s) or Maintainer(s) header keywords contain that author
are listed.

By default shelved packages are omitted from the output.  With a
prefix argument or when `epkg-list-packages-omit-shelved' is nil,
then no packages are omitted."
  (interactive (list (read-string "List packages by author: ")
                     current-prefix-arg))
  (epkg--list-packages
   (let ((email-p (string-match-p "@" author))
         (columns (epkg--list-columns-vector t))
         (classin (epkg--list-where-class-in all)))
     (-union (epkg-sql [:select :distinct $i1 :from [packages authors]
                        :where (= packages:name authors:package)
                        :and (= $i2 $s3)
                        :and class :in $v4]
                       columns (if email-p 'email 'authors:name)
                       author classin)
             (epkg-sql [:select :distinct $i1 :from [packages maintainers]
                        :where (= packages:name maintainers:package)
                        :and (= $i2 $s3)
                        :and class :in $v4]
                       columns (if email-p 'email 'maintainers:name)
                       author classin)))))

;;;###autoload
(defun epkg-list-packages-of-type (type)
  "Display a list of all packages of a certain type.

To list all packages of a certain type as well as its subtypes
use `TYPE*' instead of just `TYPE'."
  (interactive (list (epkg-read-type "List packages of type: " nil t)))
  (epkg--list-packages
   (epkgs (epkg--list-columns-vector)
          (if (eq type 'all*)
              'epkg-package
            (setq type (symbol-name type))
            (intern (if (string-suffix-p "*" type)
                        (format "epkg-%s-package--eieio-childp"
                                (substring type 0 -1))
                      (format "epkg-%s-package" type)))))))

(defun epkg--list-packages (rows)
  (with-current-buffer (get-buffer-create "*Epkgs*")
    (epkg-list-mode)
    (setq tabulated-list-entries
          (mapcar (lambda (row)
                    (list (car row)
                          (vconcat
                           (cl-mapcar (lambda (val col)
                                        (if-let ((pp (nth 5 col)))
                                            (funcall pp val)
                                          (if val (format "%s" val) "")))
                                      row epkg-list-columns))))
                  rows))
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

;;; Mode

(defvar epkg-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\r" 'epkg-list-describe-package)
    map)
  "Local keymap for Epkg-List mode buffers.")

(define-derived-mode epkg-list-mode tabulated-list-mode "Epkgs"
  "Major mode for browsing a list of packages."
  (setq x-stretch-cursor        nil)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "Package" nil))
  (setq tabulated-list-format   (vconcat (--map `(,@(-take 3 it)
                                                  ,@(-flatten (nth 3 it)))
                                                epkg-list-columns)))
  (tabulated-list-init-header))

(defun epkg-list-format-name (name)
  (list name
        'face 'epkg-list-name
        'follow-link t
        'action 'epkg-list-describe-package))

(defun epkg--list-columns-vector (&optional qualify)
  (let ((lst (--map (nth 4 it) epkg-list-columns)))
    (vconcat (if qualify (-replace 'name 'packages:name lst) lst))))

(defun epkg--list-where-class-in (all)
  (closql--where-class-in (epkg-db)
                          (if (or all (not epkg-list-packages-omit-shelved))
                              'epkg-package--eieio-childp
                            '(epkg-builtin-package-p
                              epkg-mirrored-package--eieio-childp))))

(provide 'epkg-list)
;;; epkg-list.el ends here
