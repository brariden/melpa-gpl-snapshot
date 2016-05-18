;;; company-dict.el --- A backend that emulates ac-source-dictionary
;;
;; Copyright (C) 2015-16 Henrik Lissner

;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: June 21, 2015
;; Modified: February 22, 2016
;; Version: 1.2.1
;; Keywords: company dictionary ac-source-dictionary
;; Homepage: https://github.com/hlissner/emacs-company-dict
;; Package-Requires: ((company "0.8.12"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(require 'company)

(defcustom company-dict-dir (concat user-emacs-directory "dict/")
  "Directory to look for dictionary files."
  :group 'company
  :type 'directory)

(defcustom company-dict-minor-mode-list '()
  "A list of minor modes to be aware of when looking up dictionaries (if they're active)."
  :group 'company
  :type '(repeat symbol))

(defvar company-dict-alist '()
  "A lookup alist that maps major (or minor) modes to lists of completion candidates.")

(defun company-dict--read-file (file-path)
  (decode-coding-string
   (with-temp-buffer
     (set-buffer-multibyte nil)
     (setq buffer-file-coding-system 'binary)
     (insert-file-contents-literally file-path)
     (buffer-substring-no-properties (point-min) (point-max))) 'utf-8))

(defun company-dict--get-relevant-dicts ()
  (let ((dicts (append (cdr-safe (assq 'all company-dict-alist))
                       (cdr-safe (assq major-mode company-dict-alist)))))
    (mapc (lambda (mode)
            (when (and (boundp mode) (symbol-value mode))
              (setq dicts (append dicts (cdr (assq mode company-dict-alist))))))
          company-dict-minor-mode-list)
    dicts))

(defun company-dict--init (mode)
  "Read dict files and populate dictionary."
  (let ((file (expand-file-name (symbol-name mode) company-dict-dir)))
    (when (and (not (assq mode company-dict-alist))
               (file-exists-p file))
      (add-to-list 'company-dict-alist
                   (cons mode (split-string (company-dict--read-file file) "\n" nil))))))

;;;###autoload
(defun company-dict (command &optional arg &rest ignored)
  "`company-mode' backend for user-provided dictionaries. Dictionary files are lazy
loaded."
  (interactive (list 'interactive))
  (company-dict--init 'all)
  (company-dict--init major-mode)
  (let ((dict (company-dict--get-relevant-dicts)))
    (cl-case command
      (interactive (company-begin-backend 'company-dict))
      (prefix (and dict (or (company-grab-symbol) 'stop)))
      (candidates (let ((completion-ignore-case nil)
                        (symbols dict))
                    (all-completions arg symbols)))
      (sorted t))))

(provide 'company-dict)
;;; company-dict.el ends here
