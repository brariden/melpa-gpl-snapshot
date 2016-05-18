;;; mallard-mode.el --- Major mode for editing Mallard files

;; Copyright (C) 2013 Jaromir Hradilek

;; Author: Jaromir Hradilek <jhradilek@gmail.com>
;; URL: https://github.com/jhradilek/emacs-mallard-mode
;; Version: 0.3.0
;; Keywords: XML Mallard

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing Mallard files.

;;; Code:

(require 'nxml-mode)

(defconst mallard-mode-version "0.3.0"
  "The version of mallard-mode.")

(defgroup mallard nil
  "The customization group for mallard-mode."
  :group 'languages
  :prefix "mallard-mode-")

(defcustom mallard-mode-comments-buffer "*mallard-comments*"
  "The name of the buffer for editorial comments in a Mallard document."
  :group 'mallard
  :type 'string)

(defcustom mallard-mode-comments-command '("yelp-check" "comments")
  "The command to display editorial comments in a Mallard document."
  :group 'mallard
  :type '(list (string :tag "Command")
               (string :tag "Arguments")))

(defcustom mallard-mode-hrefs-buffer "*mallard-hrefs*"
  "The name of the buffer for broken external links in a Mallard document."
  :group 'mallard
  :type 'string)

(defcustom mallard-mode-hrefs-command '("yelp-check" "hrefs")
  "The command to display broken external links in a Mallard document."
  :group 'mallard
  :type '(list (string :tag "Command")
               (string :tag "Arguments")))

(defcustom mallard-mode-status-buffer "*mallard-status*"
  "The name of the buffer for the status of a Mallard document."
  :group 'mallard
  :type 'string)

(defcustom mallard-mode-status-command '("yelp-check" "status")
  "The command to display the status of a Mallard document."
  :group 'mallard
  :type '(list (string :tag "Command")
               (string :tag "Arguments")))

(defcustom mallard-mode-validate-buffer "*mallard-validate*"
  "The name of the buffer for validation errors in a Mallard document."
  :group 'mallard
  :type 'string)

(defcustom mallard-mode-validate-command '("yelp-check" "validate")
  "The command to validate a Mallard document."
  :group 'mallard
  :type '(list (string :tag "Command")
               (string :tag "Arguments")))

(defvar mallard-directory
  (file-name-directory load-file-name)
  "The main directory of mallard-mode.")

(defvar mallard-schemas
  (expand-file-name "schema/schemas.xml" mallard-directory)
  "The location of the schema locating file for Mallard.")

(defvar mallard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map nxml-mode-map)
    (define-key map "\C-c\C-c" 'mallard-comments)
    (define-key map "\C-c\C-h" 'mallard-hrefs)
    (define-key map "\C-c\C-s" 'mallard-status)
    (define-key map "\C-c\C-v" 'mallard-validate)
    map)
  "Keymap for mallard-mode.
All commands in `nxml-mode-map' are inherited by this map.")

(defun mallard-buffer-saved-p (&optional buffer)
  "Return t if BUFFER does not contain any unsaved changes.
When BUFFER is not specified or is nil, use the current buffer."
  (not (buffer-modified-p (or buffer (current-buffer)))))

(defun mallard-executable-exists-p (executable)
  "Return t if EXECUTABLE is available in the system."
  (when (executable-find executable) t))

(defun mallard-interactive-buffer-saved-p (&optional buffer)
  "Return t if BUFFER does not contain any unsaved changes.
If it does, interactively prompt the user to save it.
When BUFFER is not specified or is nil, use the current buffer."
  (or (mallard-buffer-saved-p buffer)
      (cond ((string= (read-string "The buffer must be saved. Save now? (y/n) ") "y")
             (save-buffer) t)
            (t (message "Aborted.") nil))))

(defun mallard-verbose-executable-exists-p (executable)
  "Return t if EXECUTABLE is available in the system.
If it is not, display an error and return nil."
  (or (mallard-executable-exists-p executable)
      (progn
        (message "Executable file `%s' is not installed." executable) nil)))

(defun mallard-run-command-on-buffer (command &optional buffer)
  "Ensure that BUFFER is saved and execute shell command COMMAND on it.
Return the output of COMMAND as a string.
When BUFFER is not specified or is nil, use the current buffer."
  (when (and (mallard-interactive-buffer-saved-p buffer)
             (mallard-verbose-executable-exists-p (car command)))
    (shell-command-to-string
     (mapconcat 'identity
                (append command (list (buffer-file-name buffer))) " "))))

(defun mallard-comments ()
  "Display editorial comments in the currently edited Mallard document."
  (interactive)
  (message "Searching for editorial comments in the current buffer...")
  (let ((output (mallard-run-command-on-buffer mallard-mode-comments-command)))
    (unless (null output)
      (if (zerop (length output))
          (message "No editorial comments found.")
        (display-message-or-buffer output mallard-mode-comments-buffer)))))

(defun mallard-hrefs ()
  "Display broken external links in the currently edited Mallard document."
  (interactive)
  (message "Checking external links in the current buffer...")
  (let ((output (mallard-run-command-on-buffer mallard-mode-hrefs-command)))
    (unless (null output)
      (if (zerop (length output))
          (message "No broken external links found.")
        (display-message-or-buffer output mallard-mode-hrefs-buffer)))))

(defun mallard-status ()
  "Display the status of the currently edited Mallard document."
  (interactive)
  (message "Checking the status of the current buffer...")
  (let ((output (mallard-run-command-on-buffer mallard-mode-status-command)))
    (unless (and (null output) (zerop (length output)))
      (display-message-or-buffer output mallard-mode-status-buffer))))

(defun mallard-validate ()
  "Validate the currently edited Mallard document."
  (interactive)
  (message "Validating the current buffer...")
  (let ((output (mallard-run-command-on-buffer mallard-mode-validate-command)))
    (unless (null output)
      (if (zerop (length output))
          (message "No validation errors found.")
        (display-message-or-buffer output mallard-mode-validate-buffer)))))

(defun mallard-version ()
  "Display the current version of mallard-mode in the minibuffer."
  (interactive)
  (message "mallard-mode %s" mallard-mode-version))

;;;###autoload
(define-derived-mode mallard-mode nxml-mode "Mallard"
  "A major mode for editing Mallard files.

To start mallard-mode, either open a file with the `.page' or `.page.stub'
file extension, or run `M-x mallard-mode' to enable it for the current
buffer. Once enabled, mallard-mode loads the RELAX NG schema for Mallard,
sets appropriate indentation rules, and enables automatic line wrapping.

mallard-mode inherits commands and key bindings from `nxml-mode'.
In addition, it defines a number of commands and key bindings that integrate
it with the `yelp-check' utility in order to provide maximum comfort when
editing Mallard pages. These commands are as follows:

\\[mallard-comments] or `M-x mallard-comments' displays editorial comments.
\\[mallard-hrefs] or `M-x mallard-hrefs' displays broken external links.
\\[mallard-status] or `M-x mallard-status' displays the current status.
\\[mallard-validate] or `M-x mallard-validate' validates the current buffer.
`M-x mallard-version' displays the version of this major mode.

You can customize the behavior of some of these commands by running the
`M-x customize-mode' command."
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default fill-column 80)
  (turn-on-auto-fill))

;;;###autoload
(progn (add-to-list 'auto-mode-alist '("\\.page\\'" . mallard-mode))
       (add-to-list 'auto-mode-alist '("\\.page\\.stub\\'" . mallard-mode)))

(eval-after-load 'rng-loc
  '(add-to-list 'rng-schema-locating-files mallard-schemas))

(provide 'mallard-mode)

;;; mallard-mode.el ends here
