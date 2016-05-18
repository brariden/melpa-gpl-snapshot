;;; snakemake-mode.el --- Major mode for editing Snakemake files  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016 Kyle Meyer

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/snakemake-mode
;; Keywords: tools
;; Version: 0.4.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (magit-popup "2.4.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Snakemake mode provides support for editing Snakemake [1] files.  It
;; builds on Python mode to provide fontification, indentation, and
;; imenu indexing for Snakemake's rule blocks.
;;
;; See also snakemake.el, which is packaged with snakemake-mode.el and
;; provides an interface for running Snakemake commands.
;;
;; If Snakemake mode is installed from MELPA, no additional setup is
;; required.  It will be loaded the first time a file named 'Snakefile'
;; is opened.
;;
;; Otherwise, put snakemake-mode.el in your `load-path' and add
;;
;;     (require 'snakemake-mode)
;;
;; to your initialization file.
;;
;; snakemake-mode.el also includes support for highlighting embedded R
;; code.  See the snakemake-mode-setup-mmm function documentation for how.
;;
;; [1] https://bitbucket.org/snakemake/snakemake/wiki/browse/

;;; Code:

(require 'cl-lib)
(require 'python)


;;; Customization

;;;###autoload
(defgroup snakemake-mode nil
  "Support for Snakemake files"
  :group 'tools)

(defcustom snakemake-mode-hook nil
  "Hook run when entering `snakemake-mode'."
  :type 'hook)

(defcustom snakemake-indent-field-offset 4
  "Offset for field indentation."
  :type 'integer)

(defcustom snakemake-indent-value-offset 4
  "Offset for field values that the line below the field key."
  :type 'integer)


;;; Regexp

(defconst snakemake-rule-or-subworkflow-re
  (rx line-start
      (zero-or-more space)
      (or (and (group symbol-start (or "rule" "subworkflow"))
               " "
               (group (one-or-more (or (syntax word) (syntax symbol))))
               (zero-or-more space)
               ":")
          (and (group symbol-start "rule")
               (zero-or-more space)
               ":")))
  "Regexp matching a rule or subworkflow.")

(defconst snakemake-toplevel-command-re
  (rx line-start
      (zero-or-more space)
      (group (or "configfile"
                 "include"
                 "onerror"
                 "onsuccess"
                 "ruleorder"
                 "workdir"))
      (zero-or-more space) ":")
  "Regexp matching other toplevel commands aside from 'rule'.")

(defconst snakemake-field-key-re
  (rx (group symbol-start
             (or "benchmark"
                 "input"
                 "log"
                 "message"
                 "output"
                 "params"
                 "priority"
                 "resources"
                 "run"
                 "script"
                 "shadow"
                 "shell"
                 "threads"
                 "version"
                 "wrapper"
                 ;; Keys for subworkflow blocks
                 "snakefile"
                 "workdir"))
      (zero-or-more space) ":")
  "Regexp matching a rule or subworkflow field key.")

(defconst snakemake-field-key-indented-re
  (concat  "^\\s-+" snakemake-field-key-re)
  "Regexp matching a field key, including indentation.")

(defconst snakemake-builtin-function-re
  (rx (group symbol-start
             (or "expand" "shell" "protected" "temp" "dynamic" "touch"))
      (zero-or-more space) "(")
  "Regexp matching a call to a builtin Snakemake function.")


;;; Info and navigation

(defun snakemake-block-info ()
  "Return description of rule or subworkflow block at point."
  (save-excursion
    (save-restriction
      (widen)
      (let ((pos (point)))
        (end-of-line)
        (and (re-search-backward snakemake-rule-or-subworkflow-re nil t)
             (let ((type (or (match-string-no-properties 1)
                             "rule"))
                   (name (match-string-no-properties 2))
                   (start (or (match-beginning 1)
                              (match-beginning 3)))
                   (rule-indent (current-indentation))
                   end)
               (beginning-of-line)
               (forward-line)
               (while (and (or (< rule-indent (current-indentation))
                               (looking-at-p "^\\s-*$"))
                           (or (not (eobp))
                               (progn (setq end (point-max))
                                      nil)))
                 (setq end (line-end-position))
                 (forward-line))
               (when (<= start pos end)
                 (list type name start end))))))))

(defun snakemake-beginning-of-block (&optional arg)
  "Move to beginning of rule block.
With ARG, do it that many times.  Negative ARG signals to move
forward rather than backward."
  (when (or (null arg) (= arg 0))
    (setq arg 1))
  (funcall (if (> arg 0)
               #'re-search-backward
             (lambda (&rest args)
               (end-of-line)
               (prog1 (apply #'re-search-forward args)
                 (beginning-of-line))))
           snakemake-rule-or-subworkflow-re
           nil 'move (abs arg))
  (looking-at-p snakemake-rule-or-subworkflow-re))

(defun snakemake-end-of-block ()
  "Move to end of rule or subworkflow block."
  (let ((end (nth 3 (snakemake-block-info))))
    (when end (goto-char end))))

(defun snakemake-beginning-of-defun (&optional arg)
  "Move to beginning of current rule block or function.
With ARG, do it that many times.  Negative ARG signals to move
forward rather than backward."
  (when (or (null arg) (= arg 0))
    (setq arg 1))
  (let ((choose-fn (if (> arg 0) #'max #'min))
        (cands (delq nil
                     (mapcar
                      (lambda (f)
                        (save-excursion (and (funcall f arg) (point))))
                      (list #'snakemake-beginning-of-block
                            #'python-nav-beginning-of-defun)))))
    (cond (cands
           (goto-char (apply choose-fn cands)))
          ((> arg 0)
           (goto-char (point-min))
           nil)
          (t
           (goto-char (point-max))
           nil))))

(defun snakemake-end-of-defun ()
  "Move to end of current rule block or function."
  (or (snakemake-end-of-block)
      (python-nav-end-of-defun)))

(defun snakemake-block-or-defun-name ()
  "Return name of current rule or function.
This function is appropriate to use as the value of
`add-log-current-defun-function'."
  (or (nth 1 (snakemake-block-info))
      (python-info-current-defun)))


;;; Indentation

(defun snakemake--calculate-indentation (&optional previous)
  "Return indentation offset for the current line.

A non-nil value for PREVIOUS indicates that the previous command
was an indentation command.

When Python mode should handle the indentation, a nil value is
returned."
  (when (memq (car (python-indent-context))
              (list :after-line
                    ;; If point is on a value line following a naked
                    ;; field value, `python-indent-context' returns
                    ;; :after-block-start.
                    :after-block-start))
    (let* ((initial-indent (current-indentation))
           (goto-first-p (or (not previous) (= initial-indent 0))))
      (save-excursion
        (save-restriction
          (widen)
          (beginning-of-line)
          (if (or (looking-at-p (concat "^\\s-*" snakemake-field-key-re))
                  (looking-at-p (rx line-start
                                    (zero-or-more space)
                                    (or "\"\"\"" "'''"))))
              (and goto-first-p
                   (let (rule-indent)
                     (while (not (or rule-indent (bobp)))
                       (forward-line -1)
                       (when (looking-at-p snakemake-rule-or-subworkflow-re)
                         (setq rule-indent (current-indentation))))
                     (and rule-indent
                          (+ rule-indent snakemake-indent-field-offset))))
            ;; We need to look back to determine indentation.
            (skip-chars-backward " \t\n")
            (beginning-of-line)
            (cond
             ((cl-some (lambda (re) (looking-at-p (concat re "\\s-*$")))
                       (list snakemake-field-key-indented-re
                             snakemake-rule-or-subworkflow-re
                             snakemake-toplevel-command-re))
              (let ((above-indent (current-indentation)))
                (cond (goto-first-p
                       (+ above-indent snakemake-indent-value-offset))
                      ((< above-indent initial-indent)
                       above-indent))))
             ((looking-at (concat snakemake-field-key-indented-re "\\s-*"))
              (let ((above-indent (current-indentation)))
                (cond (goto-first-p
                       (- (match-end 0) (line-beginning-position)))
                      ((< above-indent initial-indent)
                       above-indent))))
             ((save-excursion
                (let ((above-indent (current-indentation))
                      field-indent)
                  (when (> above-indent 0)
                    (while (and (not (bobp))
                                (or (= above-indent
                                       (setq field-indent (current-indentation)))
                                    (looking-at-p "^\\s-*$")))
                      (forward-line -1)))
                  (and (looking-at
                        (concat snakemake-field-key-indented-re "\\s-*"))
                       (not (equal (match-string-no-properties 1)
                                   "run"))
                       (cond (goto-first-p
                              (- (match-end 0) (line-beginning-position)))
                             ((< field-indent initial-indent)
                              field-indent)))))))))))))

(defun snakemake-indent-line (&optional previous)
  "Snakemake mode variant of `python-indent-line'."
  (let ((follow-indentation-p
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (indent-line-to
       (or (snakemake--calculate-indentation previous)
           (python-indent-calculate-indentation previous))))
    (when follow-indentation-p
      (back-to-indentation))))

(defun snakemake-indent-line-function ()
  "Snakemake mode variant of `python-indent-line-function'."
  (snakemake-indent-line
   (and (memq this-command python-indent-trigger-commands)
        (eq last-command this-command))))


;;; Imenu

(defun snakemake-imenu-create-index ()
  "Create Imenu index for rule blocks.
If `python-imenu-create-index' returns a non-nil value, also
include these results and append a \"(rule)\" to the index
label."
  (let ((py-index (python-imenu-create-index))
        (sm-index (snakemake--imenu-build-rule-index)))
    (if py-index
        (append (mapcar (lambda (x)
                          (cons (concat (car x) " (rule)") (cdr x)))
                        sm-index)
                py-index)
      sm-index)))

(defun snakemake--imenu-build-rule-index ()
  (goto-char (point-min))
  (let (index)
    (while (re-search-forward snakemake-rule-or-subworkflow-re nil t)
      (push (cons (match-string-no-properties 2)
                  (save-excursion (beginning-of-line)
                                  (point-marker)))
            index))
    (nreverse index)))


;;; Embedded language syntax-highlighting

(declare-function mmm-add-classes "mmm-vars")
(declare-function mmm-add-mode-ext-class "mmm-vars")

(defun snakemake-mode-setup-mmm ()
  "Set up MMM mode to highlight embedded R code.

You must have the R-strings either within a R(''' ''') function call or a code
block delimited with '''#r and ''' (triple double-quotes also accepted).

For automatic highlighting of embedded regions, you need to set
`mmm-global-mode` to 'maybe in your setup file."
  (unless (require 'mmm-mode nil t)
    (user-error "You need to install mmm-mode"))

  (when (unless mmm-global-mode)
    (display-warning 'snakemake-mode "To get automatic syntax highlighting of
embedded R, you need to set mmm-global-mode to a non-nil value such as 'maybe."))

  (mmm-add-classes
   '((snakemake-R-call-double
      :submode R-mode
      :front ".*R\(\"\"\""
      :back ".*\"\"\"\)")))

  (mmm-add-classes
   '((snakemake-R-call-regular
      :submode R-mode
      :front ".*R\('''"
      :back ".*'''\)")))

  (mmm-add-classes
   '((snakemake-R-string-double
      :submode R-mode
      :front ".*\"\"\" *# *[rR]"
      :back ".*\"\"\"")))

  (mmm-add-classes
   '((snakemake-R-string-regular
      :submode R-mode
      :front ".*''' *# *[rR]"
      :back ".*'''")))

  (mmm-add-mode-ext-class 'snakemake-mode nil 'snakemake-R-call-double)
  (mmm-add-mode-ext-class 'snakemake-mode nil 'snakemake-R-call-regular)
  (mmm-add-mode-ext-class 'snakemake-mode nil 'snakemake-R-string-double)
  (mmm-add-mode-ext-class 'snakemake-mode nil 'snakemake-R-string-regular))


;;; Mode

(defvar snakemake-font-lock-keywords
  `((,snakemake-rule-or-subworkflow-re
     (1 font-lock-keyword-face nil 'lax)
     (2 font-lock-function-name-face nil 'lax)
     (3 font-lock-keyword-face nil 'lax))
    (,snakemake-toplevel-command-re 1 font-lock-keyword-face)
    (,snakemake-builtin-function-re 1 font-lock-builtin-face)
    (,snakemake-field-key-indented-re 1 font-lock-type-face)))

;;;###autoload
(define-derived-mode snakemake-mode python-mode "Snakemake"
  "Mode for editing Snakemake files."
  (set (make-local-variable 'imenu-create-index-function)
       #'snakemake-imenu-create-index)
  (set (make-local-variable 'indent-line-function) 'snakemake-indent-line-function)
  (set (make-local-variable 'indent-region-function) nil)

  (set (make-local-variable 'beginning-of-defun-function)
       #'snakemake-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       #'snakemake-end-of-defun)
  (set (make-local-variable 'add-log-current-defun-function)
       #'snakemake-block-or-defun-name)

  (set (make-local-variable 'font-lock-defaults)
       `(,(append snakemake-font-lock-keywords python-font-lock-keywords))))

;;;###autoload
(add-to-list 'auto-mode-alist '("Snakefile\\'" . snakemake-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(?:sm\\)?rules\\'" . snakemake-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.snakefile\\'" . snakemake-mode))

(provide 'snakemake-mode)
;;; snakemake-mode.el ends here
