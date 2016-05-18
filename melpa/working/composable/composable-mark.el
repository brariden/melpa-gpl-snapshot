;;; composable.mark.el --- composable editing -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Simon Friis Vindum

;; Author: Simon Friis Vindum <simon@vindum.io>
;; Keywords: lisp
;; Version: 0.0.1

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

;; Marking commands useful with composable.


;;; Code:

(defun composable-mark-line (arg)
  "Mark ARG lines."
  (interactive "p")
  (beginning-of-line)
  (push-mark
   (save-excursion
     (when (region-active-p)
       (goto-char (mark)))
     (forward-line arg)
     (point))
   nil t))

(defun composable-mark-join (arg)
  "Mark the whitespace seperating lines.
Between the line above if ARG is negative otherwise below."
  (interactive "p")
  (forward-line arg)
  (cl-flet ((move (dir)
                  (funcall (if (< 0 dir)
                               'skip-chars-forward
                             'skip-chars-backward)
                           "[:space:]\n")))
    (when (< arg 0) (end-of-line))
    (move arg)
    (push-mark nil nil t)
    (move (- arg))))

(defun composable--mark-with-forward (forward arg)
  "Mark a region based on a FORWARD movement and ARG.
The movement must move backwards with negative arguments."
  (let* ((amount (if arg (prefix-numeric-value arg)
                  (if (< (mark) (point)) -1 1)))
         (dir (/ amount (abs amount))))
    (when (not (region-active-p))
      (funcall forward dir)
      (funcall forward (- dir)))
    (push-mark
     (save-excursion
       (when (region-active-p)
         (goto-char (mark)))
       (funcall forward amount)
       (point))
     nil t)))

(defun composable-mark-word (arg)
  "Mark ARG words.
Supports negative arguments and repeating."
  (interactive "P")
  (composable--mark-with-forward 'forward-word arg))

(defun composable-mark-symbol (arg)
  "Mark ARG symbols.
Supports negative arguments and repeating."
  (interactive "P")
  (composable--mark-with-forward 'forward-symbol arg))

(provide 'composable-mark)

;;; composable-mark.el ends here
