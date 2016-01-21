;;; seeing-is-believing.el --- minor mode for running the seeing-is-believing ruby gem

;; Copyright 2015 John Cinnamond

;; Author: John Cinnamond
;; Version: 1.0.0

;;; Commentary:
;;
;; See https://github.com/joshcheek/seeing_is_believing for more information
;; about what the gem does. Not that you must install the gem before you can
;; use this mode (gem install seeing_is_believing).
;;
;; Once this mode is enabled you can use the following key bindings:
;;   <prefix> s    run seeing is believing with default settings
;;   <prefix> x    run prefix args with xmpfilter compatability
;;   <prefix> c    clear seeing is believing output from a file
;;
;; The default prefix is "C-c ?"

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(defgroup seeing-is-believing nil
  "Seeing is believing minor mode."
  :group 'tools)

(defcustom seeing-is-believing-executable
  "seeing_is_believing"
  "Name of the seeing_is_believing executable."
  :type 'string
  :group 'seeing-is-believing)

(defcustom seeing-is-believing-prefix
  "C-c ?"
  "The prefix for key bindings for running seeing-is-believing commands."
  :type 'string
  :group 'seeing-is-believing)

(defvar seeing-is-believing-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd (concat seeing-is-believing-prefix " s")) 'seeing-is-believing-run)
    (define-key map (kbd (concat seeing-is-believing-prefix " x")) 'seeing-is-believing-run-as-xmpfilter)
    (define-key map (kbd (concat seeing-is-believing-prefix " c")) 'seeing-is-believing-clear)
    map)
  "Keymap used for seeing-is-believing minor mode.")

(defun seeing-is-believing-run (&optional flags)
  "Run seeing_is_believing on the currently selected buffer or region.

Optional FLAGS are passed to the seeing_is_believing command."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max)))
        (origin (point)))
    (shell-command-on-region beg end (concat seeing-is-believing-executable " " flags) nil 'replace)
    (goto-char origin)))

(defun seeing-is-believing-run-as-xmpfilter ()
  "Run seeing_is_believing with -x to replicate the behaviour of xmpfilter."
  (interactive)
  (seeing-is-believing-run "-x"))

(defun seeing-is-believing-clear ()
  "Clear any output from seeing_is_believing from the buffer or region."
  (interactive)
  (seeing-is-believing-run "-c"))

(define-minor-mode seeing-is-believing
  "Toggle seeing-is-believing minor mode.
Seeing is believing is a ruby gem to display the results of evaluating
each line ruby code inline as a comment. This mode provides some
functions and keybindings to run it.

The following keybindings are created:
  <prefix> s    run seeing is believing with default settings
  <prefix> x    run prefix args with xmpfilter compatability
  <prefix> c    clear seeing is believing output from a file

The default prefix is \"C-c ?\"
"
  nil ; initially disabled
  " Seeing-is-believing"
  seeing-is-believing-keymap
  :group 'seeing-is-believing)

(provide 'seeing-is-believing)
;;; seeing-is-believing-mode.el ends here
