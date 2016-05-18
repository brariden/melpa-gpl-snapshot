;;; -*- lexical-binding: t -*-
;;; repeatable-motion.el --- Make repeatable versions of motions

;;; Author: William Hatch <willghatch@gmail.com>
;;; Maintainer: William Hatch <willghatch@gmail.com>
;;; Version: 0.0
;;; Homepage: https://github.com/willghatch/emacs-repeatable-motion
;;; Git-Repository: git://github.com/willghatch/emacs-repeatable-motion.git
;;; Keywords: motion repeatable
;;; Package-Requires: ((emacs "24"))

;;; License:
;;; This is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; <http://www.gnu.org/licenses/>

;;; Commentary:
;;; This package provides the ability to repeat motions and their arguments
;;; with a single key, allowing you to keep less used motions on longer
;;; prefixes.  More documentation is available in the readme, which is
;;; hosted at https://github.com/willghatch/emacs-repeatable-motion

;;; Code:

;; should these be buffer local?  Evil mode doesn't make command repetition buffer
;; local, so for now these won't be either.
(require 'cl-lib)
(defvar repeatable-motion--forward-func (lambda () (interactive) nil))
(defvar repeatable-motion--backward-func (lambda () (interactive) nil))
(defvar repeatable-motion--numeric-arg 1)

(defgroup repeatable-motion nil
  "Group for the repeatable-motion package")

(defcustom repeatable-motion-define-common-motions-p t
  "If non-nil, a bunch of common motion commands will have repeatable
  versions defined when repeatable-motion is loaded, if they are
  available."
  :group 'repeatable-motion)
(defcustom repeatable-motion-only-repeat-with-count nil
  "If non-nil, motions will only be set to repeat if they were provided with
a prefix other than 1.  This makes it behave like repmo.vim.  Why would you
want it to only be when you give a count?  I don't know, but apparently
people use repmo.vim..."
  :group 'repeatable-motion)
(defcustom repeatable-motion-definition-prefix "repeatable-motion-"
  "The prefix that defined motions will have.  Defaults to repeatable-motion-.
If nil, no normal repeatable commands will be defined."
  :group 'repeatable-motion)
(defcustom repeatable-motion-count-needed-prefix nil
  "The prefix that defined motions will have if you want them to need
a non-1 prefix argument.  If nil (the default), these functions will
not be defined.  Commands with these names act like repmo.vim, in that
they are only set to repeat if given with a prefix.  Good for up/down
line commands."
  :group 'repeatable-motion)

(defun repeatable-motion-forward (&optional prefix)
  "Repeat the last repeatable motion used, using the original prefix unless a
new one is given"
  (interactive "p")
  (setq current-prefix-arg (if (equal prefix 1)
                               repeatable-motion--numeric-arg
                             prefix))
  (call-interactively repeatable-motion--forward-func))

(defun repeatable-motion-backward (&optional prefix)
  "Repeat the last repeatable motion used, using the original prefix unless a
new one is given"
  (interactive "p")
  (setq current-prefix-arg (if (equal prefix 1)
                               repeatable-motion--numeric-arg
                             prefix))
  (call-interactively repeatable-motion--backward-func))

(eval-after-load 'evil
  '(progn
     (evil-declare-motion 'repeatable-motion-forward)
     (evil-declare-motion 'repeatable-motion-backward)))

(defun repeatable-motion--set-inclusiveness (inclusive-p)
  (when (featurep 'evil)
    (if inclusive-p
        (progn
          (evil-set-command-property 'repeatable-motion-forward :type 'inclusive)
          (evil-set-command-property 'repeatable-motion-backward :type 'inclusive))
      (progn
        (evil-set-command-property 'repeatable-motion-forward :type 'exclusive)
        (evil-set-command-property 'repeatable-motion-backward :type 'exclusive)))))

(defun repeatable-motion--make-symbol-name (orig-sym only-with-count-p)
  (let ((prefix (if only-with-count-p
                    repeatable-motion-count-needed-prefix
                  repeatable-motion-definition-prefix)))
    (if prefix (intern (concat prefix (symbol-name orig-sym)))
      nil)))

(cl-defun repeatable-motion-define
    (base-motion repeat-motion-reverse
                 &key repeat inclusive)
  "Defines a new repeatable version of a given function, named
'repeatable-motion-<original-name>', which will repeat using the given
repeat and reverse-repeat functions.  If :repeat is given, it will be
used for repeating instead of the base motion given.  If the evil
package is available, motions will be declared to work well with evil,
and definitions where :inclusive is non-nil will cause the motions
to have the inclusive property set for evil.  Defines any versions
of repeatable motions for which prefix names exist (IE versions that
need prefix arguments and versions that don't)."
  (repeatable-motion--define base-motion repeat-motion-reverse
                             :repeat repeat :inclusive inclusive :count-only nil)
  (repeatable-motion--define base-motion repeat-motion-reverse
                             :repeat repeat :inclusive inclusive :count-only t))

(cl-defun repeatable-motion--define
    (base-motion repeat-motion-reverse
                 &key repeat inclusive count-only)
  (let ((name (repeatable-motion--make-symbol-name base-motion count-only))
        (repeat-fwd (if repeat repeat base-motion)))
    (when name
      (fset name
            (lambda (&optional prefix)
              (interactive "p")
              (unless (and count-only
                           (equal prefix 1))
                (setq repeatable-motion--forward-func repeat-fwd)
                (setq repeatable-motion--backward-func repeat-motion-reverse)
                (setq repeatable-motion--numeric-arg prefix))
              (setq current-prefix-arg (list prefix))
              (repeatable-motion--set-inclusiveness inclusive)
              (call-interactively base-motion)))
      (eval-after-load 'evil
        `(progn
           (evil-declare-motion (quote ,name))
           (when ,inclusive
             (evil-add-command-properties (quote ,name) :type 'inclusive)))))))


(cl-defun repeatable-motion-define-pair
    (forward-sym backward-sym &key inclusive inclusive1 inclusive2)
  "Define a pair of repeatable functions that are opposites of each
other.  They will be named repeatable-motion-<original-name>.  Keyword
arguments :both-inclusive, :inclusive1, and :inclusive2 may be
provided, and when they are non-nil, the motions are flagged as inclusive
for evil-mode."
  (repeatable-motion-define forward-sym backward-sym :inclusive (or inclusive inclusive1))
  (repeatable-motion-define backward-sym forward-sym :inclusive (or inclusive inclusive2)))

;; provide some motions
(when repeatable-motion-define-common-motions-p
  (require 'repeatable-motion-common-motions))
(provide 'repeatable-motion)

;;; repeatable-motion.el ends here
