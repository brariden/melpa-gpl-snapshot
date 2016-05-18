;;; evil-lispy.el --- precision Lisp editing with Evil and Lispy

;; Copyright (C) 2015 Brandon Carrell

;; Author: Brandon Carrell <brandoncarrell@gmail.com>, Mika Vilpas <mika.vilpas@gmail.com>
;; URL: https://github.com/sp3ctum/evil-lispy
;; Version: 0.0.1
;; Keywords: lisp
;; Package-Requires: ((lispy "0.26.0") (evil "1.2.12"))


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; evil-lispy defines a minor mode and an additional Evil state for editing
;; Lisp code.  The goal is to encourage a workflow where you can hop between
;; Lispy State for making structured edits using Lispy bindings and the rest
;; of the standard Evil states for general editing.

;;; Code:

(require 'evil)
(require 'lispy)

(put 'evil-define-state 'lisp-indent-function 'defun)

;; ——— Customization ———————————————————————————————————————————————————————————

(defgroup evil-lispy nil
  "Evil integration with Lispy."
  :group 'lispy)

(defcustom evil-lispy-cursor '("lawn green" box)
  "the cursor used when in evil-lispy-mode")

;; ——— State ———————————————————————————————————————————————————————————————————

;;;###autoload
(evil-define-state lispy
  "An evil state for Lispy, a precision editing mode for Lisp."
  :tag "<L>"
  :message "Entering evil-lispy state. Press ESC to get out of lispy."
  :cursor evil-lispy-cursor
  :entry-hook (evil-lispy-state-entry)
  :exit-hook (evil-lispy-state-exit)
  nil)

(defun evil-lispy-state-entry ()
  (remove-hook 'activate-mark-hook #'evil-visual-activate-hook t)
  (lispy-mode 1))

(defun evil-lispy-state-exit ()
  (when (region-active-p) (deactivate-mark))
  (add-hook 'activate-mark-hook #'evil-visual-activate-hook nil t)
  (lispy-mode -1))

(defun evil-lispy-enter-state (direction extra-direction)
  "Return a lambda which enters Lispy state at the DIRECTION side of
the current form.  DIRECTION must be either 'left or 'right."
  (let ((f (intern (concat "lispy-" (symbol-name direction))))
        (g (intern (concat "lispy-" (symbol-name extra-direction)))))
    `(lambda ()
       (interactive)
       (when (looking-at lispy-left) (forward-char))
       (let ((pos (point)))
         (,f 1)
         (when (eq (point) pos) (,g 1)))
       (evil-lispy-state))))

(fset 'evil-lispy-enter-state-left (evil-lispy-enter-state 'left 'backward))
(fset 'evil-lispy-enter-state-right (evil-lispy-enter-state 'right 'forward))

(defun evil-lispy-enter-marked-state ()
  "Enters `lispy-state' with the current symbol under point marked."
  (interactive)
  (evil-lispy-state)
  (lispy-mark-symbol))

(defun evil-lispy-enter-visual-state ()
  "If we're in visual state, enter `lispy-state' with the current region
selected."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end))
        (pos (point)))
    (evil-lispy-state)
    (set-mark (if (eq pos start) end start))))

(defun evil-lispy-enter-insert-state (direction extra-direction)
  "Return a lambda which enters Insert state at the DIRECTION side of
the current form.  DIRECTION must be either 'left or 'right."
  `(lambda ()
     (interactive)
     (funcall (evil-lispy-enter-state ',direction ',extra-direction))
     (evil-insert-state)
     (cond
      ((eq ',direction 'left)
       (forward-char)
       (unless (looking-at "\s")
         (insert ?\s)
         (backward-char)))
      ((eq ',direction 'right)
       (backward-char)
       (unless (looking-back "\s")
         (insert ?\s))))))

(fset 'evil-lispy-enter-insert-state-left
      (evil-lispy-enter-insert-state 'left 'backward))
(fset 'evil-lispy-enter-insert-state-right
      (evil-lispy-enter-insert-state 'right 'forward))

(defmacro evil-lispy-defnonstring-action (function-name
                                          action
                                          &rest args-to-action)
  "Define a function that will insert the pressed key in comments and strings,
or call ACTION (a function) otherwise, with ARGS-TO-ACTION."
  (declare (indent 1))
  `(defun ,function-name (arg)
     (interactive "p")
     (if (lispy--in-string-or-comment-p)
         (self-insert-command arg)
       (apply (quote ,action) ,args-to-action))))

(evil-lispy-defnonstring-action evil-lispy-insert-to-lispy-right
  evil-lispy-enter-state-right)
(evil-lispy-defnonstring-action evil-lispy-insert-to-lispy-left
  evil-lispy-enter-state-left)

;; ——— Mode ————————————————————————————————————————————————————————————————————

(defvar evil-lispy-mode-map (make-sparse-keymap))

(define-minor-mode evil-lispy-mode
  "A minor mode for integrating Evil and Lispy."
  :lighter " evil-lispy"
  :keymap evil-lispy-mode-map
  :after-hook (evil-normal-state))

;; ——— Operations ——————————————————————————————————————————————————————————————

(defun evil-lispy-describe ()
  (interactive)
  (save-excursion
    (lispy-mark-symbol)
    (lispy-describe-inline)))

;; ——— Keys ————————————————————————————————————————————————————————————————————

(define-key evil-lispy-state-map [escape] 'evil-normal-state)

;; ——— Entering state ——————————————————
(evil-define-key 'normal evil-lispy-mode-map
  "(" #'evil-lispy-enter-state-left
  ")" #'evil-lispy-enter-state-right
  "mv" #'evil-lispy-enter-marked-state
  "<i" #'evil-lispy-enter-insert-state-left
  ">A" #'evil-lispy-enter-insert-state-right)

(evil-define-key 'visual evil-lispy-mode-map
  (kbd "RET") #'evil-lispy-enter-visual-state)

;; ——— Editing operations ——————————————
(evil-define-key 'normal evil-lispy-mode-map
  "K" #'evil-lispy-describe
  (kbd "M-k") #'lispy-kill-sentence
  (kbd "C-1") #'evil-lispy-describe
  (kbd "C-2") #'lispy-arglist-inline)

;; ——— Insert operations ———————————————
(evil-define-key 'insert evil-lispy-mode-map
  "(" #'lispy-parens

  "[" #'lispy-brackets
  "}" #'lispy-brackets

  "{" #'lispy-braces
  "\"" #'lispy-quotes
  ";" #'lispy-comment

  ;; ( should always insert parentheses
  ")" #'evil-lispy-insert-to-lispy-right
  "[" #'evil-lispy-insert-to-lispy-left
  "]" #'evil-lispy-insert-to-lispy-right

  (kbd "DEL") #'lispy-delete-backward
  (kbd "M-k") #'lispy-kill-sentence
  (kbd "C-1") #'lispy-describe-inline
  (kbd "C-2") #'lispy-arglist-inline)

(define-key lispy-mode-map "o" 'special-lispy-different)
(define-key lispy-mode-map "d" 'special-lispy-other-mode)
(define-key lispy-mode-map "i" 'special-lispy-flow)
(define-key lispy-mode-map "f" 'special-lispy-tab)

(provide 'evil-lispy)

;;; evil-lispy.el ends here
