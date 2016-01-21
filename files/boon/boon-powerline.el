;;; boon-powerline.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module gives an example setup for powerline using boon.

;;; Code:

(require 'powerline)
(require 'boon-core)

(defface boon-modeline-cmd '((t (:background "blue"))) "Face for modeline indicator of boon command state" :group 'boon)
(defface boon-modeline-ins '((t (:background "red"))) "Face for modeline indicator of boon insert state" :group 'boon)
(defface boon-modeline-off '((t (:background "orange"))) "Face for modeline indicator of boon off state" :group 'boon)
(defface boon-modeline-spc '((t (:background "green"))) "Face for modeline indicator of boon special state" :group 'boon)

(defun boon-state-face ()
  "Return a face appropriate for a powerline-style entry in the modeline."
  (cond
   (boon-command-state 'boon-modeline-cmd)
   (boon-insert-state 'boon-modeline-ins)
   (boon-special-state 'boon-modeline-spc)
   (boon-off-state 'boon-modeline-off)))

(defun boon-powerline-theme ()
  "Set up a powerline based on powerline-default-theme which also displays boon-state."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active (boon-state-face) nil))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                              (cdr powerline-default-separator-dir))))
                          (boon (when (bound-and-true-p boon-mode)
                                  (list
                                   (powerline-raw " " face0)
                                   (powerline-raw (boon-state-string) face0 'r)
                                   (funcall separator-left face0 nil)
                                   )))
                          (lhs (list
                                     (powerline-raw (if (buffer-modified-p) "*" "-" ) nil 'l)
                                     (powerline-raw mode-line-mule-info nil)
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%l" face1 'l)
                                     (powerline-raw ":" face1)
                                     (powerline-raw "%c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%p" nil)
                                     (powerline-raw " ")
                                     (powerline-buffer-size nil nil)
                                     (powerline-raw " ")
                                     (powerline-hud face2 face1)
                                     )))
                     (concat (powerline-render boon)
                             (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs))))))
  )

(provide 'boon-powerline)
;;; boon-powerline.el ends here
