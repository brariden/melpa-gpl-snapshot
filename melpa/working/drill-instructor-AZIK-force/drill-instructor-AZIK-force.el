;;; drill-instructor-AZIK-force.el --- Support AZIK input.

;; Filename: drill-instructor-AZIK-force.el
;; Description: Support AZIK input.
;; Author: Yuhei Maeda <yuhei.maeda_at_gmail.com>
;; Maintainer: Yuhei Maeda
;; Copyright (C) 2012 Yuhei Maeda all rights reserved.
;; Created: :2012-09-08
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/myuhe/drill-instructor-AZIK-force.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 0:110-1301, USA.

(eval-when-compile (require 'cl))
(require 'popup)

(defvar AZIK-force-alist nil)
(defvar AZIK-force-curriculum 'easy)

(defvar AZIK-force-conversion-alist
  '(((?k ?o ?t ?o) . ("こと" "kt"))
    ((?w ?a ?t ?a) . ("わた" "wt"))
    ((?k ?a ?m ?o) . ("かも" "km"))
    ((?s ?u ?r ?u) . ("する" "sr"))
    ((?r ?a ?r ?e) . ("られ" "rr"))
    ((?n ?e ?b ?a) . ("ねば" "nb"))
    ((?n ?i ?t ?i) . ("にち" "nt"))
    ((?s ?i ?t ?a) . ("した" "st"))
    ((?m ?o ?n ?o) . ("もの" "mn"))
    ((?t ?a ?m ?e) . ("ため" "tm"))
    ((?t ?a ?r ?a) . ("たら" "tr"))
    ((?z ?a ?r ?u) . ("ざる" "zr"))
    ((?b ?i ?t ?o) . ("びと" "bt"))
    ((?d ?a ?t ?i) . ("だち" "dt"))
    ((?t ?a ?t ?i) . ("たち" "tt"))
    ((?m ?a ?s ?u) . ("ます" "ms"))
    ((?d ?e ?m ?o) . ("でも" "dm"))
    ((?d ?f ?m ?o) . ("でも" "dm"))
    ((?n ?a ?r ?u) . ("なる" "nr"))
    ((?m ?a ?t ?a) . ("また" "mt"))
    ((?g ?a ?r ?a) . ("がら" "gr"))
    ((?w ?a ?r ?e) . ("われ" "wr"))
    ((?h ?i ?t ?o) . ("ひと" "ht"))
    ((?d ?e ?s ?u) . ("です" "ds"))
    ((?d ?f ?s ?u) . ("です" "ds"))
    ((?k ?a ?r ?a) . ("から" "kr"))
    ((?y ?o ?r ?u) . ("よる" "yr"))
    ((?t ?a ?b ?i) . ("たび" "tb"))
    ))

(defun AZIK-force-popup-tip ()
  (when (eq (length AZIK-force-alist) 4)
    (pop AZIK-force-alist))
  (setq AZIK-force-alist (append AZIK-force-alist (list last-command-event)))
  (dolist (i AZIK-force-conversion-alist)
    (when (equal AZIK-force-alist (car i))
      (cond ((eq AZIK-force-curriculum 'easy)
             (popup-tip (concat "「" (cadr i) "」と入力したい時は、「" (caddr i) "」と入力します。以後気をつけましょう。")))
            ((eq AZIK-force-curriculum 'born2die)
             (popup-tip (concat "「" (cadr i) "」と打ちたい時は、「" (caddr i) "」と打つんだ。わかったな、ウジ虫ども!!"))
             (delete-backward-char 2))
            ((eq AZIK-force-curriculum 'born2kill)
             (kill-emacs))))))

(add-hook 'post-command-hook 'AZIK-force-popup-tip)

(provide 'drill-instructor-AZIK-force)
;;; drill-instructor-AZIK-force.el ends here
