;;; general.el --- tests for find-file-in-project

;; Copyright (C) 2016 Chen Bin
;;

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(require 'find-file-in-project)

(ert-deftest ffip-test-find-by-selected ()
  (let (files)
    (setq ffip-project-root default-directory)
    (setq files (mapcar 'car (ffip-project-search "ivy" nil)))
    ;; (message "files=%s" files)
    (should (string-match-p "ivy.el" (car files)))))


(ert-deftest ffip-test-ffip ()
  (let (files)
    (setq ffip-project-root default-directory)
    (setq files (mapcar 'car (ffip-project-search nil nil)))
    (should (> (length files) 1))
    (should (not (active-minibuffer-window)))
    (setq ivy-read-called nil)
    ;; ffip will call ivy by default
    (ffip-find-files nil nil)
    (should ivy-read-called)))