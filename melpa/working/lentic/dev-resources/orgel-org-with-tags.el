;;; orgel-el-with-tags.el -- an example file

;;; Header:

;;; Commentary:

;; This file is a test file to see if we can have tags on headers.

;;; Code:

;; #+begin_src emacs-lisp
(message "Hello World")
;; #+end_src

;;; HeaderOne:                                                             :tag:

;; This is a level 1 header with a tag.

;; ** HeaderTwo                                                     :anothertag:

;; This is a level 2 header with a tag
