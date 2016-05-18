;;; jtags-test.el --- code for testing jtags mode

;; Copyright (C) 2006-2016 Johan Dykstrom

;; Author: Johan Dykstrom <jody4711-sf@yahoo.se>
;; Created: Sep 2006
;; Version: 0.98.0
;; Keywords: tools
;; URL: http://jtags.sourceforge.net

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

;; This file contains code for unit testing `jtags-mode'.  The code for
;; jtags mode itself is located in file "jtags.el".  Start by defining
;; all functions in the section "Test infrastructure" below, search for
;; this string in the code: "Evaluate this expression to define the
;; functions above!" After defining the infrastructure functions, you can
;; run the tests in section "Unit test" below.  Most of them are JUnit
;; style assertions.
;;
;; To run the unit tests in XEmacs, you must modify variable
;; `tag-table-alist' to include Emacs Lisp files like this:
;;
;; (setq tag-table-alist '(("\\.\\(java\\|el\\)$" . "c:/java/jdk1.6.0/src")))

;;; Code:

(require 'jtags)

;; ----------------------------------------------------------------------------
;; Test infrastructure:
;; ----------------------------------------------------------------------------

(defun goto-test (name n &optional end)
  "Goto test specified by string NAME, and move N lines forward.
Search forward from beginning of buffer for string NAME.  Move N
lines forward (backward if N is negative).  Goto end of line if
optional argument END is non-nil.

The function `goto-test' can be used to find test data embedded in this file.
It does not save the original buffer position."
  (goto-char (point-min))
  (search-forward name)
  (forward-line n)
  (if end
      (end-of-line)))

(defun safe-eval (expression)
  "Save point and current buffer; evaluate EXPRESSION; restore those things.
The result of evaluating EXPRESSION is returned."
  (let ((orig-buffer (current-buffer))
        (orig-pos (point))

        ;; Evaluate test expression
        (result (eval expression)))

    ;; Restore buffer and point
    (switch-to-buffer orig-buffer)
    (goto-char orig-pos)

    ;; Return result
    result))

(defun assert-equal (expected expression)
  "Return non-nil if EXPECTED is `equal' to '(eval EXPRESSION).
If the test fails, a message is printed in the \"*Messages*\" buffer. Example:

\(assert-equal 1 '(car (list 1 2 3))) -> t
\(assert-equal 2 '(max 1 2 3))        -> nil"
  (let ((result (safe-eval expression)))
    (if (equal expected result)
        (message "PASSED")
      (message "FAILED on line %d: expected `%s' but was `%s'" (line-number-at-pos) expected result)
      nil)))

(defun assert-true (expression)
  "Return non-nil if EXPRESSION is evaluated to non-nil.
If the test fails, a message is printed in the \"*Messages*\" buffer."
  (let ((result (safe-eval expression)))
    (if result
        (message "PASSED")
      (message "FAILED on line %d" (line-number-at-pos))
      nil)))

(defun assert-false (expression)
  "Return non-nil if EXPRESSION is evaluated to nil.
If the test fails, a message is printed in the \"*Messages*\" buffer."
  (let ((result (safe-eval expression)))
    (if (not result)
        (message "PASSED")
      (message "FAILED on line %d" (line-number-at-pos))
      nil)))

;; Evaluate this expression to define the functions above!
(save-excursion
  (beginning-of-defun)
  (eval-region (point-min) (point) t))

(setq jtags-trace-flag nil)

;; ----------------------------------------------------------------------------
;; Unit test:
;; ----------------------------------------------------------------------------

;;; jtags-filter-list

(assert-equal '("1" "123")
              '(jtags-filter-list '("1" "2" "3" "123") (lambda (x) (string-match "1" x))))
(assert-equal '("123")
              '(jtags-filter-list '("1" "2" "3" "123") (lambda (x) (string-match "123" x))))
(assert-equal '()
              '(jtags-filter-list '("1" "2" "3" "123") (lambda (x) (string-match "4" x))))
(assert-equal '("1" "2" "3" "123")
              '(jtags-filter-list '("1" "2" "3" "123") (lambda (x) (string-match "" x))))
(assert-equal '(4 5 6)
              '(jtags-filter-list '(1 2 3 4 5 6) (lambda (x) (> x 3))))

;;; jtags-rotate-left

(assert-equal '()      '(jtags-rotate-left '()))
(assert-equal '(1)     '(jtags-rotate-left '(1)))
(assert-equal '(2 1)   '(jtags-rotate-left '(1 2)))
(assert-equal '(2 3 1) '(jtags-rotate-left '(1 2 3)))

;;; jtags-line-to-point

(assert-equal 1  '(jtags-line-to-point 1))
(assert-equal 51 '(jtags-line-to-point 2))

;;; jtags-file-name-directory

(assert-equal "c:/"                  '(jtags-file-name-directory "c:/"))
(assert-equal "c:/Java/"             '(jtags-file-name-directory "c:/Java"))
(assert-equal "c:/Java/"             '(jtags-file-name-directory "c:/Java/"))
(assert-equal "c:/Java/"             '(jtags-file-name-directory "c:/Java/junit4.8.2/.."))
(assert-equal "c:/Java/junit4.8.2/"  '(jtags-file-name-directory "c:/Java/junit4.8.2"))
(assert-equal "c:/Java/junit4.8.2/"  '(jtags-file-name-directory "c:/Java/junit4.8.2/junit.jar"))
(assert-equal "c:/"                  '(jtags-file-name-directory "C:\\"))
(assert-equal "c:/Java/"             '(jtags-file-name-directory "C:\\Java"))
(assert-equal "c:/Java/"             '(jtags-file-name-directory "C:\\Java\\"))
(assert-equal "c:/Java/junit4.8.2/"  '(jtags-file-name-directory "C:\\Java\\junit4.8.2\\"))
(assert-equal "c:/Java/junit4.8.2/"  '(jtags-file-name-directory "C:\\Java\\junit4.8.2\\junit.jar"))

;; FAILS in Windows
(assert-equal "/"           '(jtags-file-name-directory "/"))
(assert-equal "/usr/"       '(jtags-file-name-directory "/usr"))
(assert-equal "/usr/"       '(jtags-file-name-directory "/usr/"))
(assert-equal "/usr/local/" '(jtags-file-name-directory "/usr/local"))
(assert-equal "/usr/"       '(jtags-file-name-directory "/usr/local/.."))
(assert-equal "/bin/"       '(jtags-file-name-directory "/bin/"))
(assert-equal "/bin/"       '(jtags-file-name-directory "/bin/sh"))

;;; jtags-get-class-list

(assert-equal '("Comparable" "Double" "Number" "Object" "Serializable")
              '(sort (jtags-get-class-list "Double" '("java.lang.*" "java.awt.*")) 'string<))
(assert-equal '("Accessible" "Component" "ImageObserver" "ItemSelectable" "List" "MenuContainer" "Object" "Serializable")
              '(sort (jtags-get-class-list "List" '("java.lang.*" "java.awt.*")) 'string<))

;;; jtags-get-class-list-iter

(assert-equal '("CharSequence" "Comparable" "Object" "Serializable" "String")
              '(sort (jtags-get-class-list-iter '("String") '("java.lang.*")) 'string<))
(assert-equal '("CharSequence" "Comparable" "Comparable" "Integer" "Number" "Object" "Object"
                "Serializable" "Serializable" "String")
              '(sort (jtags-get-class-list-iter '("String" "Integer") '("java.lang.*" "java.util.List")) 'string<))
(assert-equal '("Accessible" "Accessible" "ActionListener" "Component" "Component" "Container" "Container"
                "EventListener" "EventListener" "HasGetTransferHandler" "HasGetTransferHandler" "ImageObserver"
                "ImageObserver" "ItemSelectable" "JComboBox" "JComponent" "JComponent" "JScrollPane"
                "ListDataListener" "MenuContainer" "MenuContainer" "Object" "Object" "ScrollPaneConstants"
                "Serializable" "Serializable" "Serializable" "Serializable")
              '(sort (jtags-get-class-list-iter '("JComboBox" "JScrollPane") '("java.lang.*" "javax.swing.*")) 'string<))
;; Returns only JComboBox since it cannot be found in specified packages
(assert-equal '("JComboBox" "Object")
              '(sort (jtags-get-class-list-iter '("JComboBox") '("java.lang.*")) 'string<))

;;; jtags-do-get-class-list

(assert-equal '("Object")
              '(sort (jtags-do-get-class-list "Object") 'string<))
(assert-equal '("FooBar" "Object")
              '(sort (jtags-do-get-class-list "FooBar") 'string<))
(assert-equal '("CharSequence" "Comparable" "Object" "Serializable" "String")
              '(sort (jtags-do-get-class-list "String") 'string<))
(assert-equal '("Accessible" "Accessible" "Component" "Container" "Frame" "HasGetTransferHandler" "ImageObserver" "JFrame"
                "MenuContainer" "MenuContainer" "Object" "RootPaneContainer" "Serializable" "Window" "WindowConstants")
              '(sort (jtags-do-get-class-list "JFrame") 'string<))
;; With packages
(assert-equal '("CharSequence" "Comparable" "Object" "Serializable" "String")
              '(sort (jtags-do-get-class-list "String" '("java.lang.*")) 'string<))
(assert-equal '("Comparable" "Double" "Number" "Object" "Serializable")
              '(sort (jtags-do-get-class-list "Double" '("java.lang.*")) 'string<))
(assert-equal '("Accessible" "Component" "ImageObserver" "ItemSelectable" "List" "MenuContainer" "Object" "Serializable")
              '(sort (jtags-do-get-class-list "List" '("java.lang.*" "java.awt.List")) 'string<))
(assert-equal '("AbstractCollection" "AbstractSet" "Cloneable" "Collection" "Collection" "Collection" "HashSet"
                "Iterable" "Iterable" "Iterable" "Object" "Serializable" "Set" "Set")
              '(sort (jtags-do-get-class-list "HashSet" '("java.lang.*" "java.util.ArrayList" "java.util.HashSet")) 'string<))
;; Interfaces
(assert-equal '("Collection" "Iterable" "List" "Object")
              '(sort (jtags-do-get-class-list "List" '("java.lang.*" "java.util.List")) 'string<))

;;; jtags-find-super-class

(assert-equal "Number"
              '(cdr (jtags-find-super-class (jtags-lookup-identifier "Double" nil '("java.lang.*")))))
(assert-equal "Component"
              '(cdr (jtags-find-super-class (jtags-lookup-identifier "List" nil '("java.lang.*" "java.awt.*")))))
(assert-equal "Collection"
              '(cdr (jtags-find-super-class (jtags-lookup-identifier "List" nil '("java.lang.*" "java.util.*")))))
(assert-equal "Iterable"
              '(cdr (jtags-find-super-class (jtags-lookup-identifier "Collection" nil '("java.util.Collection")))))
(assert-equal "AbstractSet"
              '(cdr (jtags-find-super-class (jtags-lookup-identifier "HashSet" nil '("java.util.*")))))

(assert-false '(jtags-find-super-class (jtags-lookup-identifier "Number" nil '("java.lang.*"))))
(assert-false '(jtags-find-super-class (jtags-lookup-identifier "Iterable" nil '("java.lang.*"))))

;;; jtags-find-interfaces

(assert-true '(let ((result (jtags-find-interfaces (jtags-lookup-identifier "Long" nil '("java.lang.*")))))
                (and (equal '("Comparable") (cdr result))
                     (member "java.lang.*" (car result)))))
(assert-true '(let ((result (jtags-find-interfaces (jtags-lookup-identifier "String" nil '("java.lang.String")))))
                (and (equal '("Serializable" "Comparable" "CharSequence") (cdr result))
                     (member "java.io.Serializable" (car result))
                     (member "java.lang.*" (car result)))))
(assert-true '(let ((result (jtags-find-interfaces (jtags-lookup-identifier "HashSet" nil '("java.util.HashSet")))))
                (and (equal '("Set" "Cloneable" "Serializable") (cdr result))
                     (member "java.io.Serializable" (car result))
                     (member "java.lang.*" (car result))
                     (member "java.util.*" (car result)))))
(assert-true '(let ((result (jtags-find-interfaces (jtags-lookup-identifier "JLabel" nil '("java.lang.*" "javax.swing.*")))))
                (and (equal '("SwingConstants" "Accessible") (cdr result))
                     (member "javax.swing.*" (car result))
                     (member "javax.accessibility.*" (car result)))))

(assert-false '(jtags-find-interfaces (jtags-lookup-identifier "Object" nil '("java.lang.*"))))
(assert-false '(jtags-find-interfaces (jtags-lookup-identifier "List" nil '("java.util.*"))))

;;; jtags-recursive-tags-lookup

(assert-true '(let ((definition (jtags-recursive-tags-lookup '("StringBuffer" "class" "getField") '("StringBuffer" "Object"))))
                (and definition
                     (string-equal (jtags-definition-class definition) "Class")
                     (string-equal (jtags-definition-name definition) "getField"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("System" "class" "getPackage") '("Integer" "Number" "Object"))))
                (and definition
                     (string-equal (jtags-definition-class definition) "Class")
                     (string-equal (jtags-definition-name definition) "getPackage"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("this" "split") '("String" "Object"))))
                (and definition
                     (string-equal (jtags-definition-class definition) "String")
                     (string-equal (jtags-definition-name definition) "split"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("this" "length") '("StringBuffer" "Object"))))
                (and definition
                     (string-equal (jtags-definition-class definition) "StringBuffer")
                     (string-equal (jtags-definition-name definition) "length"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("this" "ready") '("BufferedReader" "Reader" "Object"))))
                (and definition
                     (string-equal (jtags-definition-class definition) "BufferedReader")
                     (string-equal (jtags-definition-name definition) "ready"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("super" "equals") '("StringBuffer" "Object"))))
                (and definition
                     (string-equal (jtags-definition-class definition) "Object")
                     (string-equal (jtags-definition-name definition) "equals"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("#stringliteral#" "equals") '("StringBuffer" "Object"))))
                (and definition
                     (string-equal (jtags-definition-class definition) "String")
                     (string-equal (jtags-definition-name definition) "equals"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("java" "lang" "String") nil)))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.lang")
                     (string-equal (jtags-definition-class definition) "String")
                     (string-equal (jtags-definition-name definition) "String")
                     (string-equal (jtags-definition-type definition) "class"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("java" "lang" "Class" "forName") nil)))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.lang")
                     (string-equal (jtags-definition-class definition) "Class")
                     (string-equal (jtags-definition-name definition) "forName")
                     (string-equal (jtags-definition-type definition) "Class"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("java" "lang" "Integer" "valueOf") nil '("java.lang.*" "java.io.File"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.lang")
                     (string-equal (jtags-definition-class definition) "Integer")
                     (string-equal (jtags-definition-name definition) "valueOf")
                     (string-equal (jtags-definition-type definition) "Integer"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("java" "util" "List") nil)))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.util")
                     (string-equal (jtags-definition-class definition) "List")
                     (string-equal (jtags-definition-name definition) "List")
                     (string-equal (jtags-definition-type definition) "interface"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("List") nil '("java.util.*"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.util")
                     (string-equal (jtags-definition-class definition) "List")
                     (string-equal (jtags-definition-name definition) "List")
                     (string-equal (jtags-definition-type definition) "interface"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("java" "awt" "Component") nil '("java.lang.*" "java.awt.List" "java.awt.Component"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.awt")
                     (string-equal (jtags-definition-class definition) "Component")
                     (string-equal (jtags-definition-name definition) "Component")
                     (string-equal (jtags-definition-type definition) "class"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("java" "lang" "String") '("StringBuffer" "Object"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.lang")
                     (string-equal (jtags-definition-class definition) "String")
                     (string-equal (jtags-definition-name definition) "String")
                     (string-equal (jtags-definition-type definition) "class"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("String") nil '("java.util.*" "java.lang.*"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.lang")
                     (string-equal (jtags-definition-class definition) "String")
                     (string-equal (jtags-definition-name definition) "String")
                     (string-equal (jtags-definition-type definition) "class"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("String") nil '("java.lang.*"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.lang")
                     (string-equal (jtags-definition-class definition) "String")
                     (string-equal (jtags-definition-name definition) "String")
                     (string-equal (jtags-definition-type definition) "class"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("InputStreamReader") nil '("java.lang.*" "java.io.*"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.io")
                     (string-equal (jtags-definition-class definition) "InputStreamReader")
                     (string-equal (jtags-definition-name definition) "InputStreamReader")
                     (string-equal (jtags-definition-type definition) "class"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("InputStreamReader") nil '("java.io.InputStreamReader"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.io")
                     (string-equal (jtags-definition-class definition) "InputStreamReader")
                     (string-equal (jtags-definition-name definition) "InputStreamReader")
                     (string-equal (jtags-definition-type definition) "class"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup '("java" "util" "List") nil '("java.io.*"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.util")
                     (string-equal (jtags-definition-class definition) "List")
                     (string-equal (jtags-definition-name definition) "List")
                     (string-equal (jtags-definition-type definition) "interface"))))

;; Just package names should not succeed
(assert-false '(jtags-recursive-tags-lookup '("java") nil))
(assert-false '(jtags-recursive-tags-lookup '("java" "lang") nil))
;; Invalid package for class lookup
(assert-false '(jtags-recursive-tags-lookup '("File") nil '("java.util.*" "java.lang.*")))
(assert-false '(jtags-recursive-tags-lookup '("JTable") nil '("java.util.*" "javax.swing.JList")))

;; jtags-recursive-tags-lookup-with-package

(assert-true '(let ((definition (jtags-recursive-tags-lookup-with-package '("java" "io" "InputStreamReader"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.io")
                     (string-equal (jtags-definition-class definition) "InputStreamReader")
                     (string-equal (jtags-definition-name definition) "InputStreamReader")
                     (string-equal (jtags-definition-type definition) "class"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup-with-package '("javax" "swing" "JLabel"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "javax.swing")
                     (string-equal (jtags-definition-class definition) "JLabel")
                     (string-equal (jtags-definition-name definition) "JLabel")
                     (string-equal (jtags-definition-type definition) "class"))))
(assert-true '(let ((definition (jtags-recursive-tags-lookup-with-package '("java" "lang" "Class" "forName"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.lang")
                     (string-equal (jtags-definition-class definition) "Class")
                     (string-equal (jtags-definition-name definition) "forName")
                     (string-equal (jtags-definition-type definition) "Class"))))

;; Just package names should not succeed
(assert-false '(jtags-recursive-tags-lookup-with-package '("java")))
(assert-false '(jtags-recursive-tags-lookup-with-package '("java" "lang")))
;; Invalid package for class lookup
(assert-false '(jtags-recursive-tags-lookup-with-package '("java" "lang" "File")))
(assert-false '(jtags-recursive-tags-lookup-with-package '("java" "io" "String")))

;;; jtags-import-tags-lookup

(let ((definition (jtags-import-tags-lookup '("toList") '("java.lang.*" "static java.util.stream.Collectors.toList"))))
  (assert-true definition)
  (assert-equal "java.util.stream" '(jtags-definition-package definition))
  (assert-equal "Collectors" '(jtags-definition-class definition))
  (assert-equal "toList" '(jtags-definition-name definition))
  (assert-equal "Collector" '(jtags-definition-type definition)))
(let ((definition (jtags-import-tags-lookup '("valueOf" "toHexString") '("static java.lang.Double.valueOf"))))
  (assert-true definition)
  (assert-equal "java.lang" '(jtags-definition-package definition))
  (assert-equal "Double" '(jtags-definition-class definition))
  (assert-equal "toHexString" '(jtags-definition-name definition))
  (assert-equal "String" '(jtags-definition-type definition)))
(assert-false '(jtags-import-tags-lookup '("toList") '("java.lang.*" "static java.util.Arrays.asList")))

;;; jtags-buffer-tag-table-list

;; FAILS in XEmacs
(let ((tags-table-list '("c:/java/jdk1.6.0/src" "c:/Windows")))
  (assert-equal '("c:/java/jdk1.6.0/src" "c:/Windows") '(jtags-buffer-tag-table-list)))

(let ((tags-table-list '("c:/java/jdk1.6.0/src")))
  (assert-equal '("c:/java/jdk1.6.0/src") '(jtags-buffer-tag-table-list)))

(let ((tags-table-list '()))
  (assert-equal nil '(jtags-buffer-tag-table-list)))

;; OK in XEmacs
(let ((tags-table-list '("c:/java/jdk1.6.0/src"))
      (tag-table-alist '(("\\.el$" . "c:/Java") ("\\.java$" . "c:/Windows"))))
  (assert-equal '("c:/Java/TAGS") '(jtags-buffer-tag-table-list)))

(let ((tags-table-list '("c:/java/jdk1.6.0/src"))
      (tag-table-alist '(("\\.el$" . "c:/") ("\\.java$" . "c:/Windows") ("\\.el$" . "c:/Java"))))
  (assert-equal '("c:/TAGS" "c:/Java/TAGS") '(jtags-buffer-tag-table-list)))

(let ((tags-table-list '("c:/java/jdk1.6.0/src"))
      (tag-table-alist '(("\\.el$" . "c:/") ("\\.el$" . "c:/Java"))))
  (assert-equal '("c:/TAGS" "c:/Java/TAGS") '(jtags-buffer-tag-table-list)))

(let ((tags-table-list '("c:/java/jdk1.6.0/src"))
      (tag-table-alist '(("\\.java$" . "c:/Windows"))))
  (assert-equal nil '(ignore-errors (jtags-buffer-tag-table-list))))

(let ((tags-table-list '("c:/java/jdk1.6.0/src"))
      (tag-table-alist '(("\\.el$" . "c:/") ("\\.java$" . "c:/Windows")))
      (buffer-tag-table "c:/Java"))
  (assert-equal '("c:/Java/TAGS" "c:/TAGS") '(jtags-buffer-tag-table-list)))

(defun return-true () "Return non-nil." 't)
(defun return-false () "Return nil." nil)

(let ((tags-table-list '("c:/java/jdk1.6.0/src"))
      (tag-table-alist '(("\\.el$" . "c:/") ((return-true) . "c:/Java") ("\\.java$" . "c:/Windows"))))
  (assert-equal '("c:/TAGS" "c:/Java/TAGS") '(jtags-buffer-tag-table-list)))

(let ((tags-table-list '("c:/java/jdk1.6.0/src"))
      (tag-table-alist '(("\\.el$" . "c:/") ((return-false) . "c:/Java") ("\\.java$" . "c:/Windows"))))
  (assert-equal '("c:/TAGS") '(jtags-buffer-tag-table-list)))

;;; jtags-lookup-class-members

(let ((members (jtags-lookup-class-members "Foo" '("net.sf.jtags.*" "java.util.*" "java.lang.*"))))
  (message "Members=%S" members)
  (assert-true '(member "getName" members)))
(let ((members (jtags-lookup-class-members "Exception" '("java.io.*" "java.util.*" "java.lang.*"))))
  (message "Members=%S" members)
  (assert-true '(member "Exception" members)))
(let ((members (jtags-lookup-class-members "Double" '("java.io.*" "java.util.*" "java.lang.*"))))
  (message "Members=%S" members)
  (assert-true '(member "longValue" members)))
(let ((members (jtags-lookup-class-members "List" '("java.io.*" "java.util.*" "java.lang.*"))))
  (message "Members=%S" members)
  (assert-true '(member "size" members)))
(let ((members (jtags-lookup-class-members "Serializable" '("java.io.*" "java.util.*" "java.lang.*"))))
  (message "Members=%S" members)
  (assert-true '(null members)))

;;; jtags-lookup-identifier

;; Class
(assert-true '(let ((definition (jtags-lookup-identifier "JList")))
                (and definition
                     (string-equal (jtags-definition-package definition) "javax.swing")
                     (string-equal (jtags-definition-class definition) "JList")
                     (string-equal (jtags-definition-name definition) "JList")
                     (string-equal (jtags-definition-type definition) "class")
                     (string-match "class JList" (jtags-definition-text definition)))))
;; Constructor
(assert-true '(let ((definition (jtags-lookup-identifier "String" "String")))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.lang")
                     (string-equal (jtags-definition-class definition) "String")
                     (string-equal (jtags-definition-name definition) "String")
                     (string-equal (jtags-definition-type definition) "String")
                     (string-match "String(" (jtags-definition-text definition)))))
;; Member
(assert-true '(let ((definition (jtags-lookup-identifier "String" "split")))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.lang")
                     (string-equal (jtags-definition-class definition) "String")
                     (string-equal (jtags-definition-name definition) "split")
                     (string-equal (jtags-definition-type definition) "String")
                     (string-match "split(" (jtags-definition-text definition)))))
;; Class with packages
(assert-true '(let ((definition (jtags-lookup-identifier "Double" nil '("java.lang.*"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.lang")
                     (string-equal (jtags-definition-class definition) "Double")
                     (string-equal (jtags-definition-name definition) "Double")
                     (string-equal (jtags-definition-type definition) "class")
                     (string-match "class Double" (jtags-definition-text definition)))))
;; Member with packages
(assert-true '(let ((definition (jtags-lookup-identifier "Double" "doubleValue" '("java.io.*" "java.lang.*"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.lang")
                     (string-equal (jtags-definition-class definition) "Double")
                     (string-equal (jtags-definition-name definition) "doubleValue")
                     (string-equal (jtags-definition-type definition) "double")
                     (string-match "doubleValue(" (jtags-definition-text definition)))))
;; Interface with packages
(assert-true '(let ((definition (jtags-lookup-identifier "Comparable" nil '("java.util.List" "java.io.*" "java.lang.*"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.lang")
                     (string-equal (jtags-definition-class definition) "Comparable")
                     (string-equal (jtags-definition-name definition) "Comparable")
                     (string-equal (jtags-definition-type definition) "interface")
                     (string-match "interface Comparable" (jtags-definition-text definition)))))
;; Member with packages
(assert-true '(let ((definition (jtags-lookup-identifier "FlatteningPathIterator" "cury")))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.awt.geom")
                     (string-equal (jtags-definition-class definition) "FlatteningPathIterator")
                     (string-equal (jtags-definition-name definition) "cury")
                     (string-equal (jtags-definition-type definition) "double")
                     (string-match "cury" (jtags-definition-text definition)))))
;; Ambiguous class with packages
(assert-true '(let ((definition (jtags-lookup-identifier "List" nil '("java.io.*" "java.awt.*"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.awt")
                     (string-equal (jtags-definition-class definition) "List")
                     (string-equal (jtags-definition-name definition) "List")
                     (string-equal (jtags-definition-type definition) "class")
                     (string-match "class List" (jtags-definition-text definition)))))
;; Ambiguous interface with packages
(assert-true '(let ((definition (jtags-lookup-identifier "List" nil '("java.io.*" "java.util.*"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.util")
                     (string-equal (jtags-definition-class definition) "List")
                     (string-equal (jtags-definition-name definition) "List")
                     (string-equal (jtags-definition-type definition) "interface")
                     (string-match "interface List" (jtags-definition-text definition)))))
;; Generic member with packages
(assert-true '(let ((definition (jtags-lookup-identifier "Iterator" "next" '("java.io.*" "java.util.Iterator" "java.lang.*"))))
                (and definition
                     (string-equal (jtags-definition-package definition) "java.util")
                     (string-equal (jtags-definition-class definition) "Iterator")
                     (string-equal (jtags-definition-name definition) "next")
                     (string-equal (jtags-definition-type definition) "E")
                     (string-match "next(" (jtags-definition-text definition)))))

(assert-false '(jtags-lookup-identifier "java"))
(assert-false '(jtags-lookup-identifier "javax"))
(assert-false '(jtags-lookup-identifier "FooBar"))
(assert-false '(jtags-lookup-identifier "String" "foo"))
(assert-false '(jtags-lookup-identifier "Double" nil '("java.io.*")))

;;; jtags-get-tagged-type-line-pos

;; >>> TEST DATA
public final class Integer Integer41,1265
    private int count;Integer.count606,21687
    private char tjipp[Integer.tjipp607,21700
    public static final int   MIN_VALUE Integer.MIN_VALUE46,1445
    public static String toString(Integer.toString115,4262
    public static int parseInt(Integer.parseInt541,19127
    public static Integer valueOf(Integer.valueOf596,21479
    public Integer(Integer.Integer632,22662
    public String[Integer.String2163,85659
    public String[] split(Integer.split2163,85659
    double curx,Integer.curx70,2325
    double curx, cury;Integer.cury70,2325
;; <<< TEST DATA

(assert-equal '("class" 41 1265 "public final class Integer ")
              '(let ((bound (point)))
                 (goto-test ";;; jtags-get-tagged-type-line-pos" 4)
                 (jtags-get-tagged-type-line-pos "Integer" nil bound)))
(assert-equal '("int" 606 21687 "    private int count;")
              '(let ((bound (point)))
                 (goto-test ";;; jtags-get-tagged-type-line-pos" 4)
                 (jtags-get-tagged-type-line-pos "Integer" "count" bound)))
(assert-equal '("char" 607 21700 "    private char tjipp[")
              '(let ((bound (point)))
                 (goto-test ";;; jtags-get-tagged-type-line-pos" 4)
                 (jtags-get-tagged-type-line-pos "Integer" "tjipp" bound)))
(assert-equal '("int" 46 1445 "    public static final int   MIN_VALUE ")
              '(let ((bound (point)))
                 (goto-test ";;; jtags-get-tagged-type-line-pos" 4)
                 (jtags-get-tagged-type-line-pos "Integer" "MIN_VALUE" bound)))
(assert-equal '("String" 115 4262 "    public static String toString(")
              '(let ((bound (point)))
                 (goto-test ";;; jtags-get-tagged-type-line-pos" 4)
                 (jtags-get-tagged-type-line-pos "Integer" "toString" bound)))
(assert-equal '("int" 541 19127 "    public static int parseInt(")
              '(let ((bound (point)))
                 (goto-test ";;; jtags-get-tagged-type-line-pos" 4)
                 (jtags-get-tagged-type-line-pos "Integer" "parseInt" bound)))
(assert-equal '("Integer" 632 22662 "    public Integer(")
              '(let ((bound (point)))
                 (goto-test ";;; jtags-get-tagged-type-line-pos" 4)
                 (jtags-get-tagged-type-line-pos "Integer" "Integer" bound)))
(assert-equal '("String" 2163 85659 "    public String[] split(")
              '(let ((bound (point)))
                 (goto-test ";;; jtags-get-tagged-type-line-pos" 4)
                 (jtags-get-tagged-type-line-pos "Integer" "split" bound)))
(assert-equal '("double" 70 2325 "    double curx, cury;")
              '(let ((bound (point)))
                 (goto-test ";;; jtags-get-tagged-type-line-pos" 4)
                 (jtags-get-tagged-type-line-pos "Integer" "cury" bound)))
(assert-equal '("double" 70 2325 "    double curx,")
              '(let ((bound (point)))
                 (goto-test ";;; jtags-get-tagged-type-line-pos" 4)
                 (jtags-get-tagged-type-line-pos "Integer" "curx" bound)))

;;; jtags-right-package-p

(assert-true '(jtags-right-package-p "c:/java/jdk1.6.0/src/java/lang/Double.java"
                                     nil))
(assert-true '(jtags-right-package-p "c:/java/jdk1.6.0/src/java/lang/Double.java"
                                     '("java.lang.Double")))
(assert-false '(jtags-right-package-p "c:/java/jdk1.6.0/src/java/lang/Double.java"
                                      '("java.lang.Float")))
(assert-true '(jtags-right-package-p "c:/java/jdk1.6.0/src/java/lang/Double.java"
                                     '("java.lang.*")))
(assert-true '(jtags-right-package-p "c:/java/jdk1.6.0/src/java/lang/Double.java"
                                     '("java.awt.Image" "java.io.*" "java.lang.*")))
(assert-false '(jtags-right-package-p "c:/java/jdk1.6.0/src/java/awt/geom/Arc2D.java"
                                     '("java.awt.Image" "java.io.*" "java.lang.*")))
(assert-false '(jtags-right-package-p "c:/java/jdk1.6.0/src/java/io/DataInput.java"
                                      '("java.io.DataInputStream" "java.lang.*")))
(assert-false '(jtags-right-package-p "c:/java/jdk1.6.0/src/java/io/DataInputStream.java"
                                      '("java.io.DataInput" "java.lang.*")))
(assert-true '(jtags-right-package-p "C:\\java\\jdk1.6.0\\src\\java\\lang\\Double.java"
                                     '("java.awt.Image" "java.io.*" "java.lang.*")))
(assert-true '(jtags-right-package-p "/usr/local/jdk/src/java/lang/Double.java"
                                     '("java.awt.Image" "java.io.*" "java.lang.*")))

;;; jtags-update-tags-file

(assert-true (jtags-update-tags-file "c:/java/junit4.8.2/src"))
(assert-true (jtags-update-tags-file "c:/java/junit4.8.2/src/A_TAGS_FILE"))

;;; jtags-find-identifier-backward

;; >>> TEST DATA
foo
foo().bar
System.out.println
"println"
;; toString
Class.
Double::
this::foo
for (String myString : strings
;; <<< TEST DATA

(assert-equal '"foo"
              '(progn (goto-test ";;; jtags-find-identifier-backward" 3 't) (jtags-find-identifier-backward)))
(assert-equal '"bar"
              '(progn (goto-test ";;; jtags-find-identifier-backward" 4 't) (jtags-find-identifier-backward)))
(assert-equal '"println"
              '(progn (goto-test ";;; jtags-find-identifier-backward" 5 't) (jtags-find-identifier-backward)))
(assert-equal '"#stringliteral#"
              '(progn (goto-test ";;; jtags-find-identifier-backward" 6 't) (jtags-find-identifier-backward)))
(assert-equal '"toString"
              '(progn (goto-test ";;; jtags-find-identifier-backward" 7 't) (jtags-find-identifier-backward)))
(assert-equal '"Class"
              '(progn (goto-test ";;; jtags-find-identifier-backward" 8 't) (jtags-find-identifier-backward)))
(assert-equal '"Double"
              '(progn (goto-test ";;; jtags-find-identifier-backward" 9 't) (jtags-find-identifier-backward)))
(assert-equal '"foo"
              '(progn (goto-test ";;; jtags-find-identifier-backward" 10 't) (jtags-find-identifier-backward)))
(assert-equal '"strings"
              '(progn (goto-test ";;; jtags-find-identifier-backward" 11 't) (jtags-find-identifier-backward)))

;;; jtags-in-literal

;; >>> TEST DATA
foo().bar
"println"
;; toString
//
/*
;; <<< TEST DATA

(assert-equal nil
              '(progn (goto-test ";;; jtags-in-literal" 3 't) (backward-char) (jtags-in-literal)))
(assert-equal 'string
              '(progn (goto-test ";;; jtags-in-literal" 4 't) (backward-char) (jtags-in-literal)))
(assert-equal 'comment
              '(progn (goto-test ";;; jtags-in-literal" 5 't) (backward-char) (jtags-in-literal)))
(assert-equal 'comment
              '(progn (goto-test ";;; jtags-in-literal" 6 't) (backward-char) (jtags-in-literal)))
(assert-equal 'comment
              '(progn (goto-test ";;; jtags-in-literal" 7 't) (backward-char) (jtags-in-literal)))

;;; jtags-parse-java-line

;; >>> TEST DATA
toString().substring(1, 2).length
aa_a .	bb_b()  .   cc_c(pp1, qq2).ddd.ee_e
"jtags" . toString
System.out.println(System.getProperty
Class.forName
"println"
;; toString().toUpperCase
foo[17].toString
Class.
;; This comment ends with a period.
foo.split
;; This is also a comment
bar.split
;; The next expression is split over two lines
aaa().bbb().
ccc().ddd
this::methodRef
Double::valueOf
for (String myString : strings
;; <<< TEST DATA

(assert-equal '("toString" "substring" "length")
              '(progn (goto-test ";;; jtags-parse-java-line" 3 't) (jtags-parse-java-line)))
(assert-equal '("aa_a" "bb_b" "cc_c" "ddd" "ee_e")
              '(progn (goto-test ";;; jtags-parse-java-line" 4 't) (jtags-parse-java-line)))
(assert-equal '("#stringliteral#" "toString")
              ' (progn (goto-test ";;; jtags-parse-java-line" 5 't) (jtags-parse-java-line)))
(assert-equal '("System" "getProperty")
              '(progn (goto-test ";;; jtags-parse-java-line" 6 't) (jtags-parse-java-line)))
(assert-equal '("Class" "forName")
              '(progn (goto-test ";;; jtags-parse-java-line" 7 't) (jtags-parse-java-line)))
(assert-equal '("#stringliteral#")
              '(progn (goto-test ";;; jtags-parse-java-line" 8 't) (jtags-parse-java-line)))
(assert-equal '("toString" "toUpperCase")
              '(progn (goto-test ";;; jtags-parse-java-line" 9 't) (jtags-parse-java-line)))
(assert-equal '("foo" "toString")
              '(progn (goto-test ";;; jtags-parse-java-line" 10 't) (jtags-parse-java-line)))
(assert-equal '("Class")
              '(progn (goto-test ";;; jtags-parse-java-line" 11 't) (jtags-parse-java-line)))
(assert-equal '("foo" "split")
              '(progn (goto-test ";;; jtags-parse-java-line" 13 't) (jtags-parse-java-line)))
(assert-equal '("bar" "split")
              '(progn (goto-test ";;; jtags-parse-java-line" 15 't) (jtags-parse-java-line)))
(assert-equal '("aaa" "bbb" "ccc" "ddd")
              '(progn (goto-test ";;; jtags-parse-java-line" 18 't) (jtags-parse-java-line)))
(assert-equal '("this" "methodRef")
              '(progn (goto-test ";;; jtags-parse-java-line" 19 't) (jtags-parse-java-line)))
(assert-equal '("Double" "valueOf")
              '(progn (goto-test ";;; jtags-parse-java-line" 20 't) (jtags-parse-java-line)))
(assert-equal '("strings")
              '(progn (goto-test ";;; jtags-parse-java-line" 21 't) (jtags-parse-java-line)))

;;; jtags-find-package

;; >>> TEST DATA
;; package foo.bar.util.html;
package foo.bar.util.config;

import java.util.HashMap;
"import java.util.List;"

import foo.bar.event.EventHandler;
 import foo.bar.server.base.ConnectionMgr   ;
import foo.bar.server.management .*;
	import foo.bar.util.  connection.  *  ;
import foo.bar.util.connection.*;

import static java.util.stream.Collectors.toList;
;; <<< TEST DATA

(assert-equal "foo.bar.util.config"
              '(jtags-find-package))

;;; jtags-find-imports (reuses test data from jtags-find-package)

(assert-equal '("foo.bar.util.config.*"
                "java.lang.*"
                "java.util.HashMap"
                "foo.bar.event.EventHandler"
                "foo.bar.server.base.ConnectionMgr"
                "foo.bar.server.management.*"
                "foo.bar.util.connection.*"
                "static java.util.stream.Collectors.toList")
              '(jtags-find-imports))
(assert-equal '("foo.bar.axe.*"
                "java.lang.*"
                "java.util.HashMap"
                "foo.bar.event.EventHandler"
                "foo.bar.server.base.ConnectionMgr"
                "foo.bar.server.management.*"
                "foo.bar.util.connection.*"
                "static java.util.stream.Collectors.toList")
              '(jtags-find-imports "foo.bar.axe"))

;;; jtags-extras-match-index

(assert-equal 0   '(jtags-extras-match-index '("^java\\.") "java.util.*"))
(assert-equal nil '(jtags-extras-match-index '("^java\\.") "javax.swing.JList"))
(assert-equal 1   '(jtags-extras-match-index '("^java\\." "^javax\\.") "javax.swing.JList"))
(assert-equal 0   '(jtags-extras-match-index '("^java\\." "^javax\\.") "java.util.List"))
(assert-equal 3   '(jtags-extras-match-index '("^java\\." "^javax\\." "-" "^org\\.") "org.xml.sax.*"))
(assert-equal nil '(jtags-extras-match-index '("^java\\." "^javax\\." "-" "^org\\.") "com.foo.bar.*"))
(assert-equal 3   '(jtags-extras-match-index '("^java\\." "^javax\\." "-" "^static ") "static java.util.Arrays.asList"))

;;; jtags-extras-sort-import-predicate

(assert-true  '(let ((jtags-extras-import-order-list '("^java\\.")))
                 (jtags-extras-sort-import-predicate "java.util.*" "javax.swing.*")))
(assert-false '(let ((jtags-extras-import-order-list '("^java\\.")))
                 (jtags-extras-sort-import-predicate "javax.swing.*" "java.util.*")))

(assert-true  '(let ((jtags-extras-import-order-list '("^java\\." "^javax\\.")))
                 (jtags-extras-sort-import-predicate "java.util.*" "javax.swing.*")))
(assert-false '(let ((jtags-extras-import-order-list '("^java\\." "^javax\\.")))
                 (jtags-extras-sort-import-predicate "javax.swing.*" "java.util.*")))

(assert-true  '(let ((jtags-extras-import-order-list '("^java\\." "^javax\\." "-" "^org\\.")))
                 (jtags-extras-sort-import-predicate "javax.swing.*" "org.xml.sax.*")))
(assert-false '(let ((jtags-extras-import-order-list '("^java\\." "^javax\\." "-" "^org\\.")))
                 (jtags-extras-sort-import-predicate "org.xml.sax.*" "javax.swing.*")))

(assert-true  '(let ((jtags-extras-import-order-list '("^java\\." "^javax\\." "-" "^org\\.")))
                 (jtags-extras-sort-import-predicate "org.xml.sax.*" "com.foo.bar.*")))
(assert-false '(let ((jtags-extras-import-order-list '("^java\\." "^javax\\." "-" "^org\\.")))
                 (jtags-extras-sort-import-predicate "com.foo.bar.*" "org.xml.sax.*")))

(assert-true  '(let ((jtags-extras-import-order-list '("^java\\." "^javax\\." "-" "^org\\.")))
                 (jtags-extras-sort-import-predicate "javax.swing.*" "zzzzz")))
(assert-false '(let ((jtags-extras-import-order-list '("^java\\." "^javax\\." "-" "^org\\.")))
                 (jtags-extras-sort-import-predicate "zzzzz" "javax.swing.*")))

(assert-true  '(let ((jtags-extras-import-order-list '("^java\\." "^javax\\." "-" "^org\\.")))
                 (jtags-extras-sort-import-predicate "java.sql.*" "java.text.*")))
(assert-false '(let ((jtags-extras-import-order-list '("^java\\." "^javax\\." "-" "^org\\.")))
                 (jtags-extras-sort-import-predicate "java.text.*" "java.sql.*")))

(assert-true  '(let ((jtags-extras-import-order-list '("^java\\." "^javax\\." "-" "^org\\.")))
                 (jtags-extras-sort-import-predicate "javax.swing.JButton" "javax.swing.JList")))
(assert-false '(let ((jtags-extras-import-order-list '("^java\\." "^javax\\." "-" "^org\\.")))
                 (jtags-extras-sort-import-predicate "javax.swing.JList" "javax.swing.JButton")))

;; ----------------------------------------------------------------------------

;;; jtags-test.el ends here
