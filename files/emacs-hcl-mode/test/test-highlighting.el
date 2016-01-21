;;; test-highlighting.el --- test for highlighting of hcl-mode

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(require 'ert)
(require 'hcl-mode)

(ert-deftest boolean-keywords ()
  "Syntax highlight of `boolean' keywords"

  (dolist (keyword '("true" "false" "on" "off" "yes" "no"))
    (with-hcl-temp-buffer
      keyword
      (should (face-at-cursor-p 'font-lock-constant-face)))))

(ert-deftest assignment-statement ()
  "Syntax highlight of assignment statement"
  (with-hcl-temp-buffer
    "
foo = \"var\"
"

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-variable-name-face)))

  (with-hcl-temp-buffer
    "
foo=\"var\"
"

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-variable-name-face)))

  (with-hcl-temp-buffer
    "
    foo=      \"var\"
"

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-variable-name-face)))

  (with-hcl-temp-buffer
    "
output \"name\" {
   bar = \"baz\"
   map {
       hoge = \"${bar}\"
   }
}
"

    (forward-cursor-on "bar")
    (should (face-at-cursor-p 'font-lock-variable-name-face))

    (forward-cursor-on "hoge")
    (should (face-at-cursor-p 'font-lock-variable-name-face))))

(ert-deftest string-interpolation ()
  "Syntax highlight of string interpolation"
  (with-hcl-temp-buffer
    "
foo = \"hello world\"
bar = \"${foo}\"
"

    (forward-cursor-on "{foo}")
    (forward-char 1)
    (should (face-at-cursor-p 'font-lock-variable-name-face))))

(ert-deftest single-line-comment ()
  "Syntax highlight of single line comment"

  (with-hcl-temp-buffer
    "# foo" ;; start from beginning of line

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-comment-face)))


  (with-hcl-temp-buffer
    "  bar baz # foo  " ;; start from not beginning of line

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-comment-face))))

(ert-deftest multiple-line-comment ()
  "Syntax highlight of multiple line comment"

  (with-hcl-temp-buffer
    "/* foo */"

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-comment-face)))

  (with-hcl-temp-buffer
    "
/*
 foo **
 bar **
 /////
 baz ##
 */"

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-comment-face))

    (forward-cursor-on "bar")
    (should (face-at-cursor-p 'font-lock-comment-face))

    (forward-cursor-on "baz")
    (should (face-at-cursor-p 'font-lock-comment-face))))

;;; test-highlighting ends here