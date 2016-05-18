;;; go-impl.el --- impl integration for go-mode

;; Copyright (C) 2016 Dominik Honnef

;; Author: Dominik Honnef <dominik@honnef.co>
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(defgroup go-impl nil
  "impl integration for go-mode."
  :group 'go)

(defcustom go-impl-command "impl"
  "The 'impl' command."
  :type 'string
  :group 'go-impl)

(defcustom go-impl-aliases-alist nil
  "List of aliases for interface names"
  :type '(alist :key-tpe (string) :value-type (string))
  :group 'go-impl)

(defcustom go-impl-enter-function nil
  "Move point into the first inserted function."
  :type 'boolean
  :group 'go-impl)

;;;###autoload
(defun go-impl (recv iface)
  "Run the impl tool with RECV and IFACE and insert the result at point.

If `go-impl-aliases-alist' has an entry with IFACE as the key,
that entry's value will be used in place of IFACE. This allows
creating aliases such as \"sort\" for \"sort.Interface\" or
\"string\" for \"fmt.Stringer\".

If `go-impl-enter-function' is non-nil, point will move inside
the first inserted function.

See https://github.com/josharian/impl to obtain the tool.
"

  (interactive "MReceiver: \nMInterface: ")
  (setq iface (or (cdr (assoc iface go-impl-aliases-alist))
                  iface))
  (shell-command (format "%s %s %s"
                         go-impl-command
                         (shell-quote-argument recv)
                         (shell-quote-argument iface))
                 t)
  (when go-impl-enter-function
    (forward-line)
    (back-to-indentation)))

(provide 'go-impl)

;;; go-impl.el ends here
