;; Copyright (C) 2016 Rocky Bernstein

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

;;  `realgud:rdb2' Main interface to rdb2 via Emacs
(require 'cl)
(require 'realgud)

(require 'load-relative)
(require-relative-list '("core" "track-mode") "realgud:rdb2-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:rdb2 nil
  "The realgud interface to Ruby debugger2 (AKA rdb2)"
  :group 'realgud
  :version "24.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:rdb2-command-name
  "rdb2"
  "File name for executing the and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:rdb2)

(declare-function realgud:rdb2-track-mode     'realgud:rdb2-track-mode)
(declare-function realgud-command            'realgud:rdb2-core)
(declare-function realgud:rdb2-parse-cmd-args 'realgud:rdb2-core)
(declare-function realgud:rdb2-query-cmdline  'realgud:rdb2-core)
(declare-function realgud:run-process        'realgud-core)
(declare-function realgud:flatten            'realgud-utils)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:rdb2 (&optional opt-cmd-line no-reset)
  "Invoke the rdb2 debugger and start the Emacs user interface.

OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"

  (interactive)
  (realgud:run-debugger "rdebug" 'rdebug-query-cmdline
			'rdebug-parse-cmd-args
			'realgud:rdebug-minibuffer-history
			opt-cmd-line no-reset)
  )

(provide-me "realgud-")

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
