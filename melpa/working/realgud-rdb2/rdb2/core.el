;;; Copyright (C) 2016 Rocky Bernstein <rocky@gnu.org>
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

(eval-when-compile (require 'cl))

(require 'realgud)

(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-lang-mode? 'realgud-lang)
(declare-function realgud-parse-command-arg 'realgud-core)
(declare-function realgud-query-cmdline 'realgud-core)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:rdb2-minibuffer-history nil
  "minibuffer history list for the command `rdb2'.")

(easy-mmode-defmap realgud:rdb2-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun realgud:rdb2-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'realgud:rdb2-suggest-invocation
   realgud:rdb2-minibuffer-local-map
   'realgud:rdb2-minibuffer-history
   opt-debugger))

(defun realgud:rdb2-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ORIG_ARGS should contain a tokenized list of the command line to run.

We return the a list containing
* the name of the debugger given (e.g. rdb2) and its arguments - a list of strings
* nil (a placehoder in other routines of this ilk for a debugger
* the script name and its arguments - list of strings
* whether the annotate or emacs option was given ('-A', '--annotate' or '--emacs) - a boolean

For example for the following input
  (map 'list 'symbol-name
   '(rdb2 --tty /dev/pts/1 -cd ~ --emacs ./gcd.py a b))

we might return:
   ((\"rdb2\" \"--tty\" \"/dev/pts/1\" \"-cd\" \"home/rocky\' \"--emacs\") nil \"(/tmp/gcd.py a b\") 't\")

Note that path elements have been expanded via `expand-file-name'.
"

  ;; Parse the following kind of pattern:
  ;;  rdb2 rdb2-options script-name script-options
  (let (
	(args orig-args)
	(pair)          ;; temp return from

	;; One dash is added automatically to the below, so
	;; h is really -h and -host is really --host.
	(rdb2-two-args '("x" "-command" "b" "-exec"
			"cd" "-pid"  "-core" "-directory"
			"-annotate"
			"se" "-symbols" "-tty"))
	;; rdb2 doesn't optionsl 2-arg options.
	(rdb2-opt-two-args '())

	;; Things returned
	(script-name nil)
	(debugger-name nil)
	(debugger-args '())
	(script-args '())
	(annotate-p nil))

    (if (not (and args))
	;; Got nothing: return '(nil nil nil nil)
	(list debugger-args nil script-args annotate-p)
      ;; else
      (progn

	;; Remove "rdb2" from "rdb2 --rdb2-options script
	;; --script-options"
	(setq debugger-name (file-name-sans-extension
			     (file-name-nondirectory (car args))))
	(unless (string-match "^rdb2.*" debugger-name)
	  (message
	   "Expecting debugger name `%s' to be `rdb2'"
	   debugger-name))
	(setq debugger-args (list (pop args)))

	;; Skip to the first non-option argument.
	(while (and args (not script-name))
	  (let ((arg (car args)))
	    (cond
	     ;; Annotation or emacs option with level number.
	     ((or (member arg '("--annotate" "-A"))
		  (equal arg "--emacs"))
	      (setq annotate-p t)
	      (nconc debugger-args (list (pop args) (pop args))))
	     ;; Combined annotation and level option.
	     ((string-match "^--annotate=[0-9]" arg)
	      (nconc debugger-args (list (pop args) (pop args)) )
	      (setq annotate-p t))
	     ;; path-argument ooptions
	     ((member arg '("-cd" ))
	      (setq arg (pop args))
	      (nconc debugger-args
		     (list arg (realgud:expand-file-name-if-exists
				(pop args)))))
	     ;; Options with arguments.
	     ((string-match "^-" arg)
	      (setq pair (realgud-parse-command-arg
			  args rdb2-two-args rdb2-opt-two-args))
	      (nconc debugger-args (car pair))
	      (setq args (cadr pair)))
	     ;; Anything else must be the script to debug.
	     (t (setq script-name arg)
		(setq script-args args))
	     )))
	(list debugger-args nil script-args annotate-p)))))

(defvar realgud:rdb2-command-name)

(defun realgud:rdb2-executable (file-name)
"Return a priority for wehther file-name is likely we can run rdb2 on"
  (let ((output (shell-command-to-string (format "file %s" file-name))))
    (cond
     ((string-match "ASCII" output) 2)
     ((string-match "ELF" output) 7)
     ((string-match "executable" output) 6)
     ('t 5))))


(defun realgud:rdb2-suggest-invocation (&optional debugger-name)
  "Suggest a rdb2 command invocation. Here is the priority we use:
* an executable file with the name of the current buffer stripped of its extension
* any executable file in the current directory with no extension
* the last invocation in rdb2:minibuffer-history
* any executable in the current directory
When all else fails return the empty string."
  (let* ((file-list (directory-files default-directory))
	 (priority 2)
	 (best-filename nil)
	 (try-filename (file-name-base (or (buffer-file-name) "rdb2"))))
    (when (member try-filename (directory-files default-directory))
	(setq best-filename try-filename)
	(setq priority (+ (realgud:rdb2-executable try-filename) 2)))

    ;; FIXME: I think a better test would be to look for
    ;; c-mode in the buffer that have a corresponding executable
    (while (and (setq try-filename (car-safe file-list)) (< priority 8))
      (setq file-list (cdr file-list))
      (if (and (file-executable-p try-filename)
	       (not (file-directory-p try-filename)))
	  (if (equal try-filename (file-name-sans-extension try-filename))
	      (progn
		(setq best-filename try-filename)
		(setq priority (1+ (realgud:rdb2-executable best-filename))))
	    ;; else
	    (progn
	      (setq best-filename try-filename)
	      (setq priority (realgud:rdb2-executable best-filename))
	      ))
	))
    (if (< priority 8)
	(cond
	 (realgud:rdb2-minibuffer-history
	  (car realgud:rdb2-minibuffer-history))
	 ((equal priority 7)
	  (concat "rdb2 " best-filename))
	 (t "rdb2 "))
      ;; else
      (concat "rdb2 " best-filename))
    ))

(defun realgud:rdb2-reset ()
  "Rdb2 cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (rdb2-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*rdb2-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun rdb2-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'rdb2-debugger-support-minor-mode minor-mode-map-alist)
;; 	  rdb2-debugger-support-minor-mode-map-when-deactive))


(defun realgud:rdb2-customize ()
  "Use `customize' to edit the settings of the `realgud:rdb2' debugger."
  (interactive)
  (customize-group 'realgud:rdb2))

(provide-me "realgud:rdb2-")
