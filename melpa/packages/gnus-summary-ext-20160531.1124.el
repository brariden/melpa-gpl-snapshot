;;; gnus-summary-ext.el --- Extra limit and process mark commands for the gnus summary buffer

;; Filename: gnus-summary-ext.el
;; Description: Extra limit and process mark commands for the gnus summary buffer
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-12-23 00:06:16
;; Version: 1.0
;; Package-Version: 20160531.1124
;; Last-Updated: 2016-05-26 02:40:00
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/gnus-summary-ext
;; Keywords: comm
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ()
;;
;; Features that might be required by this library:
;;
;; gnus, cl
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 16mhw12vvtzCwQyvvLAqCSBvf8i41CfKhK
;;
;; This library provides extra commands for filtering and applying process marks to
;; articles in the gnus summary buffer, some commands for performing actions on MIME
;; parts in articles, and some general functions for evaluating elisp code in marked articles.

;; You can apply complex filters for filtering the messages displayed in the *Summary* buffer
;; using `gnus-summary-ext-limit-filter', or apply the process mark to such articles with
;; `gnus-summary-ext-uu-mark-filter'.
;; You can save these filters in `gnus-summary-ext-saved-filters'.

;; See the documentation of the individual commands & functions for more
;; details.
;;;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `gnus-summary-ext-limit-to-mime-type'
;;    Limit the summary buffer to articles containing MIME parts with types matching REGEX.
;;  `gnus-summary-ext-limit-to-num-parts'
;;    Limit the summary buffer to articles containing between MIN & MAX attachments.
;;  `gnus-summary-ext-limit-to-size'
;;    Limit the summary buffer to articles of size between MIN and MAX bytes.
;;  `gnus-summary-ext-limit-to-filename'
;;    Limit the summary buffer to articles containing attachments with names matching REGEX.
;;  `gnus-summary-ext-limit-filter'
;;    Limit the summary buffer to articles which match filter expression.
;;  `gnus-summary-ext-uu-mark-filter' 
;;    Apply process mark to all articles in the summary buffer which match filter expression.
;;  `gnus-summary-ext-apply-to-marked-safely'
;;    Evaluate any lisp expression for all articles that are process/prefixed.
;;  `gnus-summary-ext-apply-to-marked'
;;    Evaluate any lisp expression for all articles that are process/prefixed.
;;  `gnus-summary-ext-act-on-parts-in-marked'
;;    Do something with all MIME parts in articles that are process/prefixed.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `gnus-summary-ext-saved-filters'
;;    An alist of named filters that can be used with `gnus-summary-ext-limit-filter'.
;;    default = nil

;;; Installation:
;;
;; Put gnus-summary-ext.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'gnus-summary-ext)


;;; Change log:
;;	
;; 2013/12/23
;;      * First released.
;;
;; See the github repo

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; 
;;

;; NOTE: for debugging you can use (gnus-summary-article-number) to get the article number of the
;; article on the current line of the *Summary* mode buffer.

;;; Require
(require 'gnus)
(eval-when-compile 'cl)

;;; Code:

;; simple-call-tree-info: DONE
(defgroup gnus-summary-ext nil
  "Gnus summary extension"
  :group 'gnus-summary)

;; simple-call-tree-info: DONE
(defcustom gnus-summary-ext-saved-filters nil
  "A list of named filters that can be used with `gnus-summary-ext-limit-filter'.
Each element has the form (NAME ARGLIST EXPRESSION [EXPRESSION ...]).

NAME is a symbol naming the filter.

ARGLIST is a list whose elements have the form (ARGUMENT
DEFAULT-VALUE). These variables are available when evaluating
the expressions.

EXPRESSION are elisp forms. They are wrapped in a `progn' and
compose the body of the filter. This body is executed when the
filter is called by name --e.g. (filter)-- as part of
`gnus-summary-ext-limit-filter' (which see)."
  :group 'gnus-summary-ext
  :type  '(repeat (cons (symbol :tag "Filter name")
                        (cons
                         (repeat :tag "Argument list"
                                 (list (symbol :tag "Argument name")
                                       (sexp :tag "Default value")))
                         (repeat (sexp :tag "Expression"))))))

;; simple-call-tree-info: DONE  
(defun gnus-summary-ext-match-mime-types (regex)
  "Return list of MIME media types matching REGEX."
  (remove-if-not (lambda (x) (string-match regex x))
                 (mailcap-mime-types)))


;;;###autoload
;; simple-call-tree-info: DONE  
(defun gnus-summary-ext-limit-to-mime-type (regex &optional reverse)
  "Limit the summary buffer to articles containing MIME parts with types matching REGEX.
If REVERSE (the prefix), limit to articles that don't match."
  (interactive "sMatch MIME type (regexp): \nP")
  (gnus-summary-limit-to-bodies
   (concat "Content-Type: " 
           (regexp-opt (gnus-summary-ext-match-mime-types regex))) reverse))

;; simple-call-tree-info: CHECK
(defmacro gnus-summary-ext-iterate-articles-safely-1 (articles &rest body)
  "Disable all relevant gnus hooks and loop over all ARTICLES performing BODY for each one.
Within BODY you can use the variable `article' to reference the current article number."
  `(let ((gnus-select-article-hook nil)	;Disable hook.
         (gnus-article-prepare-hook nil)
         (gnus-use-article-prefetch nil)
         (gnus-keep-backlog nil)
         (gnus-break-pages nil)
         (gnus-summary-display-arrow nil)
         (gnus-updated-mode-lines nil)
         (gnus-auto-center-summary nil)
         (gnus-display-mime-function nil)
         (gnus-mark-article-hook nil))
     (dolist (article ,articles)
       ,@body)))

;; simple-call-tree-info: CHECK
(defmacro gnus-summary-ext-iterate-articles-safely (articles &rest body)
  "Loop over all ARTICLES and perform BODY within each article buffer.
Within BODY you can use the variable `article' to reference the current article number.
All hooks will be disabled before selecting each article."
  `(gnus-summary-ext-iterate-articles-safely-1
    ,articles
    (gnus-summary-select-article t t nil article)
    (with-current-buffer gnus-article-buffer
      ,@body)))

;;;###autoload
;; simple-call-tree-info: DONE  
(defun gnus-summary-ext-apply-to-marked-safely (arg sexp)
  "Evaluate any lisp expression for all articles that are process/prefixed.
If no articles are marked use the article at point or articles in region, 
and if ARG is non-nil include that many articles forward (if positive) or 
backward (if negative) from the current article. 
This will evaluate SEXP after selecting each article, but will not run any hooks.

See `gnus-summary-apply-to-marked' if you want to run the appropriate hooks after
selecting each article, and see `gnus-summary-iterate' for iterating over articles
without selecting them."
  (interactive "P\nxLisp expression: ")
  (gnus-summary-ext-iterate-articles-safely
   (gnus-summary-work-articles arg)
   (article-goto-body)
   (let (gnus-newsgroup-processable)
     (eval sexp))
   (gnus-summary-remove-process-mark article))
  (gnus-summary-position-point))

;;;###autoload
;; simple-call-tree-info: DONE  
(defun gnus-summary-ext-apply-to-marked (arg sexp)
  "Evaluate any lisp expression for all articles that are process/prefixed.
If no articles are marked use the article at point or articles in region, 
and if ARG is non-nil include that many articles forward (if positive) or 
backward (if negative) from the current article. 
This will evaluate SEXP after selecting each article, and running any hooks.

See `gnus-summary-ext-apply-to-marked-safely' for selecting each article without running hooks,
and see `gnus-summary-iterate' for iterating over articles without selecting them."
  (interactive "P\nxLisp expression: ")
  (dolist (article (gnus-summary-work-articles arg))
    (gnus-summary-select-article t t nil article)
    (with-current-buffer gnus-article-buffer
      (article-goto-body)
      (eval sexp))))

;; simple-call-tree-info: CHECK
(defun gnus-summary-ext-count-parts nil
  "Count the number of parts in an article.
This must be called from within the *Article* buffer."
  (- (length (or gnus-article-mime-handles
                 (mm-dissect-buffer nil gnus-article-loose-mime)
                 (and gnus-article-emulate-mime
                      (mm-uu-dissect)))) 2))

;;;###autoload
;; simple-call-tree-info: DONE  
(defun gnus-summary-ext-limit-to-num-parts (min max &optional reverse)
  "Limit the summary buffer to articles containing between MIN & MAX attachments.
If MIN/MAX is nil then limit to articles with at most/least MAX/MIN attachments respectively.
If REVERSE (the prefix), limit to articles that don't match."
  (interactive (list (read-number "Min parts: " 1)
                     (read-number "Max parts: " 1000)
                     current-prefix-arg))
  (let ((min (or min 1))
        (max (or max 1000))
        articles)
    (gnus-summary-ext-iterate-articles-safely
     (mapcar 'car gnus-newsgroup-data)
     (article-goto-body)
     (let ((num (gnus-summary-ext-count-parts)))
       (when (and (>= num min) (<= num max))
         (push article articles))))
    (if (not articles)
        (message "No messages matched")
      (gnus-summary-limit articles)))
  (gnus-summary-position-point))

;;;###autoload
;; simple-call-tree-info: DONE  
(defun gnus-summary-ext-limit-to-size (min max &optional reverse)
  "Limit the summary buffer to articles of size between MIN and MAX bytes.
If MIN/MAX is nil then limit to sizes below/above MAX/MIN respectively.
If REVERSE (the prefix), limit to articles that don't match.

Note: the articles returned might not match the size constraints exactly, but it should be fairly close."
  (interactive (list (read-number "Min bytes: " 0)
                     (read-number "Max bytes: " 999999999999)
                     current-prefix-arg))
  (let ((min (or min -1))
        (max (or max 999999999999))
        articles)
    (gnus-summary-ext-iterate-articles-safely
     (mapcar 'car gnus-newsgroup-data)
     (article-goto-body)
     (let ((size (buffer-size)))
       (when (and (>= size min) (<= size max))
         (push article articles))))
    (if (not articles)
        (message "No messages matched")
      (gnus-summary-limit articles)))
    (gnus-summary-position-point))

;;;###autoload
;; simple-call-tree-info: DONE  
(defun gnus-summary-ext-limit-to-filename (regex &optional reverse)
  "Limit the summary buffer to articles containing attachments with names matching REGEX.
If REVERSE (the prefix), limit to articles that don't match.
Note: REGEX should match the whole filename, so you may need to put .* at the beginning and end."
  (interactive "sMatch filename (regexp): \nP")
  (gnus-summary-limit-to-bodies
   (concat "Content-Disposition: attachment; filename=" regex) reverse))

;;;###autoload
;; simple-call-tree-info: CHECK  
(cl-defun gnus-summary-ext-mime-action-on-parts (action &optional arg (pred t) noprompt noerror)
  "Perform ACTION on all MIME parts in the current buffer for which PRED evaluates to non-nil.
ARG is an optional argument for the ACTION function (a member of the cdr's of `gnus-mime-action-alist').
PRED should be a form that evaluates to non-nil for the parts to be acted on (by default PRED
is t, and so all parts are acted on).
PRED will be placed within a let form where handle is bound to the handle for the part,
size is the number of chars in the part, type is the MIME type (e.g. \"image/png\"),
subtype is the subtype (e.g. \"png\"), supertype is the supertype (e.g. \"image\"),
and filename is the filename.

The optional arguments NOPROMPT and NOERROR if non-nil will ignore prompts and errors respectively."
  (interactive
   (let* ((action (let ((name (gnus-completing-read "Action" (mapcar 'car gnus-mime-action-alist) t)))
		    (cdr (assoc name gnus-mime-action-alist))))
          (msg "Variables available in lisp expression:
handle = handle for part, size = No. of chars in part, type = MIME type (e.g. \"image/png\")
subtype = subtype (e.g. \"png\"), supertype = supertype (e.g. \"image\"),
filename = the name of the attached file

Lisp expression %s: ")
          (noprompt (y-or-n-p "Ignore prompts?"))
          (noerror (y-or-n-p "Ignore errors?"))
	  (arg2 (if (and noprompt
                         (memq action '(gnus-mime-save-part
					gnus-mime-save-part-and-strip gnus-mime-replace-part
					gnus-mime-pipe-part gnus-mime-view-part-as-type)))
                    (let ((val (read-from-minibuffer
                                (format msg
                                        (cond 
                                         ((memq action '(gnus-mime-save-part gnus-mime-save-part-and-strip))
                                          "evaluating to filepath to save part to")
                                         ((eq action gnus-mime-replace-part)
					  "evaluating to filepath for replacement")
                                         ((eq action gnus-mime-pipe-part)
					  "evaluating to command")
                                         ((eq action gnus-mime-view-part-as-type)
					  "evaluating to type")))
                                nil nil nil 'read-expression-history)))
                      (if (equal val "") nil (read val)))))
          (pred (let ((val (read-from-minibuffer
                            (format msg "matching parts (default matches all parts)")
                            nil nil nil 'read-expression-history)))
                  (if (equal val "") t (read val)))))
     (list action arg2 pred noprompt noerror)))
  (gnus-article-check-buffer)
  (let* ((action-pair (rassq action gnus-mime-action-alist))
         (n 2))
    (if action-pair
        (while (gnus-article-goto-part n)
          (let* ((handle (get-text-property (point) 'gnus-data))
                 (size (buffer-size (mm-handle-buffer handle)))
                 (type (mm-handle-media-type handle))
                 (subtype (mm-handle-media-subtype handle))
                 (supertype (mm-handle-media-supertype handle))
                 (filename (mm-handle-filename handle))
                 (gnus-expert-user noprompt))
            (if (eval pred)
                (condition-case err
                    (if arg
                        (funcall (cdr action-pair) (eval arg))
                      (funcall (cdr action-pair)))
                  (error (if noerror
                             (message "Error trying to apply action %s on part %d"
				      (car action-pair) n)
                           (signal (car err) (cdr err)))))))
          (setq n (1+ n))))))

;;;###autoload
;; simple-call-tree-info: CHECK  
(cl-defun gnus-summary-ext-act-on-parts-in-marked (arg &optional action arg2 (pred t) noprompt noerror)
  "Do something with all MIME parts in articles that are process/prefixed.
If ARG is non-nil or a prefix arg is supplied it indicates how many articles forward (if positive) or 
backward (if negative) from the current article to include. Otherwise if region is active, process
the articles within the region, otherwise process the process marked articles.
Only MIME parts for which PRED evaluates to non-nil will be acted on.
See `gnus-summary-ext-mime-action-on-parts' for a description of the ACTION, PRED, NOPROMPT,
and NOERROR args.
This command just applies that function to the articles."
  (interactive
   (let* ((action (let ((name (gnus-completing-read
			       "Action"
			       (mapcar 'car gnus-mime-action-alist) t)))
		    (cdr (assoc name gnus-mime-action-alist))))
	  (msg "Variables available in lisp expression:
handle = handle for part, size = No. of chars in part, type = MIME type (e.g. \"image/png\")
subtype = subtype (e.g. \"png\"), supertype = supertype (e.g. \"image\"),
filename = the name of the attached file

Lisp expression %s: ")
	  (noprompt (y-or-n-p "Ignore prompts?"))
	  (noerror (y-or-n-p "Ignore errors?"))
	  (arg2 (if (and noprompt
			 (memq action '(gnus-mime-save-part
					gnus-mime-save-part-and-strip gnus-mime-replace-part
					gnus-mime-pipe-part gnus-mime-view-part-as-type)))
		    (let ((val (read-from-minibuffer
				(format msg
					(cond 
					 ((memq action '(gnus-mime-save-part gnus-mime-save-part-and-strip))
					  "evaluating to filepath to save part to")
					 ((eq action gnus-mime-replace-part)
					  "evaluating to filepath for replacement")
					 ((eq action gnus-mime-pipe-part)
					  "evaluating to command")
					 ((eq action gnus-mime-view-part-as-type)
					  "evaluating to type")))
				nil nil nil 'read-expression-history)))
		      (if (equal val "") nil (read val)))))
	  (pred (let ((val (read-from-minibuffer
			    (format msg "matching parts (default matches all parts)")
			    nil nil nil 'read-expression-history)))
		  (if (equal val "") t (read val)))))
     (list current-prefix-arg action arg2 pred noprompt noerror)))
  (gnus-summary-ext-apply-to-marked
   arg `(gnus-summary-ext-mime-action-on-parts ',action ',arg2 ',pred ,noprompt ,noerror)))

;;;###autoload
;; simple-call-tree-info: DONE
(defun gnus-summary-ext-fetch-field (field-regex &optional last all list)
  "Same as `mail-fetch-field' but match field name by regular expression instead of string.
FIELD-REGEX is a regular expression matching the field name, LAST ALL and LIST are the
same as in `mail-fetch-field'."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (name (concat "^" field-regex "[ \t]*:[ \t]*")))
      (if (or all list)
	  (let ((value (if all "")))
	    (while (re-search-forward name nil t)
	      (let ((opoint (point)))
		(while (progn (forward-line 1)
			      (looking-at "[ \t]")))
		;; Back up over newline, then trailing spaces or tabs
		(forward-char -1)
		(skip-chars-backward " \t" opoint)
		(if list
		    (setq value (cons (buffer-substring-no-properties
				       opoint (point))
				      value))
		  (setq value (concat value
				      (if (string= value "") "" ", ")
				      (buffer-substring-no-properties
				       opoint (point)))))))
	    (if list
		value
	      (and (not (string= value "")) value)))
	(if (re-search-forward name nil t)
	    (progn
	      (if last (while (re-search-forward name nil t)))
	      (let ((opoint (point)))
		(while (progn (forward-line 1)
			      (looking-at "[ \t]")))
		;; Back up over newline, then trailing spaces or tabs
		(forward-char -1)
		(skip-chars-backward " \t" opoint)
		(buffer-substring-no-properties opoint (point)))))))))

;;;###autoload
;; simple-call-tree-info: DONE
(defun gnus-summary-ext-field-value (header-regex &optional not-all)
  "The same as `message-fetch-value', but match field name by regular expression instead of string.
HEADER-REGEX is a regular expression matching the header name.
If NOT-ALL is non-nil then only the first matching header is returned."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers-or-head)
      (let* ((inhibit-point-motion-hooks t)
	     (value (gnus-summary-ext-fetch-field header-regex nil (not not-all))))
	(when value
	  (while (string-match "\n[\t ]+" value)
	    (setq value (replace-match " " t t value)))
	  value)))))

;;;###autoload
;; simple-call-tree-info: DONE
(defun gnus-summary-ext-filter (expr)
  "Return list of article numbers of articles in summary buffer which match EXPR.
EXPR can be any elisp form to be eval'ed for each article which returns non-nil for required articles.
It can utilize named filters stored in `gnus-summary-ext-saved-filters' (which should be surrounded
in parentheses, e.g: (filter)), and any of the following functions:

 (subject REGEXP) : matches articles with subject field matching REGEXP
 (from REGEXP) : matches articles with from field matching REGEXP 
 (to REGEXP) : matches articles with To: field matching REGEXP
 (cc REGEXP) : matches articles with Cc: field matching REGEXP
 (recipient REGEXP) : matches articles with To: or Cc: field matching REGEXP
 (address REGEXP) : matches articles with To:, Cc: or From: field matching REGEXP
 (read) : matches articles that have been read
 (unread) : matches articles that haven't yet been read (equivalent to (not (read)))
 (replied) : matches articles which have been replied to 
 (unreplied) : matches articles which haven't been replied to (equivalent to (not (replied)))
 (age DAYS) : matches articles received before/after DAYS days ago (see `gnus-summary-limit-to-age')
 (agebetween MIN MAX) : matches articles received between MIN and MAX days ago.
 (marks STR) : matches articles with marks in STR (see `gnus-summary-limit-to-marks')

The following functions can also be used but will be much slower since they are evaluated after selecting
each article:

 (witharticle PRED)     : matches articles for which PRED returns non-nil after selecting article buffer
 (withorigarticle PRED) : matches articles for which PRED returns non-nil after selecting original (unformatted) article buffer
 (content REGEXP)  : matches articles containing text that matches REGEXP 
 (header HDRX REGEXP) : matches articles containing a header matching HDRX whose value matches REGEXP
 (filename REGEXP) : matches articles containing file attachments whose names match REGEXP
 (mimetype REGEXP) : matches articles containing mime parts with type names matching REGEXP
 (numparts MIN MAX) : matches articles with between MIN and MAX parts/attachments (inclusive).
                      Note: html and embedded images count as parts, and often there are several of these in an article.
 (size MIN MAX) : matches articles of approximate size between MIN & MAX bytes. 
                  If MAX is omitted then just check if size is bigger than MIN bytes

For example, to filter messages received within the last week, either from alice or sent to bob:
  (gnus-summary-ext-filter '(and (age -7) (or (from \"alice\") (to \"bob\"))))

To filter unreplied messages that are matched by either of the saved filters 'work' or 'friends':
  (gnus-summary-ext-filter '(and (unreplied) (or (work) (friends))))"
  (eval
   `(cl-flet* ((witharticle (pred) (gnus-summary-select-article t t nil article)
			    (with-current-buffer gnus-article-buffer (funcall pred)))
	       (withorigarticle (pred) (gnus-summary-select-article t t nil article)
				(with-current-buffer gnus-original-article-buffer (funcall pred)))
               (content (regexp) (witharticle (lambda nil
						(article-goto-body)
						(re-search-forward regexp nil t))))
	       (header (hdrx regexp) (withorigarticle (lambda nil
							(let ((str (gnus-summary-ext-field-value hdrx)))
							  (if str (string-match regexp str))))))
	       (from (regexp) (string-match regexp (mail-header-from hdr)))
	       (age (days) (let* ((younger (< days 0))
				  (days (abs days))
				  (date (gnus-date-get-time (mail-header-date hdr)))
				  (is-younger (time-less-p
					       (time-since date)
					       (days-to-time days))))
			     (if younger is-younger (not is-younger))))
	       (agebetween (min max) (and (age min) (not (age max))))
	       (marks (mrks) (let ((mrks (if (listp mrks) mrks (append mrks nil))))
			       (memq (gnus-data-mark data) mrks)))
	       (score (scr) (>= (gnus-summary-article-score article) scr))
	       (read nil (marks (list gnus-del-mark gnus-read-mark gnus-ancient-mark
				      gnus-killed-mark gnus-spam-mark gnus-kill-file-mark
				      gnus-low-score-mark gnus-expirable-mark
				      gnus-canceled-mark gnus-catchup-mark gnus-sparse-mark
				      gnus-duplicate-mark)))
	       (unread nil (not (read)))
	       (replied nil (memq article gnus-newsgroup-replied))
	       (unreplied nil (not (replied)))
	       (filename (regexp) (withorigarticle (lambda nil
						     (re-search-forward
						      (concat "Content-Disposition: attachment; filename=" regexp)
						      nil t))))
	       (mimetype (regexp)
			 (withorigarticle (lambda nil
					    (re-search-forward
					     (content (concat "Content-Type: "
							      (regexp-opt (gnus-summary-ext-match-mime-types regexp))))
					     nil t))))
	       (numparts (min &optional max) (witharticle (lambda nil
							    (let ((num (gnus-summary-ext-count-parts)))
							      (and (>= num min) (if max (<= num max) t))))))
	       (size (min &optional max) (witharticle (lambda nil 
							(let ((size (buffer-size)))
							  (and (>= size min) (if max (<= size max) t))))))
	       (subject (regexp) (string-match regexp (mail-header-subject hdr)))
	       (to (regexp) (string-match regexp (or (cdr (assoc 'To (mail-header-extra hdr))) "")))
	       (cc (regexp) (string-match regexp (or (cdr (assoc 'Cc (mail-header-extra hdr))) "")))
	       (recipient (regexp) (or (to regexp) (cc regexp)))
	       (address (regexp) (or (to regexp) (cc regexp) (from regexp)))
	       ,@(cl-loop for (name . code) in gnus-summary-ext-saved-filters
			  if (> (length code) 1)
			  collect `(,name (&optional ,@(car code)) ,@(cdr code))
			  else
			  collect (list name nil code)))
      (let (filtered)
	(gnus-summary-ext-iterate-articles-safely-1
	 (mapcar 'car gnus-newsgroup-data)
	 (let ((message-log-max nil))
	   (message "Checking article %s" article))
	 (let* ((data (assq article gnus-newsgroup-data))
		(hdr (gnus-data-header data)))
	   (when ,expr (push article filtered))))
	filtered))))

;;;###autoload
;; simple-call-tree-info: DONE
(defun gnus-summary-ext-limit-filter (expr)
  "Limit the summary buffer to articles which match EXPR.
EXPR can be any elisp form to be eval'ed for each article which returns non-nil for required articles.
It can utilize named filters stored in `gnus-summary-ext-saved-filters' (which should be surrounded
in parentheses, e.g: (filter)), and any of the builtin functions as described in `gnus-summary-ext-filter'."
  (interactive (list (read-from-minibuffer
		      "Available functions: (subject REGEX), (from REGEX), (to REGEX), (cc REGEX), (recipient REGEX), (address REGEX), (read), (unread), (replied), (unreplied), (age DAYS), (agebetween MIN MAX), (marks STR), (witharticle PRED), (withorigarticle PRED), (content REGEX), (header HDRX REGEX), (filename REGEX), (mimetype REGEX), (numparts MIN MAX), (size MIN MAX)
Filter expression (press up/down to see previous/saved filters): "
		      nil nil t 'read-expression-history
		      (mapcar (lambda (item) (concat "(" (symbol-name (car item)) ")"))
			      gnus-summary-ext-saved-filters))))
  (let ((filtered (gnus-summary-ext-filter expr)))
    (if (not filtered) (message "No messages matched"))
    (gnus-summary-limit filtered))
  (gnus-summary-position-point))

;;;###autoload
;; simple-call-tree-info: DONE
(defun gnus-summary-ext-uu-mark-filter (expr &optional arg)
  "Apply/remove process mark to all articles in the summary buffer which match EXPR.
If ARG is non-nil or a prefix arg is used then remove marks.
EXPR can be any elisp form to be eval'ed for each article which returns non-nil for required articles.
It can utilize named filters stored in `gnus-summary-ext-saved-filters' (which should be surrounded
in parentheses, e.g: (filter)), and any of the builtin functions as described in `gnus-summary-ext-filter'."
  (interactive (list (read-from-minibuffer
		      "Available functions: (subject REGEX), (from REGEX), (to REGEX), (cc REGEX), (recipient REGEX), (address REGEX), (read), (unread), (replied), (unreplied), (age DAYS), (agebetween MIN MAX), (marks STR), (witharticle PRED), (withorigarticle PRED), (content REGEX), (header HDRX REGEX), (filename REGEX), (mimetype REGEX), (numparts MIN MAX), (size MIN MAX)
Filter expression (press up/down to see previous/saved filters): "
		      nil nil t 'read-expression-history
		      (mapcar (lambda (item) (concat "(" (symbol-name (car item)) ")"))
			      gnus-summary-ext-saved-filters))
		     current-prefix-arg))
  (let ((filtered (gnus-summary-ext-filter expr)))
    (if (not filtered)
	(message "No messages matched")
      (if arg
	  (apply 'gnus-summary-remove-process-mark filtered)
	(dolist (num filtered)
	  (gnus-summary-set-process-mark num)))))
  (gnus-summary-position-point))

;;;###autoload
;; (defun gnus-summary-ext-extract-text (arg spec &optional postproc export convfn params)
;;   "Extract text from process marked according to SPEC."
;;   (interactive (append current-prefix-arg
;; 		       (extract-text-choose-prog)
;; 		       (extract-text-choose-export-args)))
;;   (let (results)
;;     (gnus-summary-ext-apply-to-marked-safely arg '()))

;;   (extract-text-process-results results postproc export convfn params)

;;   )




(provide 'gnus-summary-ext)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "gnus-summary-ext.el" (buffer-name) (buffer-string) "update")

;;; gnus-summary-ext.el ends here
