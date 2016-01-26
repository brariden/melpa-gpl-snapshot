;;; xquery-tool.el --- A simple interface to saxonb's xquery.

;; Copyright (C) 2015, 2016 Patrick McAllister

;; Author: Patrick McAllister <pma@rdorte.org>
;; Keywords: xml, xquery, emacs
;; Package-Version: 20160122.531
;; URL: https://github.com/paddymcall/xquery-tool.el

;; This program is free software: you can redistribute it and/or modify
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

;; This program lets you run an xquery against a file, via saxonb.

;; If the result contains nodes, it tries to link the results back to
;; the original file.

;; To use, customize the `xquery-tool-java-binary' and
;; `xquery-tool-saxonb-jar' settings (M-x customize-group RET
;; xquery-tool), and then call `xquery-tool-query' from a buffer
;; visiting an xml document.

;;; Code:

(require 'xmltok)
(require 'url-parse)
;; Try to load cl-lib, or cl.  Probably this is not a good idea.
;; (unless (require 'cl-lib nil t)
;;   (require 'cl))
(require 'cl-lib)

(defcustom xquery-tool-java-binary "/usr/bin/java"
  "Command name to invoke the Java Binary on your system."
  :group 'xquery-tool
  :type '(file))

;; (setq xquery-tool-java-binary "/usr/bin/java")

(defcustom xquery-tool-saxonb-jar "/usr/share/java/saxonb.jar"
  "Full path of the saxonb.jar on your system."
  :group 'xquery-tool
  :type '(file))

(defcustom xquery-tool-result-buffer-name "*xquery-tool results*"
  "Name of buffer to show results of xqueries in."
  :group 'xquery-tool)

(defcustom xquery-tool-temporary-xquery-file-name "xquery-tool-temp.xq"
  "Filename for storing one-off xqueries.
It will be created in `temporary-file-directory'."
  :group 'xquery-tool)

;; (defcustom xquery-tool-temporary-indexed-xml-file-name "xquery-temp-indexed.xml"
;;   "Filename for storing an indexed version of the xml you're querying.
;; It will be created in `temporary-file-directory'."
;;   :group 'xquery-tool)

(defcustom xquery-tool-temporary-xml-file-name "xquery-tool-temp.xml"
  "Filename for storing xml that is not in a file (region, e.g.).
It will be created in `temporary-file-directory'."
  :group 'xquery-tool)

(defcustom xquery-tool-link-namespace "tmplink"
  "Name of namespace to use for linking xquery results to original file."
  :group 'xquery-tool)

(defcustom xquery-tool-result-root-element-name "xq-tool-results"
  "Name of root element to use for wrapping results."
  :group 'xquery-tool
  :type '(string))

(defcustom xquery-tool-omit-xml-declaration nil
  "Whether to omit xml-declaration or not in output."
  :group 'xquery-tool
  :type '(boolean))

(defcustom xquery-tool-resolve-xincludes nil
  "Whether to resolve xinclude statements before running the query.

Switches on Saxon's '-xi' option, and makes
`xquery-tool-parse-to-shadow' parse included files."
  :group 'xquery-tool
  :type '(boolean))

(defvar xquery-tool-xquery-history nil
  "A var to hold the history of xqueries.")

(defvar xquery-tool-file-mappings nil
  "An assoc list of files used and their replacements.")

;; (defvar xquery-tool-last-xquery-on-full-file nil
;;   "If non-nil, the last xquery was run on a full xml document.
;; Important for knowing when to regenerate the xml source.")

(defun xquery-tool-xq-file (&optional fn)
  "Get full path to temporary file with name FN.
This is where we store the xquery.  Default for FN is
`xquery-tool-temporary-xquery-file-name'."
  (let ((fn (format "%s-%s" (or fn xquery-tool-temporary-xquery-file-name) (emacs-pid))))
    (expand-file-name fn temporary-file-directory)))

(defun xquery-tool-indexed-xml-file-name (hash)
  "Get full path to temporary file based on hashsum HASH."
  (let* ((prefix (format "xquery-tool-tmp-%s" (emacs-pid))))
    (expand-file-name (format "%s-%s" prefix hash) temporary-file-directory)))

(defun xquery-tool-xml-file (&optional fn)
    "Get full path to temporary file with name FN.
This is where temporary xml docs are stored that don't have their
own files.  Default for FN is
`xquery-tool-temporary-xml-file-name'."
    (let ((fn (or fn xquery-tool-temporary-xml-file-name)))
  (expand-file-name fn temporary-file-directory)))

;;;###autoload
(defun xquery-tool-query (xquery &optional xml-buff wrap-in-root save-namespace show-results)
  "Run the query XQUERY on the current xml document.

XQUERY can be:
 - a string: then that is used to compose an xquery;
 - a filename: then that is taken as input without further processing.

XML-BUFF should be a buffer containing an xml document and
defaults to the current buffer.  If a region is active, it will
operate only on that region.

To use this function, you might first have to customize the
`xquery-tool-java-binary' and `xquery-tool-saxonb-jar'
settings (M-x customize-group RET xquery-tool).

If WRAP-IN-ROOT is not nil (or you use a prefix arg (`C-u') in
the interactive call), the results will be wrapped in a root
element, possibly generating a well-formed XML document for a
node set.  Configure `xquery-tool-result-root-element-name' to
choose the element name.

If SAVE-NAMESPACE is not nil (or you use a double prefix arg in the
interactive call), then the attributes added to enable tracking
of elements in the source document are not deleted.

SHOW-RESULTS, true by default in interactive usage, nil
otherwise, pops up a buffer showing the results.

The function returns the buffer that the results are in."
  (interactive
   (progn
     (unless (eq major-mode 'nxml-mode)
       (if (yes-or-no-p "Are you sure this is an XML buffer? ")
	   t (error "Please call `xquery-tool-query' from a buffer visiting an XML document.")))
     (let ((xquery (read-string "Your xquery: " nil 'xquery-tool-xquery-history))
	   (wrap (<= 4 (or (car current-prefix-arg) 0)))
	   (save-namespace (<= 16 (or (car current-prefix-arg) 0))))
       (list xquery (current-buffer) wrap save-namespace 'show-results))))
  (let ((target-buffer (get-buffer-create xquery-tool-result-buffer-name))
	(xquery-file
	 (if (and (file-readable-p xquery) (file-regular-p xquery))
	     xquery
	   (xquery-tool-setup-xquery-file xquery (current-buffer))))
	(xml-shadow-file (with-current-buffer (or xml-buff (current-buffer))
			   (xquery-tool-parse-to-shadow)))
	process-status)
    (with-current-buffer target-buffer
      (if buffer-read-only (read-only-mode -1))
      (erase-buffer))
    (setq process-status
	  (call-process (shell-quote-argument xquery-tool-java-binary) ;; program
			xml-shadow-file     ;; infile
			target-buffer;; destination
			nil;; update display
			;; args
			"-classpath" (shell-quote-argument xquery-tool-saxonb-jar)
			"net.sf.saxon.Query"
			"-s:-"
			(format "-q:%s" (shell-quote-argument xquery-file))
			(if xquery-tool-resolve-xincludes "-xi:on" "-xi:off")))
    (if (= 0 process-status)
	(message "Called saxonb, setting up results ...")
      (message "Something went wrong in call to saxonb."))
    (with-current-buffer target-buffer
      (goto-char (point-min))
      (xquery-tool-setup-xquery-results target-buffer save-namespace)
      (when wrap-in-root
	(switch-to-buffer (current-buffer))
	(save-excursion
	  (goto-char (point-min))
	  (if (eq (xmltok-forward) 'processing-instruction)
	      (insert (format "\n<%s>" xquery-tool-result-root-element-name))
	    (goto-char (point-min))
	    (insert (format "<%s>\n" xquery-tool-result-root-element-name)))
	  (goto-char (point-max))
	    (insert (format "\n</%s>\n" xquery-tool-result-root-element-name))))
      ;; if wrapped, try nxml
      (if wrap-in-root (nxml-mode)
	(fundamental-mode))
      (set-buffer-modified-p nil)
      (read-only-mode)
      (goto-char (point-min)))
    (when show-results
      (display-buffer target-buffer
		      `((display-buffer-reuse-window
			 display-buffer-in-previous-window
			 display-buffer-use-some-window) . ((inhibit-same-window . t) (reusable-frames . ,(frame-list))))))
    target-buffer))

(defun xquery-tool-setup-xquery-results (&optional target-buffer save-namespaces)
  "Try to construct links for the results in TARGET-BUFFER.

Default for TARGET-BUFFER is the current buffer.  If
SAVE-NAMESPACES is nil (the default), then the shadow namespaces
used for constructing the links are removed."
  (let ((current-pos (make-marker))
	(target-buffer (if (bufferp target-buffer) target-buffer (current-buffer)))
	teied-item
	teied-candidates
	result)
    (with-temp-buffer
      (insert-buffer-substring target-buffer)
      (goto-char (point-min))
      (save-excursion
	(goto-char (point-min))
	(while (xmltok-forward)
	  (when (and (member xmltok-type '(start-tag empty-element)) (or xmltok-namespace-attributes xmltok-attributes))
	    (set-marker current-pos (point))
	    (let* ((atts (xquery-tool-get-namespace-candidates))
		   (start-att (cl-remove-if 'null
					    (mapcar (lambda (x) (if (string= (xmltok-attribute-local-name x) "start") x)) atts)))
		   target)
	      (when (= 1 (length start-att))
		(setq target (xmltok-attribute-value (elt start-att 0)))
		(make-text-button
		 (1+ xmltok-start)
		 xmltok-name-end
		 'help-echo (format "Try to go to %s." target)
		 'action 'xquery-tool-get-and-open-location
		 'follow-link t
		 'target target))
	      ;; remove all traces of xquery-tool-link-namespace namespace thing
	      (unless save-namespaces
		(xquery-tool-forget-namespace atts)
		(xquery-tool-relink-xml-base)
		(goto-char xmltok-start)
		(while (re-search-forward "\n" current-pos t)
		  (join-line))
		(if (looking-at "\\s-+>") (delete-region (point) (1- (match-end 0))))))
	    (goto-char current-pos)))
	(set-marker current-pos nil))
      (setq result (buffer-string))
      (with-current-buffer target-buffer
	(erase-buffer)
	(insert result)))))


(defun xquery-tool-get-and-open-location (position)
  "Find the target to open at POSITION."
  (let ((target (url-generic-parse-url (get-text-property position 'target))))
    (if target
	(xquery-tool-open-location target)
      (error "This does not look like an url: %s" target))))

(defun xquery-tool-open-location (url)
  "Open the location specified by URL."
  (let* ((url (if (stringp url) (url-generic-parse-url url) url))
	 (type (url-type url))
	 (file-name (decode-coding-string (url-unhex-string (car (url-path-and-query url))) 'utf-8))
	 (location (string-to-number (url-target url))))
    (if (cond
	 ((string= "file" type) (find-file-other-window file-name))
	 ((string= "buf" type) (pop-to-buffer (get-buffer (substring file-name 1)))))
	(cond
	 ((and (>= location (point-min)) (<= location (point-max)))
	  (goto-char location))
	 ((buffer-narrowed-p)
	  (if (yes-or-no-p "Requested location is outside current scope, widen? ")
	      (progn (widen)
		     (xquery-tool-open-location url))))
	 (t (error "Can't find location %s in this buffer" location)))
      (warn "No target found for this: %s." (url-recreate-url url)))))


(defun xquery-tool-get-namespace-candidates (&optional namespace)
  "Return a sorted list of atts in NAMESPACE.

Default for NAMESPACE is `xquery-tool-link-namespace'.  Will look
at `xmltok-attributes' and `xmltok-namespace-attributes', so make
sure xmltok is up to date."
  (when (member xmltok-type '(start-tag empty-element))
      (let ((namespace (or namespace xquery-tool-link-namespace)))
	(sort ;; better sort this explicitly
	 (mapcar 'cdr;; get all attribute values if they're in the namespace we added
		 (cl-remove-if 'null
			       (append
				(mapcar  (lambda (x) (when (string= (xmltok-attribute-prefix x) namespace)
						       (cons (xmltok-attribute-prefix x) x))) xmltok-attributes)
				(mapcar  (lambda (x) (when (string= (xmltok-attribute-local-name x) namespace)
						       (cons (xmltok-attribute-local-name x) x))) xmltok-namespace-attributes))))
	 (lambda (x y) (if (> (elt x 0) (elt y 0)) 'yepp))))))


(defun xquery-tool-forget-namespace (candidates)
  "Remove all attributes in CANDIDATES.

CANDIDATES is a list of `xmltok-attribute' vectors."
  (let ()
    (when candidates
      (dolist (delete-me candidates)
	;; (setq delete-me (pop candidates))
	(goto-char (xmltok-attribute-name-start delete-me))
	;; delete space before attribute, attribute, and closing quote
	(delete-region (1- (xmltok-attribute-name-start delete-me)) (1+ (xmltok-attribute-value-end delete-me))))
      (save-excursion
	(goto-char xmltok-start)
	(xmltok-forward)
	xmltok-attributes))))

(defun xquery-tool-relink-xml-base ()
  "Make the @xml:base attribute point at the original file.

POSITION is where the element starts, and defaults to
xmltok-start."
  (let ((base (xquery-tool-get-attribute "base" "xml")))
    (when (and base (rassoc (xmltok-attribute-value base) xquery-tool-file-mappings))
      (xquery-tool-set-attribute xmltok-start "base" (car (rassoc (xmltok-attribute-value base) xquery-tool-file-mappings)) "xml")
      (save-excursion
	(goto-char xmltok-start)
	(xmltok-forward)
	xmltok-attributes))))

(defun xquery-tool-setup-xquery-file (xquery &optional xml-buffer-or-file)
  "Construct an xquery file containing XQUERY.

If XML-BUFFER-OR-FILE is specified, look at that for namespace declarations."
  (let ((tmp (find-file-noselect (xquery-tool-xq-file)))
	(xml-buff (cond ((null xml-buffer-or-file) (current-buffer))
			((bufferp xml-buffer-or-file) xml-buffer-or-file)
			((and (file-exists-p xml-buffer-or-file) (file-regular-p xml-buffer-or-file))
			 (find-file-noselect xml-buffer-or-file))
			(t (error "Sorry, can't work on this source: %s." xml-buffer-or-file))))
	namespaces)
    (with-current-buffer tmp
      (erase-buffer))
    (with-current-buffer xml-buff
      (save-excursion
	(save-restriction
	  ;; (widen)
	  (when (use-region-p) (narrow-to-region (region-beginning) (region-end)))
	  (goto-char (point-min))
	  (while (and (xmltok-forward) (not (eq xmltok-type 'start-tag))) t)
	  (dolist (naspa-att xmltok-namespace-attributes)
	    (let ((naspa-val (xmltok-attribute-value naspa-att))
		  (naspa-name (xmltok-attribute-local-name naspa-att)))
	      (with-current-buffer tmp
		(if (string= naspa-name "xmlns")
		    (insert (format "declare default element namespace \"%s\";\n"
				    naspa-val))
		  (insert (format "declare namespace %s=\"%s\";\n"
				  naspa-name naspa-val)))))))))
    (with-current-buffer tmp
      (when xquery-tool-omit-xml-declaration
	(insert "declare option saxon:output 'omit-xml-declaration=yes';\n"))
      (insert xquery)
      (save-buffer)
      (buffer-file-name (current-buffer)))))

(defun xquery-tool-parse-to-shadow (&optional xmlbuffer)
  "Make XMLBUFFER (default `current-buffer') traceable.

Currently, for each start-tag or empty element in XMLBUFFER, this
adds an @`xquery-tool-link-namespace':start attribute referring
to the position in the original source.
Returns the filename to which the shadow tree was written."
  (with-current-buffer (if (bufferp xmlbuffer) xmlbuffer (current-buffer))
    (let* ((start (if (use-region-p) (region-beginning) (point-min)))
	   (end (if (use-region-p) (region-end) (point-max)))
	   (src-buffer (current-buffer))
	   (original-file-name (if (buffer-file-name (current-buffer))
				   (url-encode-url (format "file://%s" (buffer-file-name (current-buffer))))
				 (format "buf:///%s" (url-encode-url (buffer-name)))))
	   (tmp-file-name (xquery-tool-indexed-xml-file-name (secure-hash 'md5 (current-buffer) start end)))
	   (new-namespace (format " xmlns:%s=\"potemkin\"" xquery-tool-link-namespace))
	   ;; absolute start of buffer (-1)
	   (buffer-offset (cond
		    ((use-region-p) (1- (region-beginning)))
		    ((buffer-narrowed-p) (1- (point)))
		    (t 0)))
	   ;; how much the buffer grows from insertions
	   (grow-factor 0)
	   (outside-root t)
	   namespaces xi-replacement)
      (unless (file-exists-p tmp-file-name)
	(with-temp-buffer
	  (insert-buffer-substring-no-properties src-buffer start end)
	  (goto-char (point-min))
	  ;; set namespace on first start tag (hoping it's the root element)
	  (while (and (xmltok-forward) (not (member xmltok-type '(start-tag empty-element))) t))
	  ;; if this was an xml document, set stuff up
	  (when (member xmltok-type '(start-tag empty-element))
	    ;; save namespaces defined on root element
	    (when (< 0 (length xmltok-namespace-attributes))
	      (setq namespaces (mapcar (lambda (x)
					 (cons (xmltok-attribute-local-name x)
					       (xmltok-attribute-value x)))
				       xmltok-namespace-attributes)))
	    ;; add the new namespace we need for tracing
	    (save-excursion
	      (goto-char xmltok-name-end)
	      (insert new-namespace))
	    ;; `parse' document and add tracers to start-tags and empty elements
	    (goto-char (point-min));; but start from the top again
	    (while (xmltok-forward)
	      (when (member xmltok-type '(start-tag empty-element))
		;; consider xinclude option
		(when (and
		       (string= "include" (xmltok-start-tag-local-name))
		       xquery-tool-resolve-xincludes
		       (rassoc "http://www.w3.org/2001/XInclude" namespaces)
		       (string= (xmltok-start-tag-prefix) (car (rassoc "http://www.w3.org/2001/XInclude" namespaces))))
		  (goto-char xmltok-start)
		  (xmltok-save
		    (xmltok-forward)
		    (setq xi-replacement (xquery-tool-get-xinclude-shadow)))
		  (if (and xi-replacement (file-name-absolute-p xi-replacement))
		      (setq grow-factor
			    (+ grow-factor
			       (abs
				(- (point-max)
				   (progn
				     (xquery-tool-set-attribute xmltok-start
								"href"
								xi-replacement
								(car (rassoc "http://www.w3.org/2001/XInclude" namespaces)))
				     (point-max))))))
		    (message "Found xinclude element, but failed to relink it.")))
		(save-excursion
		  (goto-char xmltok-name-end)
		  (setq grow-factor;; adjust grow-factor for length of insertion
			(+ grow-factor
			   (abs
			    (-
			     (point-max)
			     (progn
			       (insert (xquery-tool-make-namespace-start-string
					original-file-name
					(+ (- xmltok-start grow-factor) buffer-offset)
					xquery-tool-link-namespace))
			       (point-max)))))))
		;; after the first start-tag, we need to take account of
		;; the namespace that was added
		(when outside-root
		  (setq grow-factor (+ grow-factor (length new-namespace)))
		  (setq outside-root nil)))))
	  (write-file tmp-file-name nil)
	  (unless (member (cons original-file-name tmp-file-name) xquery-tool-file-mappings)
	    (push (cons original-file-name tmp-file-name)  xquery-tool-file-mappings))))
      tmp-file-name)))

(defun xquery-tool-get-attributes (&optional x-atts ignore-namespaces)
  "Get attributes as an assoc list from X-ATTS (default `xmltok-attributes').

Each element of the list is a cons cell whose cdr holds the value
of the attribute and whose car specifies the attribute name. This
car is also a cons cell: its car is the namespace prefix, if any,
or the empty string \"\". Its cdr is the local name of the
attribute.

If IGNORE-NAMESPACES is not nil, the prefix is always the empty string."
  (let ((xmltok-attributes (or x-atts xmltok-attributes)))
    (nreverse
     (mapcar (lambda (x)
	       (cons
		(cons
		 (or (cond
		      (ignore-namespaces "")
		      (t (or
			  (xmltok-attribute-prefix x)
			  (xmltok-start-tag-prefix);; the default might be specified on the element
			  ""))))
		 (xmltok-attribute-local-name x))
		(xmltok-attribute-value x)))
	     xmltok-attributes))))

(defun xquery-tool-get-attribute (att &optional namespace-prefix x-atts ignore-namespaces)
  "Get attribute ATT from X-ATTS (default `xmltok-attributes').

If not in the default namespace, specify NAMESPACE-PREFIX.

If IGNORE-NAMESPACES is not nil, namespace prefixes are ignored
in matching.

Returns the vector in xmltok-attributes's format for which there
was a match, or nil."
  (let ((atts (xquery-tool-get-attributes (or x-atts xmltok-attributes) ignore-namespaces))
	(namespace-prefix (if ignore-namespaces "" (or namespace-prefix (xmltok-start-tag-prefix) "")))
	(x-atts (or x-atts xmltok-attributes)))
    (when (assoc (cons namespace-prefix att) atts)
      (elt x-atts
	   (1- (length (member
			(cons (cons namespace-prefix att)
			      (cdr (assoc (cons namespace-prefix att) atts)))
			atts)))))))

(defun xquery-tool-set-attribute (pos att-name val &optional namespace-prefix)
  "Set the attribute ATT-NAME to value VAL for the element starting at POS.

If ATT does not exist, it is added, otherwise it is set to value
VAL. Reparses the element to set up xmltok-attributes to reflect
the new status."
  (save-excursion
    (goto-char pos)
    (xmltok-forward)
    (when (member xmltok-type '(start-tag empty-element))
      (let ((att (xquery-tool-get-attribute att-name namespace-prefix))
	    (curpos (set-marker (make-marker) (point)))
	    (el-start xmltok-start);; save the element start, nxml might
	    ;; change this if it fontifies too quickly
	    (prev-point (point)))
	(unless att
	  (save-excursion
	    (goto-char xmltok-name-end)
	    (insert (format " %s%s=\"%s\"" (if namespace-prefix (format "%s:" namespace-prefix) "") att-name val))
	    (goto-char el-start)
	    (xmltok-forward)
	    (setq att (xquery-tool-set-attribute xmltok-start att-name val namespace-prefix))))
	(when (not (string= val (xmltok-attribute-value att)))
	  (save-excursion
	    (goto-char (xmltok-attribute-value-start att))
	    (delete-region (xmltok-attribute-value-start att)
			   (xmltok-attribute-value-end att))
	    (insert (format "%s" val))
	    (goto-char el-start)
	    (xmltok-forward)))
	(xquery-tool-get-attribute att-name namespace-prefix)))))


(defun xquery-tool-get-xinclude-shadow ()
  "Follow xinclude element's href and return replacement filename.

This function just looks at the current attributes, and does not
check whether this is really an xinclude element."
  (let* ((atts (mapcar (lambda (x)
			 (cons (xmltok-attribute-local-name x)
			       (xmltok-attribute-value x)))
		       xmltok-attributes))
	 (href (assoc "href" atts))
	 (parse (assoc "parse" atts))
	 (filename (car (url-path-and-query (url-generic-parse-url (cdr href))))))
    (cond
     ((null href) "");; if there's no href attribute, just use an
		     ;; empty one; this means the current document,
		     ;; which gets parsed later on anyway
     ((and
       (or (null parse) (string= (cdr parse) "xml"))
       filename
       (file-exists-p filename)
       (file-readable-p filename))
      (with-current-buffer (find-file-noselect filename)
	(save-excursion
	  (save-restriction
	    (widen)
	    (xquery-tool-parse-to-shadow)))))
     (t nil))))

(defun xquery-tool-make-namespace-start-string (&optional fn loc namespace)
  "Combine filename FN, location LOC, and NAMESPACE into a reference att."
  (let ()
    (format " %s:start=\"%s#%s\"" (or namespace xquery-tool-link-namespace) (or fn "") (or loc ""))))

(defun xquery-tool-wipe-temp-files (&optional files force)
  "Delete temporary FILES created by xquery-tool, and kill visiting buffers.

If FORCE is non-nil, don't ask for affirmation.  Essentially, all
/TMPDIR/xquery-tool-* files get deleted here."
  (interactive
   (let* ((files (directory-files temporary-file-directory 'full "^xquery-tool-"))
	  (force (when files (yes-or-no-p (format "Delete %s files (and visiting buffers): \n -%s\n ? " (length files) (mapconcat 'identity files "\n -"))))))
     (list files force)))
  (let ((files (or files (directory-files temporary-file-directory 'full "^xquery-tool-"))))
    (when files
      (if force
	  (dolist (file files)
	    ;; kill buffer
	    (while (find-buffer-visiting file)
	      (kill-buffer (find-buffer-visiting file)))
	    ;; delete by perhaps moving to trash
	    (delete-file file 'to-trash))))
    (setq xquery-tool-file-mappings nil)
    (if (called-interactively-p 'any)
	(message "No more xquery-tool tmp files."))
    files))


;; (xquery-tool-make-namespace-start-string);; " tmplink:start=\"#\""
;; (xquery-tool-make-namespace-start-string "soup.tmp");; " tmplink:start=\"soup.tmp#\""
;; (xquery-tool-make-namespace-start-string "soup.tmp" "1234");; " tmplink:start=\"soup.tmp#1234\""


(provide 'xquery-tool)

;;; xquery-tool.el ends here
