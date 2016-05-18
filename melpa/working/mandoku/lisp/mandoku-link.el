;; linking for mandoku files
;; cwittern@gmail.com [2014-05-20T19:57:42+0900]

(org-add-link-type "mandoku" 'mandoku-link-open)
(add-hook 'org-store-link-functions 'mandoku-link-store-link)

(defvar mandoku-store-link-plist nil
  "Plist with info about the most recent link created with `mandoku-store-link'.")

(defun mandoku-link-open (link)
  "Open the text (and optionally go to indicated  position) in LINK.
LINK will consist of a <textid> recognized by mandoku."
  ;; need to get the file name and then call mandoku-execute-file-search
  (let* ((coll (car (split-string link ":")))
	 (textid (if (or (equal coll "meta")
			 (equal coll "*"))
		     (car (cdr (split-string link ":")))
		   (car (split-string link "[.:]"))))
	 (page (concat (or
			(ignore-errors (replace-in-string  (car  (cdr (split-string link ":"))) "_" ":" ))
			"")))
	 (src (car (cdr (split-string link "::"))))
	 (fname (concat (if (> (length textid ) 0 )
			    (concat textid
				    (if (string-match "-" page)
					(concat "_" (car (split-string page "-")) ".txt")
					(if (string-match "org" link)
					    ".org"
					;(if (string-match "txt" link)
					   ".txt"
					;"/Readme.org")
					  )))
			  coll)))
	 (filename  (ignore-errors (if (equal coll "meta")
			(mandoku-meta-textid-to-file textid)
		      (concat "/"  (substring textid 0 4) "/" (substring textid 0 8) "/" fname)))))
					;    (message (format "%s" page))
    (if (equal coll "*")
	;; we search for
	(mandoku-display-subcoll textid)
      (if (equal coll "meta")
	  ;; this does a headline only search in meta; we need to have the ID on the headline for this to work
	  (org-open-file filename  t nil (concat "#" textid)) 
					;      (message (format "%s" (concat mandoku-meta-dir  textid ".org" )))
	(if (file-exists-p (concat mandoku-text-dir filename))
	    (ignore-errors (org-open-file (concat mandoku-text-dir "/" filename) t nil 
			 ;;(or src page))
			  (if src 
			      (concat page "::" src)
			    page)))
	(if (file-exists-p (concat mandoku-text-dir (replace-in-string filename (concat textid ".txt") "Readme.org" )))
	  (ignore-errors (org-open-file (concat mandoku-text-dir (replace-in-string filename (concat textid ".txt") "Readme.org" )) t nil 
			 ;;(or src page))
			  (if src 
			      (concat page "::" src)
			    page)))
	  (if (file-exists-p (concat mandoku-temp-dir fname))
	      (ignore-errors (org-open-file (concat mandoku-temp-dir fname) t nil 
			   (if (< 0 (length src ))
			       (concat page "::" src)
			     page)))
	  (mandoku-open-remote-file filename src page)
	  )
	  (outline-show-all)
	  ))))))



(defun mandoku-link-store-link ()
  "if we are in mandoku-view-mode, or visiting a mandoku file, then store link"
  (when (eq major-mode 'mandoku-view-mode)
  ;; this is a mandoku file, make link
    (save-excursion
      (let* ((fn (file-name-sans-extension (file-name-nondirectory (buffer-file-name ))))
	     (location (cdr (cdr (mandoku-position-at-point-internal (mandoku-start) ))))
	     (loc-format (concat (car location) (format "%2.2d" (car (cdr location)))))
	     (charpos (mandoku-charcount-at-point-internal (mandoku-start)))
	     (textid (car (split-string fn "_")))
	     (title (mandoku-get-title))
	     (juan (mandoku-get-juan))
	     (br (mandoku-get-current-branch))
	     (outline (mapconcat 'identity (mandoku-get-outline-path) " / "))
	     (link (concat "mandoku:" fn ":" loc-format))
	     (end
	      (if (org-region-active-p)
		  (let* ((lc (cdr (cdr (mandoku-position-at-point-internal (region-end) ))))
			 (lf (concat (car lc) (format "%2.2d" (car (cdr lc))))))
		    (concat lf "-" 
			    (format "%2.2d" (mandoku-charcount-at-point-internal (region-end)))))))
	     (region (if (org-region-active-p)
		     (buffer-substring (region-beginning) (region-end))))
	     (description  loc-format))
	(org-store-link-props
	 :type "mandoku"
	 :filename fn
	 :link link
	 :textid textid
	 :edition br
	 :start (concat loc-format "-" (format "%2.2d"  charpos))
	 :end end
	 :region region
	 :title title
	 :description description)

	))))

(defun mandoku-insert-link (&optional notes plink )
  "insert the link"
  (interactive)
;  (let* ((f  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
;	(linklist (or notes (concat (substring (file-name-directory (buffer-file-name)) 0 -1) ".wiki/notes/" f ".txt"))))
;;    (with-current-buffer (find-file linklist)
;;      (goto-char (point-max))
      (mandoku-link-insert-link)
)  

(defun mandoku-link-insert-link (&optional plink)
  "insert the most recent link"
  (let* ((link (or plink
		   mandoku-store-link-plist
		   org-store-link-plist))
	 (region (plist-get link :region))
	 (edition (plist-get link :edition))
	 (textid (plist-get link :textid))
	 (title (plist-get link :title))
	 (start (plist-get link :start))
	 (end (or (plist-get link :end) ""))
	 )
    (insert (concat "** " start " - " end  " " title "\n:PROPERTIES:\n:edition: " edition
		    "\n:date: " (format-time-string "%Y-%m-%dT%T%z" (current-time))
		    "\n:END:\n\n"
		    (mandoku-remove-markup region )
		    "〔" title  ", [[mandoku:" textid  ":" (substring start 0 -3)  "]["  start  "]]〕\n"
		    )))
	 
)

(provide 'mandoku-link)

