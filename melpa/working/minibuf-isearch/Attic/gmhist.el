(defvar gmhist-last-search-string nil)
(defvar gmhist-last-search-result nil)
(defun gmhist-history-search-backward (&optional forward)
  (interactive "P")
  (let*((enable-recursive-minibuffers t)
	(direction (if forward -1 1))
	(string (if (fboundp 'field-string)
		    (field-string (point-max))
		  (buffer-string)))
	(minibuffer-history-symbol minibuffer-history-variable)
	(pos (+ minibuffer-history-position direction))
	(history (symbol-value minibuffer-history-symbol))
	(len (length history))
	(md (match-data))
	regexp found
	(mkstr '(lambda (x) (if (stringp x) x (prin1-to-string x)))))
    (if (string= string (funcall mkstr gmhist-last-search-result))
	(setq string gmhist-last-search-string))
    (setq regexp (regexp-quote string))
    (while (and (if forward (> pos 0) (<= pos len))
		(not (setq found (string-match
				  regexp
				  (funcall mkstr (nth (1- pos) history))))))
      (setq pos (+ pos direction)))
    (store-match-data md)
    (if found
	(setq gmhist-last-search-result (nth (1- pos) history))
      (error "No further match with %s" regexp))
    (put minibuffer-history-symbol 'cursor-pos regexp)
    (unwind-protect
	(gmhist-goto pos)
      (put minibuffer-history-symbol 'cursor-pos nil)
      (store-match-data md))
    (setq gmhist-last-search-string string)))

(defun gmhist-stringify (x) (if (stringp x) x (prin1-to-string x)))
(defun gmhist-goto (n)
  ;; Go to history position N, 1 <= N <= length of history
  ;; N<0 means the future and inserts an empty string
  ;; N=0 means minibuffer-initial-contents (fluid var from
  ;;     gmhist-new-read-from-minibuffer)
  (run-hooks 'gmhist-before-move-hook)
  (if (fboundp 'field-beginning) (delete-field)
    (erase-buffer))
  (setq minibuffer-history-position n)
  (if (< n 0)
      nil
    (insert
     (gmhist-stringify
      (if (= n 0)
	  (or minibuffer-initial-contents "")
	(nth (1- n) (symbol-value minibuffer-history-symbol)))))
    (run-hooks 'gmhist-after-insert-hook)
    ;; next two actually would be a good application for this hook
    (goto-char (if (get minibuffer-history-symbol 'cursor-end)
		   (point-max)
		 (if (fboundp 'field-beginning) (field-beginning)
		   (point-min))))
    (let ((pos (get minibuffer-history-symbol 'cursor-pos)))
      (if (stringp pos)
	  (if (eobp)
	      (re-search-backward pos nil t)
	    (re-search-forward pos nil t))))))

(defun gmhist-history-search-forward ()
  (interactive)
  (gmhist-history-search-backward t))


;; --
(defun for-each (proc items)
  (when (not (null items))
    (funcall proc (car items))
    (for-each proc (cdr items))))
(for-each '(lambda (keymap)
	     (define-key keymap "\C-p" 'gmhist-history-search-backward))
	  (list minibuffer-local-map
		minibuffer-local-ns-map
		minibuffer-local-completion-map
		minibuffer-local-must-match-map))
