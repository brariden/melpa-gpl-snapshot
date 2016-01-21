;(progn (load "~/minibuf-isearch.el") (minibuf-isearch-setup))

(setq debug-on-error t)

;; see previous-matching-history-element @ simple.el and iswitchb

(defun minibuf-isearch-setup ()
  (interactive)
  (add-hook 'minibuffer-setup-hook 'minibuf-isearch-minibuffer-setup)
  ;; keybindings
  (defun for-each (proc items)
    (when (not (null items))
      (funcall proc (car items))
      (for-each proc (cdr items))))
  (for-each '(lambda (keymap)
	       (define-key keymap "\C-r" 'minibuf-isearch-start-isearch))
	    (list minibuffer-local-map
		  minibuffer-local-ns-map
		  minibuffer-local-completion-map
		  minibuffer-local-must-match-map))
  )

(defvar minibuf-isearch-mode-map nil)
(defun minibuf-isearch-define-mode-map ()
  (interactive)
  (let (map)
    (setq map (copy-keymap minibuffer-local-map))
    (define-key map "\C-m" 'minibuf-isearch-exit-minibuffer)
    (setq minibuf-isearch-mode-map map)
    (run-hooks 'minibuf-isearch-mode-map-hook)))

(defun minibuf-isearch-minibuffer-setup ()
  (if (minibuf-isearch-entryfn-p)
      (progn
	;;(make-local-variable 'iswitchb-use-mycompletion)
	;;(setq iswitchb-use-mycompletion t)
	(make-local-hook 'pre-command-hook)
	(add-hook 'pre-command-hook 'minibuf-isearch-pre-command nil t)
	(make-local-hook 'post-command-hook)
	(add-hook 'post-command-hook 'minibuf-isearch-post-command nil t)
	)))

(defvar minibuf-isearch-ok-this-is-minibuf-isearch nil)
(defun minibuf-isearch-entryfn-p ()
  minibuf-isearch-ok-this-is-minibuf-isearch)

(defvar minibuf-isearch-match "")
(defun minibuf-isearch-start-isearch ()
  (interactive)
  (setq minibuf-isearch-match "")
  (minibuf-isearch-do-isearch))

(defun minibuf-isearch-do-isearch ()
  ;;(interactive)
  (minibuf-isearch-define-mode-map)
  (let ((minibuffer-local-completion-map (copy-keymap minibuffer-local-map))
	(icomplete-mode nil)
	(enable-recursive-minibuffers t)
	(dummy-hist nil)
	(minibuf-isearch-ok-this-is-minibuf-isearch t))
    (completing-read "minibuf-isearch " '(("dummy" . 1)) nil nil nil dummy-hist)))

(defun minibuf-isearch-pre-command ()
  (minibuf-isearch-tidy))

(defun minibuf-isearch-post-command ()
  (minibuf-isearch-exhibit))

(defvar minibuf-isearch-eoinput 1)

(defun minibuf-isearch-tidy ()
  (if (and (boundp 'minibuf-isearch-eoinput)
	   minibuf-isearch-eoinput)
      (if (> minibuf-isearch-eoinput (point-max))
	  ;; Oops, got rug pulled out from under us - reinit:
	  (setq minibuf-isearch-eoinput (point-max))
	(let ((buffer-undo-list buffer-undo-list )) ; prevent entry
	  (delete-region minibuf-isearch-eoinput (point-max))))
    ;; Reestablish the local variable 'cause minibuffer-setup is weird:
    (set (make-local-variable 'minibuf-isearch-eoinput) 1)))

(defun minibuf-isearch-exhibit ()
  (let ((contents (buffer-substring (point-min) (point-max)))
	(buffer-undo-list t))
    (save-excursion
      (goto-char (point-max))
      (if (not (boundp 'minibuf-isearch-eoinput))
	  (make-local-variable 'minibuf-isearch-eoinput))
      (setq minibuf-isearch-eoinput (point))
      (let* ((match (if (> (string-bytes contents) 0)
			(minibuf-isearch-search-history contents
							(symbol-value minibuffer-history-variable)))))
	(if (and (> (string-bytes contents) 0) match)
	    (insert (concat " :" match))
	  (pp-to-string (symbol-value minibuffer-history-variable)) ;"(No match)"
	  )))))

(defun minibuf-isearch-search-history (str hist-list)
  (cond ((null hist-list)
	 nil)
	((and (stringp (car hist-list))
	      (string-match str (car hist-list)))
	 (car hist-list))
	(t
	 (minibuf-isearch-search-history str (cdr hist-list)))))

(defun minibuf-isearch-exit-minibuffer ()
  (interactive)
  ;; FIXME: fill
  (exit-minibuffer)
  (insert-string minibuf-isearch-match))
