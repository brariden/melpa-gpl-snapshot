;; [[file:shen-elisp.org::*Collecting%20KLambda%20files][Collecting\ KLambda\ files:1]]
(require 'shen-primitives)
(setq *klambda-directory-name* "KLambda")
(setq *klambda-directory* (file-name-as-directory (concat (file-name-directory load-file-name) *klambda-directory-name*)))
(setq *klambda-files*
      (mapcar (lambda (klFile) (concat *klambda-directory* klFile))
              '("toplevel.kl" "core.kl" "sys.kl" "sequent.kl" "yacc.kl"
                "reader.kl" "prolog.kl" "track.kl" "load.kl" "writer.kl"
                "macros.kl" "declarations.kl" "types.kl" "t-star.kl")))
;; Collecting\ KLambda\ files:1 ends here

;; [[file:shen-elisp.org::*Modifying%20The%20Elisp%20Reader%20For%20KLambda][Modifying\ The\ Elisp\ Reader\ For\ KLambda:1]]
(setq shen/*klambda-syntax-table*
      (let ((table (make-syntax-table lisp-mode-syntax-table)))
        (modify-syntax-entry 59 "_" table) ;; semi-colon
        (modify-syntax-entry ?, "_" table)
        (modify-syntax-entry ?# "_" table)
        (modify-syntax-entry ?' "_" table)
        (modify-syntax-entry ?` "_" table)
        table))

(defun shen/get-klambda-sexp-strings (klambda-file)
  (with-temp-buffer
    (insert-file-contents klambda-file)
    (with-syntax-table shen/*klambda-syntax-table*
      (let* ((klambda-code (buffer-string))
             (current-sexp-end (scan-lists 0 1 0))
             (groups nil))
        (progn
          (while current-sexp-end
            (let ((current-sexp-start (scan-lists current-sexp-end -1 0)))
              (progn
                (setq groups (nconc groups (list (buffer-substring current-sexp-start current-sexp-end))))
                (setq current-sexp-end (scan-lists current-sexp-end 1 0)))))
          groups)))))
;; Modifying\ The\ Elisp\ Reader\ For\ KLambda:1 ends here

;; [[file:shen-elisp.org::*Modifying%20The%20Elisp%20Reader%20For%20KLambda][Modifying\ The\ Elisp\ Reader\ For\ KLambda:2]]
(setq shen/*illegal-character->spelling*
      '((59 "_sneomlioccoilmoens")  ;; semicolon
        (?, "_caommmmoac")
        (35 "_hhassshh")            ;; hash
        (?' "_tkiccikt")
        (?` "_beatcokuqqukoctaeb")))

(setq shen/*spelling->illegal-character*
      (mapcar #'reverse shen/*illegal-character->spelling*))
;; Modifying\ The\ Elisp\ Reader\ For\ KLambda:2 ends here

;; [[file:shen-elisp.org::*Modifying%20The%20Elisp%20Reader%20For%20KLambda][Modifying\ The\ Elisp\ Reader\ For\ KLambda:3]]
(defun shen/remove-reserved-elisp-characters (klambda-sexp-string)
  (let ((InString nil)
        (illegal-characters
         (mapcar
          (lambda (char->spelling) (nth 0 char->spelling))
          shen/*illegal-character->spelling*))
        (res)
        (curr klambda-sexp-string))
    (cl-flet ((append-and-advance
               (&optional X)
               (progn
                 (if X (setq res (concat res X))
                   (setq res (concat res (substring curr 0 1))))
                 (setq curr (substring curr 1)))))
      (while (not (= 0 (length curr)))
        (cond
         ((char-equal (string-to-char curr) ?\")
          (if InString
              (progn
                (setq InString nil)
                (append-and-advance))
            (progn
              (setq InString 't)
              (append-and-advance))))
         ((memq (string-to-char curr) illegal-characters)
          (if InString
              (append-and-advance)
            (append-and-advance
             (car (assoc-default
                   (string-to-char curr)
                   shen/*illegal-character->spelling*)))))
         (t (append-and-advance))))
      res)))
;; Modifying\ The\ Elisp\ Reader\ For\ KLambda:3 ends here

;; [[file:shen-elisp.org::*Modifying%20The%20Elisp%20Reader%20For%20KLambda][Modifying\ The\ Elisp\ Reader\ For\ KLambda:4]]
(defun shen/put-reserved-elisp-chars-back (sexp)
  (let ((symbols (shen/find-symbols sexp)))
    (shen/internal/modify-ast sexp
                     symbols
                     (lambda (path ast)
                       (shen/change-back (shen/internal/get-element-at path ast))))))
;; Modifying\ The\ Elisp\ Reader\ For\ KLambda:4 ends here

;; [[file:shen-elisp.org::*Modifying%20The%20Elisp%20Reader%20For%20KLambda][Modifying\ The\ Elisp\ Reader\ For\ KLambda:5]]
(defun shen/change-back (symbol)
  (let* ((original-length (length (symbol-name symbol)))
         (string-left (symbol-name symbol))
         (spelling->character
          (let ((hash (make-hash-table)))
            (mapcar (lambda (spelling-character)
                      (puthash (nth 0 spelling-character) (nth 1 spelling-character) hash))
                    shen/*spelling->illegal-character*)
            hash))
         (spellings (hash-table-keys spelling->character))
         (get-character-and-remaining
          (lambda (S)
            (let ((found-at-index (shen/internal/index-of (lambda (spelling) (string-prefix-p spelling S)) spellings)))
              (if found-at-index
                  (let ((spelling (nth found-at-index spellings)))
                    (list (string (gethash spelling spelling->character))
                          (substring S (length spelling))))
                (list (string (aref S 0))
                      (substring S 1))))))
         (reversed-result))
    (while (> (length string-left) 0)
      (let ((character-and-remaining (funcall get-character-and-remaining string-left)))
        (push (nth 0 character-and-remaining) reversed-result)
        (setq string-left (nth 1 character-and-remaining))))
    (intern (apply #'concat (reverse reversed-result)))))
;; Modifying\ The\ Elisp\ Reader\ For\ KLambda:5 ends here

;; [[file:shen-elisp.org::*Modifying%20The%20Elisp%20Reader%20For%20KLambda][Modifying\ The\ Elisp\ Reader\ For\ KLambda:6]]
(defun shen/find-symbols (sexp)
  (let ((symbols)
        (current-path)
        (current-list sexp)
        (current-list-length (length sexp))
        (current-index 0)
        (locally-scoped-symbols)
        (inner-lists))
    (while (or (< current-index current-list-length)
               inner-lists)
      (cond
       ((and (= current-index current-list-length) inner-lists)
        (progn
          (setq current-path (car inner-lists))
          (setq inner-lists (cdr inner-lists))
          (setq current-list (shen/internal/get-element-at current-path sexp))
          (setq current-index 0)
          (setq current-list-length (length current-list))))
       ((< current-index current-list-length)
        (let ((current-token (nth current-index current-list)))
          (cond
           ((symbolp current-token)
            (push (cons current-index current-path) symbols))
           ((consp current-token)
            (push (cons current-index current-path)
                  inner-lists))
           (t nil))
          (setq current-index (+ current-index 1))))
       (t nil)))
    symbols))
;; Modifying\ The\ Elisp\ Reader\ For\ KLambda:6 ends here

;; [[file:shen-elisp.org::*Iterating%20over%20KLambda%20Files][Iterating\ over\ KLambda\ Files:1]]
(setq *temp-shen-buffer*
      (find-file-noselect
       (concat (file-name-as-directory default-directory)
               (file-relative-name "shen.el"))))
(defun eval-klambda-files (klambda-files)
  (with-current-buffer *temp-shen-buffer*
    (progn
      (erase-buffer)
      (insert (format "%s\n" ";; -*- lexical-binding: t -*- "))
      (insert (format "%s\n" ";; Local Variables:"))
      (insert (format "%s\n" ";; byte-compile-warnings: (not redefine callargs free-vars unresolved obsolete noruntime cl-functions interactive-only make-local mapcar constants suspicious lexical)"))
      (insert (format "%s\n" ";; End:"))
      (insert (format "%s\n" "(require 'shen-primitives)"))
      (insert (format "%s\n" "(setq max-lisp-eval-depth 60000)"))
      (insert (format "%s\n" "(setq max-specpdl-size 13000)"))
      (goto-char (point-max))
      (dolist (klambda-file klambda-files nil)
        (eval-klambda-file klambda-file))
      (goto-char (point-max))
      (insert (format "%s\n" "(provide 'shen)"))
      (save-buffer))))
(defun eval-klambda-file (klambda-file)
  (dolist (klambda-sexp-string (shen/get-klambda-sexp-strings klambda-file) nil)
    (eval-klambda-sexp-string klambda-sexp-string)))
(defun eval-klambda-sexp-string (klambda-sexp-string)
  (let ((ast (shen/put-reserved-elisp-chars-back
              (read
               (shen/remove-reserved-elisp-characters
                klambda-sexp-string)))))
    (shen/kl-to-buffer ast *temp-shen-buffer*)))
;; Iterating\ over\ KLambda\ Files:1 ends here

;; [[file:shen-elisp.org::*The%20Runner][The\ Runner:1]]
(defun compile-and-load (F)
  (byte-compile-file
   (concat (file-name-as-directory default-directory)
           (file-relative-name F))
   't))
(defun load-klambda () (eval-klambda-files *klambda-files*))
(defun load-only ()
  (progn
    (compile-and-load "shen-primitives.el")
    (compile-and-load "install.el")))
(defun runner ()
  (progn
    (compile-and-load "shen-primitives.el")
    (compile-and-load "install.el")
    (eval-klambda-files *klambda-files*)
    (compile-and-load "shen.el")
    (compile-and-load "shen-overlays.el")
    (compile-and-load "shen-repl.el")
    (add-to-load-path default-directory)
    (shen/repl)))
;; The\ Runner:1 ends here
