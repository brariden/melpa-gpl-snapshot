;; [[file:shen-elisp.org::*Symbol%20Table][Symbol\ Table:1]]
(defun shen/migrate-symbol-table ()
  (let ((SymbolTable (shen/value 'shen.*symbol-table*)))
    (if (not (hash-table-p SymbolTable))
        (let ((NewTable (make-hash-table)))
          (dolist (Entry SymbolTable NewTable)
            (puthash (car Entry) (cdr Entry) NewTable))
          (shen/set 'shen.*symbol-table* NewTable))
      SymbolTable)))
;; Symbol\ Table:1 ends here

;; [[file:shen-elisp.org::*Symbol%20Table][Symbol\ Table:2]]
(defun shen/shen.lookup-func
    (Name Table)
  (let ((Form (gethash Name Table)))
    (if (not Form)
        (shen/simple-error
         (shen/app Name " has no lambda expansion\n" 'shen.a))
      Form)))

(defun shen/shen.update-symbol-table
    (Name Arity)
  (let ((lambda-function
         (shen/eval-kl
          (shen/shen.lambda-form Name Arity))))
    (puthash Name lambda-function (shen/value 'shen.*symbol-table*))
    (shen/value 'shen.*symbol-table*)))
;; Symbol\ Table:2 ends here

;; [[file:shen-elisp.org::*Questions][Questions:1]]
(defun shen/y-or-n? (S)
  (progn
    (shen/shen.prhush (shen/shen.proc-nl S) (shen/stoutput))
    (let ((Input (format "%s" (read-from-minibuffer " (y/n) " ))))
      (cond
       ((string-equal Input "y") 'true)
       ((string-equal Input "n") 'false)
       (t (progn
            (shen/shen.prhush  "please answer y or n~%" (shen/stoutput))
            (shen/y-or-n? S)))))))

(defun shen/shen.pause-for-user nil
  (let ((Byte (read-from-minibuffer "")))
    (if (and (= 1 (length Byte)) (= (string-to-char Byte) ?^))
        (shen/simple-error "input aborted\n")
      (shen/nl 1))))
;; Questions:1 ends here

;; [[file:shen-elisp.org::*Changing%20Directories][Changing\ Directories:1]]
(defun shen/cd (Path)
  (if (shen/shen->predicate (shen/= Path ""))
      (shen/set '*home-directory* "")
    (let ((PathString (concat Path "/")))
      (progn
        (setq default-directory PathString)
        (shen/set '*home-directory* PathString))
      PathString)))
;; Changing\ Directories:1 ends here

;; [[file:shen-elisp.org::*Provide%20it][Provide\ it:1]]
(provide 'shen-overlays)
;; Provide\ it:1 ends here
