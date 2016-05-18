;; [[file:shen-elisp.org::*Implementation%20Constants][Implementation\ Constants:1]]
;; -*- lexical-binding: t -*-
(defconst shen/prefix "shen/")
;; Implementation\ Constants:1 ends here

;; [[file:shen-elisp.org::*Symbols][Symbols:1]]
(defsubst shen/symbol-p (X)
  (not (or (consp X) (bufferp X) (vectorp X) (numberp X) (stringp X))))
;; Symbols:1 ends here

;; [[file:shen-elisp.org::*Symbols][Symbols:2]]
(defsubst shen/intern (String)
  (intern String))
;; Symbols:2 ends here

;; [[file:shen-elisp.org::*Symbols][Symbols:3]]
(defsubst shen/symbol->string (X)
  (symbol-name X))
;; Symbols:3 ends here

;; [[file:shen-elisp.org::*Assignments][Assignments:1]]
(defun shen/set (X Y)
  (set (intern (concat shen/prefix (format "%s" X))) Y))

(defun shen/value (X)
  (condition-case ex
          (symbol-value (intern (concat shen/prefix (format "%s" X))))
        ('error (error (format "%s has not been assigned" X)))))
;; Assignments:1 ends here

;; [[file:shen-elisp.org::*KLambda%20Constants][KLambda\ Constants:1]]
(shen/set '*home-directory* "")
(shen/set '*stoutput* standard-output)
(shen/set '*stinput* [()])
(shen/set '*language* "Elisp")
(shen/set '*implementation* "Elisp")
(shen/set '*porters* "Aditya Siram")
(shen/set '*release* "0.0.0.1")
(shen/set '*port* 1.7)
(shen/set '*os* "Linux")
;; KLambda\ Constants:1 ends here

;; [[file:shen-elisp.org::*Boolean%20Operations][Boolean\ Operations:1]]
(defmacro shen/if (X Y Z)
  `(if (eq ,X 'true) ,Y ,Z))
(defmacro shen/and (X Y) `(shen/predicate->shen (and (eq ,X 'true) (eq ,Y 'true))))
(defmacro shen/or (X Y) `(shen/predicate->shen (or (eq ,X 'true) (eq ,Y 'true))))
;; Boolean\ Operations:1 ends here

;; [[file:shen-elisp.org::*Boolean%20Operations][Boolean\ Operations:2]]
(defmacro shen/cond (&rest CASES)
  (let* ((predicates-quoted-cases
          (mapcar (lambda (predicate-result-pair)
                    (list (if (shen/symbol-p (nth 0 predicate-result-pair))
                              (list 'quote (nth 0 predicate-result-pair))
                            (list 'shen/shen->predicate (nth 0 predicate-result-pair)))
                          (nth 1 predicate-result-pair)))
                  CASES))
         (fallthrough-added (append predicates-quoted-cases (list '(t (error "One of the cond predicates must be true."))))))
    `(cond ,@fallthrough-added)))
;; Boolean\ Operations:2 ends here

;; [[file:shen-elisp.org::*Lambdas][Lambdas:3]]
(defmacro shen/lambda (X Y)
  (if (eq X nil)
        `(lambda () ,Y)
      `(lambda (,X) ,Y)))
;; Lambdas:3 ends here

;; [[file:shen-elisp.org::*Lets][Lets:2]]
(defmacro shen/let (X Y Z)
  `(let ((,X ,Y)) ,Z))
;; Lets:2 ends here

;; [[file:shen-elisp.org::*Defuns][Defuns:1]]
(defmacro shen/defun (F Args Body) `(defun ,F ,Args ,Body))
;; Defuns:1 ends here

;; [[file:shen-elisp.org::*Other%20Generic%20Functions][Other\ Generic\ Functions:1]]
(defsubst shen/= (X Y)
  (shen/predicate->shen
   (cond ((and (consp X) (consp Y)) (equal X Y))
         ((and (stringp X) (stringp Y)) (string-equal X Y))
         ((and (numberp X) (numberp Y)) (= X Y))
         ((and (vectorp X) (vectorp Y)) (and (= (length X) (length Y)) (equal X Y)))
         (t (equal X Y)))))
;; Other\ Generic\ Functions:1 ends here

;; [[file:shen-elisp.org::*Other%20Generic%20Functions][Other\ Generic\ Functions:2]]
(defmacro shen/freeze (X)
  `(function (lambda nil ,X)))
(defsubst shen/type (X MyType) (declare (ignore MyType)) X)
;; Other\ Generic\ Functions:2 ends here

;; [[file:shen-elisp.org::*Lists][Lists:1]]
(defsubst shen/cons (A Rest)
  (cons A Rest))
;; Lists:1 ends here

;; [[file:shen-elisp.org::*Lists][Lists:2]]
(defsubst shen/hd (List)    (car List))
(defsubst shen/tl (List)    (cdr List))
(defsubst shen/cons? (List) (shen/predicate->shen (consp List)))
;; Lists:2 ends here

;; [[file:shen-elisp.org::*Strings][Strings:1]]
(defun shen/str (X)
  (cond ((null X) (error "null is not an atom in Shen; str cannot convert it to a string.~%"))
        ((symbolp X) (symbol-name X))
        ((or (numberp X) (stringp X) (functionp X)) (format "%s" X))
        ((and (bufferp X) (buffer-file-name X)) (buffer-name X))
        ((eq X standard-input) "standard-input")
        ((eq X standard-output) "standard-output")
        (t
         (error (format "%s is not an atom, stream or closure; str cannot convert it to a string." X)))))
;; Strings:1 ends here

;; [[file:shen-elisp.org::*Strings][Strings:2]]
(defsubst shen/pos (S N) (string (aref S N)))
;; Strings:2 ends here

;; [[file:shen-elisp.org::*Strings][Strings:3]]
(defsubst shen/tlstr (X) (substring X 1))
;; Strings:3 ends here

;; [[file:shen-elisp.org::*Strings][Strings:4]]
(defsubst shen/string? (S) (shen/predicate->shen (stringp S)))
(defsubst shen/cn (Str1 Str2) (concat Str1 Str2))
(defsubst shen/n->string (N) (string N))
(defsubst shen/string->n (S) (string-to-char S))
;; Strings:4 ends here

;; [[file:shen-elisp.org::*Error%20Handling][Error\ Handling:1]]
(define-error 'shen/error "Shen error" 'error)
(defsubst shen/simple-error (E)
  (signal 'shen/error
          (if (stringp E)
              (list E)
            E)))
(defmacro shen/trap-error (X F)
  `(condition-case ex ,X ('error (funcall ,F ex))))
(defsubst shen/error-to-string (E) (format "%s" E))
;; Error\ Handling:1 ends here

;; [[file:shen-elisp.org::*Vectors][Vectors:1]]
(defsubst shen/absvector (N) (make-hash-table :size N :rehash-size 3.0))
(defsubst shen/address-> (Vector N Value) (progn (puthash N Value Vector) Vector))
(defsubst shen/<-address (Vector N) (gethash N Vector))
(defsubst shen/absvector? (X) (shen/predicate->shen (hash-table-p X)))
;; Vectors:1 ends here

;; [[file:shen-elisp.org::*Arithmetic%20Operations][Arithmetic\ Operations:1]]
(defconst shen/multiplication-limit (floor (sqrt most-positive-fixnum)))
(defconst shen/addition-limit (floor (/ most-positive-fixnum 2)))
;; Arithmetic\ Operations:1 ends here

;; [[file:shen-elisp.org::*Arithmetic%20Operations][Arithmetic\ Operations:2]]
(defun shen/number-op (X Y max op)
  (cond
   ((and (integerp X) (integerp Y))
    (if (and (< X max)
             (> X (- max))
             (< Y max)
             (> Y (- max)))
        (apply op (list X Y))
      (apply op (list (float X) (float Y)))))
   ((and (floatp X) (numberp Y)) (apply op (list X (float Y))))
   ((and (numberp X) (floatp Y)) (apply op (list (float X) Y)))
   (t (error (format "Trying to %s. Both %s and %s must be numbers" op X Y)))))
;; Arithmetic\ Operations:2 ends here

;; [[file:shen-elisp.org::*Arithmetic%20Operations][Arithmetic\ Operations:3]]
(defsubst shen/* (X Y) (shen/number-op X Y shen/multiplication-limit #'*))
(defsubst shen/+ (X Y) (shen/number-op X Y shen/addition-limit #'+))
(defsubst shen/- (X Y) (shen/number-op X Y shen/addition-limit #'-))
;; Arithmetic\ Operations:3 ends here

;; [[file:shen-elisp.org::*Arithmetic%20Operations][Arithmetic\ Operations:4]]
(defsubst shen// (X Y)
  (cond
   ((or (not (numberp X)) (not (numberp Y)))
    (error (format "Both %s and %s must be numbers." X Y)))
   ((and (integerp X) (integerp Y))
    (let* ((Div (/ (float X) (float Y)))
           (Truncated (floor Div)))
      (if (= Truncated Div)
          Truncated
        Div)))
   (t (/ (float X) (float Y)))))
;; Arithmetic\ Operations:4 ends here

;; [[file:shen-elisp.org::*Arithmetic%20Operations][Arithmetic\ Operations:5]]
(defsubst shen/> (X Y)     (shen/predicate->shen (> X Y)))
(defsubst shen/< (X Y)     (shen/predicate->shen (< X Y)))
(defsubst shen/>= (X Y)    (shen/predicate->shen (>= X Y)))
(defsubst shen/<= (X Y)    (shen/predicate->shen (<= X Y)))
(defsubst shen/number? (N) (shen/predicate->shen (numberp N)))
;; Arithmetic\ Operations:5 ends here

;; [[file:shen-elisp.org::*Time][Time:1]]
(defconst shen/2^16 65536)
(defun shen/get-time (Time)
  (cl-flet
      ((timespec-to-number (spec)
                           (let* ((high (nth 0 spec))
                                  (low (nth 1 spec)))
                             (+ (* high shen/2^16) low))))
    (cond ((eq Time 'run) (timespec-to-number (get-internal-run-time)))
          ((eq Time 'real)(timespec-to-number (current-time)))
          ((eq Time 'unix)(timespec-to-number (current-time)))
          (t (error (format "get-time does not understand parameter %s." Time))))))
;; Time:1 ends here

;; [[file:shen-elisp.org::*Streams%20and%20I/O][Streams\ and\ I/O:1]]
(defsubst shen/streamp (X) (and (bufferp X) (buffer-file-name X)))
;; Streams\ and\ I/O:1 ends here

;; [[file:shen-elisp.org::*Streams%20and%20I/O][Streams\ and\ I/O:2]]
(defun shen/open (Path Direction)
  (let ((Path (concat (file-name-as-directory (shen/value '*home-directory*))
                      (file-relative-name Path)))
        (Buffer))
    (cond
     ((equal Direction 'in)
      (if (not (file-exists-p Path))
          (error (format "Path does not exist: %s" Path))
        (progn
          (setq Buffer (find-file-noselect Path))
          (with-current-buffer
              Buffer
            (progn
              (make-local-variable 'shen/shen-buffer)
              (setq buffer-read-only 't
                    shen/shen-buffer 't)
              (goto-char (point-min))))
          Buffer)))
     ((equal Direction 'out)
      (progn
        (setq Buffer (find-buffer-visiting Path))
        (if (bufferp Buffer)
            (if (and (buffer-local-value 'buffer-read-only Buffer) (buffer-local-value 'shen/shen-buffer Buffer))
                (error (format  "A stream to %s already open read-only. Call (close \"%s\") followed by (open \"%s\" 'out). " Path Path Path))
              Buffer)
          (progn
            (setq Buffer (find-file-noselect Path))
            (with-current-buffer Buffer
              (progn
                (goto-char (point-max))
                (make-local-variable 'shen/shen-buffer)
                (setq shen/shen-buffer 't))))))))))
;; Streams\ and\ I/O:2 ends here

;; [[file:shen-elisp.org::*Streams%20and%20I/O][Streams\ and\ I/O:3]]
(defun shen/close (Stream)
  (if (not Stream)
      (error "Stream is nil.")
    (if (or (not (local-variable-p 'shen/shen-buffer Stream))
            (not (buffer-local-value 'shen/shen-buffer Stream)))
        (error (format "Buffer %s for file %s was not opened by Shen's (open ...) function." Stream (buffer-file-name Stream)))
      (cond ((buffer-local-value 'buffer-read-only Stream) (kill-buffer Stream))
            (t (with-current-buffer
                   Stream
                 (progn
                   (write-file (buffer-file-name Stream))
                   (kill-buffer Stream)
                   '())))))))

(defun shen/write-byte (Byte &optional S)
  (if S
      (cond
       ((bufferp S)
        (if (not (buffer-local-value 'buffer-read-only S))
            (error (format "Buffer %s is read-only." S))
          (if (buffer-local-value 'shen/shen-buffer S)
              (write-char Byte S)
            (error (format "Buffer %s was not opened by Shen." S)))))
       ((functionp S) ;; (ref:write-byte-function)
        (funcall S Byte))
       (t (write-char (shen/stoutput) Byte)))
    (funcall (shen/stoutput) Byte)))

(defun shen/read-byte (&optional S)
  (cond
   ((and (bufferp S) (buffer-file-name S))
    (if (buffer-local-value 'shen/shen-buffer S)
        (with-current-buffer S
          (let ((current-byte))
            (if (eq (point) (point-max))
                -1
              (progn
                (setq current-byte (get-byte))
                (forward-char)
                current-byte))))
      (error (format "Buffer %s was not opened by Shen." S))))
   ((vectorp S) (if (not (aref S 0))
                    -1
                  (pop (aref S 0))))
   (t (error (format "Unrecognized stream format %s" S)))))
;; Streams\ and\ I/O:3 ends here

;; [[file:shen-elisp.org::*Lookup][Lookup:1]]
(defun shen/internal/lookup-with-default (KEY ALIST DEFAULT)
  (car (or (assoc-default KEY ALIST) (list DEFAULT))))
;; Lookup:1 ends here

;; [[file:shen-elisp.org::*Boolean%20Operations][Boolean\ Operations:1]]
(defsubst shen/shen->predicate (X)
  (eq X 'true))
(defsubst shen/predicate->shen (X)
  (if X (quote true) (quote false)))
;; Boolean\ Operations:1 ends here

;; [[file:shen-elisp.org::*AST%20Getter/Setter][AST\ Getter/Setter:1]]
(defun shen/internal/get-element-at (path ast)
  (let ((res ast))
    (dolist (current-index (reverse path) res)
      (setq res (nth current-index res)))))
;; AST\ Getter/Setter:1 ends here

;; [[file:shen-elisp.org::*AST%20Getter/Setter][AST\ Getter/Setter:2]]
(defun shen/internal/nset-element-at (path ast new-element)
  (if (= 0 (length path))
      (setf ast new-element)
    (let ((place-fn)
          (path (reverse path)))
      (progn
        (dotimes (current-index (length path) nil)
          (setq place-fn
                (if (= current-index 0)
                    `(nth ,(nth current-index path) (quote ,ast))
                  `(nth ,(nth current-index path) ,place-fn))))
        (if (or (consp new-element) (shen/symbol-p new-element))
            (eval `(setf ,place-fn (quote ,new-element)) 't)
          (eval `(setf ,place-fn ,new-element)) 't)
        ast))))
;; AST\ Getter/Setter:2 ends here

;; [[file:shen-elisp.org::*Find%20All][Find\ All:1]]
(defun shen/internal/find-all (X ast)
  (if (not (consp ast))
      'shen/not-found
    (let ((lists-left-to-search `((() ,ast)))
          (found 'shen/not-found))
      (while lists-left-to-search
        (let* ((search-candidate (car lists-left-to-search))
               (search-candidate-path (nth 0 search-candidate))
               (current-list (nth 1 search-candidate)))
          (progn
            (setq lists-left-to-search (cdr lists-left-to-search))
            (dotimes (current-index (length current-list) nil)
              (let ((current-element (nth current-index current-list))
                    (current-path (cons current-index search-candidate-path)))
                (if (equal X current-element)
                    (if (consp found)
                        (push current-path found)
                      (setq found (list current-path)))
                  (if (consp current-element)
                      (push `(,current-path ,current-element)
                            lists-left-to-search))))))))
      found)))
;; Find\ All:1 ends here

;; [[file:shen-elisp.org::*Find%20Containing%20List][Find\ Containing\ List:1]]
(defun shen/internal/list-containing-first-occurrence-of (list-pred ast)
  (if (not (consp ast))
      'shen/not-found
    (let ((lists-left-to-search `((() ,ast)))
          (found 'shen/not-found))
      (progn
        (while (and lists-left-to-search (eq found 'shen/not-found))
          (let* ((search-candidate (car lists-left-to-search))
                 (search-candidate-path (nth 0 search-candidate))
                 (current-list (nth 1 search-candidate))
                 (current-list-length (length current-list)))
            (if (funcall list-pred current-list)
                (setq found search-candidate-path)
              (progn
                (setq lists-left-to-search
                      (append
                       (let ((reversed-lists-in-current-list))
                         (dotimes (current-index current-list-length (reverse reversed-lists-in-current-list))
                           (if (consp (nth current-index current-list))
                               (setq reversed-lists-in-current-list
                                     (cons (list (cons current-index search-candidate-path)
                                                 (nth current-index current-list))
                                           reversed-lists-in-current-list)))))
                       (cdr lists-left-to-search)))))))
        found))))
;; Find\ Containing\ List:1 ends here

;; [[file:shen-elisp.org::*Path%20Utilities][Path\ Utilities:1]]
(defun shen/internal/get-path-relative-to (parent-path path)
  (and (shen/internal/starts-with-path parent-path path)
       (shen/internal/path-slice path 0 (- (length path) (length parent-path)))))

(defun shen/internal/starts-with-path (parent-path path)
  (and (<= (length parent-path) (length path))
       (equal parent-path
              (shen/internal/path-slice path
                               (- (length path)
                                  (length parent-path))))))

(defun shen/internal/get-path-parent (path) (cdr path))

(defun shen/internal/path-slice (path start &optional end)
  (let ((start-to-end (nthcdr start path))
        (res))
    (if end
        (dotimes (i (- (if (< end (length path))
                           end
                         (length path))
                       start)
                    (nreverse res))
          (push (nth i start-to-end) res))
      start-to-end)))
;; Path\ Utilities:1 ends here

;; [[file:shen-elisp.org::*AST%20Modification][AST\ Modification:1]]
(defun shen/internal/modify-ast (ast paths tx-fn)
  (let ((deepest-first (sort paths (lambda (A B) (> (length A) (length B)))))
        (current-ast ast))
    (dolist (path deepest-first current-ast)
      (setq current-ast
            (shen/internal/nset-element-at path ast (funcall tx-fn path ast))))))
;; AST\ Modification:1 ends here

;; [[file:shen-elisp.org::*List%20Filtering][List\ Filtering:1]]
(defun shen/internal/partition (pred Xs)
  (let ((a)
        (b))
    (dotimes (i (length Xs) (list a b))
      (push (nth i Xs)
            (if (funcall pred (nth i Xs)) a b)))))
;; List\ Filtering:1 ends here

;; [[file:shen-elisp.org::*List%20Filtering][List\ Filtering:2]]
(defun shen/internal/filter (pred Xs &optional include-index)
  (let ((accum))
    (dotimes (i (length Xs) accum)
      (if (funcall pred (nth i Xs))
          (push (if include-index
                    (list (nth i Xs) i)
                  (nth i Xs))
                accum)))))
;; List\ Filtering:2 ends here

;; [[file:shen-elisp.org::*List%20Filtering][List\ Filtering:3]]
(defun shen/internal/index-of (pred Xs)
  (let ((found)
        (index 0))
    (while (and (not found) (< index (length Xs)))
      (progn
        (if (funcall pred (nth index Xs))
            (setq found index))
        (setq index (+ index 1))))
    found))
;; List\ Filtering:3 ends here

;; [[file:shen-elisp.org::*List%20Filtering][List\ Filtering:4]]
(defun shen/internal/delete-first-eq (needle Xs)
  (let ((index (shen/internal/index-of (lambda (X) (eq X needle)) Xs)))
    (if index
        (let ((current-index 0)
              (copy))
          (while (< current-index (length Xs))
            (progn
              (if (not (= current-index index))
                  (push (nth current-index Xs) copy))
              (setq current-index (1+ current-index))))
          (nreverse copy))
      Xs)))
;; List\ Filtering:4 ends here

;; [[file:shen-elisp.org::*Prefixing%20Utilities][Prefixing\ Utilities:1]]
(defun shen/internal/prefix-symbol (X)
  (if (shen/symbol-p X)
      (intern (concat shen/prefix (format "%s" X)))
    X))
;; Prefixing\ Utilities:1 ends here

;; [[file:shen-elisp.org::*Prefixing%20Utilities][Prefixing\ Utilities:2]]
(defun shen/internal/symbol-prefixed-p (X)
  (and (shen/symbol-p X) (string-prefix-p shen/prefix (symbol-name X))))
;; Prefixing\ Utilities:2 ends here

;; [[file:shen-elisp.org::*Prefixing%20Utilities][Prefixing\ Utilities:3]]
(defun shen/internal/unprefix-symbol (X)
  (if (shen/internal/symbol-prefixed-p X)
      (intern (substring (symbol-name X) (length shen/prefix)))
    X))
;; Prefixing\ Utilities:3 ends here

;; [[file:shen-elisp.org::*Walking%20The%20AST][Walking\ The\ AST:1]]
(defun shen/internal/get-function-symbol-and-funcall-paths (ast)
  (let ((namespace-only)        ;; (ref:namespace-only)
        (quote-only)            ;; (ref:quote-only)
        (possibly-apply-function)) ;; (ref:possibly-apply-function)
    (if (not (consp ast))
        (if (shen/symbol-p ast)
            (list nil '(nil) '(nil) nil nil)
          (list nil nil nil nil nil))
      (let ((current-path)                     ;; (ref:current-path)
            (current-list ast)                 ;; (ref:current-list)
            (current-list-length (length ast)) ;; (ref:current-list-length)
            (current-index 0)                  ;; (ref:current-index)
            (locally-scoped-symbols)           ;; (ref:locally-scoped-symbols)
            (inner-lists)                      ;; (ref:inner-lists)
            (cond-predicate-action-p)
            (inner-lists-in-cond-form))        ;; (ref:inner-lists-in-cond-form)
        (while (or (< current-index current-list-length) ;; (ref:continue iterating)
                   inner-lists)
          (cond
           ((and (= current-index current-list-length) inner-lists) ;; (ref:sublists left)
            (progn
              (setq locally-scoped-symbols (nth 0 (car inner-lists)))
              (setq current-path (nth 1 (car inner-lists)))
              (setq cond-predicate-action-p (nth 2 (car inner-lists)))
              (setq inner-lists-in-cond-form nil)
              (setq inner-lists (cdr inner-lists))
              (setq current-list (shen/internal/get-element-at current-path ast))
              (setq current-index 0)
              (setq current-list-length (length current-list))))
           ((and (< current-index current-list-length)              ;; (ref:not a list)
                 (not (consp (nth current-index current-list))))
            (let ((current-token (nth current-index current-list)))
              (if (= 0 current-index)
                  (if (and (not (eq current-token 'nil))
                           (shen/symbol-p current-token))
                      (progn
                        (if (and (not (memq current-token locally-scoped-symbols))
                                 (not (eq current-token 'defun)))
                            (push (cons 0 current-path)
                                  namespace-only))
                        (cond
                         ((or (eq current-token 'lambda)
                              (eq current-token 'shen/lambda)) ;; (ref:lambda form)
                          (progn
                            (push (nth 1 current-list) locally-scoped-symbols)
                            (setq current-index 2)))
                         ((eq current-token 'defun) ;; (ref:defun form)
                          (progn
                            (push (cons 1 current-path) namespace-only)
                            (setq locally-scoped-symbols
                                  (append (nth 2 current-list) locally-scoped-symbols))
                            (setq current-index 3)))
                         ((or (eq current-token 'let)
                              (eq current-token 'shen/let))  ;; (ref:let form)
                          (progn
                            (push (nth 1 current-list) locally-scoped-symbols)
                            (setq current-index 2)))
                         ((or (eq current-token 'cond)
                              (eq current-token 'shen/cond)) ;; (ref:cond form)
                          (progn
                            (setq inner-lists-in-cond-form 't)
                            (setq current-index 1)))
                         (t
                          (progn
                            (if (not cond-predicate-action-p)
                                (push (list (cons 0 current-path)
                                            (memq current-token locally-scoped-symbols))
                                      possibly-apply-function))
                            (setq current-index 1)))))
                    (setq current-index (1+ current-index)))
                (if (and (not (eq current-token 'nil))
                         (shen/symbol-p current-token))
                    (progn
                      (if (not (memq current-token locally-scoped-symbols))
                          (push (cons current-index current-path)
                                quote-only))
                      (setq current-index (1+ current-index)))
                  (setq current-index (1+ current-index))))))
           ((and (< current-index current-list-length)             ;; (ref:a sublist)
                 (consp (nth current-index current-list)))
            (progn
              (if (and (= 0 current-index) (not cond-predicate-action-p))
                  (push (list (cons current-index current-path)
                              nil)
                        possibly-apply-function))
              (push (list locally-scoped-symbols
                          (cons current-index current-path)
                          inner-lists-in-cond-form)
                    inner-lists)
              (setq current-index (+ current-index 1))))
           (t nil)))
        (list namespace-only quote-only possibly-apply-function))))) ;; (ref:returns)
;; Walking\ The\ AST:1 ends here

;; [[file:shen-elisp.org::Primitive%20Macros][Primitive\ Macros]]
(setq shen/*primitive-macros*
      '(shen/if
        shen/and
        shen/or
        shen/cond
        shen/lambda
        shen/let
        defun
        shen/freeze
        shen/trap-error))
;; Primitive\ Macros ends here

;; [[file:shen-elisp.org::*Function%20Application][Function\ Application:2]]
(defun shen/internal/apply-function (f args locally-scoped)
  (cond
   (locally-scoped       ;;(ref:higher-order function)
    `(shen/internal/apply-higher-order-function ,f (list ,@args)))
   ((consp f)            ;;(ref:a list)
    `(shen/internal/apply-function-expression ,f (list ,@args)))
   (t
    (if (fboundp 'shen/arity)
        (let ((arity (shen/internal/check-partial-application f (length args)))) ;; (ref:known arity)
          (if (= arity -1)
              `(,f ,@args)
          `(shen/internal/apply-partially (function ,f) (list ,@args))))
      `(,f ,@args)))))

(defun shen/internal/apply-higher-order-function (f args)
  (condition-case apply-ex (apply f args)
    ('wrong-number-of-arguments
     (condition-case ex
         (let ((arity (shen/internal/check-partial-application f (length args))))
          (if (= arity -1)
              (signal (car apply-ex) (cdr apply-ex))
            (apply (eval (shen/internal/make-lambda-expression f arity (length args)) 't) args)))
       ('wrong-number-of-arguments
        (shen/internal/apply-incrementally f args))))))

(defun shen/internal/apply-function-expression (exp args)
  (condition-case ex (apply exp args)
    ('wrong-number-of-arguments (shen/internal/apply-incrementally exp args))))

(defun shen/internal/apply-partially (f args)
  (let ((arity (shen/internal/check-partial-application f (length args))))
    (if (= arity -1)
        (apply f args)
      (apply (eval (shen/internal/make-lambda-expression f arity (length args)) 't) args))))

(defun shen/internal/make-lambda-expression (f arity num-args) ;; (ref:curried lambda)
  (let* ((all-args (let ((single-apply-args)
                         (blast-apply-args))
                     (dotimes (i arity (list (reverse blast-apply-args)
                                             (reverse single-apply-args)))
                       (push (intern (format "A%d" i))
                             (if (and num-args (< i num-args))
                                 blast-apply-args
                               single-apply-args)))))
         (blast-apply-args (nth 0 all-args))
         (single-apply-args (nth 1 all-args))
         (expression `(apply (function ,f) (list ,@(append blast-apply-args single-apply-args)))))
    (dolist (arg (reverse single-apply-args) expression)
      (setq expression `(shen/lambda ,arg ,expression)))
    (if blast-apply-args
        `(lambda ,(reverse blast-apply-args) ,expression)
      expression)))

(defun shen/internal/apply-incrementally (f args) ;; (ref:incremental application)
  (let ((result f)
        (current-args args))
    (while current-args
      (setq result (funcall result (car current-args)))
      (setq current-args (cdr current-args)))
    result))

(defun shen/internal/check-partial-application (f num-args)
  (let ((arity (condition-case ex (shen/arity (shen/internal/unprefix-symbol f)) ('error -1))))
    (cond
     ((eq -1 arity) -1)
     ((= arity num-args) -1)
     ((> num-args arity) -1)
     (t arity))))
;; Function\ Application:2 ends here

;; [[file:shen-elisp.org::*Detecting%20Recursive%20Calls][Detecting\ Recursive\ Calls:1]]
(defun shen/internal/find-recursive-call-paths (function-name args ast)
  (if (not (consp ast))
      'shen/not-found
    (let ((lists-left-to-search `((() ,ast))) ;; (ref:lists-left-to-search)
          (found 'shen/not-found))  ;; (ref:tail-calls-found)
      (while lists-left-to-search
        (let* ((search-candidate (car lists-left-to-search))
               (search-candidate-path (nth 0 search-candidate))
               (current-list (nth 1 search-candidate))
               (current-list-length (length current-list))
               (current-head (car current-list))
               (push-if-list     ;; (ref:push-if-list)
                (lambda (indexes)
                  (mapc
                   (lambda (index)
                     (if (consp (nth index current-list))
                         (setq lists-left-to-search
                               (append lists-left-to-search
                                       (list
                                        (list (cons index search-candidate-path)
                                              (nth index current-list)))))))
                   indexes))))
          (progn
            (setq lists-left-to-search (cdr lists-left-to-search))
            (cond ((and (eq current-head function-name)
                        (= (length (cdr current-list)) (length args)))
                   (if (not (consp found))
                       (setq found (list search-candidate-path))
                     (push search-candidate-path found)))
                  ((eq current-head 'shen/cond)
                   (progn
                     (mapc
                      (lambda (action-index-pair)
                        (setq lists-left-to-search
                              (let ((path-to-action
                                     (append (list 1 (1+ (nth 1 action-index-pair)))
                                             search-candidate-path)))
                                (append lists-left-to-search
                                        (list
                                         (list path-to-action
                                               (nth 0 action-index-pair)))))))
                      (mapcar
                       (lambda (predicate-action-index)
                         (list (nth 1 (nth 0 predicate-action-index))
                               (nth 1 predicate-action-index)))
                       (shen/internal/filter  ;; (ref:cond-filter)
                        (lambda (predicate-action-pair)
                          (consp (nth 1 predicate-action-pair)))
                        (cdr current-list)
                        't)))))
                  ((eq current-head 'shen/if)
                   (if (= 4 current-list-length)
                       (funcall push-if-list '(2 3))
                     (funcall push-if-list '(2))))
                  ((eq current-head 'shen/trap-error)
                   (funcall push-if-list '(1 2)))
                  ((or (eq current-head 'shen/let)
                       (eq current-head 'defun))
                   (funcall push-if-list '(3)))
                  ((eq current-head 'shen/lambda)
                   (funcall push-if-list '(2)))
                  (t (funcall push-if-list (list (- current-list-length 1))))))))
      found)))
;; Detecting\ Recursive\ Calls:1 ends here

;; [[file:shen-elisp.org::*Detecting%20Function%20Application%20Context][Detecting\ Function\ Application\ Context:1]]
(defun shen/start-of-function-chain (tail-call-path ast)
  (let* ((from-the-top (reverse tail-call-path))
         (current-from-top-path)
         (path-left-to-tail-call (reverse tail-call-path))
         (start tail-call-path) ;; (ref:start-accumulator)
         (locally-scoped))
    (cl-flet ((append-and-advance
               (X &optional reset-start)
               (progn
                 (setq start
                       (if reset-start ;; (ref:reset-start)
                           tail-call-path
                         current-from-top-path))
                 (setq current-from-top-path
                       (append (reverse (shen/internal/path-slice path-left-to-tail-call 0 X))
                               current-from-top-path)
                       path-left-to-tail-call (shen/internal/path-slice path-left-to-tail-call X))

                 )))
      (while (not (equal current-from-top-path tail-call-path))
        (let* ((current-list (shen/internal/get-element-at current-from-top-path ast))
               (current-head (car current-list)))
          (cond
           ((or (not (shen/symbol-p current-head))
                (eq 'shen/if current-head))  ;; (ref:if-stop-recording)
            (append-and-advance 1 't))
           ((eq 'defun current-head)    ;; (ref:defun-stop-recording)
            (progn
              (setq locally-scoped (append (nth 2 current-list) locally-scoped))
              (append-and-advance 1 't)))
           ((eq 'shen/let current-head)      ;; (ref:let-stop-recording)
            (progn
              (setq locally-scoped (append (list (nth 1 current-list)) locally-scoped))
              (append-and-advance 1 't)))
           ((eq 'shen/lambda current-head)   ;; (ref:lambda-stop-recording)
            (progn
              (setq locally-scoped (append (nth 1 current-list) locally-scoped))
              (append-and-advance 1 't)))
           ((eq 'shen/cond current-head)     ;;; (ref:cond-stop-recording)
            (append-and-advance 2 't))
           ((eq 'shen/do current-head)       ;;; (ref:do-stop-recording)
            (append-and-advance 1 't))
           (t (append-and-advance 1)))))
      start)))
;; Detecting\ Function\ Application\ Context:1 ends here

;; [[file:shen-elisp.org::*Getting%20the%20Tail%20Calls][Getting\ the\ Tail\ Calls:1]]
(defun shen/internal/get-tail-call-paths (ast)
  (let* ((function-name (nth 1 ast))
         (args (nth 2 ast))
         (body (nth 3 ast))
         (recursive-call-paths (shen/internal/find-recursive-call-paths function-name args body)))
    (if (eq recursive-call-paths 'shen/not-found)
        'shen/not-found
      (let ((accum))
        (dolist (tail-call-path recursive-call-paths (if accum (reverse accum) 'shen/not-found))
          (let* ((context (shen/start-of-function-chain tail-call-path body)))
            (if (equal context tail-call-path)
                (push (append tail-call-path (list 3)) accum))))))))
;; Getting\ the\ Tail\ Calls:1 ends here

;; [[file:shen-elisp.org::*Generating%20A%20TCO'ed%20Function][Generating\ A\ TCO\'ed\ Function:1]]
(defun shen/trampoline-body (ast)
  (let* ((args (nth 2 ast))
         (body (nth 3 ast))
         (tail-trampoline (make-symbol "tail-trampoline")))
    `(cl-flet ((,tail-trampoline ,args ,body))
       (let ((result (funcall (function ,tail-trampoline) ,@args)))
         (while (vectorp result)
           (setq result (apply (function ,tail-trampoline) (aref result 0))))
         result))))
;; Generating\ A\ TCO\'ed\ Function:1 ends here

;; [[file:shen-elisp.org::*Modifying%20The%20AST][Modifying\ The\ AST:1]]
(defun shen/internal/parse-ast (ast)
  (if (not (consp ast))
      (if (shen/symbol-p ast) (list 'quote ast) ast)
   (let* ((function-and-symbol-paths (shen/internal/get-function-symbol-and-funcall-paths ast)) ;;; (ref:paths)
         (namespace-only (nth 0 function-and-symbol-paths))
         (quote-only (nth 1 function-and-symbol-paths))
         (possibly-apply-function (nth 2 function-and-symbol-paths))
         (current-ast ast))
    (progn
      (shen/internal/namespace-and-quote current-ast namespace-only quote-only) ;;; (ref:quote and namespace)
      (let ((apply-function (shen/internal/filter
                             (lambda (path-local)
                               (let ((token (shen/internal/get-element-at (nth 0 path-local) ast)))
                                 (not (memq token shen/*primitive-macros*))))
                             possibly-apply-function)))
        (if (eq (car current-ast) 'defun) ;;; (ref:defun form)
            (let* ((tail-call-paths (shen/internal/get-tail-call-paths ast)))
              (if (not (eq tail-call-paths 'shen/not-found))
                  (let ((not-in-tail-call apply-function)
                        (in-tail-call))
                    (progn
                      (dolist (path tail-call-paths nil)
                        (let* ((tco-non-tco-pair ;;; (ref:inside the recursive call)
                                (shen/internal/partition
                                 (lambda (apply-function-path-local)
                                   (shen/internal/starts-with-path path (nth 0 apply-function-path-local)))
                                 not-in-tail-call))
                               (funcalled-tco
                                (let* ((normalized-paths
                                        (shen/internal/filter
                                         (lambda (path-local) (not (equal (nth 0 path-local) '(0))))
                                         (mapcar
                                          (lambda (in-tco-path-local)
                                            (list
                                             (shen/internal/get-path-relative-to path (nth 0 in-tco-path-local))
                                             (nth 1 in-tco-path-local)))
                                          (nth 0 tco-non-tco-pair))))
                                       (tail-call (shen/internal/get-element-at path current-ast)))
                                  (list
                                   path
                                   `(vector (list ,@(cdr (shen/internal/add-funcalls tail-call normalized-paths)))))))) ;;; (ref:package up the arguments)
                          (progn
                            (setq not-in-tail-call (nth 1 tco-non-tco-pair))
                            (push funcalled-tco in-tail-call))))
                      (dolist (path-tail-call in-tail-call nil)  ;;; (ref:Sub in the recurs marker)
                        (shen/internal/modify-ast current-ast (list (nth 0 path-tail-call))
                                         (lambda (path current-ast) (nth 1 path-tail-call))))
                      (setq current-ast (shen/internal/add-funcalls current-ast not-in-tail-call)) ;;; (ref:rest of the function applications)
                      (setq current-ast `(defun ,(nth 1 current-ast) ,(nth 2 current-ast) ,(shen/trampoline-body current-ast))))) ;;; (ref:write out the defun)
                (setq current-ast (shen/internal/add-funcalls current-ast apply-function)))
              current-ast)
          (progn
            (setq current-ast (shen/internal/add-funcalls current-ast apply-function))
            current-ast)))))))
;; Modifying\ The\ AST:1 ends here

;; [[file:shen-elisp.org::*Modifying%20The%20AST][Modifying\ The\ AST:2]]
(defun shen/internal/namespace-and-quote (ast namespace-only-paths quote-only-paths)
  (progn
    (shen/internal/modify-ast ast namespace-only-paths
                     (lambda (path ast)
                       (let ((element (shen/internal/get-element-at path ast)))
                         (if (not (shen/internal/symbol-prefixed-p element))
                             (shen/internal/prefix-symbol (shen/internal/get-element-at path ast))
                           element))))
    (shen/internal/modify-ast ast quote-only-paths
                     (lambda (path ast)
                       (list 'quote (shen/internal/get-element-at path ast))))
    ast))
;; Modifying\ The\ AST:2 ends here

;; [[file:shen-elisp.org::*Modifying%20The%20AST][Modifying\ The\ AST:3]]
(defun shen/internal/add-funcalls (ast apply-function)
  (let ((paths-only (mapcar (lambda (path-local) (nth 0 path-local)) apply-function)))
    (shen/internal/modify-ast ast (mapcar #'shen/internal/get-path-parent paths-only)
                     (lambda (path ast)
                       (let* ((current-funcalled-list (shen/internal/get-element-at path ast))
                              (function-name (car current-funcalled-list))
                              (function-arguments (cdr current-funcalled-list)))
                         (shen/internal/apply-function
                          function-name
                          function-arguments
                          (shen/internal/lookup-with-default (cons 0 path) apply-function nil)))))))
;; Modifying\ The\ AST:3 ends here

;; [[file:shen-elisp.org::*(Unused)%20Isolating%20and%20Filling][\(Unused\)\ Isolating\ and\ Filling:1]]
(defun shen/make-holed-context (tail-call-path function-chain-path ast)
  (let* ((function-chain (shen/internal/get-element-at function-chain-path ast))
         (tail-call (shen/internal/get-element-at tail-call-path ast))
         (tail-call-relative-path
          (shen/internal/path-slice tail-call-path 0
                  (- (length tail-call-path)
                     (length function-chain-path)))))
    (shen/internal/nset-element-at tail-call-relative-path function-chain 'shen/__hole__)))

(defun shen/used-in-context (context locally-scoped)
  (mapcar (lambda (symbol-index-pair)
            (nth 1 symbol-index-pair))
          (shen/internal/filter
           (lambda (v)
             (not (eq 'shen/not-found (shen/internal/find-all v context))))
           locally-scoped
           't)))

(defun shen/substitute-in-context (context locally-scoped-alist)
  (let ((current-context context))
    (dolist (locally-scoped-pair locally-scoped-alist current-context)
      (let* ((name (nth 0 locally-scoped-pair))
             (value (nth 1 locally-scoped-pair))
             (all-matching-paths (shen/internal/find-all name current-context)))
        (if (not (eq all-matching-paths 'shen/not-found))
            (dolist (path all-matching-paths nil)
              (shen/internal/nset-element-at path current-context value)))))))
;; \(Unused\)\ Isolating\ and\ Filling:1 ends here

;; [[file:shen-elisp.org::*Consolidate%20Call%20Chains][Consolidate\ Call\ Chains:1]]
(defun shen/internal/consolidate (ast matcher-fn collector-fn tx-fn)
  (let* ((current-ast ast)
         (location-containing-chain
          (shen/internal/list-containing-first-occurrence-of matcher-fn ast)))
    (while (not (eq location-containing-chain 'shen/not-found))
      (let ((current-chain (shen/internal/get-element-at location-containing-chain current-ast))
            (accum))
        (progn
          (while (funcall matcher-fn current-chain)
            (let ((collected (funcall collector-fn accum current-chain)))
              (setq accum (nth 0 collected))
              (setq current-chain (nth 1 collected))))
          (setq current-ast
                (shen/internal/nset-element-at
                 location-containing-chain
                 current-ast
                 (funcall tx-fn accum current-chain)))
          (setq location-containing-chain
                (shen/internal/list-containing-first-occurrence-of matcher-fn current-ast)))))
    current-ast))
;; Consolidate\ Call\ Chains:1 ends here

;; [[file:shen-elisp.org::*Consolidate%20Cons][Consolidate\ Cons:1]]
(defun shen/internal/consolidate-cons (ast)
  (shen/internal/consolidate
   ast
   (lambda (current-list)
     (and current-list
          (consp current-list)
          (eq 3 (length current-list))
          (eq (nth 0 current-list) 'shen/cons)))
   (lambda (accum current-chain)
     (list (cons (nth 1 current-chain) accum)
           (nth 2 current-chain)))
   (lambda (accum remaining-chain)
     (if (eq remaining-chain 'nil)
         (cons 'list (reverse accum))
       (list 'append (cons 'list (reverse accum)) remaining-chain)))))
;; Consolidate\ Cons:1 ends here

;; [[file:shen-elisp.org::*Consolidate%20@s][Consolidate\ @s:1]]
(defun shen/internal/consolidate-@s (ast)
  (shen/internal/consolidate
   ast
   (lambda (current-list)
     (and current-list
          (consp current-list)
          (eq 3 (length current-list))
          (eq (nth 0 current-list) 'shen/@s)))
   (lambda (accum current-chain)
     (list (cons (nth 1 current-chain) accum)
           (nth 2 current-chain)))
   (lambda (accum remaining-chain)
     (list 'concat (cons 'concat (reverse accum)) remaining-chain))))
;; Consolidate\ @s:1 ends here

;; [[file:shen-elisp.org::*Consolidate%20tl][Consolidate\ tl:1]]
(defun shen/internal/consolidate-tl (ast)
  (shen/internal/consolidate
   ast
   (lambda (current-list)
     (and current-list
          (consp current-list)
          (eq 2 (length current-list))
          (eq (nth 0 current-list) 'shen/tl)))
   (lambda (accum current-chain)
     (list (if (not accum) 1 (+ accum 1))
           (nth 1 current-chain)))
   (lambda (accum remaining-chain)
     (list 'nthcdr accum remaining-chain))))
;; Consolidate\ tl:1 ends here

;; [[file:shen-elisp.org::*Add%201+'s][Add\ 1+\'s:1]]
(defun shen/internal/add-1+ (ast)
  (shen/internal/consolidate
   ast
   (lambda (current-list)
     (and current-list
          (consp current-list)
          (eq 3 (length current-list))
          (and (eq (nth 0 current-list) 'shen/+)
               (or (eq (nth 1 current-list) 1)
                   (eq (nth 2 current-list) 1)))))
   (lambda (accum current-list)
     (if (eq (nth 1 current-list) 1)
         (list (nth 2 current-list) nil)
       (list (nth 1 current-list) nil)))
   (lambda (accum remaining-chain)
     (list '1+ accum))))
;; Add\ 1+\'s:1 ends here

;; [[file:shen-elisp.org::*Nil%20Comparisons%20To%20Null][Nil\ Comparisons\ To\ Null:1]]
(defun shen/internal/nil-to-null (ast)
  (shen/internal/consolidate
   ast
   (lambda (current-list)
     (and current-list
          (consp current-list)
          (eq 3 (length current-list))
          (and (eq (nth 0 current-list) 'shen/=)
               (or (eq (nth 1 current-list) 'nil)
                   (eq (nth 2 current-list) 'nil)))))
   (lambda (accum current-list)
     (if (eq (nth 1 current-list) 'nil)
         (list (nth 2 current-list) nil)
       (list (nth 1 current-list) nil)))
   (lambda (accum remaining-chain)
     `(shen/predicate->shen (null ,accum)))))
;; Nil\ Comparisons\ To\ Null:1 ends here

;; [[file:shen-elisp.org::*Evaluate%20KLambda][Evaluate\ KLambda:1]]
(defun shen/internal/kl-to-elisp (Kl)
  (shen/internal/nil-to-null
   (shen/internal/add-1+
    (shen/internal/consolidate-tl
     (shen/internal/consolidate-@s
      (shen/internal/consolidate-cons (shen/internal/parse-ast Kl)))))))
;; Evaluate\ KLambda:1 ends here

;; [[file:shen-elisp.org::*Evaluate%20KLambda][Evaluate\ KLambda:2]]
(defun shen/eval-kl (X)
  (if (and (consp X) (eq (car X) 'defun))
      (progn
        (byte-compile (eval (shen/internal/kl-to-elisp (copy-tree X)) 't))
        (nth 1 X))
    (eval (shen/internal/kl-to-elisp X) 't)))
;; Evaluate\ KLambda:2 ends here

;; [[file:shen-elisp.org::*Overrides][Overrides:1]]
(setq shen/*overrides*
      (let ((table (make-hash-table :test 'equal)))
        ;; (ref:performance)
        (puthash 'map
                 `(defun shen/map (F Xs)
                    (mapcar (lambda (X)
                              (shen/internal/apply-higher-order-function F (list X)))
                            Xs))
                 table)
        (puthash 'shen.lazyderef
                 `(defun shen/shen\.lazyderef
                      (X ProcessN)
                    (let ((Current X)
                          (KeepLooking 't))
                      (while KeepLooking
                        (shen/if
                         (shen/shen.pvar? Current)
                         (shen/let Value (shen/shen.valvector Current ProcessN)
                                   (shen/if (shen/= Value 'shen.-null-)
                                            (setq KeepLooking nil)
                                            (setq Current Value)))
                         (setq KeepLooking nil)))
                      Current))
                 table)
        (puthash 'append
                 `(defun shen/append (Xs Ys) (append Xs Ys))
                 table)
        (puthash 'shen.string->bytes
                 `(defun shen/shen.string->bytes (S)
                    (string-to-list S))
                 table)
        (puthash 'shen.sum
                 `(defun shen/shen.sum (Xs) (apply #'+ Xs))
                 table)
        (puthash 'shen.mod
                 `(defun shen/shen.mod (N Div) (mod N Div))
                 table)
        (puthash 'integer?
                 `(defun shen/integer? (N) (integerp N))
                 table)
        (puthash 'abs
                 `(defun shen/shen.abs (N) (abs N))
                 table)
        (puthash 'nth
                 `(defun shen/nth (I Xs) (nth I Xs))
                 table)
         ;; (ref:hash-tables)
        (puthash 'shen/hash
                 `(defun shen/hash
                      (String Limit)
                    (let ((Hash (shen/mod (shen/sum (shen/shen.string->bytes String)) Limit)))
                      (if (= 0 Hash) 1 Hash)))
                 table)
        (puthash '(set *property-vector* (vector 20000))
                 `(shen/set '*property-vector* (make-hash-table :size 1000 :test (quote equal)))
                 table)
        (puthash 'get
                 `(defun shen/get
                      (Pointer Key Table)
                    (let ((Subtable (gethash Pointer Table)))
                      (if (not Subtable)
                          (shen/simple-error
                           (format "pointer not found: %s\n" Pointer))
                        (let ((Value (gethash Key Subtable)))
                          (if (not Value)
                              (shen/simple-error
                               (format "value not found: %s\n" (list Pointer Key))))
                          Value))))
                 table)
        (puthash 'put
                 `(defun shen/put
                      (Pointer Key Value Table)
                    (let ((Subtable (gethash Pointer Table)))
                      (if (not Subtable)
                          (let ((Subtable (make-hash-table :test 'equal)))
                            (progn
                              (puthash Pointer Subtable Table)
                              (puthash Key Value Subtable)))
                        (puthash Key Value Subtable))))
                 table)
        (puthash 'unput
                 `(defun shen/unput
                      (Pointer Key Table)
                    (let ((Subtable (gethash Pointer Table)))
                      (and Subtable
                           (remhash Key Subtable))
                      Pointer))
                 table)
        (puthash 'shen.resize-vector
                 `(defun shen/shen.resize-vector (Vector NewSize Fill)
                    (let* ((VectorLimit (shen/<-address Vector 0))
                           (Current-Index (1+ VectorLimit)))
                      (puthash 0 NewSize Vector)
                      (while (<= Current-Index NewSize)
                        (puthash Current-Index Fill Vector)
                        (setq Current-Index (1+ Current-Index)))
                      Vector))
                 table)
        ;; (ref:namespacing)
        (puthash 'function
                 `(defun shen/function (S)
                    (shen/shen\.lookup-func
                     (shen/internal/unprefix-symbol S)
                     (shen/value 'shen\.*symbol-table*)))
                 table)
        ;; (ref:klambda bugs)
        (puthash 'untrack
                 `(defun shen/untrack (F)
                    (progn
                      (shen/set 'shen.*tracking* (shen/internal/delete-first-eq F (shen/value 'shen.*tracking*)))
                      (shen/eval (shen/ps F))))
                 table)
        table))
;; Overrides:1 ends here

;; [[file:shen-elisp.org::*Evaluating%20Bootstrapped%20KLambda][Evaluating\ Bootstrapped\ KLambda:1]]
(defun shen/patch-klambda (ast)
 (if (eq (car ast) 'defun)
       (let ((override (gethash (nth 1 ast) shen/*overrides*)))
         (or override
             (shen/internal/parse-ast ast)))
     (let ((patched (gethash ast shen/*overrides* )))
       (or patched
           (shen/internal/parse-ast ast)))))
;; Evaluating\ Bootstrapped\ KLambda:1 ends here

;; [[file:shen-elisp.org::*Evaluating%20Bootstrapped%20KLambda][Evaluating\ Bootstrapped\ KLambda:2]]
(defun shen/kl-to-buffer (X B)
  (with-current-buffer B
    (save-excursion
      (goto-char (point-max))
      (insert (pp-to-string
               (shen/internal/nil-to-null
                (shen/internal/add-1+
                 (shen/internal/consolidate-tl
                  (shen/internal/consolidate-@s
                   (shen/internal/consolidate-cons
                    (shen/patch-klambda X)))))))))))
;; Evaluating\ Bootstrapped\ KLambda:2 ends here

;; [[file:shen-elisp.org::*Providing%20The%20Primitives][Providing\ The\ Primitives:1]]
(provide 'shen-primitives)
;; Providing\ The\ Primitives:1 ends here
