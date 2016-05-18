(require 'debug-print)

;; Note that the below are result with `eval-last-sexp'.
;; Evaluation with `debug-print-eval-last-sexp' will cause duplicate replacement.

(debug-print:code-walk '() nil)
; => nil
(debug-print:code-walk '(a b c d) nil)
; => (a b c d)
(debug-print:code-walk '(func ::?= a) nil)
; => (func
;     (debug-print a nil))
(debug-print:code-walk '(func ::?= a b) nil)
; => (func
;     (debug-print a nil)
;     b)
(keu:replace-in-tree '((::?= @ ())) '(func ::?= a b))
; => (func a b)
(debug-print:code-walk ''(+ 1 ::?= (+ 2 3)) nil)
; => '(+ 1
;        (debug-print
;         (+ 2 3)
;         nil))
(debug-print:code-walk '`(+ 1 ::?= (+ 2 3)) nil)
; => '(+ 1
;        (debug-print
;         (+ 2 3)
;         nil))
;; The following should return '(+ 1 5) and display debugging message,
;; but it does not work! I don't know any ideas for it. Users have to
;; use `debug-print' directly.
(debug-print:code-walk `'(+ 1 ,::?= (+ 2 3)) nil)
; => '(+ 1
;        (debug-print
;         (+ 2 3)
;         nil))

(debug-print:code-walk '(defun fact (n)
                          (if (zerop n)
                              1
                              (* n ::?= (fact (- n 1)))))
                       nil)
; => (defun fact
;      (n)
;      (if
;          (zerop n)
;          1
;          (* n
;             (debug-print
;              (fact
;               (- n 1))
;              "fact"))))

(debug-print:code-walk '(defsubst hoge (n) (foo ::?= n))  nil)
; => (prog1
;        (defun hoge
;          (n)
;          (foo
;           (debug-print n "hoge")))
;      (progn
;        (put 'hoge 'byte-optimizer 'byte-compile-inline-expand)))

(debug-print:code-walk '(defmacro fact* (n)
                          (if (zerop n)
                              1
                              `(* ,n ::?= (fact* ,(- n 1)))))
                       nil)
; => (defmacro fact*
;      (n)
;      (if
;          (zerop n)
;          1
;          (list '* n '::\?=
;                (list 'fact*
;                      (- n 1)))))



(debug-print-init)

;; an example from Gauche's document
(eval-with-debug-print
 (defun fact (n)
   (if (zerop n)
       1
       (* n ::?= (fact (- n 1))))))
(fact 5)
; => 120
;; and in the buffer "*debug-print*"
; ::?="fact"::(fact (- n 1))
; ::?="fact"::(fact (- n 1))
; ::?="fact"::(fact (- n 1))
; ::?="fact"::(fact (- n 1))
; ::?="fact"::(fact (- n 1))
; ::?-    1
; ::?-    1
; ::?-    2
; ::?-    6
; ::?-    24

;; test recursive macro definition
(eval-with-debug-print
 (defmacro fact* (n)
   (if (zerop n)
       1
       `(* ,n ::?= (fact* ,(- n 1))))))
(eval-with-debug-print (fact* 5))
; => 120
;; and in the buffer "*debug-print*"
; ::?=""::(* 4 (debug-print (* 3 (debug-print (* 2 (debug-print (* 1 (debug-print 1 nil)) nil)) nil)) nil))
; ::?=""::(* 3 (debug-print (* 2 (debug-print (* 1 (debug-print 1 nil)) nil)) nil))
; ::?=""::(* 2 (debug-print (* 1 (debug-print 1 nil)) nil))
; ::?=""::(* 1 (debug-print 1 nil))
; ::?=""::1
; ::?-    1
; ::?-    1
; ::?-    2
; ::?-    6
; ::?-    24

;; test `defadvice'
(eval-with-debug-print
 (defadvice fact (around test disable)
   ad-do-it))
