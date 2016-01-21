;;; signature-api.el --- Language api

;;; Commentary:

;; Language api, subclass the various class, and method matcher
;; classes and supply regular expressions to match a source code line.
;; Then put an instance of a subclass of signature-language onto
;; signature--languages.

;;; Code:

(defvar signature--languages nil
 "This variable holds all parseable languages.")

(defclass signature-language ()
 ((name :initarg :name)
  (extension :initarg :extension)
  (source-line-matchers :initarg :source-line-matchers))
 :abstract "I am the generic language definition!"
 :documentation "source-line-matchers is an ordered list of
 source-line matchers. They should inherit from one of the
 supplied subclasses of signature-source-line-matcher.")

;; Abstract classes definining how the stack is handled when their
;; subclasses are matching a line of code:

(defclass signature-source-line-matcher ()
 ((regexp :initarg :regexp))
 :abstract "Use one of my predefined subclasses!"
 :documentation "Base class for source line matchers.")

(defclass signature-source-line-push-scope (signature-source-line-matcher)
 ()
 :abstract "Subclass me or use a predefined subclass!"
 :documentation "My subclasses push state on the stack.")

(defclass signature-source-line-pop-scope (signature-source-line-matcher)
 ()
 :abstract "Subclass me our use a predefined subclass!"
 :documentation "My subclasses pop state off the stack.")

(defclass signature-source-line-no-scope (signature-source-line-matcher)
 ()
 :abstract "Subclass me our use a predefined subclass!"
 :documentation "My subclasses do neither push or pop the stack.")

;; Base classes implementing basic language constructs. Subclass these
;; with a regexp matching the specific language:

(defclass signature-match-class       (signature-source-line-push-scope) ())
(defclass signature-match-conditional (signature-source-line-push-scope) ())
(defclass signature-match-loop        (signature-source-line-push-scope) ())
(defclass signature-match-method      (signature-source-line-push-scope) ())
(defclass signature-match-comment     (signature-source-line-no-scope) ())

;; A base class that matches any non-blank line. It can be used as
;; fallback matcher in language definitions:

(defclass signature-match-any (signature-source-line-no-scope)
 ((regexp :initform "[^\s]+")))

(provide 'signature-api)

;;; signature-api.el ends here
