;;; signature-stack.el --- Stack handling for source code parsing.

;;; Commentary:

;; These functions take a matcher and the current stack and decide if
;; pushing to the stack is warranted. The basic implementation does
;; not care about the stack but your subclassed implementation might.

;;; Code:

(defgeneric signature--push-state-p (matcher &optional stack)
 "Generic function to decide wether the line pushes a new scope to the stack.")

(defgeneric signature--pop-state-p (matcher &optional stack)
 "Generic function to decide wether the line pops a scope off the stack.")

;; Pushing to the stack:

(defmethod signature--push-state-p ((m signature-source-line-matcher) &optional stack)
 "Default is not to push onto the stack."
 nil)

(defmethod signature--push-state-p ((m signature-source-line-push-scope) &optional stack)
 "Classes inheriting from signature-source-line-push-scope push onto the stack."
 t)

;; Poping the stack:

(defmethod signature--pop-state-p ((m signature-source-line-matcher) &optional stack)
 "Default is not to pop the stack."
 nil)

(defmethod signature--pop-state-p ((m signature-source-line-pop-scope) &optional stack)
 "Classes inheriting from signature-soure-line-pop-scope pop the stack."
 t)

(provide 'signature-stack)

;;; signature-stack.el ends here
