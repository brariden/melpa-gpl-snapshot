;;; signature-markers.el --- Visual markers.

;;; Commentary:

;; Visual markers to use for the various language constructs in the
;; signature. This is how conditional, methods, loops etc get
;; translated into the ascii signature.

;;; Code:

(defmethod signature--marker ((m signature-source-line-matcher))
 "How to denote something non-descript."
 "")

(defmethod signature--marker ((m signature-match-comment))
 "How to denote a comment."
 "'")

(defmethod signature--marker ((m signature-match-any))
 "How to denote anything."
 ".")

(defmethod signature--marker-enter ((m signature-match-conditional))
 "Signature character representing entering a conditional."
 "(")

(defmethod signature--marker-exit ((m signature-match-conditional))
 "Signature character representing exiting a conditional."
 ")")

(defmethod signature--marker-enter ((m signature-match-class))
 "Signature character representing entering a class."
 "<")

(defmethod signature--marker-exit ((m signature-match-class))
 "Signature character representing exiting a class."
 ">")

(defmethod signature--marker-enter ((m signature-match-method))
 "Signature character representing entering a method."
 "{")

(defmethod signature--marker-exit ((m signature-match-method))
 "Signature character representing exiting a method."
 "}")

(defmethod signature--marker-enter ((m signature-match-loop))
 "Signature character representing entering a loop."
 "[")

(defmethod signature--marker-exit ((m signature-match-loop))
 "Signature character representing exiting a loop."
 "]")

(provide 'signature-markers)

;;; signature-markers.el ends here
