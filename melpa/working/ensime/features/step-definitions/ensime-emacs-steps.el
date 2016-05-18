;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I open temp \\(java\\|scala\\) file \"\\(.+\\)\"$"
      (lambda (suffix arg)
        (find-file (make-temp-file arg nil (format ".%s" suffix)))))

(When "^I insert import \"\\(.+\\)\"$"
     (lambda (import)
       (ensime-insert-import import)))

(When "^I go to the end of the line$"
      (lambda () (end-of-line)))

(Then "^I should have an empty completion prefix$"
      (lambda ()
        (let ((expected (ensime-completion-prefix-at-point)))
          (assert (equal "" expected) nil
                  (format "Expected prefix %s, but was %s" "" expected)))))

(Then "^I should have completion prefix \"\\(.+\\)\"$"
      (lambda (prefix)
        (let ((expected (ensime-completion-prefix-at-point)))
          (assert (equal prefix expected) nil
                  (format "Expected prefix %s, but was %s" prefix expected)))))

(Then "^I should see unique prefixes:"
      (lambda (prefixes)
        (-each (cdr prefixes)
          (-lambda ((prefix))
            (When (format "I place the cursor after %S" prefix))
            (Then (format "I should have completion prefix %S" prefix))))))

(Then "^I should see prefixes at the end of lines:"
      (lambda (prefixes-lines)
        (-each (cdr prefixes-lines)
          (-lambda ((prefix line))
            (When (format "I go to line %S" line))
            (And (format "I go to the end of the line"))
            (Then (format "I should have completion prefix %S" prefix)))))) 
