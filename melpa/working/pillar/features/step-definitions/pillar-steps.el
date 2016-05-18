(defun pillar-steps::faces-at-point ()
  "Return a list of faces at the current point."
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if (listp face)
        face
      (list face))))

(defun pillar-steps::fontify ()
  "Make sure the buffer is completely fontified."
  (setq font-lock-fontify-buffer-function
        #'font-lock-default-fontify-buffer)
  (font-lock-fontify-buffer))

(defun pillar-steps::character-fontified-p (property valid-values)
  "Check if character at point as face PROPERTY.
The value of the face PROPERTY must be one of VALID-VALUES."
  (pillar-steps::fontify)
  (cl-member-if
   (lambda (face)
     (memq (face-attribute face property nil t) valid-values))
   (pillar-steps::faces-at-point)))

(defun pillar-steps::character-bold-p ()
  "Make sure the character at point is bold."
  (pillar-steps::character-fontified-p
   :weight
   '(semi-bold bold extra-bold ultra-bold)))

(Then "current point should be in bold"
  (lambda ()
    (cl-assert
     (pillar-steps::character-bold-p)
     nil
     "Expected current point to be in bold")))

(defun pillar-steps::character-italic-p ()
  "Make sure the character at point is italic."
  (pillar-steps::character-fontified-p
   :slant
   '(italic oblique)))

(Then "current point should be in italic"
  (lambda ()
    (cl-assert
     (pillar-steps::character-italic-p)
     nil
     "Expected current point to be in italic")))

(defun pillar-steps::character-strike-through-p ()
  "Make sure the character at point is in strike-through."
  (pillar-steps::character-fontified-p
   :strike-through
   '(t)))

(Then "current point should be in strike-through"
  (lambda ()
    (cl-assert
     (pillar-steps::character-strike-through-p)
     nil
     "Expected current point to be in strike-through")))

(defun pillar-steps::character-underline-p ()
  "Make sure the character at point is underlined."
  (pillar-steps::character-fontified-p
   :underline
   '(t)))

(Then "^current point should be in underline$"
  (lambda ()
    (cl-assert
     (pillar-steps::character-underline-p)
     nil
     "Expected current point to be in underline")))

(Then "^current point should have the \\([-[:alnum:]]+\\) face$"
  (lambda (face)
    (pillar-steps::fontify)
    (cl-assert
     (cl-member
      (intern face)
      (pillar-steps::faces-at-point)))
    nil))

(Then "^current point should have no face$"
  (lambda ()
    (pillar-steps::fontify)
    (cl-assert
     (null (pillar-steps::faces-at-point)))
    nil))

(Given "^I delete other windows$"
  (lambda ()
    (delete-other-windows)))

(Given "^I convert the buffer to latex$"
  (lambda ()
    (p2l-convert-buffer)))

(Given "^I load latex2pillar$"
  (lambda ()
    (require 'pillar-latex2pillar)))

(Then "^buffer should be empty$"
  (lambda ()
    (let ((message "Buffer supposed to be empty but contains %s chars"))
      (cl-assert
       (= (point-min) (point-max)) nil "foo" (- (point-max) (point-min))))))

(Given "^I insert a new line$"
  (lambda ()
    (newline 1)))

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
