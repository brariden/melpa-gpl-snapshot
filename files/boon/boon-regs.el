;;; boon-regs.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:


;; A region list has the following form: ((mark . point) (mark . point) ...)

;;; Code:

(defun boon-reg-point (reg)
  (cdr reg))

(defun boon-reg-mark (reg)
  (car reg))

(defun boon-mk-reg (mrk pnt)
  (cons mrk pnt))

(defun boon-normalize-reg (reg)
  "Normalize the region REG by making sure that mark < point."
  (boon-mk-reg (boon-reg-begin reg) (boon-reg-end reg)))

(defun boon-reg-to-markers (reg)
  "Put copy the markers defining REG borders, and return that."
  (boon-mk-reg (copy-marker (boon-reg-mark reg)) (copy-marker (boon-reg-point reg))))

(defun boon-borders (reg how-much)
  "Given a normalized region REG, return its borders (as a region list).
The size of the borders is HOW-MUCH."
  ;; TODO: if the results would tourch or overlap, return the input region
  (list (boon-mk-reg (boon-reg-end reg)   (- (boon-reg-end reg) how-much))
        (boon-mk-reg (boon-reg-begin reg) (+ (boon-reg-begin reg) how-much))))

(defun boon-include-surround-spaces (reg)
  (save-excursion
    (let* ((beg (boon-reg-begin reg))
           (end (boon-reg-end   reg))
           (space-at-end (progn
                           (goto-char end)
                           (looking-at "\\s-"))))
      (if space-at-end
          (boon-mk-reg beg (progn
                             (skip-syntax-forward "-")
                             (point)))
        (boon-mk-reg (progn (goto-char beg)
                            (skip-syntax-backward "-")
                            (point))
                     end)))))

(defun boon-reg-begin (reg)
  "The begining of region REG."
  (min (boon-reg-point reg) (boon-reg-mark reg)))

(defun boon-reg-end (reg)
  "The end of region REG."
  (max (boon-reg-point reg) (boon-reg-mark reg)))

(defun boon-content (reg)
  "Given a region REG, return its contents (crop the region by 1)."
  (boon-mk-reg (+ (boon-reg-begin reg) 1) (- (boon-reg-end reg) 1)))

(defun boon-reg-after (r1 r2)
  "Return non-nil when R1 occurs after R2."
  (> (boon-reg-begin r1) (boon-reg-end r2)))

(provide 'boon-regs)
;;; boon-regs.el ends here
