;;; signature-display.el --- How to display the signature buffer

;;; Commentary:

;; This file defines faces and function to store text properties along
;; with the signature charachters.

;;; Code:

(defface signature-file-name-face
 '((t (:underline t :foreground "lightgreen")))
 "How to display the titles of episteme memories"
 :group 'signature-display)

(defun signature--render-file-name (filename prefix)
 "Strip FILENAME of PREFIX and add text properties for rendering."
 (let ((base-file-name (s-chop-prefix prefix filename)))
  (add-text-properties 0 (length base-file-name)
   `(face signature-file-name-face file ,filename) base-file-name)
  base-file-name))

(defun signature--render-signature-char (char file line)
 "Assign text properties to CHAR. The text properties are FILE and LINE."
 (if (= (length char) 1)
  (progn (add-text-properties 0 1 `(file ,file line ,line) char) char)
  char))

(provide 'signature-display)

;;; signature-display.el ends here
