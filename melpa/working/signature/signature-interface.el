;;; signature-interface.el --- External interface

;;; Commentary:

;; The signature-report is the main entry to signature.

;;; Code:

(defun signature-quit ()
 "Kill signature buffer."
 (interactive)
 (kill-buffer))

(defun signature-visit-location-at-point ()
 "Visit FILE:LINE for charachter at point."
 (interactive)
 (let ((file (get-text-property (point) 'file))
       (line (get-text-property (point) 'line)))
  (when file
   (with-current-buffer (find-file file)
    (when line
     (forward-line (1- line)))))))

(defvar signature-mode-map
 (let ((map (make-sparse-keymap)))
  (define-key map (kbd "M-.") 'signature-visit-location-at-point)
  (define-key map (kbd "C-q") 'signature-quit)
  map)
 "Keymap for command `signature-mode'.")

(define-derived-mode signature-mode fundamental-mode "Signature")

;;;###autoload
(defun signature-report ()
 "Report on signatures for files matching a glob."
 (interactive)
 (with-current-buffer (get-buffer-create "*signature*")
  (setq buffer-read-only nil)
  (erase-buffer)
  (let* ((glob (read-string "Glob: "))
         (boring-prefix (expand-file-name (car (split-string glob "*")))))
   (dolist (file (f-glob glob))
    (insert (signature--render-file-name file boring-prefix))
    (newline)
    (cl-multiple-value-bind (class-count method-count line-count signature-string) (signature--parse-file file)
     (insert (format "%dc\t%dm\t%dL" class-count method-count line-count))
     (newline)
     (insert signature-string))
    (newline)
    (newline))
   (setq buffer-read-only t)
   (signature-mode)
   (switch-to-buffer "*signature*"))))

(provide 'signature-interface)

;;; signature-interface.el ends here
