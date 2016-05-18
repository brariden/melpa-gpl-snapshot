;;; ivariants-browse.el --- Ideographic Variants Browser  -*- lexical-binding: t -*-

;; Filename: ivariants-browse.el
;; Description: Ideographic Variants Browser
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2014-01-01
;; Package-Requires: ((emacs "24.3") (ivs-edit "1.0"))
;; Version: 1.140721
;; Keywords: i18n languages
;; Namespace: ivariants-browse-
;; Human-Keywords: Ideographic Variants
;; URL: http://github.com/kawabata/ivariants

;;; Code:

(require 'ivariants-table)
(require 'ivs-edit)
(require 'tree-widget)
(require 'wid-edit)
(require 'cl-lib)

(defun ivariants-browse-expander (widget)
  "Return the list of tree as a child of WIDGET."
  (when (characterp (widget-get widget :char))
    (let* ((char      (widget-get widget :char))
           (node-aj1  (ivariants-browse-ivs
                       char 'Adobe-Japan1))
           (node-hd   (ivariants-browse-ivs
                       char 'Hanyo-Denshi))
           (alist     (get-char-code-property char 'ivariants))
           (ivariants)
           (nodes))
      (dolist (item alist)
        (setq ivariants (append (cdr item) ivariants)))
      (if node-aj1 (push node-aj1 nodes))
      (if node-hd  (push node-hd nodes))
      (dolist (item alist)
        (let ((attribute (car item))
              (variants (cdr item)))
          (dolist (variant variants)
            (push
             (list 'tree-widget
                   :tag (format "%s %s"
                                (if (characterp variant) (char-to-string variant)
                                  variant)
                                (or (gethash attribute ivariants-name-table)
                                    (symbol-name attribute)))
                   :char variant :open nil
                   :expander 'ivariants-browse-expander)
             nodes))))
      (nreverse nodes))))

(defun ivariants-browse-ivs (char ivd)
  "Return the widget node of CHAR with IVD."
  (let* ((entries (gethash char ivs-edit-table))
         (entries (cl-remove-if-not (lambda (x) (equal (elt x 1) ivd)) entries))
         children)
    (when entries
      (dolist (entry entries)
        (push
         (list 'tree-widget
               :tag (format "%c%c (%s)" char (car entry) (elt entry 2))
               :open t :has-children nil)
         children))
      `(tree-widget :tag ,(format "%c【%s】" char ivd)
                    :open nil
                    ,@(nreverse children)))))

;;;###autoload
(defun ivariants-browse (&optional char)
  (interactive
   (let* ((char (char-after (point)))
          (mnemonics (and char (category-set-mnemonics (char-category-set char)))))
     (list (string-to-char
            (read-string "Char? : " (if (and mnemonics (string-match "C" mnemonics))
                                        (char-to-string char)))))))
  (switch-to-buffer "*Search IVariants*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    (mapc #'delete-overlay (car all)))
  (widget-insert "【Ideographic Variants】\n\n")
  (widget-create 'tree-widget
                 :tag (format "%c (%04X)" char char)
                 :char char
                 :open nil ; t
                 :expander 'ivariants-browse-expander
                 :has-children t)
  (widget-insert "\n")
  ;; Insert the Close button
  (widget-create 'push-button
                 :keymap tree-widget-button-keymap
                 :notify 'ivariants-browse-close
                 "Close")
  (use-local-map widget-keymap)
  (widget-setup)
  (help-mode)
  (goto-char (point-min))
  (widget-forward 1))

(defun ivariants-browse-close (&rest _ignore)
  "Close the current dialog.
IGNORE arguments."
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'ivariants-browse)

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:

;;; ivariants-browse.el ends here
