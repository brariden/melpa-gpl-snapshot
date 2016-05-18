;;; ivariants-to.el --- Ideographic Variants Converter  -*- lexical-binding: t -*-

;; Filename: ivariants-to.el
;; Description: Ideographic Variants Converter
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2014-01-01
;; Package-Requires: ((emacs "24.3"))
;; Version: 1.140329
;; Keywords: i18n languages
;; Namespace: ivariants-to-
;; Human-Keywords: Ideographic Variants
;; URL: http://github.com/kawabata/ivariants

;;; Code:

(require 'ivariants-table)

(defun ivariants-to (from to &rest attributes)
  "Convert region FROM TO to specified variants with ATTRIBUTES.
If variant with first attribute is first, then that replaces.
If not, then second attribute will be searched, and so on."
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\cC" nil t)
        (let* ((char (string-to-char (match-string 0)))
               (table (aref ivariants-table char))
               (attrs attributes))
          (while attrs
            (let ((chars (assoc-default (car attrs) table)))
              (if (null chars) (setq attrs (cdr attrs))
                (let ((string (mapconcat (lambda (x) (if (characterp x) (string x) x)) chars "")))
                  (replace-match
                   (if (= 1 (length chars)) string (concat "[" string "]"))))
                (setq attrs nil)))))))))

;;;###autoload
(defun ivariants-to-simplified (from to)
  "Convert region FROM TO from traditional to simplified character.
Note: Japanese characters will be ignored."
  (interactive "r")
  (ivariants-to from to 'cjkvi/simplified))

;;;###autoload
(defun ivariants-to-traditional (from to)
  "Convert region FROM TO from simplified to traditional character.
Note: Japanese characters will be ignored."
  (interactive "r")
  (ivariants-to from to 'cjkvi/traditional))

;;;###autoload
(defun ivariants-to-pr-china (from to)
  "Convert region FROM TO from Taiwan/Japanese character to P.R.China's character."
  (interactive "r")
  (ivariants-to from to 'dailyuse/pr-china))

;;;###autoload
(defun ivariants-to-japan (from to)
  "Convert region FROM TO from Chinese character to Japanese character."
  (interactive "r")
  (ivariants-to from to 'dailyuse/japan))

;;;###autoload
(defun ivariants-to-taiwan (from to)
  "Convert region FROM TO from Chinese/Japanese character to Taiwanese character."
  (interactive "r")
  (ivariants-to from to 'dailyuse/taiwan))

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:

(provide 'ivariants-to)
;;; ivariants-to.el ends here
