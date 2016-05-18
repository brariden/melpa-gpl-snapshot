;;; ivariants.el --- Ideographic variants editor and browser -*- lexical-binding: t -*-

;; Filename: ivariants.el
;; Description: Ideographic Variants Editor
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2014-01-01
;; Package-Requires: ((emacs "24.3") (ivs-edit "1.0"))
;; Version: 1.140406
;; Keywords: i18n languages
;; Namespace: ivariants-
;; Human-Keywords: Ideographic Variants
;; URL: http://github.com/kawabata/ivariants

;;; Commentary:

;; * ivariants.el … Ideographic Variants Editor and Browser
;;
;; `ivariants.el' and related files provide various Ideographic Variants
;; editing and browsing capabilities.
;;
;; ** Inserting Variants
;;
;; `M-x ivariants-insert' inserts the variants at the current position.
;;
;; ** Browsing Variants
;;
;; `M-x ivariants-browse' let you browse the variants.
;;
;; ** Converting to Simplified/Traditional/Taiwanese/Japanese forms.
;;
;; Following commands for specified region are provided.
;;
;; - ivariants-to-simplified
;; - ivariants-to-traditional
;; - ivariants-to-pr-china
;; - ivariants-to-japan
;; - ivariants-to-taiwan
;;
;; ** Configuration
;;
;; Typical configurations are:
;;
;; : (global-set-key (kbd "M-I")   'ivariants-insert)
;; : (global-set-key (kbd "C-c i") 'ivariants-browse)
;;
;; * Notes
;;
;; Sometimes, "traditional character" of P.R.C and Taiwanese character may
;; differ. For example, "说"'s traditional character is "説" while
;; Taiwanese character is "說".
;;
;; * Variant Data
;;
;; Variant data are taken from https://github.com/cjkvi/cjkvi-variants
;; and http://unicode.org/Public/UCD/latest/ucd/StandardizedVariants.txt.
;; 'DailyUse' is a modified version of data from
;; http://kanji.zinbun.kyoto-u.ac.jp/~yasuoka/CJK.html.

;;; Code:

(require 'ivariants-table)
(require 'cl-lib)

(defvar ivariants-order
  '(proper "ᴾ" traditional "ᵀ" simplified "ˢ" variant-simplified "ⱽ"
    japan "ⱼ" taiwan "ₜ" duplicate "ᴰ" compatibility "ᶜ" pseudo-simplified "ₚ"
    variant "|" non-cognate "☠" borrowed "ᵇ" kangxi "ᴷ" radical "ᴿ" \.)
  "Order to list in \\[ivariants-insert].
String is an indicator/separator if previous kind of variants are displayed.
\"\\.\" is a wildcard for the rest of all variants.")

;; calculation

(defun ivariants-by-category (char category-str)
  "Get all CHAR variants of CATEGORY-STR."
  (let ((alist (aref ivariants-table char))
        result)
    (dolist (item alist)
      (let ((prop (symbol-name (car item)))
            (regexp (concat "/" category-str))
            (chars (cdr item)))
        (if (string-match regexp prop)
            (if (listp chars)
                (setq result (append result chars))
              (setq result (append result (list chars)))))))
    (cl-remove-duplicates result :test 'equal)))

(defun ivariants-by-category-string (char category)
  "Categorize CHAR variants by symbol CATEGORY.
Returned value is a list of string."
  (mapcar
   (lambda (char)
     (if (characterp char) (char-to-string char) char))
   (ivariants-by-category char (symbol-name category))))

(defun ivariants-char-string (char)
  "Collect all uniqe variants of CHAR.
Lists are ordered according to `ivariants-order'."
  (let ((variants-all)
        (variants-group))
    (mapconcat
     (lambda (category)
       (if (stringp category)
           (when variants-group
             (setq variants-group nil)
             category)
         (let* ((variants (ivariants-by-category-string char category)))
           (setq variants
                 (cl-set-difference variants variants-all :test 'equal))
           (when variants
             (setq variants-all (cl-union variants variants-all :test 'equal))
             (setq variants-group (cl-union variants variants-group :test 'equal)))
           (apply 'concat variants))))
     ivariants-order "")))

;;;###autoload
(defun ivariants-insert ()
  "Insert variants short string form after point."
  (interactive)
  (let ((string (ivariants-char-string (char-after (point)))))
    (if string (insert "《" string "》")
      (message "No varinats found!"))))

(provide 'ivariants)

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:

;;; ivariants.el ends here
