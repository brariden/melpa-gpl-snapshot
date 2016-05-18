;;; ivariants-table.el --- Ideographic Variants Table -*- lexical-binding: t -*-

;; Filename: ivariants-table.el
;; Description: Ideographic Variants Table
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2013-03-25
;; Package-Requires: ((emacs "24.3"))
;; Version: 1.140720
;; Keywords: i18n languages
;; Namespace: ivariants-
;; Human-Keywords: Ideographic Variants
;; URL: http://github.com/kawabata/ivariants

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (when (featurep 'ivariants) (unload-feature 'ivariants t))
  (when (featurep 'ivariants-browse) (unload-feature 'ivariants-browse t))
  (when (featurep 'ivariants-table) (unload-feature 'ivariants-table t)))

(declare-function ivariants-add-charstr "ivariants-table" (charstr prop valstr))
(declare-function ivariants-parse-files "ivariants-table" ())
(declare-function ivariants-parse-standardized-variants  "ivariants-table" ())
(declare-function ivariants-parse-dailyuse "ivariants-table" ())
(declare-function ivariants-compat-chars "ivariants-table" ())

(eval-when-compile
  (defvar ivariants-files)
  (setq ivariants-files
    '(;; chinese
      "cjkvi-simplified.txt"
      "dypytz-variants.txt"
      "hydzd-borrowed.txt"
      "hydzd-variants.txt"
      ;; japanese
      "hyogai-variants.txt"
      "jinmei-variants.txt"
      "jisx0212-variants.txt"
      "jisx0213-variants.txt"
      "joyo-variants.txt"
      "jp-borrowed.txt"
      "koseki-variants.txt"
      "x0212-x0213-variants.txt"
      ;; misc
      "cjkvi-variants.txt"
      "duplicate-chars.txt"
      "non-cjk.txt"
      "non-cognates.txt"
      "numeric-variants.txt"
      "radical-variants.txt"
      "ucs-scs.txt"
      ))

  (defvar ivariants-char-table nil)
  (setq ivariants-char-table (make-char-table 'char-code-property-table))
  (defvar ivariants-attr-name-table nil)
  (setq ivariants-attr-name-table (make-hash-table :test 'equal))

  ;; utilities
  (defun ivariants-add-charstr (charstr prop valstr)
    (when (= 1 (length charstr))
      (let* ((char (string-to-char charstr))
             (val (if (= 1 (length valstr)) (string-to-char valstr) valstr))
             (alist (aref ivariants-char-table char))
             (item (assq prop alist)))
        (if item
            (unless (memq val item) (nconc item (list val)))
          (setq alist (cons (list prop val) alist))
          (aset ivariants-char-table char alist)))))

  (require 'bytecomp)
  (defvar ivariants-directory
    (expand-file-name
     "tables"
     (file-name-directory (or byte-compile-current-file
                              load-file-name
                              buffer-file-name))))

  (defun ivariants-parse-files ()
    "Parse variant data files."
    (interactive)
    (let* ((reverse-table (make-hash-table :test 'equal :size 50000)))
      (dolist (i ivariants-files)
        (with-temp-buffer
          (message "Now loading [%s]..." i)
          (insert-file-contents (expand-file-name i ivariants-directory))
          (goto-char (point-min))
          (while (re-search-forward "^\\(.+?\\),\\(.+?\\),\\([^,\n]+\\)" nil t)
            (let ((a (match-string 1))
                  (b (intern (match-string 2)))
                  (c (match-string 3)))
              (if (equal b '<rev>) (puthash (intern a) (intern c) reverse-table)
                (if (equal b '<name>) (puthash (intern a) c ivariants-attr-name-table)
                  (let ((rev (gethash b reverse-table)))
                    (ivariants-add-charstr a b c)
                    (if rev
                        (ivariants-add-charstr c rev a)))))))))))

  (defun ivariants-parse-standardized-variants ()
    "Parse StandardizedVariants.txt."
    (interactive)
    (puthash 'cjkvi/standard-variants "Standardized Variants"
             ivariants-attr-name-table)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "StandardizedVariants.txt"
                                              ivariants-directory))
      (while
          (re-search-forward
           "^\\([0-9A-F]+\\) \\([0-9A-F]+\\); CJK COMPATIBILITY IDEOGRAPH-\\([0-9A-F]+\\);"
           nil t)
        (let ((char (string-to-number (match-string 1) 16))
              (vari (string-to-number (match-string 2) 16))
              (comp (string-to-number (match-string 3) 16)))
          (ivariants-add-charstr (string comp) 'cjkvi/standard-variants
                                 (string char vari))))))

  (defun ivariants-parse-dailyuse ()
    (interactive)
    (puthash 'dailyuse/japan "常用漢字（日本）" ivariants-attr-name-table)
    (puthash 'dailyuse/pr-china "一级字表（中国）" ivariants-attr-name-table)
    (puthash 'dailyuse/taiwan "常用國字（臺灣）" ivariants-attr-name-table)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "DailyUse" ivariants-directory))
      (while
          (re-search-forward "^\\([^#]\\)\t\\(.\\)\t\\(.\\)" nil t)
        (let* ((japan (match-string 1))
               (pr-china (match-string 2))
               (taiwan (match-string 3)))
          (when (and (not (equal japan "*")) (not (equal pr-china "*")))
            (ivariants-add-charstr japan 'dailyuse/pr-china pr-china)
            (ivariants-add-charstr pr-china 'dailyuse/japan japan))
          (when (and (not (equal taiwan "*")) (not (equal pr-china "*")))
            (ivariants-add-charstr pr-china 'dailyuse/taiwan taiwan)
            (ivariants-add-charstr taiwan 'dailyuse/pr-china pr-china))
          (when (and (not (equal japan "*")) (not (equal taiwan "*")))
            (ivariants-add-charstr taiwan 'dailyuse/japan japan)
            (ivariants-add-charstr japan 'dailyuse/taiwan taiwan))))
      (goto-char (point-min))
      ;; remove single, identical mapping
      (while
          (re-search-forward "^\\([^#]\\)\t\\(.\\)\t\\(.\\)" nil t)
        (dolist (char
                 (mapcar 'string-to-char
                         (list (match-string 1) (match-string 2) (match-string 3))))
          (let ((table (aref ivariants-char-table char)))
            (dolist (attr '(dailyuse/japan dailyuse/pr-china dailyuse/taiwan))
              (setq table
                    (cl-remove-if
                     (lambda (x) (and (equal attr (car x)) (equal char (cadr x))
                                  (null (cddr x)))) table)))
            (aset ivariants-char-table char table))))))

  (defun ivariants-compat-chars ()
    (puthash 'cjkvi/compatibility "互換漢字" ivariants-attr-name-table)
    (puthash 'cjkvi/unified "統合漢字" ivariants-attr-name-table)
    (let ((ranges '((?豈 . ?龎)
                    (?丽 . ?𪘀))))
      (dolist (range ranges)
        (cl-do ((char (car range) (1+ char))) ((= char (cdr range)))
          (let ((unified (car (get-char-code-property char 'decomposition))))
            (when unified
              (ivariants-add-charstr
               (string unified) 'cjkvi/compatibility (string char))
              (ivariants-add-charstr
               (string char) 'cjkvi/unified (string unified))))))))

  ;; execute
  (ivariants-parse-files)
  (ivariants-parse-standardized-variants)
  (ivariants-parse-dailyuse)
  (ivariants-compat-chars))

(defvar ivariants-name-table
  (eval-when-compile ivariants-attr-name-table))

(defvar ivariants-table
  (eval-when-compile ivariants-char-table))

(set-char-table-extra-slot ivariants-table 0 'ivariants)
(define-char-code-property 'ivariants ivariants-table)

(provide 'ivariants-table)

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:

;;; ivariants-table.el ends here
