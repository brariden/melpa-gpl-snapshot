;;; hfst-mode.el --- major mode for editing HFST files

;; Copyright (C) 2010-2016 Kevin Brubeck Unhammer
;; Copyright (C) 2006 Sebastian Nagel (sfst.el used as basis)

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.3.0
;; Package-Requires:
;; Url: http://wiki.apertium.org/wiki/Emacs
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides syntax highlighting and a "go to lexicon"-function useful
;; for editing Helsinki Finite State Transducer descriptions.

;; Usage:
;; (add-to-list 'load-path "/path/to/hfst-mode-folder")
;; (autoload 'hfst-mode "~/path/to/hfst-mode/hfst.el")
;; ; Change these lines if you name your files something other
;; ; than .twol and .lexc:
;; (add-to-list 'auto-mode-alist '("\\.twol$" . hfst-mode))
;; (add-to-list 'auto-mode-alist '("\\.lexc$" . hfst-mode))

;; For information about Helsinki Finite State Transducer Tools
;; (HFST), see http://hfst.github.io/"

;;; TODO:
;;; - Recognize if we are in a lexc or twolc (or xfst) file,
;;;   and apply specific keywords etc.
;;; - Recognize END keyword in lexc files and comment all that is
;;;   below it
;;; - Compile list of lexicon names for quick go-to?
;;; - A feature like dix.el's dix-xmlise-using-above-elt
;;; - Use define-derived-mode instead of this old-school stuff

;;; Code:

(defconst hfst-mode-version "0.3.0" "Version of hfst-mode.")

;;;============================================================================
;;;
;;; Define the formal stuff for a major mode named hfst.
;;;

(defvar hfst-mode-hook nil)

(defvar hfst-mode-map (make-sparse-keymap)
  "Keymap for hfst minor mode.")

(defvar hfst-mode-syntax-table
  (let ((hfst-mode-syntax-table (make-syntax-table)))
    ; comments start with !
    (modify-syntax-entry ?!  "<" hfst-mode-syntax-table)
    ; and last until end-of-line
    (modify-syntax-entry ?\n ">!" hfst-mode-syntax-table)
    ;; todo: % is the escape character (I think)
    (modify-syntax-entry ?% "\\" hfst-mode-syntax-table)
    ;; dots appear as symbols in flag diacritics:
    (modify-syntax-entry ?. "_" hfst-mode-syntax-table)
    (modify-syntax-entry ?« "_" hfst-mode-syntax-table)
    (modify-syntax-entry ?» "_" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?\" "." hfst-mode-syntax-table)
;;     (modify-syntax-entry ?\\ "\\" hfst-mode-syntax-table)
;;    (modify-syntax-entry ?\" "." hfst-mode-syntax-table)
;;     (modify-syntax-entry ?\' "$" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?< "\"" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?< "." hfst-mode-syntax-table)
;;     (modify-syntax-entry ?> "." hfst-mode-syntax-table)
;;     (modify-syntax-entry ?{ "(}" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?} "){" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?( "()" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?) ")(" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?[ "(]" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?] ")[" hfst-mode-syntax-table)
    hfst-mode-syntax-table)
  "Syntax table for hfst-mode.")

(defface hfst-mode-font-lock-escaped-face
  '((((class color) (min-colors 88) (background light)) (:background "Pink" :weight bold))
    (((class color) (min-colors 88) (background dark)) (:background "Red1" :weight bold))
    (((class color) (min-colors 16) (background light)) (:background "Pink" :weight bold))
    (((class color) (min-colors 16) (background dark)) (:background "Red1" :weight bold))
    (((class color) (min-colors 8)) (:background "red"))
    (t (:inverse-video t :weight bold)))
  "Font Lock mode face used to escaped characters (using background colour since we may have spaces)."
  :group 'font-lock-faces)

(defconst hfst-mode-font-lock-keywords
  `(;; keywords TODO: alphabet doesn't match if on first line!
    (,(concat "\\(?:\\Sw\\|^\\)"
	      (regexp-opt '("Alphabet" "Multichar_Symbols" "Sets" "Rules" "Definitions"
			    "Diacritics" "Rule-variables" "where" "in" "matched" "END")
			  'group)
	      "\\Sw")
     (1 'font-lock-keyword-face nil t))
    ;; todo: lexicon names always start with a capital letter, but can
    ;; you have eg. Æ? or just A-Z?
    ("\\(LEXICON\\) +\\(\\(\\sw\\|\\s_\\)+\\)" ; Root is special, note it in any way?
     (1 'font-lock-keyword-face nil t)
     (2 font-lock-variable-name-face))
    ;; flag diacritics:
    ("@\\sw\\.\\(\\(\\sw\\|\\s_\\)+\\)@"
     (1 'font-lock-variable-name-face nil t))
    ;; End symbol:
    ("\\(^\\|[^%]\\)\\(#\\)"
     (2 'font-lock-warning-face nil t))
    ;; escape symbol:
    ("%." 0 'hfst-mode-font-lock-escaped-face nil t)
    ;; operators:
    (,(regexp-opt '("<=>" "<=" "=>" "/<=" "_" ";"
		    "=" ":" ">"
		    "\\" "~" "+" "?" "*" "-" "^")
		  'group)
     (1 'font-lock-function-name-face nil t)))
  "Expressions to highlight in hfst-mode.")

(defun hfst-mode-font ()
  "Set font-lock variables for hfst mode."
  (make-local-variable 'font-lock-keywords-case-fold-search) ; For GNU Emacs.
  (setq font-lock-keywords-case-fold-search nil)
  (put major-mode 'font-lock-keywords-case-fold-search nil) ; For XEmacs.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(hfst-mode-font-lock-keywords nil nil)))

;;;###autoload
(defun hfst-mode ()
  "Major mode for editing Helsinki Finite State Transducer files.
Supported formats include .lexc and .twolc.
For more information on Helsinki Finite State Transducer Tools, see
http://hfst.github.io/"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'hfst-mode
	mode-name "HFST")
  (use-local-map hfst-mode-map)
  (setq parse-sexp-ignore-comments t)
  (set-syntax-table hfst-mode-syntax-table)
  ;;   (make-local-variable 'indent-line-function)
  ;;   (setq indent-line-function 'hfst-mode-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "! ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "! *")
  (make-local-variable 'completion-ignore-case)
  (setq completion-ignore-case nil)
  (hfst-mode-font)
  (run-mode-hooks 'hfst-mode-hook))

;;; Interactive functions -----------------------------------------------------

(defun hfst-mode-lexref-at-point ()
  "The lexicon referenced to by this entry."
  (let ((start (save-excursion
		 (re-search-backward "[^%];\\|LEXICON \\S *")
		 (match-end 0)))
	(end (save-excursion (re-search-forward "[^%];"))))
    (save-excursion
      (goto-char start)
      (re-search-forward "\\s \\(\\S +\\)\\s *;")
      (match-string-no-properties 1))))

(defun hfst-mode-goto-lexicon ()
  "Go to the lexicon defined at point/line.
Call from an entry to go to its pardef.  Mark is pushed so you can
go back with \\[universal-argument] \\[set-mark-command]."
  (interactive)
  (let ((lexname (hfst-mode-lexref-at-point))
	pos)
    (if (save-excursion
	  (goto-char (point-min))
	  (if (re-search-forward (concat "^\\s *LEXICON " lexname "\\s *\\($\\|!\\)")
                                 nil 'noerror)
	      (setq pos (match-beginning 0))))
	(progn (push-mark)
	       (goto-char pos))
      (message "Couldn't find LEXICON %s" lexname))))

;;; Keybindings --------------------------------------------------------------
(define-key hfst-mode-map (kbd "M-.") #'hfst-mode-goto-lexicon)
(define-key hfst-mode-map (kbd "M-,") #'pop-to-mark-command)

;;; Run hooks -----------------------------------------------------------------
(run-hooks 'hfst-mode-load-hook)

(provide 'hfst-mode)

;;;============================================================================

;;; hfst-mode.el ends here
