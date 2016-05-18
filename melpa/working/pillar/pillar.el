;;; pillar.el --- Major mode for editing Pillar files  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>
;; Version: 0.1
;; Package-Requires: ((makey "0.3"))
;; Keywords: markup major-mode
;; URL: http://github.com/DamienCassou/pillar-mode
;;
;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing Pillar files

;;; Code:

(require 'makey) ;; for popup handling
(require 'regexp-opt)

(require 'cl-lib)

(defgroup pillar nil
  "Major mode for editing text files in Pillar format."
  :prefix "pillar-"
  :group 'wp
  :link '(url-link "http://www.smalltalkhub.com/#!/~Pier/Pillar"))

(defgroup pillar-faces nil
  "Faces used in Pillar Mode"
  :group 'pillar
  :group 'faces)

(defcustom pillar-executable "pillar"
  "Path to the executable pillar."
  :group 'pillar
  :type 'string)

(defvar pillar-font-lock-keywords nil
  "Syntax highlighting for Pillar files.")

(setq pillar-font-lock-keywords nil)

(defun pillar-preprocess-regex (regex)
  "Replace [[anything]] by (.|\n)* in REGEX.
Return a new regex."
  (replace-regexp-in-string
   "\\[\\[anything\\]\\]"
   "\\(.\\|\n\\)*?"
   regex
   t ;; don't interpret capital letters
   t ;; don't interpret replacement string as a regex
   ))

(defmacro pillar-defformat (name face-spec &optional regex regex-group)
  "Generate necessary vars and faces for face NAME.
NAME is the name of the specific face to create without prefix or
suffix (e.g., bold).  FACE-SPEC is passed unchanged to `defface'.

Optional argument REGEX is the regular expression used to match
text for this face.  Optional argument REGEX-GROUP indicates which
group in REGEX represents the matched text."
  (unless (symbolp name) (error "NAME must be a symbol"))
  (let ((face-spec-gen (cl-gensym))
        (regex-gen (cl-gensym))
        (regex-group-gen (cl-gensym))
        (face-name (intern (format "pillar-%S-face" name)))
        (regex-name (intern (format "pillar-regex-%S" name))))
    `(let ((,face-spec-gen ,face-spec)
           (,regex-gen ,regex)
           (,regex-group-gen ,regex-group))
       ;; Save face specification to a dedicated variable
       (defvar ,face-name ',face-name
         ,(format "Face name to use for %s text." name))
       ;; Save face specification to a dedicated face
       (defface ,face-name
         ,face-spec-gen
         ,(format "Face for %s text." name)
         :group 'pillar-faces)
       ;; Save regexp to a dedicated variable
       (when ,regex-gen
         (defconst ,regex-name
           (pillar-preprocess-regex ,regex-gen)
           ,(format "Regular expression for matching %s text." name))
         ;; Associates regex with face name for syntax highlighting:
         (add-to-list 'pillar-font-lock-keywords
                      (cons ,regex-name
                            (if ,regex-group-gen
                                (list ,regex-group-gen ,face-name)
                              ,face-name)))))))

(defmacro pillar-defformat-special-text (name face-spec markup key)
  "Same as `pillar-defformat` with special treatment and shortcuts.
Generate necessary vars and faces for face NAME.  NAME is the
name of the specific face to create without prefix or
suffix (e.g., bold).  FACE-SPEC is passed unchanged to `defface'.
MARKUP is the regular expression to be found before and after
text for this face.  KEY is the assigned shortcut key."
  (unless (symbolp name) (error "NAME must be a symbol"))
  (let ((markup-gen (cl-gensym))
        (insert-markup-fn-name (intern (format "pillar-insert-%S-markup" name))))
    `(let ((,markup-gen ,markup))
       (pillar-defformat
        ,name
        '((t ,(append '(:inherit pillar-special-text-face) face-spec)))
        (concat "[^\\]\\(" (regexp-quote ,markup-gen) ".*?[^\\]" (regexp-quote ,markup-gen) "\\)")
        1)
       (defun ,insert-markup-fn-name ()
         (interactive)
         (pillar-insert-special-text-markup ,markup-gen))
       (add-to-list 'pillar-key-mode-special-font-actions
                    '(,(format "%c" key)
                      ,(capitalize (format "%s" name))
                      ,insert-markup-fn-name)))))

(defun pillar-font-lock-extend-region ()
  "Extend the search region to include an entire block of text.
This helps improve font locking for block constructs such as pre blocks."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (re-search-backward "\n\n" nil t)))
      (when found
        (goto-char font-lock-end)
        (when (re-search-forward "\n\n" nil t)
          (beginning-of-line)
          (setq font-lock-end (point)))
        (setq font-lock-beg found)))))

;; Syntax table
(defvar pillar-syntax-table nil "Syntax table for `pillar-mode'.")
(setq pillar-syntax-table
      (let ((synTable (copy-syntax-table text-mode-syntax-table)))

        ;; a comment starts with a '%' and ends with a new line
        (modify-syntax-entry ?% "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)

        synTable))

(defun pillar-insert-special-text-markup (markup)
  "Insert MARKUP at point or around selection."
  (cond
   ((mark)
    (save-excursion
      (insert markup)
      (goto-char (mark))
      (insert markup))
    (forward-char (length markup)))
   (t
    (insert markup)
    (save-excursion
      (insert markup)))))

(defvar pillar-key-mode-special-font-actions nil)

(defun pillar-key-mode-groups ()
  "Return a list of shortcut keys for popup."
  `((special-font
     (description "Formats")
     (actions ("All" ,@pillar-key-mode-special-font-actions)))))

;;;###autoload
(define-derived-mode pillar-mode text-mode "Pillar"
  "Major mode for editing Pillar CMS files."
  :syntax-table pillar-syntax-table
  (eval-when-compile
    "These 2 variables are automatically generated."
    (defvar pillar-regex-header-1)
    (defvar pillar-regex-header-2))

  ;; Don't fill paragraphs as Pillar expects everything on one line
  (setq fill-paragraph-function (lambda (ignored) t))
  ;; Natural Pillar tab width
  (setq tab-width 4)
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(pillar-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  ;; imenu
  (set (make-local-variable 'imenu-generic-expression)
       (list (list nil pillar-regex-header-1 1)
             (list nil pillar-regex-header-2 1)))
  ;; comments
  (set (make-local-variable 'comment-start) "%")
  ;; Multiline font lock
  (add-hook 'font-lock-extend-region-functions
            'pillar-font-lock-extend-region))


;;; File compilation

(defun pillar-compile (format extension)
  "Compile the current buffer file using FORMAT and save it in a file with the extension EXTENSION."
  (let* ((current-file (buffer-name (current-buffer)))
         (pillar-file (expand-file-name current-file))
         (output-file (concat (expand-file-name (file-name-base current-file))
                              "."
                              (symbol-name extension))))
    (pillar-compile-file current-file output-file format)))

(defun pillar-compile-file (input-file output-file format)
  "Compile INPUT-FILE to OUTPUT-FILE in FORMAT.
Supported formats are `latex', `html' and `markdown'."
  (shell-command (concat pillar-executable
                         " export --to="
                         (symbol-name format)
                         " "
                         input-file
                         " > "
                         output-file)))

(defmacro pillar-defoutput (format extension)
  "Define an output FORMAT for Pillar, which use the file extension EXTENSION.
This macro defines an interactive function `pillar-compile-to-FORMAT'."
  (unless (symbolp format)
    (error "FORMAT must the a symbol"))
  (let ((fn-name (intern (concat "pillar-compile-to-" (symbol-name format)))))
    `(defun ,fn-name ()
       (interactive)
       (pillar-compile ',format ',extension))))

(pillar-defoutput latex tex)
(pillar-defoutput html html)
(pillar-defoutput markdown md)

(defun pillar-compile-popup ()
  "Open a popup with compilation options."
  (interactive)
  (makey-initialize-key-groups
   '((pillar-compile
      (description "Pillar compilation")
      (actions
       ("LaTeX"
        ("l" "Compile to LaTex" pillar-compile-to-latex))
       ("HTML"
        ("h" "Compile to HTML" pillar-compile-to-html))
       ("Markdown"
        ("m" "Mardown" pillar-compile-to-markdown))))))
  (makey-key-mode-popup-pillar-compile))


;;; Markup insertion

(defun pillar-insert-special-text-markup-popup ()
  "Show a popup with shortcuts."
  (interactive)
  (declare-function makey-key-mode-popup-special-font "makey" t t)
  (makey-initialize-key-groups (pillar-key-mode-groups))
  (makey-key-mode-popup-special-font))

(define-key pillar-mode-map (kbd "C-c C-f") 'pillar-insert-special-text-markup-popup)
(define-key pillar-mode-map (kbd "C-c C-c") 'pillar-compile-popup)

(pillar-defformat
 special-text
 '((t (:inherit font-lock-variable-name-face))))

(pillar-defformat-special-text bold (:weight bold) "\"\"" ?b)
(pillar-defformat-special-text italic (:slant italic) "''" ?i)
(pillar-defformat-special-text strikethrough (:strike-through t) "--" ?-)
(pillar-defformat-special-text subscript (:height 0.8) "@@" ?@)
(pillar-defformat-special-text superscript (:height 0.8) "^^" ?^)
(pillar-defformat-special-text underlined (:underline t) "__" ?_)
(pillar-defformat-special-text link (:inherit link) "*" ?*)
(pillar-defformat-special-text link-embedded (:inherit link) "+" ?+)
(pillar-defformat-special-text monospaced (:inherit font-lock-constant-face) "==" ?=)

(pillar-defformat
 note
 '((t (:inherit pillar-special-text-face :weight bold)))
 "^@@note .*$")

(pillar-defformat
 todo
 '((t (:inherit pillar-special-text-face :weight bold)))
 "^@@todo .*$")

(pillar-defformat
 header
 '((t (:inherit font-lock-function-name-face :weight bold))))

(pillar-defformat
 header-1
 '((t (:inherit pillar-header-face :height 1.3)))
 "^!\\([^!].*\\)$")

(pillar-defformat
 header-2
 '((t (:inherit pillar-header-face :height 1.25)))
 "^!!\\([^!].*\\)$")

(pillar-defformat
 header-3
 '((t (:inherit pillar-header-face :height 1.2)))
 "^!!!\\([^!].*\\)$")

(pillar-defformat
 header-4
 '((t (:inherit pillar-header-face :height 1.15)))
 "^!!!!\\([^!].*\\)$")

(pillar-defformat
 script
 '((t (:inherit pillar-monospaced-face)))
 "\\[\\[\\[[[anything]]\\]\\]\\]")

(pillar-defformat
 description-term
 '((t (:weight bold)))
 "^;.*$")

(pillar-defformat
 description-data
 '((t (:slant italic :foreground "grey31")))
 "^:.*$")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pillar$" . pillar-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pier$" . pillar-mode))

(provide 'pillar)
;;; pillar.el ends here
