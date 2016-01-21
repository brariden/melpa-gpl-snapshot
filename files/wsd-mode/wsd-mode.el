;;; wsd-mode.el --- Emacs major-mode for www.websequencediagrams.com

;; Author     : Jostein Kjønigsen <jostein@gmail.com>
;; Created    : December 2014
;; Modified   : September 2015
;; Version    : 0.4.3
;; Keywords   : wsd diagrams design process modelling uml
;; X-URL      : https://github.com/josteink/wsd-mode
;;

;;; Commentary:
;;
;; This is a major-mode for modelling and editing sequence-diagrams
;; using the syntax employed by the online service
;; www.websequencediagrams.com.
;;
;; The mode supports inline rendering the diagrams through the API
;; provided through the website and persisting these to image-files next
;; to the files used to generate them.
;;
;; It will automatically activate for files with a WSD-extension.
;;
;;
;; Features:
;;
;; - syntax higlighting of reccognized keywords
;; - automatic indentation of block-statements
;; - generating and saving diagrams generated through WSD's online API.
;; - support for WSD premium features (svg-export, etc) if API-key is
;;   provided.
;; - rendering diagrams inline in emacs, or in external OS viewer if image
;;   format is not supported by Emacs.
;;
;;
;; Customization:
;;
;; The mode can be slightly customized. Documenting this fully is on the
;; TODO-list.
;;
;; To create mode-specific emacs-customizations, please use the
;; wsd-mode-hook.
;;
;; A short summary of customizable variables:
;;
;; - wsd-api-key (default blank. required for premium-features.)
;; - wsd-format (default png. svg requires premium, thus api-key.)
;; - wsd-style (default modern-blue)
;; - wsd-indent-offset (default 4)
;;

;;; Versions:
;;
;;    0.1.0 - Initial release.
;;    0.2.0 - Fix file-format bug and temp-buffer reuse.
;;            Improved syntax-highlighting.
;;            Fix OSX rendering issues.
;;    0.3.0 - Fix compatiblity issues with Emacs 24.3.
;;            Improved org-babel support.
;;    0.4.0 - integrate with customize framework
;;    0.4.1 - Bug-fixes & optimizations.
;;          - Support org-babel :file-parameter properly.
;;    0.4.2 - Limited flycheck support.
;;          - Improved fontication for additional keywords (autonumber, destroy,
;;            option footer) and syntax (*-activation).
;;          - Support for some pro-only features.
;;    0.4.3 - Improved syntax-support for pro-only features
;;            (almost feature complete).


;;; Code:

(require 'wsd-core)

(ignore-errors
  (require 'ob-wsdmode))

;; notes about derived mode here: http://www.emacswiki.org/emacs/DerivedMode

(defcustom wsd-indent-offset 4
  "Indentation offset for `wsd-mode'."
  :type 'integer
  :group 'wsd-mode)

;; regex construction helper functions

(defun wsd-single-p (ls)
  "Check if `LS' contain one item only."
  (and ls
       (null (cdr ls))))

(defun wsd-list-intersperse (seperator ls)
  "Intersperse `SEPERATOR' in between all items in `LS'."
  (if (or (null ls)
          (wsd-single-p ls))
      ;; when zero or one elements, onlt return remaining list
      (car ls)
    ;; otherwise intersperse an item and recurse.
    (concat (car ls) seperator (wsd-list-intersperse seperator (cdr ls)))))

(defun wsd-rx-or (&rest regexps)
  "Combine `REGEXPS' into a regexp representing them all in a logical or."

  (cond
   ;; nothing sent in, return empty string.
   ((null regexps)
    "")
   ;; param is list of list(s). reformat and recurse.
   ((listp (car regexps))
    (apply #'wsd-rx-or (car regexps)))
   ;; all else
   (t
    (concat "\\(" (wsd-list-intersperse "\\|" regexps) "\\)"))))

(defun wsd-rx-word (rx)
  "Wrap `RX' in a word-only container."
  (concat "\\<" rx "\\>"))

(defun wsd-rx-lstart (rx)
  "Prefix `RX' with a line-start clause."
  (concat "^[[:space:]]*" rx))

;; indentation code

(defun wsd-get-current-line ()
  "Return the content of the current line in the buffer.

   Handles nullability and down-casing."
  ;; includes trailing newline, but that shouldn't be an issue.
  ;; omit the second param t, because that breaks on Emacs <= 24.3.
  (let* ((thing (thing-at-point 'line)))
    (if (equal nil thing)
        nil
      (downcase thing))))

(defun wsd-get-buffer-lines ()
  "Return the list of lines found from the current point in the buffer, back to the start."
  (interactive)

  (save-excursion
    (beginning-of-line)

    (let* ((lines '()))
      (while (not (equal (point) (point-min)))
        (beginning-of-line-text)
        (let* ((line (wsd-get-current-line)))
          (setq lines (cons line lines)))
        (beginning-of-line)
        (forward-line -1))
      lines)))

;; else not included as +-word as qit doesnt affect the -overall- indentation either way
(defconst wsd-rx-indentation-plus
  (wsd-rx-lstart
   (wsd-rx-or
    (wsd-rx-word (wsd-rx-or "alt" "opt" "loop"))
    ;; the following keywords only indent when not followed by colon:
    (concat (wsd-rx-word (wsd-rx-or "state" "note" "ref")) "[^:]*$")
    (concat "parallel[[:space:]]+{"))))

(defconst wsd-rx-indentation-minus
  (wsd-rx-lstart
   (wsd-rx-or
    (wsd-rx-word "end")
    "}")))

(defun wsd-get-indentation-from-line (line)
  "Get the indentation for the single supplied `LINE'."
  (cond
   ((string-match-p wsd-rx-indentation-plus line)  wsd-indent-offset)
   ((string-match-p wsd-rx-indentation-minus line) (- 0 wsd-indent-offset))
   (t 0)))

(defun wsd-calculate-indentation-level (current adjustment)
  "Calculate new indentation level based on `CURRENT' and `ADJUSTMENT'."
  (let* ((adjusted (+ current adjustment)))
    (if (> 0 adjusted)
        0
      adjusted)))

(defun wsd-get-indentation-from-lines (lines)
  "Get the overall indentation from the supplied `LINES'."
  (let* ((indent-col   0))
    (dolist (line lines)
      (setq indent-col (wsd-calculate-indentation-level
                        indent-col
                        (wsd-get-indentation-from-line line))))
    indent-col))

(defun wsd-get-adjustment-indent ()
  "Adjust overall document indentation with specific reverse compensation for branch-starting keywords based on the current line."
  (if (string-match-p
       (wsd-rx-or (wsd-rx-lstart (wsd-rx-word "else"))
                  wsd-rx-indentation-plus)
       (wsd-get-current-line))
      (- 0 wsd-indent-offset)
    0))

(defun wsd-get-line-indent ()
  "Get the indentation level of the current line."
  (let* ((lines             (wsd-get-buffer-lines))
         (lines-indent      (wsd-get-indentation-from-lines lines))
         (adjustment-indent (wsd-get-adjustment-indent)))
    (wsd-calculate-indentation-level lines-indent adjustment-indent)))

(defun wsd-indent-line ()
  "Indent current line for `wsd-mode'."
  (interactive)
  (indent-line-to (wsd-get-line-indent)))

;;;###autoload
(define-derived-mode wsd-mode fundamental-mode "wsd-mode"
  "Major-mode for websequencediagrams.com"
  (let* (;; some keywords should only trigger when starting a line.
         (line-starters '("title" "participant" "deactivate" "activate"
                          "alt" "else" "opt" "loop" "state" "note"
                          "end" "end state" "end note" "end ref"
                          "autonumber" "destroy" "option footer"
                          "parallel" "ref"))
         ;; combine into one big OR regexp, ^<> start of line, whole word only.
         (rx-line-starters (wsd-rx-lstart (wsd-rx-word (wsd-rx-or line-starters))))

         ;; some keywords are OK almost anywhere, or at least treat them as such.
         (keywords '("over" "right of" "left of" "as"))
         (rx-keywords (wsd-rx-word (wsd-rx-or keywords)))

         ;; lines starting with # are treated as comments
         (rx-comments "#.*$")

         ;; operators
         (operators '("-->-" "-->" "->+" "->*" "->-" "->" ": "))
         (rx-operators (regexp-opt operators t)))
    (font-lock-add-keywords nil (list (cons rx-keywords 'font-lock-keyword-face)
                                      (cons rx-line-starters 'font-lock-keyword-face)
                                      (cons rx-comments 'font-lock-comment-face)
                                      (cons rx-operators 'font-lock-comment-face))))

  (make-local-variable 'wsd-indent-offset)
  (set (make-local-variable 'indent-line-function) 'wsd-indent-line)

  (when (fboundp 'flycheck-mode-on-safe)
    (flycheck-mode-on-safe)))

(define-key wsd-mode-map (kbd "C-c C-c") #'wsd-show-diagram-inline)
(define-key wsd-mode-map (kbd "C-c C-e") #'wsd-show-diagram-online)
;; (define-key wsd-mode-map (kbd "C-c C-k") #'wsd-strip-errors)


;;; Autoload mode trigger
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wsd$" . wsd-mode))

;; Flycheck support must be loaded after other code has been intialized.
(ignore-errors
  (require 'wsd-flycheck))

(provide 'wsd-mode)

;;; wsd-mode.el ends here
