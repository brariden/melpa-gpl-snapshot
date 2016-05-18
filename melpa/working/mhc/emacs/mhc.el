;;; mhc.el --- Message Harmonized Calendaring system.

;; Description: Message Harmonized Calendaring system.
;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 1994-07-04
;; Version: 1.1.1
;; Keywords: calendar
;; URL: http://www.quickhack.net/mhc
;; Package-Requires: ((calfw "20150703"))

;;;
;;; Commentary:
;;;

;; Mhc is the personal schedule management package.
;; Please visit http://www.quickhack.net/mhc for details.
;;
;; Minimum setup:
;;
;;  (setq load-path
;;        (cons "~/src/mhc/emacs" load-path))
;;  (autoload 'mhc "mhc")
;;
;; and M-x mhc

;;; Code:

(eval-when-compile (require 'cl))

;; For Mule 2.3
(eval-and-compile
  (when (boundp 'MULE)
    (require 'poe)
    (require 'pcustom)))

(require 'mhc-vars)
(require 'mhc-record)
(require 'mhc-parse)
(require 'mhc-file)
(require 'mhc-process)
(require 'mhc-db)
(require 'mhc-message)
(require 'mhc-misc)
(require 'mhc-date)
(require 'mhc-guess)
(require 'mhc-schedule)
(require 'mhc-face)
(require 'mhc-calendar)
(require 'mhc-draft)

(cond
 ((eval-when-compile  (and (not (featurep 'xemacs))
                           (>= emacs-major-version 21)
                           (if (eq system-type 'windows-nt)
                               ;; Meadow2 or NTEmacs21.3(and the later
                               ;; version) supports the image feature.
                               (or (featurep 'meadow)
                                   (>= emacs-major-version 22)
                                   (>= emacs-minor-version 3))
                             t)))
  (require 'mhc-e21))
 ((eval-when-compile
    (condition-case nil
        (require 'bitmap)
      (error nil)))
  (require 'mhc-bm))
 ((eval-when-compile (featurep 'xemacs))
  (require 'mhc-xmas))
 (t (defun mhc-use-icon-p ())))

(require 'mhc-minibuf)
(require 'mhc-summary)
(provide 'mhc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu setup
;;
(defvar mhc-mode-menu-spec
      '("Mhc"
        ["This month"   mhc-goto-this-month t]
        ["Next month"   mhc-goto-next-month t]
        ["Prev month"   mhc-goto-prev-month t]
        ["Goto month"   mhc-goto-month t]
        ["Goto date"    mhc-goto-date t]
        ["Import"       mhc-import t]
        ["Set category" mhc-set-default-category t]
        "----"
        ["Goto today"   mhc-goto-today (mhc-summary-buffer-p)]
        ["Modify"       mhc-modify (mhc-summary-buffer-p)]
        ["Edit"         mhc-edit (mhc-summary-buffer-p)]
        ["Rescan"       mhc-rescan-month (mhc-summary-buffer-p)]
        ["Delete"       mhc-delete (mhc-summary-buffer-p)]
        ["Insert Schedule" mhc-insert-schedule (not buffer-read-only)]
        ["3 months Mini calendar" mhc-calendar t]
        ["Toggle 3 months calendar" mhc-calendar-toggle-insert-rectangle
         (mhc-summary-buffer-p)]
        "----"
        ["Reset"        mhc-reset (mhc-summary-buffer-p)]
        ("Network"
         ["Online" mhc-file-toggle-offline mhc-file/offline]
         ["Offline" mhc-file-toggle-offline (not mhc-file/offline)])
        "----"
        ("PostScript"
         ["PostScript" mhc-ps t]
         ["Preview" mhc-ps-preview t]
         ["Print" mhc-ps-print t]
         ["Save" mhc-ps-save t]
         ["Insert buffer" mhc-ps-insert-buffer t])))

(defvar mhc-prefix-key "\C-c."
  "*Prefix key to call MHC functions.")

(defvar mhc-mode-map nil "Keymap for `mhc-mode'.")
(defvar mhc-prefix-map nil "Keymap for 'mhc-key-prefix'.")

(if (and mhc-mode-map mhc-prefix-map)
    ()
  (setq mhc-mode-map (make-sparse-keymap))
  (setq mhc-prefix-map (make-sparse-keymap))
  (define-key mhc-prefix-map "g" 'mhc-goto-month)
  (define-key mhc-prefix-map "j" 'mhc-goto-date)
  (define-key mhc-prefix-map "." 'mhc-goto-this-month)
  (define-key mhc-prefix-map "n" 'mhc-goto-next-month)
  (define-key mhc-prefix-map "N" 'mhc-goto-next-year)
  (define-key mhc-prefix-map "p" 'mhc-goto-prev-month)
  (define-key mhc-prefix-map "P" 'mhc-goto-prev-year)
  (define-key mhc-prefix-map "f" 'mhc-goto-today)
  (define-key mhc-prefix-map "|" 'mhc-import)
  (define-key mhc-prefix-map "m" 'mhc-modify)
  (define-key mhc-prefix-map "e" 'mhc-edit)
  (define-key mhc-prefix-map "s" 'mhc-rescan-month)
  (define-key mhc-prefix-map "d" 'mhc-delete)
  (define-key mhc-prefix-map "c" 'mhc-set-default-category)
  (define-key mhc-prefix-map "i" 'mhc-insert-schedule)
  (define-key mhc-prefix-map "?" 'mhc-calendar)
  (define-key mhc-prefix-map "t" 'mhc-calendar-toggle-insert-rectangle)
  (define-key mhc-prefix-map "T" 'mhc-file-toggle-offline)
  (define-key mhc-prefix-map "R" 'mhc-reset)
  (define-key mhc-mode-map mhc-prefix-key mhc-prefix-map)
  (cond
   ((featurep 'xemacs)
    (define-key mhc-mode-map [(button1)] 'mhc-calendar-mouse-goto-date)
    (define-key mhc-mode-map [(button2)] 'mhc-calendar-mouse-goto-date-view))
   (t
    (define-key mhc-mode-map [mouse-1] 'mhc-calendar-mouse-goto-date)
    (define-key mhc-mode-map [mouse-2] 'mhc-calendar-mouse-goto-date-view))))

(defvar mhc-mode nil "Non-nil when in mhc-mode.")

(defcustom mhc-mode-hook nil
  "Hook run in when entering MHC mode."
  :group 'mhc
  :type 'hook)

;; Avoid warning of byte-compiler.
(defvar mhc-mode-menu)
(eval-and-compile
  (autoload 'easy-menu-add "easymenu"))

(defun mhc-mode (&optional arg) "\
\\<mhc-mode-map>
   MHC is the mode for registering schdule directly from email.
   Requres Mew or Wanderlust or Gnus.

   Key assinment on mhc-mode.

\\[mhc-goto-this-month] Review the schedule of this month
\\[mhc-goto-next-month] Review the schedule of next month
\\[mhc-goto-prev-month] Review the schedule of previous month
\\[mhc-goto-month]      Jump to your prefer month
\\[mhc-goto-date]       Jump to your prefer date
\\[mhc-rescan-month]    Rescan the buffer of the month
\\[mhc-goto-today]      Move cursor to today (Only available reviewing this month)
\\[mhc-import]  Register the reviewing mail to schdule
\\[mhc-delete]  Delete the schdule on the cursor line
\\[mhc-set-default-category]    Edit the schdule on the cursor line
\\[mhc-modify]  Modify the schdule on the cursor line
\\[mhc-edit]    Create new schdule file
\\[mhc-set-default-category]    Change default category
\\[mhc-calendar]        Display 3 months mini calendar
\\[mhc-calendar-toggle-insert-rectangle]        Toggle 3 months calendar
\\[mhc-reset]   Reset MHC

   '\\[universal-argument]' prefix is available on using '\\[mhc-rescan-month]', '\\[mhc-goto-this-month]', '\\[mhc-goto-month]', '\\[mhc-goto-date]'
  , it works to assign the category (see below).

   The prefix arg '\\[mhc-goto-next-month]', '\\[mhc-goto-prev-month]' is also available and you can indicate
   the number of months to forward/back.

   Field names using by MHC.

   X-SC-Category:
   Space-seperated Keywords. You can set default category to scan.
   You can also indicate keywords by typing '\\[mhc-rescan-month]', '\\[mhc-goto-this-month]', '\\[mhc-goto-month]', '\\[mhc-goto-date]' with C-u.
"
  (interactive "P")
  (make-local-variable 'mhc-mode)
  (setq mhc-mode
        (if (null arg)
            (not mhc-mode)
          (> (prefix-numeric-value arg) 0)))
  (when (featurep 'xemacs)
    (easy-menu-add mhc-mode-menu))
  (force-mode-line-update)
  (run-hooks 'mhc-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexical analyzer part for category.
;;

(defsubst mhc-expr/new ()
  (vector nil nil nil nil))

(defsubst mhc-expr/token (expr-obj)        ;; literal
  (aref expr-obj 0))
(defsubst mhc-expr/token-type (expr-obj)   ;; symbolized
  (aref expr-obj 1))
(defsubst mhc-expr/string (expr-obj)       ;; currently parsing string.
  (aref expr-obj 2))

(defsubst mhc-expr/set-token (expr-obj val)
  (aset expr-obj 0 val))
(defsubst mhc-expr/set-token-type (expr-obj val)
  (aset expr-obj 1 val))
(defsubst mhc-expr/set-string (expr-obj val)
  (aset expr-obj 2 val))

(defconst mhc-expr-token-type-alist
  '(
    ("[^!&|()\t \n]+"  . symbol)
    ("!"              . negop)
    ("&&"             . andop)
    ("||"             . orop)
    ("("              . lparen)
    (")"              . rparen)))

;; Eat one token from parsing string in obj.
(defun mhc-expr/gettoken (obj)
  (let ((string (mhc-expr/string obj))
        (token-alist mhc-expr-token-type-alist)
        (token-type nil)
        (token      nil))
    ;; delete leading white spaces.
    (if (string-match "^[\t ]+" string)
        (setq string (substring string (match-end 0))))
    (while (and token-alist (not token-type))
      (if (string-match (concat "^" (car (car token-alist))) string)
          (setq token      (substring string 0 (match-end 0))
                string     (substring string (match-end 0))
                token-type (cdr (car token-alist))))
      (setq token-alist (cdr token-alist)))

    (mhc-expr/set-token      obj token)
    (mhc-expr/set-string     obj string)
    (mhc-expr/set-token-type obj token-type)
    obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recursive descent parser for category.
;;

;;
;; expression -> term ("||" term)*
;;
(defun mhc-expr/expression (obj)
  (let ((ret (list (mhc-expr/term obj))))
    (while (eq (mhc-expr/token-type obj) 'orop)
      (mhc-expr/gettoken obj)
      (setq ret (cons (mhc-expr/term obj) ret)))
    (if (= 1 (length ret))
        (car ret)
      (cons 'or (nreverse ret)))))

;;
;; term       -> factor ("&&" factor)*
;;
(defun mhc-expr/term (obj)
  (let ((ret (list (mhc-expr/factor obj))))
    (while (eq (mhc-expr/token-type obj) 'andop)
      (mhc-expr/gettoken obj)
      (setq ret (cons (mhc-expr/factor obj) ret)))
    (if (= 1 (length ret))
        (car ret)
      (cons 'and (nreverse ret)))))

;;
;; factor     -> "!"* category_name || "(" expression ")"
;;
(defun mhc-expr/factor (obj)
  (let ((ret)
        (neg-flag nil))
    (while (eq (mhc-expr/token-type obj) 'negop)
      (setq neg-flag (not neg-flag))
      (mhc-expr/gettoken obj))
    (cond
     ;; symbol
     ((eq (mhc-expr/token-type obj) 'symbol)
      (setq ret (list 'mhc-schedule-in-category-p
                      'schedule (mhc-expr/token obj)))
      (mhc-expr/gettoken obj))
     ;; ( expression )
     ((eq (mhc-expr/token-type obj) 'lparen)
      (mhc-expr/gettoken obj)
      (setq ret (mhc-expr/expression obj))
      (if (not (eq (mhc-expr/token-type obj) 'rparen))
          (error "Syntax error."))
      (mhc-expr/gettoken obj))
     ;; error
     (t
      (error "Syntax error.")
      ;; (error "Missing category name or `(' %s %s"
      ;;  mhc-expr-token mhc-expr-parsing-string)
      ))
    (if neg-flag (list 'not ret) ret)))

(defun mhc-expr-parse (string)
  (let ((obj (mhc-expr/new)) (ret nil))
    (if (or (not string) (string= string ""))
        t
      (mhc-expr/set-string obj string)
      (mhc-expr/gettoken obj)
      (setq ret (mhc-expr/expression obj))
      (if (mhc-expr/token obj)
          (error "Syntax Error.")
        ret))))

(defun mhc-expr-compile (string)
  (byte-compile
   `(lambda (schedule)
      ,(mhc-expr-parse string)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; category
;;
(defvar mhc-default-category nil)
(defvar mhc-default-category-predicate-sexp
  (mhc-expr-compile ""))

(defvar mhc-default-category-hist nil)

(defun mhc-set-default-category ()
  (interactive)
  (setq mhc-default-category
        (read-from-minibuffer "Default Category: "
                              (or mhc-default-category "")
                              nil nil 'mhc-default-category-hist))
  (setq mhc-default-category-predicate-sexp
        (mhc-expr-compile mhc-default-category))
  (if (mhc-summary-buffer-p)
      (mhc-rescan-month)))

; (defun mhc-category-convert (lst)
;   (let (ret inv)
;     ;; preceding `!' means invert logic.
;     (if (and lst (string-match "^!" (car lst)))
;       (setq lst (cons (substring (car lst) (match-end 0)) (cdr lst))
;             inv t))
;     (cons inv lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; goto-*

(defun mhc-goto-month (&optional date hide-private)
  "*Show schedules of specified month.
If HIDE-PRIVATE, priavate schedules are suppressed."
  (interactive
   (list
    (mhc-input-month "Month ")
    (if mhc-default-hide-private-schedules
        (not current-prefix-arg)
      current-prefix-arg)))
  (mhc-scan-month date
                  'mhc-mua
                  mhc-default-category-predicate-sexp
                  hide-private))

(defvar mhc-goto-date-func 'mhc-goto-date-calendar)
                                        ; or mhc-goto-date-summary
(defun mhc-goto-date (&optional hide-private)
  "*Show schedules of specified date.
If HIDE-PRIVATE, private schedules are suppressed."
  (interactive
   (list
    (if mhc-default-hide-private-schedules
        (not current-prefix-arg)
      current-prefix-arg)))
  (let* ((owin (get-buffer-window (current-buffer)))
         (buf (mhc-summary-get-import-buffer))
         (win (if buf (get-buffer-window buf) nil))
         date)
    (save-excursion
      (when win (select-window win))
      (setq date (car (mhc-input-day "Date: " (mhc-date-now) (mhc-guess-date))))
      (select-window owin))
    (funcall mhc-goto-date-func date hide-private)))
(defun mhc-goto-date-calendar (date hide-private)
  (mhc-calendar-goto-month date))
(defun mhc-goto-date-summary (date hide-private)
  ;; XXX mhc-calendar-scanのパクリです
  (mhc-goto-month date hide-private)
  (goto-char (point-min))
  (if (mhc-summary-search-date date)
      (progn
        (beginning-of-line)
        (if (not (pos-visible-in-window-p (point)))
            (recenter)))))

;;;###autoload
(defun mhc-goto-this-month (&optional hide-private)
  "*Show schedules of this month.
If HIDE-PRIVATE, private schedules are suppressed."
  (interactive
   (list
    (if mhc-default-hide-private-schedules
        (not current-prefix-arg)
      current-prefix-arg)))
  (mhc-setup)
  (mhc-goto-month (mhc-date-now) hide-private))

(defun mhc-goto-next-month (&optional arg)
  (interactive "p")
  (mhc-goto-month (mhc-date-mm+
                   (or (mhc-current-date-month) (mhc-date-now)) arg)
                  mhc-default-hide-private-schedules))

(defun mhc-goto-next-year (&optional arg)
  (interactive "p")
  (mhc-goto-next-month (* (or arg 1) 12)))

(defun mhc-goto-prev-month (&optional arg)
  (interactive "p")
  (mhc-goto-next-month (- arg)))

(defun mhc-goto-prev-year (&optional arg)
  (interactive "p")
  (mhc-goto-next-year (- arg)))

(defun mhc-goto-today (&optional no-display)
  "*Go to the line of today's schedule or first day of month.
Unless NO-DISPLAY, display it."
  (interactive "P")
  (let ((now (mhc-date-now))
        (buf-date (mhc-current-date-month)))
    (when buf-date
      (goto-char (point-min))
      (mhc-date-let now
        (if (and (= yy (mhc-date-yy buf-date))
                 (= mm (mhc-date-mm buf-date)))
            (when (mhc-summary-search-date now)
              (forward-line 0)
              (or (pos-visible-in-window-p (point))
                  (recenter))
              (or no-display
                  (mhc-summary-display-article)))
          (when (and mhc-use-wide-scope
                     (mhc-summary-search-date (mhc-date-mm-first buf-date)))
            (forward-line 0)
            (or (pos-visible-in-window-p (point))
                (recenter))
            (or no-display
                (mhc-summary-display-article)))))
      ;; Emacs-21.3.50 something wrong
      (beginning-of-line))))

(defun mhc-rescan-month (&optional hide-private)
  "*Rescan schedules of this buffer.
If HIDE-PRIVATE, private schedules are suppressed."
  (interactive
   (list
    (if mhc-default-hide-private-schedules
        (not current-prefix-arg)
      current-prefix-arg)))
  (move-to-column 1)
  (let ((line (+ (count-lines (point-min) (point))
                 (if (= (current-column) 0) 1 0))))
    (mhc-scan-month (or (mhc-current-date-month) (mhc-date-now))
                    'mhc-mua
                    mhc-default-category-predicate-sexp
                    hide-private)
    (goto-char (point-min))
    (if (eq selective-display t)
        (re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line))))
  (beginning-of-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make scan form.

(defvar mhc-face-week-color-paint-thick nil)

(defun mhc-expand-date-scope-backward (date scope)
  "Expand date scope backward involving the whole first week of month.
DATE can be any date of the target month.
SCOPE is one of:
  + 'week: Expand to involve the whole first week of month.
  + 'wide: Just like 'week, but if 'week does not expand nothing,
    it takes 7 days.
  + number: Expand N days backward."
(let ((edge-date (mhc-date-mm-first date)))
  (cond
   ((integerp scope)
    (mhc-date- edge-date scope))
   ((eq scope 'week)
    (mhc-date-ww-first edge-date mhc-start-day-of-week))
   ((eq scope 'wide)
    (mhc-date-ww-first (mhc-date-- edge-date) mhc-start-day-of-week)))))

(defun mhc-expand-date-scope-forward (date scope)
  "Expand date scope forward involving the whole last week of month.
DATE can be any date of the target month.
SCOPE is one of:
  + 'week: Expand to involve the whole last week of month.
  + 'wide: Just like 'week, but if 'week does not expand nothing,
    it takes 7 days.
  + number: Expand N days forward."
(let ((edge-date (mhc-date-mm-last date)))
  (cond
   ((integerp scope)
    (mhc-date+ edge-date scope))
   ((eq scope 'week)
    (mhc-date-ww-last edge-date mhc-start-day-of-week))
   ((eq scope 'wide)
    (mhc-date-ww-last (mhc-date++ edge-date) mhc-start-day-of-week)))))

(defun mhc-scan-month (date mailer category-predicate secret)
  "Make summary buffer for a month indicated by DATE.
DATE can be any date of the target month.
If MAILER is 'direct, insert scanned result into current buffer.
CATEGORY-PREDICATE must be a function that can take one mhc-schedule
argument and return a boolean value indicates opacity of the article.
If SECRET is non-nil, hide articles those categories are
listed in ``mhc-category-as-private''."
  (let* ((from (mhc-date-mm-first date))
         (to (mhc-date-mm-last date))
         (today (mhc-date-now))
         ;; need three months for mini-calendar
         (dayinfo-list (mhc-db-scan (mhc-date-mm-- from) (mhc-date-mm++ to))))
    (unless (eq 'direct mailer)
      (mhc-summary-generate-buffer date)
      (setq mhc-summary-buffer-current-date-month
            (mhc-date-mm-first date)))
    (when mhc-use-wide-scope
      (setq from (mhc-expand-date-scope-backward date mhc-use-wide-scope))
      (setq to   (mhc-expand-date-scope-forward  date mhc-use-wide-scope)))
    (message "%s" (mhc-date-format date "Scanning %04d/%02d..." yy mm))
    (mhc-summary-make-contents
     dayinfo-list
     from to mailer category-predicate secret)
    (unless (eq 'direct mailer)
      (when mhc-insert-calendar
        (mhc-calendar-insert-rectangle-at
         date
         (- (mhc-misc-get-width) mhc-calendar-width)
         mhc-vertical-calendar-length
         dayinfo-list))
      (mhc-summary-mode)
      (mhc-mode 1)
      (setq mhc-summary-buffer-current-date-month
            (mhc-date-mm-first date))
      (mhc-goto-today t)
      (message "%s" (mhc-date-format date "Scanning %04d/%02d...done" yy mm)))))

(defun mhc-search (string &optional subject-only)
  "Search events by STRING.
If SUBJECT-ONLY is non-nil, it will search only on X-SC-Subject:"
  (interactive "sSearch: \nP")
  (let* ((match (mhc-db-search :subject string :body (unless subject-only string))))
    (if (null match)
        (message "No match")
      (mhc-scan match))))

(defun mhc-search-recurrence (recurrence-tag)
  "Search events by RECURRENCE-TAG."
  (interactive "sSearch recurrence-tag: ")
  (let* ((match (mhc-db-search :recurrence_tag recurrence-tag)))
    (if (null match)
        (message "No match")
      (mhc-scan match))))

(defun mhc-scan (events &optional insert-current-buffer clip-from clip-to)
  "Create mhc-summary buffer using EVENTS list.
If INSERT-CURRENT-BUFFER is non-nil, insert contents in the current buffer.
if CLIP-FROM and CLIP-TO are specified, clip EVENTS by date using these two params."
  (unless insert-current-buffer
    (mhc-summary-generate-buffer "MHC SEARCH"))
  (message "Listing MHC events...")
  (mhc-summary-make-contents events clip-from clip-to)
  (mhc-summary-mode)
  (goto-char (point-min))
  (message "Listing MHC events...done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; import, edit, delete, modify

(defcustom mhc-input-sequences '(date time subject location category recurrence-tag alarm)
  "*Sequence of the inputs."
  :group 'mhc
  :type '(repeat (choice (const :tag "Date" date)
                         (const :tag "Time" time)
                         (const :tag "Subject" subject)
                         (const :tag "Location" location)
                         (const :tag "Category" category)
                         (const :tag "Recurrence tag" recurrence-tag)
                         (const :tag "Alarm" alarm))))

(defun mhc-edit (&optional import-buffer)
  "Edit a new schedule.
If optional argument IMPORT-BUFFER is specified, import its content.
Returns t if the importation was succeeded."
  (interactive
   (if current-prefix-arg
       (list (get-buffer (read-buffer "Import buffer: "
                                      (current-buffer))))))
  (let ((draft-buffer (generate-new-buffer mhc-draft-buffer-name))
        (current-date (or (mhc-summary-current-date) (mhc-calendar-get-date) (mhc-date-now)))
        (succeed t)
        msgp date time subject location category recurrence-tag priority alarm)
    (and (called-interactively-p 'interactive)
         (mhc-window-push))
    (set-buffer draft-buffer)
    (if import-buffer
        (progn
          (insert-buffer-substring-no-properties
           (if (consp import-buffer)
               (cdr import-buffer)
             import-buffer))
          (mhc-header-narrowing
            (setq msgp (or (mhc-header-get-value "from")
                           (mhc-header-get-value "x-sc-subject")))
            (mhc-header-delete-header
             (concat "^\\("
                     (mhc-regexp-opt mhc-draft-unuse-hdr-list)
                     "\\)")
             'regexp))
          (mhc-highlight-message)
          (switch-to-buffer draft-buffer t)))
    (condition-case ()
        (if import-buffer
            (progn
              (delete-other-windows-vertically)
              (goto-char (point-min))
              (if (y-or-n-p "Do you want to import this article? ")
                  (let* ((original (with-current-buffer
                                       (if (consp import-buffer)
                                           (cdr import-buffer)
                                         import-buffer)
                                     (mhc-parse-buffer)))
                         (schedule (car (mhc-record-schedules original)))
                         (inputs (copy-sequence mhc-input-sequences))
                         input)
                    (while (setq input (car inputs))
                      (setq inputs (delq input inputs))
                      (cond
                       ((eq input 'date)
                        ;; input date
                        (setq date
                              (mhc-input-day "Date: "
                                             current-date
                                             (mhc-guess-date))))
                       ((eq input 'time)
                        ;; input time
                        (setq time
                              (mhc-input-time "Time: "
                                              (mhc-schedule-time-as-string
                                               schedule)
                                              (mhc-guess-time
                                               (mhc-minibuf-candidate-nth-begin)))))
                       ((eq input 'subject)
                        ;; input subject
                        (setq subject
                              (mhc-input-subject
                               "Subject: "
                               (mhc-misc-sub
                                (or (mhc-record-subject original)
                                    (mhc-header-narrowing
                                      (mhc-header-get-value "subject")))
                                "^\\(Re:\\)? *\\(\\[[^\]]+\\]\\)? *"
                                ""))))
                       ((eq input 'location)
                        ;; input location
                        (setq location
                              (mhc-input-location
                               "Location: "
                               (mhc-schedule-location schedule))))
                       ((eq input 'category)
                        ;; input category
                        (setq category
                              (mhc-input-category
                               "Category: "
                               (mhc-schedule-categories-as-string schedule))))
                        ;; input recurrence tag
                       ((eq input 'recurrence-tag)
                        (setq recurrence-tag
                              (mhc-input-recurrence-tag
                               "Recurrence Tag: "
                               (mhc-schedule-recurrence-tag-as-string schedule))))
                       ;; input alarm
                       ((eq input 'alarm)
                        (if mhc-ask-alarm
                            (setq alarm
                                  (mhc-input-alarm
                                   "Alarm: "
                                   mhc-default-alarm))))))
                    ;;
                    (setq priority (mhc-schedule-priority schedule)))
                ;; Answer was no.
                (message "") ; flush minibuffer.
                (and (called-interactively-p 'interactive)
                     (mhc-window-pop))
                (setq succeed nil)
                (kill-buffer draft-buffer)))
          ;; No import (it succeeds).
          (let ((inputs (copy-sequence mhc-input-sequences))
                input)
            (while (setq input (car inputs))
              (setq inputs (delq input inputs))
              (cond
               ((eq input 'date)
                (setq date (mhc-input-day "Date: " current-date)))
               ((eq input 'time)
                (setq time (mhc-input-time "Time: ")))
               ((eq input 'subject)
                (setq subject (mhc-input-subject "Subject: ")))
               ((eq input 'location)
                (setq location (mhc-input-location "Location: ")))
               ((eq input 'category)
                (setq category (mhc-input-category "Category: ")))
               ((eq input 'recurrence-tag)
                (setq recurrence-tag (mhc-input-recurrence-tag "Recurrence Tag: " (or subject ""))))
               ((eq input 'alarm)
                (if mhc-ask-alarm
                    (setq alarm (mhc-input-alarm "Alarm: " mhc-default-alarm))))))))
      ;; Quit.
      (quit
       (and (called-interactively-p 'interactive)
            (mhc-window-pop))
       (setq succeed nil)
       (kill-buffer draft-buffer)))
    (if succeed
        (progn
          (switch-to-buffer draft-buffer t)
          (set-buffer draft-buffer)
          (if (and import-buffer msgp)
              (if (consp import-buffer)
                  (mhc-draft-reedit-buffer (car import-buffer) 'original)
                ;; Delete candidate overlay if exists.
                (if mhc-minibuf-candidate-overlay
                    (delete-overlay mhc-minibuf-candidate-overlay))
                ;; Already imported to current buffer.
                (mhc-draft-reedit-buffer (current-buffer)))
            ;; Delete candidate overlay if exists.
            (if mhc-minibuf-candidate-overlay
                (delete-overlay mhc-minibuf-candidate-overlay))
            (mhc-draft-setup-new))
          (mhc-header-narrowing
            (mhc-header-delete-header
             (concat "^\\("
                     (mhc-regexp-opt (mhc-header-list))
                     "\\)")
             'regexp))
          (goto-char (point-min))
          (insert "X-SC-Subject: " subject
                  "\nX-SC-Location: " location
                  "\nX-SC-Day: "
                  (mapconcat
                   (lambda (day)
                     (mhc-date-format day "%04d%02d%02d" yy mm dd))
                   date " ")
                  "\nX-SC-Time: "
                  (mhc-time-range-to-string time)
                  "\nX-SC-Category: "
                  (mapconcat (function capitalize) category " ")
                  "\nX-SC-Priority: " (if priority
                                          (number-to-string priority)
                                        "")
                  "\nX-SC-Recurrence-Tag: " recurrence-tag
                  "\nX-SC-Cond: "
                  "\nX-SC-Duration: "
                  "\nX-SC-Alarm: " (or alarm "")
                  "\nX-SC-Record-Id: " (mhc-record-create-id)
                  "\nX-SC-Sequence: 0\n")
          (goto-char (point-min))
          (mhc-draft-mode)
          succeed))))

(defcustom mhc-default-import-original-article nil
  "*If non-nil value, import a schedule with MIME attachements."
  :group 'mhc
  :type 'boolean)

(defun mhc-import (&optional get-original)
  "Import a schedule from the current article.
The default action of this command is to import a schedule from the
current article without MIME attachements.  If you want to import a
schedule including MIME attachements, call this command with a prefix
argument GET-ORIGINAL.
Set non-nil to `mhc-default-import-original-article', and
the default action of this command is changed to the latter."
  (interactive
   (list (if mhc-default-import-original-article
             (not current-prefix-arg)
           current-prefix-arg)))
  (mhc-window-push)
  (unless (mhc-edit (mhc-summary-get-import-buffer get-original))
    ;; failed.
    (mhc-window-pop)))

(defun mhc-import-from-region (beg end)
  "Import a schedule from region BEG END."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (let ((str (buffer-substring beg end)))
      (mhc-import)
      (goto-char (point-max))
      (insert str)
      (goto-char (point-min)))))

(defun mhc-delete ()
  "Delete the current schedule."
  (interactive)
  (mhc-delete-file (mhc-summary-record)))

(defcustom mhc-delete-file-hook nil
  "Normal hook run after mhc-delete-file."
  :group 'mhc
  :type 'hook)

(defun mhc-delete-file (record)
  (interactive)
  (if (not (and record (file-exists-p (mhc-record-name record))))
      (message "File does not exist (%s)." (mhc-record-name record))
    (if (not (y-or-n-p (format "Do you delete %s ?"
                               (mhc-record-subject-as-string record))))
        (message "Never mind..")
      (if (and
           (mhc-record-occur-multiple-p record)
           (not (y-or-n-p
                 (format
                  "%s has multiple occurrences. Delete all(=y) or one(=n) ?"
                  (mhc-record-subject-as-string record)))))
          (mhc-db-add-exception-rule
           record
           (or (mhc-summary-current-date)
               (mhc-calendar-view-date)))
        (mhc-db-delete-file record))
      (or (and (mhc-summary-buffer-p)
               (mhc-rescan-month mhc-default-hide-private-schedules))
          (and (mhc-calendar-p) (mhc-calendar-rescan)))
      (run-hooks 'mhc-delete-file-hook))))

(defun mhc-reuse-create ()
  "Create new draft buffer using stored template."
  (interactive)
  (let ((date-list (mapconcat
                    (lambda (day)
                      (mhc-date-format day "%04d%02d%02d" yy mm dd))
                    (mhc-input-day "Date: " (mhc-summary-current-date))
                    " "))
        (time-list (mhc-time-range-to-string
                    (mhc-input-time "Time: "
                                    (mhc-schedule-time-as-string
                                     (car (mhc-record-schedules
                                           (mhc-parse-string (mhc-draft-template)))))))))
    (mhc-window-push)
    (mhc-draft-new (mhc-draft-template)
                   `(("x-sc-record-id" . ,(mhc-record-create-id))
                     ("x-sc-sequence"  . 0)
                     ("x-sc-time"  . ,time-list)
                     ("x-sc-day" . ,date-list)))))

(defun mhc-reuse-copy (&optional filename)
  "Copy current schedule to template."
  (interactive)
  (let* ((file (or filename (mhc-summary-filename)))
         (record (mhc-parse-file file)))
    (if (and (stringp file) (file-exists-p file))
        (with-temp-buffer
          (mhc-insert-file-contents-as-coding-system
           mhc-default-coding-system file)
          (mhc-header-decode-ewords)
          (mhc-draft-store-template
           (buffer-substring-no-properties (point-min) (point-max)))
          (message "%s is copied." (mhc-record-subject-as-string record)))
      (message "No file here."))))

(defun mhc-modify ()
  "Modify the current schedule."
  (interactive)
  (mhc-modify-file (mhc-summary-filename)))

(defcustom mhc-browse-x-url-function 'browse-url
  "*A function to browse URL."
  :group 'mhc
  :type 'function)

(defun mhc-browse-x-url ()
  "Browse X-URL field."
  (interactive)
  (let ((filename (mhc-summary-filename))
        url)
    (with-temp-buffer
      (mhc-insert-file-contents-as-coding-system
       mhc-default-coding-system filename)
      (if (setq url (mhc-header-narrowing
                      (or (mhc-header-get-value "x-uri")
                          (mhc-header-get-value "x-url"))))
          (progn
            (funcall mhc-browse-x-url-function url)
            (message "X-URL browser started."))
        (message "No X-URL field.")))))

(defun mhc-modify-file (file)
  (if (and (stringp file) (file-exists-p file))
      (let* ((name (format
                    "*mhc draft %s*"
                    (file-name-nondirectory file)))
             (buffer (get-buffer name)))
        (if (buffer-live-p buffer)
            (progn
              (message "Specified file(%s) has already been opened." file)
              (switch-to-buffer-other-window buffer))
          (mhc-window-push)
          (set-buffer (setq buffer (get-buffer-create name)))
          (mhc-draft-reedit-file file)
          (set-buffer-modified-p nil)
          (switch-to-buffer-other-window buffer)
          (goto-char (point-min))
          (mhc-draft-mode)
          (set (make-local-variable 'mhc-draft-buffer-file-name) file)))
    (message "Specified file(%s) does not exist." file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (Category . (parent-face fg bg))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc.

;;
;; Convinient function when you want to insert your schedule into an
;; editing buffer.
;;
(defun mhc-insert-schedule (&optional hide-private)
  (interactive "P")
  (set-mark (point))
  (mhc-scan-month (mhc-input-month "Month ")
                  'direct ;; insert into current buffer.
                  mhc-default-category-predicate-sexp
                  hide-private)
  (exchange-point-and-mark))

(defun mhc-view-file ()
  "View the schedule on the current line in View mode in another window."
  (interactive)
  (let ((path (mhc-summary-filename)))
    (view-file-other-window path)))


;;; Temporary buffers

(defvar mhc-tmp-buffer-list nil)

(defun mhc-get-buffer-create (name)
  "Return NAME buffer for temporary use of MHC."
  (let ((buf (get-buffer name)))
    (or (and buf (buffer-name buf))
        (progn
          (setq buf (get-buffer-create name)
                mhc-tmp-buffer-list (cons buf mhc-tmp-buffer-list))
          (buffer-disable-undo buf)))
    buf))

(defun mhc-kill-all-buffers ()
  "Kill all buffers for temporary use of MHC."
  (while mhc-tmp-buffer-list
    (if (buffer-name (car mhc-tmp-buffer-list))
        (kill-buffer (car mhc-tmp-buffer-list)))
    (setq mhc-tmp-buffer-list
          (cdr mhc-tmp-buffer-list))))


;;; Setup and exit

(defcustom mhc-setup-hook nil
  "Run hook after mhc-setup."
  :group 'mhc
  :type 'hook)

(defvar mhc-setup-p nil)

(defun mhc-setup ()
  (unless mhc-setup-p
    (condition-case nil
        (progn
          (or (featurep 'easymenu) (require 'easymenu))
          (easy-menu-define mhc-mode-menu
                            mhc-mode-map
                            "Menu used in mhc mode."
                            mhc-mode-menu-spec)
          (easy-menu-define mhc-calendar-mode-menu
                            mhc-calendar-mode-map
                            "Menu used in mhc calendar mode."
                            mhc-calendar-mode-menu-spec))
      (error nil))
    (or (assq 'mhc-mode minor-mode-alist)
        (setq minor-mode-alist
              (cons (list 'mhc-mode (mhc-file-line-status))
                    minor-mode-alist)))
    (or (assq 'mhc-mode minor-mode-map-alist)
        (setq minor-mode-map-alist
              (cons (cons 'mhc-mode mhc-mode-map)
                    minor-mode-map-alist)))
    (mhc-face-setup)
    (mhc-calendar-setup)
    (mhc-file-setup)
    (setq mhc-default-category-predicate-sexp
          (mhc-expr-compile mhc-default-category))
    (and (mhc-use-icon-p) (mhc-icon-setup))
    (and mhc-calendar-link-hnf (mhc-calendar-hnf-face-setup))
    (mhc-summary-line-inserter-setup)
    (mhc-guess-location-setup)
    (autoload 'mhc-ps "mhc-ps" "*Create PostScript calendar with selected method." t)
    (autoload 'mhc-ps-preview "mhc-ps" "*Preview PostScript calendar." t)
    (autoload 'mhc-ps-print "mhc-ps" "*Print PostScript calendar." t)
    (autoload 'mhc-ps-save "mhc-ps" "*Save PostScript calendar." t)
    (autoload 'mhc-ps-insert-buffer "mhc-ps" "*Insert PostScript calendar." t)
    (setq mhc-setup-p t)
    (run-hooks 'mhc-setup-hook)))

;;;###autoload
(defun mhc ()
  "Show schedules of this month."
  (interactive)
  (mhc-setup)
  (mhc-goto-this-month))

(defalias 'mhc-mua-setup 'mhc-setup)

(defun mhc-reset ()
  "Reset MHC."
  (interactive)
  (message "MHC resetting...")
  (mhc-face-setup)
  (mhc-calendar-setup)
  (and (mhc-use-icon-p) (mhc-icon-setup))
  (and mhc-calendar-link-hnf (mhc-calendar-hnf-face-setup))
  (mhc-summary-line-inserter-setup)
  (mhc-guess-location-setup)
  (or (and (mhc-summary-buffer-p)
           (mhc-rescan-month mhc-default-hide-private-schedules))
      (and (mhc-calendar-p) (mhc-calendar-rescan)))
  (message "MHC resetting...done"))

(defcustom mhc-exit-hook nil
  "Run hook after mhc-exit."
  :group 'mhc
  :type 'hook)

(defun mhc-exit ()
  (setq mhc-setup-p nil)
  (mhc-file-exit)
  (mhc-kill-all-buffers)
  (run-hooks 'mhc-exit-hook))

(defun mhc-version ()
  "Show mhc version."
  (interactive)
  (message mhc-version))

;;; Copyright Notice:

;; Copyright (C) 1999, 2000 Yoshinari Nomura. All rights reserved.
;; Copyright (C) 2000 MHC developing team. All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS''
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
;; THE TEAM OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mhc.el ends here
