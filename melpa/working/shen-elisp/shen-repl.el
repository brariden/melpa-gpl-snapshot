;; -*- lexical-binding: t -*-

;; [[file:shen-elisp.org::*Shen%20REPL][Shen\ REPL:2]]
(require 'comint)
(require 'shen-primitives)
(require 'shen)
(require 'shen-overlays)
;; Shen\ REPL:2 ends here

;; [[file:shen-elisp.org::*Credits][Credits:1]]
(defconst shen/shen.credits
  (format "%s\n%s\n%s\n%s\n\n"
          "Shen, copyright (C) 2010-2015 Mark Tarver"
          (format "www.shenlanguage.org, %s" (shen/value '*version*))
          (format "running under %s, implementation: %s" (shen/value '*language*) (shen/value '*implementation*))
          (format "port %s ported by %s" (shen/value '*port*) (shen/value '*porters*))))
;; Credits:1 ends here

;; [[file:shen-elisp.org::*Prompt][Prompt:1]]
(defconst shen/repl-prompt-regex
  (rx line-start
      (char ?( )
            (1+ digit)
            (or (char ?-) (char ?+))
            (char ?))
      (char ? )))
;; Prompt:1 ends here

;; [[file:shen-elisp.org::*Prompt][Prompt:2]]
(defun shen/make-prompt nil
  (format "(%d%s) "
          (shen/length (shen/value 'shen.*history*))
          (if (shen/shen->predicate (shen/value 'shen.*tc*))
              "+"
            "-")))
;; Prompt:2 ends here

;; [[file:shen-elisp.org::*Input%20Events][Input\ Events:1]]
(defvar shen/repl-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'shen/repl-send-input)
    (define-key map "\C-m" 'shen/repl-return)
    map))

(defvaralias 'shen/repl-mode-map 'shen/repl-map)

(defun shen/repl-return nil
  (interactive)
  (shen/repl-send-input))

(defvar shen/repl-input)

(defun shen/repl-send-input nil
  (interactive)
  (progn
    (comint-send-input)
    (condition-case ex
        (progn
          (shen/shen.initialise_environment)
          (shen/repl-eval (string-to-list shen/repl-input)))
      ('error
       (comint-output-filter (shen/repl-process) (format "%s\n%s" ex  (shen/make-prompt)))
       (signal (car ex) (cdr ex))))
    (with-current-buffer *shen-repl*
      (goto-char (point-max)))))
;; Input\ Events:1 ends here

;; [[file:shen-elisp.org::*Evaluating%20User%20Input][Evaluating\ User\ Input:1]]
(defun shen/repl-standard-output-impl (process)
  (let* ((output-buffer nil)
         (flush-timer nil)
         (flush-buffer
          (lambda ()
            (comint-output-filter
             process
             (apply #'string (nreverse output-buffer)))
            (redisplay)
            (setf output-buffer nil)
            (when flush-timer
              (cancel-timer flush-timer)
              (setf flush-timer nil)))))
    (lambda (char)
      (let (flush-now)
        (cond ((and (eq char t) output-buffer)
               (push ?\n output-buffer)
               (setf flush-now t))
              ((characterp char)
               (push char output-buffer)))
        (if flush-now
            (funcall flush-buffer)
          (unless flush-timer
            (setf flush-timer (run-with-timer 0.1 nil flush-buffer))))))))
;; Evaluating\ User\ Input:1 ends here

;; [[file:shen-elisp.org::*Evaluating%20User%20Input][Evaluating\ User\ Input:2]]
(defun shen/repl-process nil
  ;; Return the current buffer's process.
  (get-buffer-process (current-buffer)))

(defun shen/repl-eval (input-string)
  (let* ((active-process (shen/repl-process))
         (shen/repl-temp-buffer)
         (clean-up (lambda (active-process &optional ex)
                     (progn
                       (comint-output-filter active-process
                                             (if ex
                                                 (format "\n%s\n\n%s" (nth 1 ex) (shen/make-prompt))
                                               (format "\n%s" (shen/make-prompt))))
                       (funcall (shen/value '*stoutput*) t)
                       (shen/set '*stoutput* standard-output)))))
    (condition-case ex
        (progn
          (shen/set '*stoutput* (shen/repl-standard-output-impl active-process))
          (set-buffer (get-buffer *shen-repl*))
          (let* ((Lineread
                  (shen/compile #'shen/shen.<st_input> input-string
                                (lambda (Err) (signal (car Err) (cdr Err)))))
                 (It (shen/shen.record-it input-string))
                 (History (shen/value 'shen.*history*))
                 (NewLineread (shen/shen.retrieve-from-history-if-needed
                               (shen/@p Lineread input-string)
                               History))
                 (NewHistory (shen/shen.update_history NewLineread History))
                 (Parsed (shen/fst NewLineread)))
            (if (not Parsed)
                (funcall clean-up active-process)
              (progn
                (shen/shen.toplevel Parsed)
                (funcall (shen/value '*stoutput*) t)
                (comint-output-filter active-process (format "\n%s" (shen/make-prompt)))))))
      ('shen/error (funcall clean-up active-process ex)))))
;; Evaluating\ User\ Input:2 ends here

;; [[file:shen-elisp.org::*The%20REPL%20Mode][The\ REPL\ Mode:1]]
(defconst shen/syntax-table
  (let ((table (make-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry 59 "_") ;; semi-colon
    (modify-syntax-entry ?, "_")
    (modify-syntax-entry ?# "_")
    (modify-syntax-entry ?' "_")
    (modify-syntax-entry ?` "_")
    table))

(defun shen/repl-input-sender (_proc input)
  (setq shen/repl-input input))

(defun shen/repl-pm nil
  ;; Return the process mark of the current buffer.
  (process-mark (get-buffer-process (current-buffer))))

(defun shen/repl-set-pm (pos)
  ;; Set the process mark in the current buffer to POS.
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

(define-derived-mode shen/repl-mode comint-mode "shen-repl-mode"
  :syntax-table shen/syntax-table
  (setq comint-prompt-regexp shen/repl-prompt-regex)
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-read-only t)
  (setq comint-input-sender 'shen/repl-input-sender)
  (setq-local comment-use-syntax 'undecided)
  (unless (comint-check-proc (current-buffer))
    (condition-case nil
        (start-process "shen/repl" (current-buffer) "cat")
      (file-error (start-process "shen/repl" (current-buffer) "hexl")))
    (set-process-query-on-exit-flag (shen/repl-process) nil)
    (goto-char (point-max))
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)
    (insert shen/shen.credits)
    (shen/repl-set-pm (point-max))
    (comint-output-filter (shen/repl-process) "(0-) ")
    (set-marker comint-last-input-start (shen/repl-pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter)))

(defconst *shen-repl* "*shen-repl*")
;; The\ REPL\ Mode:1 ends here

;; [[file:shen-elisp.org::*Starting%20the%20REPL][Starting\ the\ REPL:1]]
;;;###autoload
(defun shen/repl nil
  (interactive)
  (let (old-point)
    (unless (get-buffer *shen-repl*)
      (with-current-buffer (get-buffer-create *shen-repl*)
        (make-local-variable 'lexical-binding)
        (load "shen-primitives.elc")
        (load "shen.elc")
        (load "shen-overlays.elc")
        (shen/migrate-symbol-table)
        (setq lexical-binding 't)
        (shen/set 'shen.*history* '())
        (shen/set '*home-directory* "")
        (shen/set 'shen.*tc* 'false)
        (unless (zerop (buffer-size)) (setq old-point (point)))
        (shen/repl-mode)))
    (switch-to-buffer *shen-repl*)
    (when old-point (push-mark old-point))))
;; Starting\ the\ REPL:1 ends here

;; [[file:shen-elisp.org::*Provide%20it][Provide\ it:1]]
(provide 'shen-repl)
;; Provide\ it:1 ends here
