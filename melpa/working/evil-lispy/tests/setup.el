(require 'evil-lispy)
(require 'dash)
(require 'buttercup)
(require 's)
(require 'shut-up)

(defun buffer-status-as-string ()
  (let ((p (point))
        (m (mark)))
    ;; show mark as well (other side of selection, if any)
    (goto-char p)
    (insert "|")

    ;; show mark as well (other side of selection, if any)
    (when mark-active
      (goto-char (mark))
      (insert "~")))

  (let ((result-lines (->> (buffer-substring-no-properties (point-min)
                                                           (point-max))
                           (s-split "\n"))))
    (if (= 1 (length result-lines))
        (-first-item result-lines)
      result-lines)))

(defmacro with-test-buffer (contents &rest test-forms)
  "This awesome macro is adapted (borrowed) from
  https://github.com/abo-abo/lispy/blob/master/lispy-test.el#L15"
  (declare (indent 1))
  `(progn
     (-when-let (b (get-buffer "evil-lispy-test-buffer"))
       (kill-buffer b))
     (let ((temp-buffer (get-buffer-create "evil-lispy-test-buffer")))
       (save-window-excursion
         (switch-to-buffer temp-buffer)
         (emacs-lisp-mode)
         (evil-mode)
         (evil-lispy-mode)

         (insert ,contents)

         (evil-goto-first-line)
         (when (search-forward "|")
           (backward-delete-char 1))

         ,@test-forms

         temp-buffer))))

(buttercup-define-matcher :to-have-buffer-contents (test-buffer
                                                    expected-contents)
  (with-current-buffer test-buffer
    (let ((contents (buffer-status-as-string)))
      (buttercup--apply-matcher :to-equal `(,contents ,expected-contents)))))

(buttercup-define-matcher :to-be-in-lispy-mode (test-buffer)
  (with-current-buffer test-buffer
    (buttercup--apply-matcher :to-be-truthy `(,lispy-mode))))

(defmacro -doto (eval-initial-value &rest forms)
  "todo document"
  (let ((retval (gensym)))
    `(let ((,retval ,eval-initial-value))
       ,@(mapcar (lambda (form)
                   (if (sequencep form)
                       `(,(-first-item form) ,retval ,@(rest form))
                     `(funcall form ,retval)))
                 forms)
       ,retval)))

;; these are borrowed from omnisharp-emacs
;;
(defun ot--keyboard-input (&rest text-vectors)
  "Simulates typing. Can be used to do interactive input, but
detecting situations in the middle of input is impossible.
Be careful: weird errors may happen if you try to call functions in the middle
of this function. Only use text-vectors."
  (condition-case error
      (execute-kbd-macro (reduce 'vconcat text-vectors))
    (error (print (format "ot--keyboard-input error: %s" error)))))

(defun ot--meta-x-command (command)
  (vconcat
   (ot--press-key "M-x")
   (ot--type command)
   (ot--press-key "RET")))

(defun ot--type (text)
  (string-to-vector text))

(defun ot--press-key (key-or-chord)
  (edmacro-parse-keys key-or-chord))
