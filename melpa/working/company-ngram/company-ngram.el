;;; company-ngram.el --- N-gram based completion
;;
;; Author: kshramt
;; Version: 0.0.1
;; URL: https://github.com/kshramt/company-ngram
;; Package-Requires: ((cl-lib "0.5") (company "0.8.0"))

;; This program is distributed under the terms of
;; the GNU General Public License version 3
;; (see <http://www.gnu.org/licenses/>).
;;
;;; Commentary:
;;
;; ; ~/.emacs.d/init.el
;;
;; (with-eval-after-load 'company-ngram
;;   ; ~/data/ngram/*.txt are used as data
;;   (setq company-ngram-data-dir "~/data/ngram")
;;   ; company-ngram does not support python2
;;   (setq company-ngram-python "/path/to/python3")
;;   (company-ngram-init)
;;   (add-to-list 'company-backends 'company-ngram-backend)
;;   ; or use `M-x turn-on-company-ngram' and
;;   ; `M-x turn-off-company-ngram' on individual buffers
;;   )
;; (require 'company-ngram nil t)
;;
;;; Code:

(require 'cl-lib)
(require 'company)
(require 'json)

(defgroup company-ngram nil
  "N-gram based completion"
  :group 'company-ngram
  :prefix "company-ngram-")


;;; BACKENDS


(defconst company-ngram-dir
  (file-name-directory load-file-name))
(defconst company-ngram-ngram-py
  (concat (file-name-as-directory company-ngram-dir)
          "ngram.py"))


(defcustom company-ngram-python "python3"
  "Path to Python executable"
  :type 'string
  :group 'company-ngram
  )
(defcustom company-ngram-n 5
  "Maximum N of N-gram"
  :type 'integer
  :group 'company-ngram
  )
(defcustom company-ngram-n-out-max 10000
  "Maximum number of candidates"
  :type 'integer
  :group 'company-ngram
  )
(defcustom company-ngram-sleep-for 0.01
  "Time interval (s) to check completeness of output."
  :type 'float
  :group 'company-ngram
  )
(defcustom company-ngram-timeout 0.5
  "Timeout to wait for output from the server"
  :type 'float
  :group 'company-ngram
  )
(defcustom company-ngram-data-dir "~/data/ngram"
  "`company-ngram-data-dir/*.txt' are used to generate N-gram data"
  :type 'string
  :group 'company-ngram
  )


(defvar company-ngram-candidates nil)
(defvar company-ngram-prev-words nil)


;;;###autoload
(defun turn-on-company-ngram ()
  (interactive)
  (set (make-local-variable 'company-backends)
       (cons 'company-ngram-backend company-backends)))


;;;###autoload
(defun turn-offcompany-ngram ()
  (interactive)
  (set (make-local-variable 'company-backends)
       (remove 'company-ngram-backend company-backends)))


;;;###autoload
(defun company-ngram-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ngram-backend))
    (prefix (company-ngram--prefix))
    (candidates (get-text-property 0 :candidates arg))
    (annotation (concat " " (get-text-property 0 :ann arg)))
    (sorted t)
    (no-cache t)
    )
  )


(defun company-ngram--prefix ()
  (let* ((p2 (point))
         (p1 (max (- p2 (* 30 company-ngram-n)) 1)) ; length of the longest word in /usr/share/dict/words was 24
         (s (buffer-substring p1 p2))
         (l (split-string s))
         (is-suffix-space (string-suffix-p " " s))
         (words
          (if is-suffix-space
              (last l (1- company-ngram-n))
            (last (butlast l) (1- company-ngram-n))))
         (pre (if is-suffix-space
                  " "
                (car (last l))))
         (candidates
          (all-completions
           pre
           (if (equal words company-ngram-prev-words)
               company-ngram-candidates
             (progn
               (setq company-ngram-candidates
                     (mapcar
                      (lambda (c)
                        (let ((s (car c)))
                          (put-text-property
                           0 1 :ann
                           (cadr c) s)
                          s))
                      (company-ngram-query words)))
               (setq company-ngram-prev-words words)
               (mapcar
                (lambda (w)
                  (let ((sp " "))
                    (put-text-property
                     0 1 :ann
                     (get-text-property 0 :ann w) sp)
                    (concat sp w)))
                company-ngram-candidates)
               ))))
         )
    (when candidates
      (put-text-property 0 1 :candidates candidates pre)
      (cons pre t)))
  )


(defvar company-ngram-process nil)


;;;###autoload
(defun company-ngram-init ()
  (company-ngram--init company-ngram-python
                       company-ngram-ngram-py
                       company-ngram-n
                       company-ngram-data-dir)
  )
(defun company-ngram--init (python ngram-py n dir)
  (condition-case nil
      (kill-process company-ngram-process)
    (error nil))
  (condition-case nil
      (with-current-buffer (process-buffer company-ngram-process)
        (erase-buffer)
        (let ((kill-buffer-query-functions
               (remove 'process-kill-buffer-query-function
                       kill-buffer-query-functions)))
          (kill-buffer)))
    (error nil))
  (setq company-ngram-process
        (company-ngram---init python
                              ngram-py
                              n
                              dir))
  (with-current-buffer (process-buffer company-ngram-process)
    (buffer-disable-undo)
    (erase-buffer)
    (insert "\n\n")))
(defun company-ngram---init (python ngram-py n dir)
  (let ((process-connection-type nil)
        (process-adaptive-read-buffering t))
    (start-process "company-ngram"
                   (generate-new-buffer-name "*company-ngram*")
                   python
                   ngram-py
                   (format "%d" n)
                   (expand-file-name dir)
                   )))


(defun company-ngram-query (words)
  (company-ngram--query company-ngram-process
                        company-ngram-n-out-max
                        company-ngram-timeout
                        words))
(defun company-ngram--query (process n-out-max timeout words)
  (with-current-buffer (process-buffer process)
    (with-local-quit
      (company-ngram-plain-wait timeout)
      (erase-buffer)
      (process-send-string process
                           (concat (format "%d\t%e\t" n-out-max timeout)
                                   (mapconcat 'identity words "\t")
                                   "\n"))
      (accept-process-output process)
      )
    (company-ngram-plain-wait timeout)
    (company-ngram-get-plain)
    ))


(defun company-ngram-get-plain ()
  (mapcar (lambda (l) (split-string l "\t"))
          (split-string (buffer-string) "\n" t)))


(defun company-ngram-plain-wait (l)
  (let ((i (1+ (ceiling (/ l company-ngram-sleep-for)))))
    (while (and (not (company-ngram-plain-ok-p)) (> i 0))
      (sleep-for company-ngram-sleep-for)
      (decf i))))


(defun company-ngram-plain-ok-p ()
  (let ((pmax (point-max)))
    (equal (buffer-substring (max (- pmax 2) 1)
                             pmax)
           "\n\n")))


(provide 'company-ngram)

;;; company-ngram.el ends here
