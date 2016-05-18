;;; chee-dired.el --- functions to integrate chee with dired and image-dired

;; Copyright © 2016- Eike Kettner

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(eval-when-compile (require 'cl))
(require 'image-dired)
(require 'dash)
(require 'chee-settings)
(require 'chee-proc)

(defvar chee-dired-buffer-name "*chee-dired*")

(defun chee-dired-get-ls-switches ()
  "Return the switches to use with ls."
  (or chee-dired-ls-switches dired-listing-switches))

(defun chee--thumb-command (query &optional concurrent dir rec pagenum decrypt method)
  "Create a list to use with `chee-proc-async-sexp'. The command
assembles chee's `thumb' subcommand."
  (append
   (list "thumb"
         "--size" chee-thumb-size
         "--pattern" "lisp")
   (if concurrent
       (list "--concurrent"))
   (if (stringp dir)
       (list "--file" dir))
   (if rec
       (list "--recursive"))
   (let* ((page (or (and (numberp pagenum) (> pagenum 0) pagenum) 1))
          (skip (* chee-page-size (1- page))))
     (list "--first" (format "%s" chee-page-size) "--skip" (format "%s" skip)))
   (if decrypt
       (list "-d"))
   (cond ((string= "default" method) nil)
         ((stringp method) (list "--method" method))
         (t nil))
   (list query)))

(defun chee-dired-get-buffer ()
  (get-buffer-create chee-dired-buffer-name))

(defun chee--dired-bind-paging-keys ()
  "Sets up a key map for navigating pages. Must be evaluated
inside `chee-dired' function."
  (let ((lmap (make-sparse-keymap)))
    (set (make-local-variable 'chee--dired-next-page-fn)
         `(lambda () (chee-dired ,query ,concurrent ,dir ,rec ,(1+ (or page 1)) ,decrypt ,method ,repodir)))
    (set (make-local-variable 'chee--dired-prev-page-fn)
         `(lambda () (chee-dired ,query ,concurrent ,dir ,rec ,(1- (or page 2)) ,decrypt ,method ,repodir)))

    (set-keymap-parent lmap (current-local-map))
    (define-key lmap (kbd "M-n") (lambda ()
                                   (interactive)
                                   (funcall chee--dired-next-page-fn)))
    (define-key lmap (kbd "M-p") (lambda ()
                                   (interactive)
                                   (funcall chee--dired-prev-page-fn)))

    (use-local-map lmap)))

(defun chee-dired (query &optional concurrent dir rec page decrypt method repodir)
  (message "Showing page %s" (or page 1))
  (with-current-buffer (chee-dired-get-buffer)
    (let ((inhibit-read-only t))
      (widen)
      (erase-buffer)
      (with-current-buffer (image-dired-create-thumbnail-buffer)
        (chee--dired-bind-paging-keys)
        (erase-buffer)))
    (kill-all-local-variables)
    (setq default-directory (or dir "/"))
    (setq buffer-read-only t)
    (set (make-local-variable 'dired-sort-inhibit) t)
    (set (make-local-variable 'revert-buffer-function)
         `(lambda (ignore-auto noconfirm)
            (chee-dired ,query ,concurrent ,dir ,rec ,page ,decrypt ,method ,repodir)))
    (set (make-local-variable 'dired-subdir-alist)
         (list (cons (or dir "/") (point-min-marker))))
    (let* ((pos (point))
           (inhibit-read-only t)
           (ls-switches (chee-dired-get-ls-switches))
           (cmd (chee--thumb-command query concurrent dir rec page decrypt method)))
      (dired-mode (or dir "/") ls-switches)
      (chee--dired-bind-paging-keys)
      (insert "  " (or dir "/") ":\n")
      (insert "  chee " (mapconcat 'identity cmd " ") "\n")
      (dired-insert-set-properties pos (point))
      (let ((proc (chee-proc-async-sexp cmd (function chee--dired-callback) repodir)))
        (chee--setup-kill-key proc (current-buffer))
        (chee--setup-kill-key proc (image-dired-create-thumbnail-buffer))))))

(defun chee--dired-callback (proc plist)
  (-if-let (err (plist-get plist :error-message))
      (message err)
    (let ((original (plist-get plist :origin-path))
          (thumb (plist-get plist :path))
          (buf (get-buffer "*chee-dired*"))
          (inhibit-read-only t)
          (ls-switches (chee-dired-get-ls-switches)))
      (when (and original (buffer-name buf))
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-max))
              (let ((pos (point)))
                (insert (s-trim (shell-command-to-string (concat "ls " ls-switches " " (shell-quote-argument original) "| tail -n -1"))))
                (insert "\n")
                (dired-insert-set-properties pos (point))))))
        (with-current-buffer (image-dired-create-thumbnail-buffer)
          (save-excursion
            (save-restriction
              (goto-char (point-max))
              (let ((beg (point)))
                (image-dired-insert-thumbnail thumb original buf)
                (add-text-properties beg (point) (list :chee-data plist)))
              (cond ((eq 'dynamic image-dired-line-up-method)
                     (image-dired-line-up-dynamic))
                    ((eq 'fixed image-dired-line-up-method)
                     (image-dired-line-up))
                    ((eq 'interactive image-dired-line-up-method)
                     (image-dired-line-up-interactive))
                    ((eq 'none image-dired-line-up-method)
                     nil)
                    (t
                     (image-dired-line-up-dynamic))))))))))

(defun chee--setup-kill-key (proc buf)
  (with-current-buffer buf
    (let ((killfn (lexical-let ((procbuf (process-buffer proc)))
                    (lambda ()
                      (interactive)
                      (with-current-buffer procbuf
                        (message "Killing chee …")
                        (chee-proc-kill)))))
          (map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map (kbd "C-c C-k") killfn)
      (use-local-map map))))


(defun chee-dired-make-info-string (plist)
  (format "%s, %s (%s %s)"
          (or (plist-get plist :origin-created)
              (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time (/ (plist-get plist :origin-lastmodified) 1000))))
          (file-size-human-readable (plist-get plist :origin-length))
          (or (plist-get plist :origin-make) "")
          (or (plist-get plist :origin-model) "")))

(defun chee-dired--format-info (fmt plist)
  (format-spec fmt
               (list (cons ?a (or (plist-get plist :origin-created)
                                  (format-time-string "%Y-%m-%d %H:%M:%S"
                                                      (seconds-to-time
                                                       (/ (plist-get plist :origin-lastmodified) 1000)))))
                     (cons ?s (file-size-human-readable (plist-get plist :origin-length)))
                     (cons ?m (or (plist-get plist :origin-make) ""))
                     (cons ?o (or (plist-get plist :origin-model) ""))
                     (cons ?w (or (plist-get plist :origin-width) ""))
                     (cons ?h (or (plist-get plist :origin-height) ""))
                     (cons ?f (or (plist-get plist :origin-filename) "<unkwown-file>")))))

(defun chee-dired--modify-info (origfn &rest args)
  "Advice function to `image-dired-format-properties-string'
adding more format specifiers. You can use additional format
specifiers with `image-dired-display-properties-format':

- s  file size
- a  created or lastmodified time
- m  camera make
- o  camera model"
  (let ((res (apply origfn args))
        (plist (get-text-property (point) :chee-data)))
    (if plist
        ;; b,f,t,c already used by image-dired-format-properties-string
        (chee-dired--format-info (concat res chee-dired-properties-format) plist)
      res)))

(advice-add 'image-dired-format-properties-string :around #'chee-dired--modify-info)

(defun chee-dired--display-info (origfn &rest args)
  "Advice function to `image-dired-display-thumbnail-original-image'."
  (let ((cheedata (get-text-property (point) :chee-data))
        (origres (apply origfn args)))
    (when (and chee-dired-detail-info-format cheedata)
      (with-current-buffer image-dired-display-image-buffer
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (insert "\n")
            (insert (chee-dired--format-info chee-dired-detail-info-format cheedata))))))
    origres))

(advice-add 'image-dired-display-thumbnail-original-image :around #'chee-dired--display-info)

(provide 'chee-dired)
;;; chee-dired.el ends here
