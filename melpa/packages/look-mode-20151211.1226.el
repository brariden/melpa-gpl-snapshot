;;; look-mode.el --- quick file viewer for image and text file browsing
;; Package-Version: 20151211.1226

;;; Copyright (C) 2008,2009 Peter H. Mao

;; Author: Peter H. Mao <peter.mao@gmail.com> <peterm@srl.caltech.edu>
;; Version %Id: 13%

;; look-mode.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version. (http://www.gnu.org/licenses/gpl-3.0.txt)

;; look-mode.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Change log:
;;
;; 2009-10-02: fixed look-pwd to properly handle dirs with spaces
;;
;; 2009-08-21: added function look-at-this-file to fix bug in arxiv-reader
;;
;; 2009-01-08: fixed regexp in list-subdirectories-recursively

;;; Commentary:
;;
;; This package provides a function to load a list of files into a
;; temporary buffer for viewing.  The buffer (*look*) is writable,
;; so when used with eimp.el (Emacs Image Manipulation Package), one
;; can resize images without any danger of overwriting the original
;; file.  This may also be of interest to someone wishing to scan
;; the files of a directory.
;;
;; Presently, the file list is generated by a system call to "ls" so this
;; probably is not Windows compatible.  One moves through the file list 
;; using the keybindings:
;;            C-. or M-] or M-n (look-at-next-file)
;;            C-, or M-[ or M-p (look-at-previous-file)
;;
;; After loading, M-l is bound to "look-at-files" from dired
;;
;; C-c l should access the Customize interface for the "look" group.

;;; Setup:
;;
;; put this file and eimp.el into a directory in your load-path
;; or cons them onto your load-path.
;; ex: (setq load-path (cons "~/my_lisp_files/" load-path))
;;     (load "look-mode")
;; eimp gets loaded if jpg's are identified

;;; Usage:
;;
;; (look-at-files &optional ls-args)
;;
;; LS-ARGS (string) are the arguments that you would give ls to get the
;; desired file list.
;; 
;; two lists are set up: the file list and the subdirectory list
;; to display the subdirectories on the header line, set
;; look-show-subdirs to 't'
;;
;; Exclusion of files is difficult with ls, so I added some
;; functionality to exclude files matching given regexps.  To use
;; this feature, add the regexp as a string constant to the variable
;; look-skip-file-list. For example,
;; (add-to-list 'look-skip-file-list "^n[eo]") 
;; will exclude files matching the cmd line ne* and no* I use this
;; with my arXiv reader and then issue a
;; (pop look-skip-file-list) afterwards to reset the list
;; look-skip-directory-list works in the same fashion.

;;; Bugs:
;;
;; 1. can't handle zip files, so they are excluded by default

;;; Future:
;; see arxiv-reader

;;; Code:

;; Customizations
(defgroup look nil
  "View files in a temporary, writeable buffer"
  :prefix "look-"
  :group 'applications)
(defcustom look-skip-file-list '(".zip$")
  "list of regular filename regexps to skip over"
  :group 'look
  :type '(repeat regexp))
(defcustom look-skip-directory-list nil
  "list of directory name regexps to skip over"
  :group 'look
  :type '(repeat regexp))
(defcustom look-show-subdirs nil
  "'t' means show subdirectories on the header line"
  :group 'look
  :type 'boolean)
(defcustom look-recurse-dirlist t
  "incorporate all subdirectories of matching directories into
look-subdir-list"
  :group 'look
  :type 'boolean)

;; Variables that make the code work
(defvar look-forward-file-list nil
  "list of files stored by the command look-at-files for future
  viewing")
(defvar look-reverse-file-list nil
  "list of files stored by the command look-at-files for reverse
  lookup")
(defvar look-subdir-list nil
  "subdirectories found in the file listing")
(defvar look-hilight-subdir-index 1
  "subdirectory index to hilight")
(defvar look-current-file nil
  "the file being viewed in the look-buffer")
(defvar look-pwd nil
  "the directory that look started in")
(defvar look-buffer "*look*"
  "default buffer for look mode")
;;overlay code suggested by Martin Rudalics
;;http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-12/msg00195.html
(defvar look-header-overlay (make-overlay (point-min) (point-min))
  "makes overlay at top of buffer")
(defvar look-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-.") 'look-at-next-file)
    (define-key map (kbd "C-,") 'look-at-previous-file)
;    (define-key map (kbd "M-]") 'look-at-next-file)
;    (define-key map (kbd "M-[") 'look-at-previous-file)
    (define-key map (kbd "M-n") 'look-at-next-file)
    (define-key map (kbd "M-p") 'look-at-previous-file)
    (define-key map (kbd "C-c l")
      (lambda () (interactive)
        (customize-group 'look)))
    map)
  "Keymap for Look mode.")

(define-minor-mode look-mode
  "a minor mode for flipping through files"
  :init-value nil ; maybe make this t?
  :lighter " Look"
  :keymap look-minor-mode-map
  )

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "\M-l" 'look-at-files)
            ))

(defun look-reset-variables ()
  "re-initializes look-mode's variables"
  (interactive)
  (setq look-forward-file-list nil)
  (setq look-reverse-file-list nil)
  (setq look-subdir-list nil)
  (setq look-skip-file-list '(".zip$"))
  (setq look-skip-directory-list nil)
  (setq look-show-subdirs nil)
  (setq look-current-file nil)
  (setq look-buffer "*look*")
)

;;;; Navigation Commands

(defun look-at-files (look-wildcard)
  "Look at files in a directory.  Insert them into a temporary
buffer one at a time.  This function gets the file list and passes
it to look-at-next-file"
  (interactive "sEnter filename (w/ wildcards): ")
  (if (and (string-match "[Jj][Pp][Ee]?[Gg]" look-wildcard)
           (not (featurep 'eimp)))
      (require 'eimp nil t))
  (if (string= look-wildcard "")
      (setq look-wildcard "*"))
  (setq look-forward-file-list nil)
  (setq look-subdir-list (list "./"));nil)
  (setq look-reverse-file-list nil)
  (setq look-current-file nil)
  (setq look-pwd (replace-regexp-in-string 
                  "~" (getenv "HOME")
                  (replace-regexp-in-string 
                   "^Directory " "" (pwd))))
  (let ((look-file-list (file-expand-wildcards look-wildcard))
        (fullpath-dir-list nil))
  ; use relative file names to prevent weird side effects with skip lists
  ; cat look-pwd with filename, separate dirs from files,
  ; remove files/dirs that match elements of the skip lists ;;
    (dolist (lfl-item look-file-list look-forward-file-list)
      (if (and (file-regular-p lfl-item)
               ; check if any regexps in skip list match filename
               (catch 'skip-this-one 
                 (dolist (regexp look-skip-file-list t)
                   (if (string-match regexp lfl-item)
                       (throw 'skip-this-one nil))))
              )
          (setq look-forward-file-list
                (nconc look-forward-file-list
                       (list (concat look-pwd lfl-item))))
        (if (and (file-directory-p lfl-item)
                 ; check if any regexps in skip list match directory
                 (catch 'skip-this-one 
                   (dolist (regexp look-skip-directory-list t)
                     (if (string-match regexp lfl-item)
                         (throw 'skip-this-one nil))))
                 )
            (if look-recurse-dirlist
                (setq fullpath-dir-list
                      (nconc fullpath-dir-list
                             (list lfl-item)
                             (list-subdirectories-recursively
                              (concat look-pwd lfl-item) look-skip-directory-list)))
              (setq fullpath-dir-list
                    (nconc fullpath-dir-list
                           (list lfl-item))))
          )))
    ; now strip look-pwd off the subdirs in subdirlist    
    ; or maybe I should leave everything as full-path....
    (dolist (fullpath fullpath-dir-list look-subdir-list)
      (setq look-subdir-list
            (nconc look-subdir-list
                   (list (file-name-as-directory
                          (replace-regexp-in-string look-pwd "" fullpath)))))
      )
    );tel
  (get-buffer-create look-buffer)
  (look-at-next-file)
  )

(defun look-at-next-file ()
  "gets the next file in the list.  Discards the file from the
list if it is not a regular file or symlink to one."
  (interactive); pass no args on interactive call
  (kill-buffer look-buffer); clear the look-buffer
  (switch-to-buffer look-buffer); reopen the look-buffer
  (if look-current-file; if it is non-nil
      (push look-current-file look-reverse-file-list)
    )
  (if look-forward-file-list
      (progn
        (setq look-current-file (pop look-forward-file-list))
             ; get the next file in the list
        (insert-file-contents look-current-file); insert it into the *look* buffer
        (normal-mode); get the "normal mode" for this file
        (if (eq major-mode default-major-mode)
            (look-set-mode-with-auto-mode-alist t)
          )
        (look-update-header-line)
        )
    (look-no-more)
    )
  (look-mode); assert look mode
  (if (and look-current-file (featurep 'eimp)
           (string-match "[Jj][Pp][Ee]?[Gg]" 
                         (or (file-name-extension look-current-file) "")))
      (eimp-fit-image-to-window nil) ; scale to window if its a jpeg
    )
  )

(defun look-at-previous-file ()
  "gets the previous file in the list"
  (interactive); pass no args on interactive call
  (kill-buffer look-buffer); clear the look-buffer
  (switch-to-buffer look-buffer); reopen the look-buffer
  (if look-current-file
      (push look-current-file look-forward-file-list)
    )
  (if look-reverse-file-list
      (progn
        (setq look-current-file (pop look-reverse-file-list))
                                        ; get the next file in the list
        (insert-file-contents look-current-file) ; insert it into the *look* buffer
        (normal-mode)
        (if (eq major-mode default-major-mode)
            (look-set-mode-with-auto-mode-alist t)
          )
        (look-update-header-line)
        )
    (look-no-more)
    )
  (look-mode); assert look mode
  (if (and look-current-file (featurep 'eimp)
           (string-match "[Jj][Pp][Ee]?[Gg]" 
                         (or (file-name-extension look-current-file) "")))
      (eimp-fit-image-to-window nil) ; scale to window if its a jpeg
    )
  )

(defun look-at-this-file ()
  "reloads current file in the buffer"
  (interactive); pass no args on interactive call
  (kill-buffer look-buffer); clear the look-buffer
  (switch-to-buffer look-buffer); reopen the look-buffer
  (if look-current-file
      (progn 
        (insert-file-contents look-current-file) ; insert it into the *look* buffer
        (if (eq major-mode default-major-mode)
            (look-set-mode-with-auto-mode-alist t))
        (look-update-header-line))
    (look-no-more))
  (look-mode); assert look mode
  (if (and look-current-file (featurep 'eimp)
           (string-match "[Jj][Pp][Ee]?[Gg]" 
                         (or (file-name-extension look-current-file) "")))
      (eimp-fit-image-to-window nil) ; scale to window if its a jpeg
    )
  )

;;;; subroutines

(defun look-keep-header-on-top (window start)
  "Used by look-update-header-line to keep overlay at the top of
the buffer."
  (move-overlay look-header-overlay start start))
(defun lface-header (text)
  (propertize text 'face 'header-line))
(defun lface-hilite (text)
  (propertize text 'face '(:background "yellow" :foreground "black" :weight bold)))
(defun lface-number (text)
  (propertize text 'face '(:background "grey" :foreground "black" :weight bold)))

(defun look-update-header-line ()
  "defines the header line for look-mode"
  (let ((look-header-line (lface-header
                           (concat "["
                                   (number-to-string (length look-reverse-file-list))
                                   "| " 
                                   (replace-regexp-in-string look-pwd "" look-current-file)
                                   " |"
                                   (number-to-string (length look-forward-file-list)) "]"
                                   )))
        (jj 1)
        )
    (if look-show-subdirs
        ; list all but the first item in look-subdir-list
        (while (< jj (length look-subdir-list))
          (setq look-header-line
                (concat look-header-line
                        (if (= jj 1)
                            (lface-header "\n")
                          (lface-header " "))
                        (if (= jj look-hilight-subdir-index)
                            (lface-hilite (number-to-string jj))
                          (lface-number (number-to-string jj)))
                        (lface-header (replace-regexp-in-string ;remove trailing '/'
                                       "/$" "" (nth jj look-subdir-list)))
                        ))
          (setq jj (1+ jj))
          )
      )
    (overlay-put look-header-overlay 'before-string (concat look-header-line
                                                            (lface-header "\n")))
    (move-overlay look-header-overlay (window-start) (window-start) (get-buffer look-buffer))
    (add-hook 'window-scroll-functions 'look-keep-header-on-top nil t)
    ))
  
(defun look-no-more ()
  "what to do when one gets to the end of a file list"
  (setq look-current-file nil)
  (if look-forward-file-list
      (setq header-line-format "No more files to display.  Use look-at-next-file (M-n or C-.[think:>]) to go forward")
    (setq header-line-format "No more files to display.  Use look-at-previous-file (M-p or C-,[think:<]) to go back")
    )
)  

(defun look-set-mode-with-auto-mode-alist (&optional keep-mode-if-same)
  "Taken shamelessly from set-auto-mode in files.el.
Uses the look-current-file to set the mode using auto-mode-alist"
  (let ((name look-current-file)
        (remote-id (file-remote-p look-current-file))
        done
        mode)
    ;; Remove remote file name identification.
    (when (and (stringp remote-id)
               (string-match (regexp-quote remote-id) name))
      (setq name (substring name (match-end 0))))
    ;; Remove backup-suffixes from file name.
    (setq name (file-name-sans-versions name))
    (while name
      ;; Find first matching alist entry.
      (setq mode
            (if (memq system-type '(vax-vms windows-nt cygwin))
                ;; System is case-insensitive.
                (let ((case-fold-search t))
                  (assoc-default name auto-mode-alist
                                 'string-match))
              ;; System is case-sensitive.
              (or
               ;; First match case-sensitively.
               (let ((case-fold-search nil))
                 (assoc-default name auto-mode-alist
                                'string-match))
               ;; Fallback to case-insensitive match.
               (and auto-mode-case-fold
                    (let ((case-fold-search t))
                      (assoc-default name auto-mode-alist
                                     'string-match))))))
      (if (and mode
               (consp mode)
               (cadr mode))
          (setq mode (car mode)
                name (substring name 0 (match-beginning 0)))
        (setq name nil))
      (when mode
        (set-auto-mode-0 mode keep-mode-if-same)
        (setq done t))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generally useful, but here for now ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-subdirectories-recursively (&optional head-dir exclusion-list)
  "recursively list directories under the head directory,
excluding directory names that match exclusion-list"
; for look, this should be relative to look-pwd
  (unless head-dir (setq head-dir "./"))
  (let ((recursive-dir-list nil)
        lsr-dir)
    (dolist (lsr-dir (directory-files head-dir t) recursive-dir-list)
      (if (and (file-directory-p lsr-dir)
               (not (string-match "^\\.\\.?$" (file-name-nondirectory lsr-dir)))
               (not (catch 'found-one
                      (dolist (exclude-regexp exclusion-list nil)
                        (if (string-match exclude-regexp (file-name-nondirectory lsr-dir))
                            (throw 'found-one t)))))
               )
          (setq recursive-dir-list
                (nconc recursive-dir-list
                       (list lsr-dir)
                       (list-subdirectories-recursively lsr-dir exclusion-list)))
        ) ) 
    recursive-dir-list
    )
  )

(provide 'look-mode)

;;; look-mode.el ends here