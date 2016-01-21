;;; addressbook-bookmark.el --- An address book based on Standard Emacs bookmarks.

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Copyright (C) 2009~2014 Thierry Volpiatto, all rights reserved.
;; X-URL: https://github.com/thierryvolpiatto/addressbook-bookmark

;; Compatibility: GNU Emacs 24.1+
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)
(require 'derived)
(require 'bookmark)
(require 'message)
(eval-when-compile (require 'gnus-sum))

(declare-function mu4e-message-at-point "ext:mu4e-message.el")

(defgroup addressbook-bookmark nil
  "An addressbook linked to bookmarks."
  :prefix "addressbook-"
  :group 'bookmark)

;;; User variables.

(defcustom addressbook-separator
  (propertize (make-string 45 ?-) 'face 'abook-separator)
  "*String used to separate contacts in addressbook buffer."
  :group 'addressbook-bookmark
  :type 'string)

(defcustom addressbook-align-image nil
  "If true, images will be padded to the margin"
  :group 'addressbook-bookmark
  :type 'boolean)

;;; Faces
(defface abook-separator
    '((t (:foreground "red")))
  "*Face used for lines separating addressbook entries."
  :group 'addressbook)

;;; Mode Map.
(defvar addressbook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")       'addressbook-quit)
    (define-key map (kbd "m")       'addressbook-set-mail-buffer)
    (define-key map (kbd "e")       'addressbook-edit)
    (define-key map (kbd "C-c C-c") 'addressbook-set-mail-buffer)
    (define-key map (kbd "C-c f c") 'addressbook-set-mail-buffer-and-cc)
    (define-key map (kbd "r")       'addressbook-bookmark-set)
    (define-key map (kbd "C-c g m") 'addressbook-google-map)
    map))

(define-derived-mode addressbook-mode
    special-mode "addressbook"
    "Interface for addressbook.

Special commands:
\\{addressbook-mode-map}"
    (set (make-local-variable 'revert-buffer-function)
         #'addressbook-mode-revert))

(defun addressbook-mode-revert (&optional revert-auto no-confirm)
  (interactive)
  (let ((inhibit-read-only t)
        (cur-name (car (addressbook-get-contact-data)))
        (name-list (save-excursion
                     (goto-char (point-min))
                     (cl-loop while (re-search-forward "^Name:" nil t)
                           collect (car (addressbook-get-contact-data))))))
    (erase-buffer)
    (cl-loop for name in name-list
          do (save-excursion (addressbook-pp-info name t)))
    (goto-char (point-min))
    (search-forward cur-name nil t) (forward-line 0)))

(defun addressbook-quit ()
  "Quit addressbook buffer."
  (interactive)
  (with-current-buffer "*addressbook*"
    (quit-window)))

(defun addressbook-set-mail-buffer-1 (&optional bookmark-name append cc)
  "Setup a mail buffer with BOOKMARK-NAME email using `message-mode'."
  (bookmark-maybe-load-default-file)
  (let ((mail-list ())
        (mail-bufs (message-buffers)))
    (setq mail-list
          (if (eq major-mode 'addressbook-mode)
                (split-string
                 (assoc-default
                  'email (addressbook-get-contact-data)) " ?, ?")
              (split-string
               (assoc-default
                'email (assoc bookmark-name bookmark-alist)) " ?, ?")))
    (cond ((and (or cc append) mail-bufs) ; A mail buffer exists, use it.
           (pop-to-buffer
            (if (and mail-bufs (> (length mail-bufs) 1))
                (completing-read "MailBuffer: " mail-bufs nil t)
                (car mail-bufs))))
          ((or cc append)                 ; No mail buffer found create one.
           (compose-mail nil nil nil nil 'switch-to-buffer-other-window))
          (t                              ; create a new mail buffer.
           (compose-mail nil nil nil nil 'switch-to-buffer-other-window)))
    (goto-char (point-min))
    (save-excursion
      (if cc
          (if (eq cc 'bcc) (message-goto-bcc) (message-goto-cc))
          (or (search-forward "To: " nil t)
              (search-forward "Newsgroups: " nil t)))
      (end-of-line)
      (let ((email (if (> (length mail-list) 1)
                       (completing-read "Choose mail: " mail-list nil t)
                       (car mail-list))))
        (if append
            (progn
              (message-next-header)
              (forward-line -1)
              (end-of-line)
              (insert (concat ",\n    " email)))
            (insert email))))
    (search-forward "Subject: ")))

(defun addressbook-set-mail-buffer (append)
  "Prepare email buffer with `message-mode' from addressbook buffer."
  (interactive "P")
  (addressbook-set-mail-buffer-1 nil append))

(defun addressbook-set-mail-buffer-and-cc (append)
  "Add a cc field to a mail buffer for this bookmark."
  (interactive "P")
  (addressbook-set-mail-buffer-1 nil append 'cc))

;;; Completion in message buffer with TAB.
;;
;;;###autoload
(defun addressbook-turn-on-mail-completion ()
  (bookmark-maybe-load-default-file)
  (setq message-tab-body-function nil)
  (setq message-completion-alist
        `((,message-newgroups-header-regexp . message-expand-group)
          ("^\\(Newsgroups\\|Followup-To\\|Posted-To\\|Gcc\\):"
           . addressbook-message-complete)
          ("^\\(Resent-\\)?\\(To\\|B?Cc\\):"
           . addressbook-message-complete)
          ("^\\(Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):"
           . addressbook-message-complete)
          ("^\\(Disposition-Notification-To\\|Return-Receipt-To\\):"
           . addressbook-message-complete))))

(defun addressbook-bookmark-addressbook-p (bookmark)
  (if (listp bookmark)
      (string= (assoc-default 'type bookmark) "addressbook")
      (string= (assoc-default
                'type (assoc bookmark bookmark-alist)) "addressbook")))

(defun addressbook-alist-only ()
  (cl-loop for b in bookmark-alist
           when (addressbook-bookmark-addressbook-p b)
           collect b))

(defun addressbook-message-complete ()
  "Provide addressbook completion for `message-mode'."
  (let* ((ls        (addressbook-alist-only))
         (names     (cl-loop for l in ls collect (car l)))
         (alist     (cl-loop for m in ls collect
                          (cons (car m) (assoc-default 'email m))))
         (cand      (completing-read "Name: " names nil t
                                     (thing-at-point 'symbol)))
         (mail-list (split-string (assoc-default cand alist) " ?, ?")))
    (end-of-line)
    (while (not (looking-back ": \\|," (point-at-bol))) (delete-char -1))
    (insert (if (> (length mail-list) 1) ; Contact have more than one address.
                (completing-read "Address: " mail-list nil t)
                (car mail-list)))
    (goto-char (point-min)) (search-forward "Subject: " nil t)))

(defun addressbook-bookmark-make-entry (name email phone
                                        web street city state zipcode country note image-path)
  "Build an addressbook bookmark entry."
  `(,name
    ,@(bookmark-make-record-default 'no-file 'no-context 0)
    (type . "addressbook")
    (location . "Addressbook entry")
    (image . ,image-path)
    (email . ,email)
    (phone . ,phone)
    (web . ,web)
    (street . ,street)
    (city . ,city)
    (state . ,state)
    (zipcode . ,zipcode)
    (country . ,country)
    (note . ,note)
    (handler . addressbook-bookmark-jump)))

(defun addressbook-read-name (prompt)
  "Prompt as many time PROMPT is not empty."
  (let ((var ()))
    (cl-labels ((multiread ()
                  (let ((str (read-string prompt))
                        (sep (if (> (length var) 1) ", " "")))
                    (if (string= str "")
                        (mapconcat 'identity (nreverse var) sep)
                        (push str var)
                        (multiread)))))
      (multiread))))

;;;###autoload
(defun addressbook-bookmark-set ()
  "Record addressbook bookmark entries interactively."
  (interactive)
  (let ((count 0))
    (cl-labels
        ((record ()
           (let ((name       (read-string "Name: "))
                 (email      (addressbook-read-name "Mail: "))
                 (phone      (addressbook-read-name "Phone: "))
                 (web        (addressbook-read-name "Web: "))
                 (street     (read-string "Street: "))
                 (city       (read-string "City: "))
                 (state      (read-string "State: "))
                 (zipcode    (read-string "Zipcode: "))
                 (country    (read-string "Country: "))
                 (note       (read-string "Note: "))
                 (image-path (read-string "Image path: ")))
               
             (bookmark-maybe-load-default-file)
             (let ((old-entry (assoc name bookmark-alist))
                   (new-entry (addressbook-bookmark-make-entry
                               name email phone web street city state zipcode country note image-path))) 
               (if (and old-entry
                        (string= (assoc-default 'type old-entry) "addressbook"))
                   (setf (cdr old-entry)
                         (cdr (addressbook-bookmark-make-entry
                               name email phone web street city state zipcode country note image-path)))
                   (push new-entry bookmark-alist)))
             (bookmark-bmenu-surreptitiously-rebuild-list)
             (addressbook-maybe-save-bookmark)
             (cl-incf count)
             (if (y-or-n-p (format "`%s' Recorded. Add a new contact? " name))
                 (record)
                 (message "%d Contact(s) added." count)))))
      (record))))

(defun addressbook-maybe-save-bookmark ()
  "Increment save counter and maybe save `bookmark-alist'."
  (setq bookmark-alist-modification-count (1+ bookmark-alist-modification-count))
  (when (bookmark-time-to-save-p) (bookmark-save)))

(defun addressbook--bookmark-from-mail (data)
  "Record an addressbook bookmark from a mail buffer."
  (let* ((name (read-string "Name: "
                            (when (and data (string-match "^.* \<" data))
                              (replace-regexp-in-string
                               " \<\\|\>" "" (match-string 0 data)))))
         (mail (read-string "Email: "
                            (if (and data (string-match "\<.*\>$" data))
                                (replace-regexp-in-string
                                 "\<\\|\>" "" (match-string 0 data))
                                data)))
         (old-entry (assoc name bookmark-alist))
         (new-entry (addressbook-bookmark-make-entry
                     name mail "" "" "" "" "" "" "" "" "")))
    (when data
      (if (and old-entry
               (string= (assoc-default 'type old-entry) "addressbook"))
          (let* ((old-mail-ls (split-string (assoc-default 'email old-entry) " ?, ?"))
                 (new-mail-ls (if (member mail old-mail-ls)
                                  (append (list mail) old-mail-ls)
                                  (list mail)))
                 (mail-str (mapconcat 'identity new-mail-ls ", ")))
            (setq new-entry (addressbook-bookmark-make-entry
                             name mail-str "" "" "" "" "" "" "" "" ""))
            (setf (cdr old-entry)
                  (cdr new-entry)))
          (push new-entry bookmark-alist))
      (message "`%s' bookmarked" name)
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (addressbook-maybe-save-bookmark))))

;;;###autoload
(defun addressbook-gnus-sum-bookmark ()
  "Record an addressbook bookmark from a gnus summary buffer."
  (interactive)
  (addressbook--bookmark-from-mail (aref (gnus-summary-article-header
                                         (cdr gnus-article-current)) 2)))

(defun addressbook-get-mu4e-from-field ()
  "Return from field contents from a mu4e buffer."
  (let* ((msg  (mu4e-message-at-point))
         (from (plist-get msg :from)))
    (format "%s <%s>" (caar from) (cdar from))))

;;;###autoload
(defun addressbook-mu4e-bookmark ()
  (interactive)
  (addressbook--bookmark-from-mail
   (addressbook-get-mu4e-from-field)))

(defun addressbook-bookmark-edit (bookmark)
  "Edit an addressbook bookmark entry."
  (let* ((old-name       (car bookmark))
         (old-mail       (assoc-default 'email bookmark))
         (old-phone      (assoc-default 'phone bookmark))
         (old-web        (assoc-default 'web bookmark))
         (old-street     (assoc-default 'street bookmark))
         (old-zipcode    (assoc-default 'zipcode bookmark))
         (old-city       (assoc-default 'city bookmark))
         (old-state      (assoc-default 'state bookmark))
         (old-country    (assoc-default 'country bookmark))
         (old-note       (assoc-default 'note bookmark))
         (old-image-path (assoc-default 'image bookmark))
         (name           (read-string "Name: " old-name))
         (mail           (read-string "Mail: " old-mail))
         (phone          (read-string "Phone: " old-phone))
         (web            (read-string "Web: " old-web))
         (street         (read-string "Street: " old-street))
         (city           (read-string "City: " old-city))
         (state          (read-string "State: " old-state))
         (zipcode        (read-string "Zipcode: " old-zipcode))
         (country        (read-string "Country: " old-country))
         (note           (read-string "Note: " old-note))
         (image-path     (read-string "Image path: " old-image-path))
         (new-entry      (addressbook-bookmark-make-entry
                          name mail phone web street city state
                          zipcode country note image-path)))
    (when (y-or-n-p "Save changes? ")
      (setcar bookmark name)
      (setcdr bookmark (cdr new-entry))
      (addressbook-maybe-save-bookmark)
      new-entry)))

(defun addressbook-edit ()
  "Edit contact from addressbook buffer."
  (interactive)
  (let ((bmk (addressbook-get-contact-data))
        data beg end
        (inhibit-read-only t))
    (setq data (addressbook-bookmark-edit bmk))
    (when data
      (save-excursion
        (addressbook--goto-name)
        (setq beg (point)
              end (+ beg 5))
        (set-text-properties beg end `(name ,(car data))))
      (revert-buffer))))

;;;###autoload
(defun addressbook-bmenu-edit ()
  "Edit an addresbook bookmark entry from bmenu list."
  (interactive)
  (let* ((name      (bookmark-bmenu-bookmark))
         (bmk       (assoc name bookmark-alist))
         (new-entry (addressbook-bookmark-edit bmk)))
    (when new-entry
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (goto-char (point-min))
      (while (not (string= (car new-entry) (bookmark-bmenu-bookmark)))
        (forward-line 1))
      (forward-line 0)
      (bookmark-bmenu-ensure-position))))

(defun addressbook-pp-info (name &optional append)
  "Print addressbook entries to an addressbook buffer."
  (bookmark-maybe-load-default-file)
  (let* ((data              (assoc name bookmark-alist))
         (buf               (get-buffer-create "*addressbook*"))
         (mail              (assoc-default 'email data))
         (phone             (assoc-default 'phone data))
         (web               (assoc-default 'web data))
         (street            (assoc-default 'street data))
         (zipcode           (assoc-default 'zipcode data))
         (state             (assoc-default 'state data))
         (country           (assoc-default 'country data))
         (note              (assoc-default 'note data))
         (city              (assoc-default 'city data))
         (image-path        (assoc-default 'image data))
         (image             (unless (or (not image-path)
                                        (string= image-path "")
                                        (not (file-exists-p image-path)))
                              (create-image image-path)))
         (inhibit-read-only t))
    (set-buffer buf)
    (unless (search-forward addressbook-separator nil t)
      ;; Fixme what is (getenv "USER") on windows system?
      (let ((user (or (getenv "USER") "Unknown user")))
        (insert (propertize (format "Addressbook %s" user)
                            'face '((:foreground "green" :underline t)))
                "\n\n" addressbook-separator "\n")))
    (if append
        (goto-char (point-max))
        (goto-char (point-min))
        (search-forward addressbook-separator)
        (forward-line 1) (delete-region (point) (point-max)))
    ;; Dont append entry if already there.
    (unless (save-excursion (goto-char (point-min)) (search-forward name nil t))
      (insert (concat (propertize "Name:"
                                  'face '((:underline t))
                                  'name name)
                      "    " name))
      (when image
        (if addressbook-align-image
            (let* ((imgwidth (ceiling (car (image-size image))))
                   (padwidth (- (frame-width) imgwidth (current-column))))
              (if (> padwidth 0)
                  (insert (make-string padwidth ?\s)))))
        (insert-image image))
      (insert "\n"
              (if (string= mail "") ""
                  (concat (propertize "Mail:" 'face '((:underline t)))
                          "    " mail "\n"))
              (if (string= phone "") ""
                  (concat (propertize "Phone:" 'face '((:underline t)))
                          "   " phone "\n"))
              (if (string= web "") ""
                  (concat (propertize "Web:" 'face '((:underline t)))
                          "     " web "\n"))
              (if (string= street "") ""
                  (concat (propertize "Street:" 'face '((:underline t)))
                          "  " street "\n"))
              (if (string= city "") ""
                  (concat (propertize "City:" 'face '((:underline t)))
                          "    " city "\n"))
              (if (string= state "") ""
                  (concat (propertize "State:" 'face '((:underline t)))
                          "   " state "\n"))
              (if (string= zipcode "") ""
                  (concat (propertize "Zipcode:" 'face '((:underline t)))
                          " " zipcode "\n"))
              (if (string= country "") ""
                  (concat (propertize "Country:" 'face '((:underline t)))
                          " " country "\n"))
              (if (string= note "") ""
                  (concat (propertize "Note:" 'face '((:underline t)))
                          "    " note "\n"))
              addressbook-separator "\n")
      (addressbook-mode)
      (setq show-trailing-whitespace nil)
      (setq buffer-read-only t))))

(defun addressbook--goto-name ()
  (search-backward addressbook-separator nil t)
  (search-forward "Name: " nil t)
  (forward-line 0))
  
(defun addressbook-get-contact-data ()
  "Get bookmark entry of contact at point in addressbook buffer."
  (save-excursion
    (addressbook--goto-name)
    (assoc (get-text-property (1+ (point-at-bol)) 'name) bookmark-alist)))

(defun addressbook-google-map (&optional bookmark)
  "Show a google map for this address.
This use `google-maps' you can find here:
http://julien.danjou.info/google-maps-el.html."
  (interactive)
  (if (fboundp 'google-maps)
      (let* ((bmk     (or bookmark (addressbook-get-contact-data)))
             (street  (assoc-default 'street bmk))
             (city    (assoc-default 'city bmk))
             (zipcode (assoc-default 'zipcode bmk))
             (state   (assoc-default 'state bmk))
             (country (assoc-default 'country bmk)))
        (if (not (string= city "")) ; We need at least a city name.
            (google-maps (concat street " " city " " state " " zipcode " " country))
            (message "No address known for this contact")))
      (message "Google maps not available.")))

;;;###autoload
(defun addressbook-bookmark-jump (bookmark)
  "Default handler to jump to an addressbook bookmark."
  (let ((buf (save-window-excursion
               (if current-prefix-arg
                   (addressbook-pp-info (car bookmark) 'append)
                   (addressbook-pp-info (car bookmark)))
               (current-buffer))))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))


(provide 'addressbook-bookmark)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; addressbook-bookmark.el ends here
