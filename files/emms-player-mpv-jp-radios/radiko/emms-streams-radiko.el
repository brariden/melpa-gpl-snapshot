;;; emms-streams-radiko.el --- emms stream list for Radiko -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; This program is free software; you can redistribute it and/or modify
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

;; This provides emms stream list for Radiko.

;; (require 'emms-streams-radiko)

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'url)

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-radiko--playerurl
  "http://radiko.jp/player/swf/player_4.1.0.00.swf")

(defvar emms-stream-radiko--playerfile
  (expand-file-name "radiko_player.swf" temporary-file-directory))

(defvar emms-stream-radiko--keyfile
  (expand-file-name "radiko_authkey.png" temporary-file-directory))

(defvar emms-stream-radiko--url-auth1
  "https://radiko.jp/v2/api/auth1_fms")

(defvar emms-stream-radiko--url-auth2
  "https://radiko.jp/v2/api/auth2_fms")

(defun emms-stream-radiko--wget-playerfile ()
  "Wget player.swf to `emms-stream-radiko--playerfile'."
  (unless (file-exists-p emms-stream-radiko--playerfile)
    (unless (zerop (call-process-shell-command
                    (format "wget -q -O %s %s"
                            emms-stream-radiko--playerfile
                            emms-stream-radiko--playerurl)))
      (error "Failed to wget the player.swf"))))

(defun emms-stream-radiko--write-keydata ()
  "Write keydata from emms-radiko-keyfile."
  (unless (file-exists-p emms-stream-radiko--keyfile)
    (unless (zerop (call-process-shell-command
                    (format "swfextract -b 14 %s -o %s"
                            emms-stream-radiko--playerfile
                            emms-stream-radiko--keyfile)))
      (error "Failed to write the keydata"))))

(defvar emms-stream-radiko--auth-fms-base-headers
  '(("pragma" . "no-cache")
    ("X-Radiko-App" . "pc_1")
    ("X-Radiko-App-Version" . "2.0.1")
    ("X-Radiko-User" . "test-stream")
    ("X-Radiko-Device" . "pc"))
  "Base headers for auth fms.")

(defun emms-stream-radiko--access-auth1-fms ()
  "Return auth1_fms."
  (let* ((url-request-method "POST")
         (url-request-data "\\r\\n")
         (url-request-extra-headers
          emms-stream-radiko--auth-fms-base-headers)
         (buf (url-retrieve-synchronously
               emms-stream-radiko--url-auth1 t)))
    (with-current-buffer buf
      (prog1 (buffer-substring-no-properties
              (point-min) (point-max))
        (kill-buffer buf)))))

(defun emms-stream-radiko--get-auth1-value (key auth1)
  "Return value of KEY from AUTH1."
  (if (string-match (format "%s: " key) auth1)
      (let ((pos (match-end 0)))
        (if (string-match "\n\\|\r\n" auth1 pos)
            (substring auth1 pos (match-beginning 0))
          (error "Failed to get %s value" key)))
    (error "Failed to get %s value" key)))

(defun emms-stream-radiko--get-authtoken (auth1)
  "Return X-Radiko-Authtoken value from  AUTH1."
  (emms-stream-radiko--get-auth1-value "X-Radiko-AuthToken" auth1))

(defun emms-stream-radiko--get-offset (auth1)
  "Return x-radiko-keyoffset value from  AUTH1."
  (emms-stream-radiko--get-auth1-value "X-Radiko-KeyOffset" auth1))

(defun emms-stream-radiko--get-length (auth1)
  "Return x-radiko-keylength value from  AUTH1."
  (emms-stream-radiko--get-auth1-value "X-Radiko-KeyLength" auth1))

(defun emms-stream-radiko--get-partialkey (keyfile auth1)
  "Return partialkey from KEYFILE, AUTH1."
  (car (split-string
        (shell-command-to-string
         (format "dd if=%s bs=1 skip=%s count=%s 2> /dev/null | base64"
                 keyfile
                 (emms-stream-radiko--get-offset auth1)
                 (emms-stream-radiko--get-length auth1))))))

(defun emms-stream-radiko--access-auth2-fms (auth1)
  "Return auth2_fms from AUTH1."
  (let* ((url-request-method "POST")
         (url-request-data "\\r\\n")
         (url-request-extra-headers
          `(("X-Radiko-Authtoken" .
             ,(emms-stream-radiko--get-authtoken auth1))
            ("X-Radiko-Partialkey" .
             ,(emms-stream-radiko--get-partialkey
               emms-stream-radiko--keyfile auth1))
            ,@emms-stream-radiko--auth-fms-base-headers))
         (buf (url-retrieve-synchronously
               emms-stream-radiko--url-auth2 t)))
    (with-current-buffer buf
      (prog1 (buffer-substring-no-properties
              (point-min) (point-max))
        (kill-buffer buf)))))

(defun emms-stream-radiko--get-area-id (auth2)
  "Retrun area-id from AUTH2."
  (if (string-match "^JP[0-9]+[,]" auth2)
      (substring auth2 (match-beginning 0) (1- (match-end 0)))
    (error "Failed to get area-id")))

;; For stream-list

(defvar emms-stream-radiko-stream-list-cache nil)

(defvar emms-stream-radiko--base-url-station-list
  "http://radiko.jp/v2/station/list/")

(defun emms-stream-radiko--get-area-id-url (area-id)
  "Return AREA-ID link."
  (format "%s%s.xml" emms-stream-radiko--base-url-station-list area-id))

(defun emms-stream-radiko--fetch-stream-list (area-id &optional buf)
  "Retrun AREA-ID radiko stream list.
string -> stream-list
\(emms-radiko-wget-radiko-stream-list \"JP13\"\) => stream-list"
  (let ((buf (or buf (url-retrieve-synchronously
                      (emms-stream-radiko--get-area-id-url area-id)))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (cl-loop for station
               in (xml-get-children (libxml-parse-xml-region
                                     (point) (point-max))
                                    'station)
               for id = (car (xml-node-children (car (xml-get-children station 'id))))
               for name = (car (xml-node-children (car (xml-get-children station 'name))))
               collect (list name (format "radiko://%s" id) 1 'streamlist)
               finally do (kill-buffer buf)))))

(defun emms-stream-radiko-fetch-current-area-stream-list (&optional updatep)
  "Return stream-list of the current area."
  (if (and (not updatep) (consp emms-stream-radiko-stream-list-cache))
      emms-stream-radiko-stream-list-cache
    (setq emms-stream-radiko-stream-list-cache
          (progn
            (emms-stream-radiko--wget-playerfile)
            (emms-stream-radiko--write-keydata)
            (emms-stream-radiko--fetch-stream-list
             (emms-stream-radiko--get-area-id
              (emms-stream-radiko--access-auth2-fms
               (emms-stream-radiko--access-auth1-fms))))))))

(defun emms-stream-radiko--wget-playerfile-async (&optional cont)
  "Run CONT with no arguments after doing wget playerfile."
  (if (file-exists-p emms-stream-radiko--playerfile)
      (when (functionp cont) (funcall cont))
    (cl-labels
        ((wget-playerfile-async-filter (proc _)
          (let ((ps (process-status proc)))
            (when (eq ps 'exit)
              (unless (file-exists-p emms-stream-radiko--playerfile)
                (error "Failed to get %s" emms-stream-radiko--playerfile))
              (when (functionp cont) (funcall cont))))))
      (set-process-sentinel
       (start-process "radiko-wget-playerfile-async"
                      nil
                      "wget" "-q" "-O"
                      emms-stream-radiko--playerfile
                      emms-stream-radiko--playerurl)
       #'wget-playerfile-async-filter))))

(defun emms-stream-radiko--write-keydata-async (&optional cont)
  "Run CONT after writing keydata."
  (if (file-exists-p  emms-stream-radiko--keyfile)
      (when (functionp cont) (funcall cont))
    (cl-labels
        ((write-keydate-async-filter (proc _)
          (let ((ps (process-status proc)))
            (when (eq ps 'exit)
              (unless (file-exists-p emms-stream-radiko--keyfile)
                (error "Failed to write %s" emms-stream-radiko--keyfile))
              (when (functionp cont) (funcall cont))))))
     (set-process-sentinel
      (start-process "radiko-write-keydata-async"
                     nil
                     "swfextract" "-b" "14"
                     emms-stream-radiko--playerfile
                     "-o"
                     emms-stream-radiko--keyfile)
      #'write-keydate-async-filter))))

(defun emms-stream-radiko--access-auth1-fms-async (&optional cont)
  "Send auth1 to CONT."
  (let ((url-request-method "POST")
        (url-request-data "\\r\\n")
        (url-request-extra-headers
         emms-stream-radiko--auth-fms-base-headers)
        (auth1 ""))
    (url-retrieve
     emms-stream-radiko--url-auth1
     (lambda (status &rest _)
       (when (memq :error status)
         (error "Failed to get auth1_fms : %s" (cdr status)))
       (setq auth1
             (buffer-substring-no-properties
              (point-min) (point-max)))
       (kill-buffer)
       (when (functionp cont) (funcall cont auth1))))))

(defun emms-stream-radiko--access-auth2-fms-async (auth1 cont)
  "Send auth2 of AUTH1 to CONT."
  (let ((url-request-method "POST")
        (url-request-data "\\r\\n")
        (url-request-extra-headers
         `(("X-Radiko-Authtoken" .
            ,(emms-stream-radiko--get-authtoken auth1))
           ("X-Radiko-Partialkey" .
            ,(emms-stream-radiko--get-partialkey
              emms-stream-radiko--keyfile auth1))
           ,@emms-stream-radiko--auth-fms-base-headers))
        (auth2 ""))
    (url-retrieve
     emms-stream-radiko--url-auth2
     (lambda (status &rest _)
       (when (memq :error status)
         (error "Failed to get auth1_fms : %s" (cdr status)))
       (setq auth2 (buffer-substring-no-properties
                    (point-min) (point-max)))
       (kill-buffer)
       (funcall cont auth2)))))

(defun emms-stream-radiko-update-cache-async-1 (area-id)
  "Helper function for `emms-stream-radiko-update-cache-async'."
  (url-retrieve
   (emms-stream-radiko--get-area-id-url area-id)
   (lambda (status &rest _)
     (when (memq :error status)
       (error "Failed to get radiko station list : %s" (cdr status)))
     (setq emms-stream-radiko-stream-list-cache
           (emms-stream-radiko--fetch-stream-list nil (current-buffer)))
     (unless emms-stream-radiko-stream-list-cache
       (error "Failed to get radiko stream list"))
     (message "Updated radiko stream list cache"))))

;;;###autoload
(defun emms-stream-radiko-update-cache-async ()
  "Update cache asynchronously."
  (cl-labels
      ((fetch-stream-list-async ()
        (emms-stream-radiko--access-auth1-fms-async
         (lambda (auth1)
           (emms-stream-radiko--access-auth2-fms-async
            auth1 (lambda (auth2)
                    (emms-stream-radiko-update-cache-async-1
                     (emms-stream-radiko--get-area-id auth2))))))))
    (emms-stream-radiko--wget-playerfile-async
     (lambda ()
       (emms-stream-radiko--write-keydata-async
        #'fetch-stream-list-async)))))

;;;###autoload
(defun emms-stream-radiko-get-stream-list ()
  "Return new stream-list from cache."
  (cl-copy-list emms-stream-radiko-stream-list-cache))

;;;###autoload
(defun emms-stream-radiko-add-bookmark (&optional updatep)
  "Create radiko bookmarks, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (if (eq updatep -1) (emms-stream-radiko-update-cache-async)
    (let ((buf (get-buffer emms-stream-buffer-name)))
      (unless (buffer-live-p buf)
        (error "%s is not a live buffer" emms-stream-buffer-name))
      (set-buffer buf))
    (let* ((stream-list (emms-stream-radiko-fetch-current-area-stream-list updatep))
           (line       (emms-line-number-at-pos (point)))
           (index      (+ (/ line 2) 1)))
      (dolist (stream stream-list)
        (setq emms-stream-list (emms-stream-insert-at index stream
                                                      emms-stream-list))
        (cl-incf index))
      (emms-stream-redisplay)
      (goto-char (point-min))
      (forward-line (1- line)))))

;; For media player

(defvar emms-stream-radiko--base-url-station-stream
  "http://radiko.jp/v2/station/stream/")

(defun emms-stream-radiko--get-channel-url (channel)
  "Return CHANNEL link."
  (format "%s%s.xml" emms-stream-radiko--base-url-station-stream channel))

(defun emms-stream-radiko--wget-rtmpe (channel)
  "Return stream-url from CHANNEL.
\"channel\"
=> \"rtmpe://f-radiko.smartstream.ne.jp/channel/_definst_/simul-stream.stream\""
  (let ((buf (url-retrieve-synchronously
              (emms-stream-radiko--get-channel-url  channel) t))
        (rtmpe nil))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (let* ((stream-xml (libxml-parse-xml-region (point) (point-max)))
             (item (car (xml-get-children stream-xml 'item))))
        (setq rtmpe (car (xml-node-children item))))
      (kill-buffer buf))
    (if (stringp rtmpe) rtmpe
      (error "Failed to parse stream/%s.xml" channel))))

;;;###autoload
(defun emms-stream-radiko-stream-url-to-rtmpe (stream-url)
  "Return rtmpe from STREAM-URL."
  (emms-stream-radiko--wget-rtmpe
   (replace-regexp-in-string "\\`radiko://" "" stream-url)))

(provide 'emms-streams-radiko)
;;; emms-streams-radiko.el ends here
