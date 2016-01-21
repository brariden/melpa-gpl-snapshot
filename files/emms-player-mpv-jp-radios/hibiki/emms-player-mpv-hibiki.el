;;; emms-player-mpv-hibiki.el --- An emms simple mpv player for HiBiKi Radio Station -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-hibiki.

;; (require 'emms-player-mpv-hibiki)
;; (add-to-list 'emms-player-list 'emms-player-mpv-hibiki)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'later-do)
(require 'emms-streams-hibiki)

(define-emms-simple-player-mpv mpv-hibiki '(streamlist)
  "\\`hibiki://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-hibiki "." t
 'emms-player-mpv-hibiki--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-hibiki 'get-media-title
                 'emms-player-mpv-hibiki--get-media-title)

(defun emms-player-mpv-hibiki--loading-message ()
  "Loading message."
  (message "Loading 響 ... "))

(defun emms-player-mpv-hibiki--track-name-to-input-form (track-name)
  "Return m3u8 link from TRACK-NAME."
  (let ((m3u8 (emms-stream-hibiki-stream-url-to-m3u8 track-name)))
    (later-do 'emms-player-mpv-hibiki--loading-message)
    m3u8))

(defun emms-player-mpv-hibiki--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name(emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))


(define-obsolete-variable-alias 'emms-player-mpv-hibiki--url-request-extra-headers
  'emms-stream-hibiki--url-request-extra-headers "20151128")
(define-obsolete-function-alias 'emms-player-mpv-hibiki--url-retrieve-synchronously
  'emms-stream-hibiki--url-retrieve-synchronously "20151128")
(define-obsolete-function-alias 'emms-player-mpv-hibiki--url-to-json
  'emms-stream-hibiki--url-to-json "20151128")
(define-obsolete-variable-alias 'emms-player-mpv-hibiki--base-url-programs
  'emms-stream-hibiki--base-url-access-programs "20151128")
(define-obsolete-function-alias 'emms-player-mpv-hibiki--get-programs-url
  'emms-stream-hibiki--get-access-programs-url "20151128")
(define-obsolete-variable-alias 'emms-player-mpv-hibiki--base-url-play_check
  'emms-stream-hibiki--base-url-play_check "20151128")
(define-obsolete-function-alias 'emms-player-mpv-hibiki--get-play_check-url
  'emms-stream-hibiki--get-play_check-url "20151128")
(define-obsolete-function-alias 'emms-player-mpv-hibiki--access_id-to-video_id
  'emms-stream-hibiki--access_id-to-video_id "20151128")
(define-obsolete-function-alias 'emms-player-mpv-hibiki--url-to-playlist_url
  'emms-stream-hibiki--url-to-playlist_url "20151128")

(provide 'emms-player-mpv-hibiki)
;;; emms-player-mpv-hibiki.el ends here
