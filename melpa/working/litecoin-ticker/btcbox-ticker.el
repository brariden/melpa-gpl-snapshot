;;; btcbox-ticker --- show japanese bitcoin price on btcbox

;; Copyright (C) 2016  Zhe Lei.

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


;;; Code:

(defvar btcbox-url "https://www.btcbox.co.jp/api/v1/ticker/")

(defvar btcbox-ticker-mode-line nil)

(defvar btcbox-ticker-timer nil)

(defun btcbox-ticker-poll-info ()
  (let (json info sell-price buy-price)
    (with-current-buffer (url-retrieve-synchronously btcbox-url t)
      (goto-char (point-min))
      (re-search-forward "gzip" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max))))
    (setq info (json-read-from-string json))
    (setq sell-price (assoc-default 'sell info))
    (setq buy-price (assoc-default 'buy info))
    (setq btcbox-ticker-mode-line
          (format " [Sell:¥%s Buy:%s]" sell-price buy-price))
    (list sell-price buy-price)))

(define-minor-mode btcbox-ticker-mode
  "Minor mode to display the last btcbox price"
  :init-value nil
  :global t
  :lighter btcbox-ticker-mode-line
  (if btcbox-ticker-mode
      (setq btcbox-ticker-timer
	    (run-at-time "0 sec" 10 #'btcbox-ticker-poll-info))
    (cancel-timer btcbox-ticker-timer)
    (setq btcbox-ticker-timer nil)))

(provide 'btcbox-ticker)

