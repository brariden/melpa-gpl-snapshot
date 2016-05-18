;;; docbook-snippets.el --- Yasnippets for DocBook

;; Copyright (C) 2013 Jaromir Hradilek

;; Author: Jaromir Hradilek <jhradilek@gmail.com>
;; URL: https://github.com/jhradilek/emacs-docbook-snippets
;; Keywords: snippets DocBook
;; Package-Requires: ((yasnippet "0.8.0"))

;; Based on the clojure-snippets.el file by Max Penet.

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; DocBook 4.5 yasnippets according to DocBook: The Definitive Guide,
;; version 2.0.17

;;; Code:

(setq docbook-snippets-dir
      (file-name-directory (or (buffer-file-name) load-file-name)))

;;;###autoload
(defun docbook-snippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" docbook-snippets-dir)))
    (add-to-list 'yas-snippet-dirs snip-dir t)
    (yas-load-directory snip-dir)))

;;;###autoload
(eval-after-load 'yasnippet
  '(docbook-snippets-initialize))

(require 'yasnippet)

(provide 'docbook-snippets)

;;; docbook-snippets.el ends here
