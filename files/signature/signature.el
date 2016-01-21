;;; signature.el --- Signature Survey

;; Copyright (C) 2014  Peter Stiernström

;; Author: Peter Stiernström <peter@stiernstrom.se>
;; Version: 0.5
;; Package-Requires ((cl-lib "0.5") (s "1.9.0") (f "0.16.2"))
;; Keywords:

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

;; Generate a signature survery from your code.

;;; Code:

;; External dependencies:

(require 'eieio)
(require 'cl)
(require 's)
(require 'f)

;; Parts of signature:

(require 'signature-api)
(require 'signature-markers)
(require 'signature-stack)
(require 'signature-display)
(require 'signature-backend)
(require 'signature-ruby)
(require 'signature-interface)

(provide 'signature)

;;; signature.el ends here
