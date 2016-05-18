;;; fabric-version-test.el --- Tests for version information

;; Copyright (C) Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; Author   : Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; Commentary:
;; Unit tests for version information

;;; License:

;; This file is NOT part of GNU Emacs.

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


(require 'test-helper)

(ert-deftest fabric-mode-library-version ()
  :expected-result (if (executable-find "cask") :passed :failed)
  ;;  The default directory must end with a slash
  (let* ((cask-version (car (process-lines "cask" "version")))
	 ;;(lib-version (fabric-mode-library-version)))
	 )
    ;;(message "CS mode : %s" lib-version)
    (message "CS Cask version: %s" cask-version)
    ;;(should (string= version (fabric-mode-library-version)))))
    (should (string= "0.2.0" cask-version))))



(provide 'fabric-version-test)

;;; version-test.el ends here
