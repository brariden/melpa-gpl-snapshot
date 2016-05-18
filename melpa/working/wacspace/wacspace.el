;;; wacspace.el --- The WACky WorkSPACE manager for emACS -*- lexical-binding: t -*-

;; Copyright © 2013-2014 Emanuel Evans

;; Author: Emanuel Evans <emanuel.evans@gmail.com>
;; URL: http://github.com/shosti/wacspace.el
;; Version: 0.4.2
;; Created: 26 March 2013
;; Keywords: workspace
;; Package-Requires: ((emacs "24") (dash "1.2.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides context-aware workspace management for Emacs.  See
;; http://github.com/shosti/wacspace.el for full documentation.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'wacs-util)
(require 'wacs-configuration)
(require 'wacs-interactive)
(require 'wacs-helper)
(require 'wacs-init)

(provide 'wacspace)

;;; wacspace.el ends here
