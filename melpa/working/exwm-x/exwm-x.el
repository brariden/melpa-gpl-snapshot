;;; exwm-x.el --- Addition tools for exwm (emacs x window manager)

;; * Header
;; Copyright 2016 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5")(exwm "0.1")(switch-window "0.10")(dmenu "0.1")(start-menu "0.1"))
;; Keywords: window-manager, exwm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; * Exwm-X                                                         :README:doc:

;; ** What is Exwm-X

;; Exwm-X is an extension of exwm (emacs x window manager), which can
;; make exwm easier for Mouse-Control-People to use.

;; ** Feature
;; 1. Shortcuts, toolbar and other window operate buttons in mode-line.
;; 2. Move or resize a floating-window without press WIN key.
;; 3. Jump-or-exec, which will switch to an exist app instead of launch it again.

;; ** Pictures and videos
;; 1. Tilling windows

;;    [[./snapshots/tilling-window.png]]

;; 2. Floating windows

;;    [[./snapshots/floating-window.png]]

;; 3. Exwm-X videos

;;    [[https://github.com/tumashu/exwm-x-videos]]


;; ** Install
;; 1. Config melpa repository, please see：http://melpa.org/#/getting-started
;; 2. M-x package-install RET exwm-x RET

;; ** Configure

;; *** Edit "~/.initrc" file or "~/.xsession" file
;; You should edit "~/.initrc" file or "~/.xsession" file like below example:

;; #+BEGIN_EXAMPLE
;; # The below line make sure "exwm-x-example" package correct loaded,
;; # don't delete!
;; export exwm_x_enable="yes"

;; # Emacs X input method (exim) setting
;; # export XMODIFIERS=@im=exim
;; # export GTK_IM_MODULE=xim
;; # export QT_IM_MODULE=xim
;; # export CLUTTER_IM_MODULE=xim

;; # xhost +

;; exec dbus-launch --exit-with-session emacs
;; #+END_EXAMPLE

;; *** Edit emacs configure
;; Add the below two lines to your emacs configure file:

;; #+BEGIN_EXAMPLE
;; (require 'exwm-x)
;; (require 'exwm-x-example) ;; Adjust this line.
;; #+END_EXAMPLE

;; Note: Package "exwm-x-example" is Exwm-X buildin example, user can use it to test Exwm-X's
;; features. If it doesn't suit for your need, just copy and paste its useful pieces
;; to your own exwm config :-)


;;; Code:

;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(require 'exwm-x-core)
(require 'exwm-x-modeline)
(require 'exwm-x-utils)
(require 'exwm-x-mouse)
(require 'exwm-x-move-border)
(require 'exwm-x-debian)
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'exwm-x)

;;; exwm-x.el ends here
;; #+END_SRC
