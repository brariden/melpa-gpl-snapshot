
;;   This file is part of Bodhi.
;;
;;   Bodhi is free software: you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, either version 3 of the License, or
;;   (at your option) any later version.
;;
;;   Bodhi is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with Bodhi.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
; wikipedia.org/wiki/Dharmachakra
;
; This code performs the common setup.
; This is lighter than other starter kits.
; Also, this is way more mouse-friendly
; There are very good reasons for this.
;
;
; Keymap is not there.
; keymap might be evil-mode, ergoemacs,
; or one of my bodhi implementation.



; ~~~~
; ~~~~ Lighter startup
; See als http://ergoemacs.org/emacs/emacs_make_modern.html

(setq
        inhibit-splash-screen t
        inhibit-startup-message t)

  (setq vc-handled-backends nil)

; ~~~~
; ~~~~ Display

  (global-linum-mode 1) ;; line number
  (column-number-mode t) ;; column number
  (global-visual-line-mode 1) ; wrap line

  (setq frame-title-format '("%b")) ; lighter title


; GUI-friendly, not GUIlty.

  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  ;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

 (setq x-select-enable-clipboard t
       x-select-enable-primary t)


; ~~~~
; ~~~~ Behaviour

  (setq make-backup-files nil
        auto-save-default nil)

  (setq term-buffer-maximum-size 0)

  (defalias 'yes-or-no-p 'y-or-n-p)


; we're in 2014
  (require 'recentf)
  (recentf-mode 1)


; a sentence has a single dot, sorry
; why isn't it default???
(setq sentence-end-double-space nil)

; do not break on real files

(defun dhamma-big-files ()
  "Disable font lock when perf makes it necessary."
  (when (> (buffer-size) (* 1024 1024))
    ;(setq buffer-read-only t)
    ;(buffer-disable-undo)
    (font-lock-mode nil)
    (fundamental-mode)))

(add-hook 'find-file-hooks 'dhamma-big-files)

; ~~~~
; ~~~~ Friends

  (require 'package)
  (add-to-list 'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
   '("marmalade" . "http://marmalade-repo.org/packages/") t)


(provide 'dhammacakka)
