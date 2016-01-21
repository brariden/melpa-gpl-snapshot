;;; tc-pre.el --- preliminary part for T-Code package -*-emacs-lisp-*-

;; Copyright (C) 2002 KITAJIMA Akira.

;; Author: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Created: 16 Aug 2002
;; Version: $Id: tc-pre-base.in,v 2.2 2002/11/12 09:40:38 kitajima Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

;;; Code:

(defconst tcode-emacs-version
  (cond ((string-match "XEmacs" emacs-version)
	 'xemacs)
	((and (boundp 'mule-version)
	      (>= (string-to-number mule-version) 4))
	 'mule-4)
	((and (boundp 'mule-version)
	      (= (string-to-number mule-version) 3))
	 'mule-3)
	((numberp (string-match "^19" emacs-version))
	 'mule-2)
	((featurep 'mule)
	 'mule-1)
	(t
	 (or (boundp 'emacs-major-version)
	     (setq emacs-major-version 18))
	 'nemacs))
  "日本語Emacsのタイプ。
nemacs, mule-1, mule-2, mule-3, mule-4, xemacsのいずれか。")

(defconst tcode-isearch-type 
  (cond ((eq tcode-emacs-version 'nemacs)
	 'tc-is18)
	((memq tcode-emacs-version '(mule-1 mule-2 mule-3 xemacs))
	 'tc-is19)
	((numberp (string-match "^24\.[34]" emacs-version))
	 'tc-is243)
	((numberp (string-match "^2[234]" emacs-version))
	 'tc-is22)
	(t
	 'tc-is20))
  "isearchで用いるTコード用モジュールのタイプ。")

(defmacro tcode-xemacs-p ()
  (list 'eq 'tcode-emacs-version (list 'quote 'xemacs)))

(defmacro tcode-mule-4-p ()
  (list 'eq 'tcode-emacs-version (list 'quote 'mule-4)))

(defmacro tcode-mule-3-p ()
  (list 'eq 'tcode-emacs-version (list 'quote 'mule-3)))

(defmacro tcode-mule-2-p ()
  (list 'eq 'tcode-emacs-version (list 'quote 'mule-2)))

(defmacro tcode-mule-1-p ()
  (list 'eq 'tcode-emacs-version (list 'quote 'mule-1)))

(defmacro tcode-nemacs-p ()
  (list 'eq 'tcode-emacs-version (list 'quote 'nemacs)))

(defvar tcode-load-immediate nil
  "nil でないとき、Tコード用のすべてのモジュールを一度にロードする。")

(defvar tcode-init-file-name "~/.tc"
  "tc-setup.el を読み出すときに読まれる個人用設定ファイルの名前。
ファイル名を変更したい場合は tc-setup.el を読み出す前にこの値を
設定しなければならない。")

(defvar tcode-use-isearch nil
  "nil でないとき、Tコードを使用できるようにisearchを拡張する。")

(defvar tcode-use-as-default-input-method nil
  "nil でないとき、Tコードをデフォールトのinput methodにする。")

(defvar tcode-default-input-method "japanese-T-Code"
  "Tコード入力環境でのデフォールトのinput method。
変数`tcode-package-name-alist'に登録されている名前が指定できる。")

(defvar tcode-data-directory nil
  "Tコードの各種データファイルを置くためのディレクトリ。
末尾に\"/\"を付けなければならない。")

;;;; Version

(defconst tcode-version "2.3.1")

(defun tcode-version ()
  "Tコード入力環境のバージョンを表示する。"
  (interactive)
  (if (called-interactively-p 'interactive)
      (message (concat "T-Code input environment version "
		       (tcode-version)
		       (if (tcode-xemacs-p)
			   " on XEmacs "
			 " on Emacs ")
		       emacs-version
		       (if (boundp 'nemacs-version)
			   (concat "/NEmacs " nemacs-version))
		       (if (boundp 'mule-version)
			   (concat "/Mule " mule-version))))
    tcode-version))

;;;; package
(defvar tcode-package-name-alist
  '(("japanese-T-Code" . "tc-tbl")
    ("japanese-TT-Code" . "ttc-tbl")
    ("japanese-Try-Code" . "try-tbl")
    ("japanese-TUT-Code" . "tutc-tbl"))
  "名前とテーブル名との対応")

;;;; site information

(require 'find-func)
(defconst tcode-site-data-directory (expand-file-name "../tcode" (find-library-name "tc-pre")))

(provide 'tc-pre)

;;; tc-pre.el ends here
