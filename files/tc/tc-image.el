;;; tc-image.el --- tc help display using image.

;; Copyright (C) 2001, 2002 YAGI Tatsuya

;; Author: YAGI Tatsuya <yagi@is.titech.ac.jp>
;; Version: $Id: tc-image.el,v 2.5 2002/08/17 01:53:58 kitajima Exp $
;; Maintainer: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

;;; Code:
(or (and (fboundp 'image-type-available-p)
	 (or (image-type-available-p 'xpm)
	     (image-type-available-p 'pbm)))
    (error "You cannot display image."))

(require 'tc-help)

(defvar tc-image-type
  (if (image-type-available-p 'xpm) 'xpm 'pbm))

(defconst tc-image-type-alist
  '((pbm ;; type
     "P4\n%d %d\n";; header
     "" ;; trailer
     tc-image-set-pbm-pixel ;; function
     tc-image-make-blank-data-pbm
     )
    (xpm ;; type
     ;; header
     "/* XPM */
static char * image_name[] = {
\"%d %d 7 1\",
\" \tc None\",
\"-\tc white\",
\"X\tc black\",
\"x\tc grey\",
\"R\tc red\",
\"G\tc green\",
\"B\tc blue\",
"
     "};\n" ;; trailer
     tc-image-set-xpm-pixel ;; function
     tc-image-make-blank-data-xpm
     ))
  "Associative list of button image type data.
First  element is image type.
Second element is image header format.
Third  element is image trailer string.
Fourth element is function to set pixel value.
Fifth  element is function to generate blank image data.
")

(defvar tc-image-button
  '(
    ;; no strokes(top or center)
    ("    "
     "    "
     "    "
     "    ")
    ;; no strokes(side)
    ("    "
     " XX "
     " XX "
     "    ")
    ;; first stroke
    (" XX "
     "XXXX"
     "XXXX"
     " XX ")
    ;; second stroke
    (" XX "
     "X  X"
     "X  X"
     " XX ")
    ;; first stroke and second stroke
    (" XX "
     "XX X"
     "X XX"
     " XX ")
    ;; third stroke
    ("   X"
     "  XX"
     " XXX"
     "XXXX")
    ;; first stroke and third stroke
    ("XXXX"
     "XXXX"
     "XXXX"
     "XXXX")
    ;; second stroke and third stroke
    ("XXXX"
     "X  X"
     "X  X"
     "XXXX")
    ;; first stroke and second stroke and third stroke
    ("XXXX"
     "XX X"
     "X XX"
     "XXXX")
    )
  "List of button images.
1st element is image for no strokes(top or center).
2nd element is image for no strokes(side).
3rd element is image for stroke (1).
4th element is image for stroke (2).
5th element is image for stroke (1 2).
6th element is image for stroke (3).
7th element is image for stroke (1 3).
8th element is image for stroke (2 3).
9th element is image for stroke (1 2 3).
")

(defvar tc-image-margins '(0 0 0 0 0 0)
  "Margin for image of buttons.
List of left-margin, top-margin, right-margin, bottom-margin,
linespace and columnspace.")

(defvar tc-image-columns 5
  "*Number of columns which help image takes in line.")

(defvar tc-image-unit-stroke 3
  "*Number of keys displayed in one image.
Appropriate value is 1, 2 or 3.")

(defvar tc-image-cache nil)

(defvar tc-image-white-chars '(32 ?-)
  "List of characters which are treated as black dots in bitmap image.")


(defun tc-image-set-xpm-pixel (image offset w h x y value)
  (aset image (+ offset (* (+ w 4) y) x 1) value))

(defun tc-image-white-p (char)
  (memq char tc-image-white-chars))

(defun tc-image-set-pbm-pixel (image offset w h x y value)
  (let* ((w2 (1+ (lsh (1- w) -3)))
	 (pos (+ offset (* w2 y) (lsh x -3)))
	 (mask (lsh 1 (- 7 (logand x 7))))
	 (octet (aref image pos)))
    (if (tc-image-white-p value)
	;; clear bit
	(aset image pos (logand (lognot mask) octet))
      ;; set bit
      (aset image pos (logior mask octet)))))

(defun tc-image-make-blank-data-xpm (w h &optional val)
  (mapconcat 'identity
	     (make-list h (concat "\"" (make-string w (or val 32))
				  "\",\n"))
	     ""))

(defun tc-image-make-blank-data-pbm (w h &optional val)
  (make-string (* h (1+ (lsh (1- w) -3)))
	       (if (and val (not (tc-image-white-p val))) 255 0)))

(defun tc-image-button-width ()
  (apply (function max)
	 (mapcar (function length)
		 (apply (function append) tc-image-button))))

(defun tc-image-button-height ()
  (apply (function max)
	 (mapcar (function length)
		 tc-image-button)))

(defun tc-image-make-button-cache (n)
  (let* ((button (nth n tc-image-button))
	 (w (tc-image-button-width))
	 (h (tc-image-button-height))
	 (v (make-vector h nil)))
    (dotimes (i h)
      (aset v i (concat (nth i button)
			(make-string (- w (length (nth i button))) 32))))
    v))

(defun tc-image-make-cache ()
  (let* ((button-width  (tc-image-button-width))
	 (button-height (tc-image-button-height))
	 (linespace     (or (nth 4 tc-image-margins) 0))
	 (columnspace   (or (nth 5 tc-image-margins) 0))
	 v-margins h-margins
	 w h ret)
    (if (numberp linespace)   (setq linespace   (list linespace)))
    (if (numberp columnspace) (setq columnspace (list columnspace)))
    (setq h-margins `(,(or (nth 0 tc-image-margins) 0)
		      ,@(mapcar (lambda (i) (nth (% i (length columnspace))
						 columnspace))
				'(0 1 2 3 4 5 6 7 8))
		      ,(or (nth 2 tc-image-margins) 0))
	  v-margins `(,(or (nth 1 tc-image-margins) 0)
		      ,@(mapcar (lambda (i) (nth (% i (length linespace))
						 linespace))
				'(0 1 2))
		      ,(or (nth 3 tc-image-margins) 0)))
    (setq w (apply '+ (* 10 button-width) h-margins)
	  h (apply '+ (* 4 button-height) v-margins))
    (let* ((alist (assq tc-image-type tc-image-type-alist))
	   (image-header (format (nth 1 alist) w h))
	   (image-trailer (nth 2 alist))
	   (set-func (nth 3 alist))
	   (image (concat image-header
			  (funcall (nth 4 alist) w h)
			  image-trailer)))
      (let ((tc-image-cache
	     (vector w h image (length image-header)
		     set-func
		     (let ((v (make-vector 10 nil))
			   (x 0))
		       (dotimes (i 10)
			 (setq x (+ x (nth i h-margins)))
			 (aset v i x)
			 (setq x (+ x button-width)))
		       v)
		     (let ((v (make-vector 4 nil))
			   (y 0))
		       (dotimes (j 4)
			 (setq y (+ y (nth j v-margins)))
			 (aset v j y)
			 (setq y (+ y button-height)))
		       v)
		     (tc-image-make-button-cache 0)
		     (tc-image-make-button-cache 1)
		     (tc-image-make-button-cache 2)
		     (tc-image-make-button-cache 3)
		     (tc-image-make-button-cache 4)
		     (tc-image-make-button-cache 5)
		     (tc-image-make-button-cache 6)
		     (tc-image-make-button-cache 7)
		     (tc-image-make-button-cache 8)
		     )))
	(dotimes (key 40)
	  (tc-image-set-button image key (if (or (< key 10)
						 (memq (% key 10) '(4 5)))
					     0 1)))
	tc-image-cache
	))))

(defun tc-image-internal-set-button (image offset w h x y type set-func)
  (let* ((button (aref tc-image-cache (+ 7 type)))
	 (bw (length (aref button 0)))
	 (bh (length button))
	 line)
    (dotimes (i bh)
      (setq line (aref button i))
      (dotimes (j bw)
	(funcall set-func image offset w h (+ x j) (+ y i) (aref line j))))))

(defun tc-image-set-button (image key type)
  (tc-image-internal-set-button
   image (aref tc-image-cache 3)
   (aref tc-image-cache 0) (aref tc-image-cache 1)
   (aref (aref tc-image-cache 5) (% key 10))
   (aref (aref tc-image-cache 6) (/ key 10))
   type
   (aref tc-image-cache 4)))

(setq tc-image-cache (tc-image-make-cache))

(defun tc-image-get-key-1 (&optional key1 key2 key3)
  (let ((image (copy-sequence (aref tc-image-cache 2))))
    (when key1
      (if (eq key1 key2)
	  (if (eq key1 key3)
	      ;; key1 == key2 == key3
	      (tc-image-set-button image key1 8)
	    ;; key1 == key2 != key3
	    (tc-image-set-button image key1 4))
	(if (eq key1 key3)
	    ;; key1 == key3 != key2
	    (tc-image-set-button image key1 6)
	  ;; key1 != key2, key1 != key3
	  (tc-image-set-button image key1 2))))
    (when (and key2 (not (eq key1 key2)))
      (if (eq key2 key3)
	  ;; key1 != key2 == key3
	  (tc-image-set-button image key2 7)
	;; key1 != key2 != key3
	(tc-image-set-button image key2 3)))
    (when (and key3 (not (eq key1 key3)) (not (eq key2 key3)))
      ;; key1 != key3, key2 != key3
      (tc-image-set-button image key3 5))
    (let ((s (make-string tc-image-columns ?X)))
      (put-text-property
       0 (length s) 'display
       (create-image image tc-image-type t ':ascent 'center)
       s)
      s)))

(defun tc-image-stroke-to-string (stroke)
  (let ((lis (list tcode-stroke-to-string-opener)))
    (let ((dat (tcode-stroke-prefix-match stroke)))
      (when dat
	(setq stroke (cdr dat))
	(setcdr lis (list (nth 2 (car dat))))))
    (while stroke
      (cond ((<= 40 (car stroke))
	     (setq lis (cons (tcode-key-to-help-string (car stroke)) lis)
		   lis (cons tcode-stroke-to-string-separator lis)
		   stroke (cdr stroke)))
	    ((or (> 2 tc-image-unit-stroke)
		 (and (nth 1 stroke)
		      (<= 40 (nth 1 stroke))))
	     (setq lis (cons (tc-image-get-key-1 (car stroke)) lis)
		   lis (cons tcode-stroke-to-string-separator lis)
		   stroke (cdr stroke)))
	    ((or (> 3 tc-image-unit-stroke)
		 (and (nth 2 stroke)
		      (<= 40 (nth 2 stroke))))
	     (setq lis (cons (tc-image-get-key-1 (car stroke) (nth 1 stroke))
			     lis)
		   lis (cons tcode-stroke-to-string-separator lis)
		   stroke (nthcdr 2 stroke)))
	    (t
	     (setq lis (cons (tc-image-get-key-1 (car stroke) (nth 1 stroke)
						 (nth 2 stroke))
			     lis)
		   lis (cons tcode-stroke-to-string-separator lis)
		   stroke (nthcdr 3 stroke)))))
    (setq lis (cons tcode-stroke-to-string-closer (cdr lis)))
    (apply (function concat) (nreverse lis))))

(provide 'tc-image)
;;; tc-image.el ends here
