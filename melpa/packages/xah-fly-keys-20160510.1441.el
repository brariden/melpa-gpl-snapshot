;;; xah-fly-keys.el --- A efficient modal keybinding set minor mode based on ergonomics.

;; Copyright © 2013-2015, by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Version: 4.4.5
;; Package-Version: 20160510.1441
;; Created: 10 Sep 2013
;; Keywords: convenience, emulations, vim, ergoemacs
;; Homepage: http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; xah-fly-keys is a efficient keybinding for emacs. (more efficient than vim)

;; It is a modal mode like vi, but key choices are based on statistics of command call frequency.

;; --------------------------------------------------
;; MANUAL INSTALL
;; put the file xah-fly-keys.el in ~/.emacs.d/lisp/
;; create the dirs if doesn't exist.

;; put the following in your emacs init file:

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'xah-fly-keys)
;; (xah-fly-keys 1)

;; --------------------------------------------------
;; HOW TO USE

;; M-x xah-fly-keys to toggle the mode on/off.

;; Important command/insert mode switch keys:

;; xah-fly-command-mode-activate (press 【home】 or 【Ctrl+8】 or 【F8】)
;; xah-fly-insert-mode-activate  (when in command mode, press letter 【i】 key)

;; When in command mode:
;; 【i】 activates insertion mode
;; 【SPACE】 is a leader key. For example, 【SPACE p】 calls query-replace.

;; The leader key sequence basically replace ALL emacs commands that starts with C-x key.

;; When using xah-fly-keys, you don't need to press Control or Meta, with the following exceptions:

;; C-c for major mode commands.
;; C-g for cancel.
;; C-q for quoted-insert.
;; C-h for getting a list of keys following a prefix/leader key.

;; Leader key

;; All emacs C-x keys have a key sequence. Most commands are 2 to 3 keys. The first key we call it leader key.

;; You NEVER need to press Ctrl+x

;; When in command mode, the 【SPACE】 is a leader key.

;; globally, the leader key is the 【f9】 and 【menu】 key. (on typical PC keyboard, the menu key usually at right side of space bar.)

;; the following stardard keys with Control are supported, when the variable xah-fly-use-control-key is t

;; (when xah-fly-use-control-key
;;     (progn
;;       (define-key xah-fly-key-map (kbd "<C-tab>") 'xah-next-user-buffer)
;;       (define-key xah-fly-key-map (kbd "<C-S-iso-lefttab>") 'xah-previous-user-buffer)
;;       (define-key xah-fly-key-map (kbd "C-v") 'yank)
;;       (define-key xah-fly-key-map (kbd "C-t") 'toggle-input-method)
;;       (define-key xah-fly-key-map (kbd "C-w") 'xah-close-current-buffer)
;;       (define-key xah-fly-key-map (kbd "C-z") 'undo)
;;       (define-key xah-fly-key-map (kbd "C-n") 'xah-new-empty-buffer)
;;       (define-key xah-fly-key-map (kbd "C-o") 'find-file)
;;       (define-key xah-fly-key-map (kbd "C-s") 'save-buffer)
;;       (define-key xah-fly-key-map (kbd "C-S-s") 'write-file)
;;       (define-key xah-fly-key-map (kbd "C-S-t") 'xah-open-last-closed)
;;       (define-key xah-fly-key-map (kbd "C-,") 'flyspell-goto-next-error)
;;       (define-key xah-fly-key-map (kbd "C-+") 'text-scale-increase)
;;       (define-key xah-fly-key-map (kbd "C--") 'text-scale-decrease)
;;       (define-key xah-fly-key-map (kbd "C-0") (lambda () (interactive) (text-scale-set 0)))))

;; That't it.

;; On the Mac, I highly recommend using a app called Sail to set your capslock to send Home. So that it acts as xah-fly-command-mode-activate. You can set capslock or one of the cmd key to Home. See http://xahlee.info/kbd/Mac_OS_X_keymapping_keybinding_tools.html

;; I recommend you clone xah-fly-keys.el, and modify it, and use your modified version. Don't worry about upgrade. (I still make key tweaks every week, for the past 3 years.)

;; If you have a bug, post on github. If you have question, post on xah-fly-keys home page.

;; For detail about design and other info, see home page at
;; http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; If you like this project, Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.


;;; Code:

(require 'dired) ; in emacs
(require 'dired-x) ; in emacs
(require 'ido) ; in emacs


(defvar xah-fly-command-mode-activate-hook nil "Hook for `xah-fly-command-mode-activate'")
(defvar xah-fly-insert-mode-activate-hook nil "Hook for `xah-fly-insert-mode-activate'")

(defvar xah-fly-use-control-key nil "if true, define standard keys for open, close, paste, etc.")
(setq xah-fly-use-control-key t)


;; cursor movement

(defun xah-jump-to-last-local-mark ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in local buffer `mark-ring'.
version 2016-04-04"
  (interactive)
  (set-mark-command t))

(defun xah-forward-block (&optional φn)
  "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
Version 2015-07-06"
  (interactive "p")
  (let ((φn (if (null φn) 1 φn)))
    (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" φn)))

(defun xah-backward-block (&optional φn)
  "Move cursor to previous text block.
See: `xah-forward-block'
Version 2015-07-08"
  (interactive "p")
  (let ((φn (if (null φn) 1 φn))
        (ξi 1))
    (while (<= ξi φn)
      (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq ξi φn)))
      (setq ξi (1+ ξi)))))

(defun xah-beginning-of-line-or-block (&optional φn)
  "Move cursor to beginning of line, or beginning of current or previous text block.
 (a text block is separated by blank lines)"
  (interactive "p")
  (let ((φn (if (null φn) 1 φn)))
    (if (equal φn 1)
        (if (or (equal (point) (line-beginning-position))
                (equal last-command this-command )
                ;; (equal last-command 'xah-end-of-line-or-block )
                )
            (xah-backward-block φn)
          (beginning-of-line))
      (xah-backward-block φn))))

(defun xah-end-of-line-or-block (&optional φn)
  "Move cursor to end of line, or end of current or next text block.
 (a text block is separated by blank lines)"
  (interactive "p")
  (let ((φn (if (null φn) 1 φn)))
    (if (equal φn 1)
        (if (or (equal (point) (line-end-position))
                (equal last-command this-command )
                ;; (equal last-command 'xah-beginning-of-line-or-block )
                )
            (xah-forward-block)
          (end-of-line))
      (progn (xah-forward-block φn)))))

(defvar xah-brackets nil "string of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
;; make xah-left-brackets based on xah-brackets
  (setq xah-left-brackets '())
  (dotimes (x (- (length xah-brackets) 1))
    (when (= (% x 2) 0)
      (push (char-to-string (elt xah-brackets x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes (x (- (length xah-brackets) 1))
    (when (= (% x 2) 1)
      (push (char-to-string (elt xah-brackets x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defvar xah-punctuation-regex nil "a regex string for the purpose of jumping to punctuations in programing modes.")
(setq xah-punctuation-regex "[\\!\?\"'#$%&*+,/:;<=>@^`|~]+")

(defun xah-forward-punct (&optional φn)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `xah-punctuation-regex'"
  (interactive "p")
  (search-forward-regexp xah-punctuation-regex nil t φn))

(defun xah-backward-punct (&optional φn)
  "Move cursor to the previous occurrence of punctuation.
See `xah-forward-punct'"
  (interactive "p")
  (search-backward-regexp xah-punctuation-regex nil t φn))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (search-backward-regexp (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (search-forward-regexp (regexp-opt xah-right-brackets) nil t))

(defun xah-forward-equal-quote ()
  "Move cursor to the next occurrence of 「='」 or 「=\"」, with or without space.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-05-05"
  (interactive)
  (search-forward-regexp "=[ \n]*\\('+\\|\\\"+\\)" nil t))

(defun xah-forward-equal-sign ()
  "Move cursor to the next occurrence of equal sign 「=」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-06-15"
  (interactive)
  (search-forward-regexp "=+" nil t))

(defun xah-backward-equal-sign ()
  "Move cursor to previous occurrence of equal sign 「=」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-06-15"
  (interactive)
  (when (search-backward-regexp "=+" nil t)
    (while (search-backward "=" (- (point) 1) t)
      (left-char))))

(defun xah-forward-comma-sign ()
  "Move cursor to the next occurrence of comma 「,」.
Version 2016-01-19"
  (interactive)
  (search-forward-regexp ",+" nil t))

(defun xah-backward-comma-sign ()
  "Move cursor to previous occurrence of comma sign 「,」.
Version 2016-01-19"
  (interactive)
  (when (search-backward-regexp ",+" nil t)
    (while (search-backward "," (- (point) 1) t)
      (left-char))))

(defun xah-forward-quote ()
  "Move cursor to the next occurrence of ' or \" or `.
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-26"
  (interactive)
  (if (search-forward-regexp "'+\\|`+\\|\\\"+" nil t)
      t
    (progn
      (message "No more quotes after.")
      nil)))

(defun xah-forward-quote-twice ()
  "Call `xah-forward-quote' twice.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-26"
  (interactive)
  (when (xah-forward-quote)
    (xah-forward-quote)))

(defun xah-backward-quote ()
  "Move cursor to the previous occurrence of '' or \" or `.
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-26"
  (interactive)
  (if (search-backward-regexp "'+\\|`+\\|\\\"+" nil t)
      (when (char-before) ; isn't nil, at beginning of buffer
        (while (char-equal (char-before) (char-after))
          (left-char)
          t))
    (progn
      (message "No more quotes before.")
      nil)))

(defun xah-forward-dot-comma ()
  "Move cursor to the next occurrence of 「.」 「,」 「;」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-03-24"
  (interactive)
  (search-forward-regexp "\\.+\\|,+\\|;+" nil t))

(defun xah-backward-dot-comma ()
  "Move cursor to the previous occurrence of 「.」 「,」 「;」
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-03-24"
  (interactive)
  (search-backward-regexp "\\.+\\|,+\\|;+" nil t))

;; (defun goto-point-min ()
;;   "Goto the beginning of buffer.
;; This is different from `beginning-of-buffer'
;; because that marks the previous position."
;;   (interactive)
;;   (goto-char (point-min))
;; )

;; (defun goto-point-max ()
;;   "Goto the end of buffer.
;; This is different from `end-of-buffer'
;; because that marks the previous position."
;;   (interactive)
;;   (goto-char (point-max))
;; )

;; (defun xah-forward-space ()
;;   "Move cursor to the next occurrence of white space."
;;   (interactive)
;;   (re-search-forward "[ \t\n]+" nil t))

;; (defun xah-backward-space ()
;;   "Move cursor to the next occurrence of white space."
;;   (interactive)
;;   ;; (skip-chars-backward "^ \t\n")
;;   ;; (re-search-backward "[ \t\n]+" nil t)
;;   (posix-search-backward "[ \t\n]+" nil t)
;;   )


;; text selection

(defun xah-delete-current-line ()
  "Delete current line."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (when (looking-at "\n")
    (delete-char 1)))

;; (defun xah-copy-line-or-region ()
;;   "Copy current line, or text selection.
;; When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

;; URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
;; Version 2015-05-06"
;;   (interactive)
;;   (let (ξp1 ξp2)
;;     (if current-prefix-arg
;;         (progn (setq ξp1 (point-min))
;;                (setq ξp2 (point-max)))
;;       (progn (if (use-region-p)
;;                  (progn (setq ξp1 (region-beginning))
;;                         (setq ξp2 (region-end)))
;;                (progn (setq ξp1 (line-beginning-position))
;;                       (setq ξp2 (line-end-position))))))
;;     (kill-ring-save ξp1 ξp2)
;;     (if current-prefix-arg
;;         (message "buffer text copied")
;;       (message "text copied"))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-12-30"
  (interactive)
  (let (ξp1 ξp2)
    (if current-prefix-arg
        (setq ξp1 (point-min) ξp2 (point-max))
      (if (use-region-p)
          (setq ξp1 (region-beginning) ξp2 (region-end))
        (setq ξp1 (line-beginning-position) ξp2 (line-end-position))))
    (if (eq last-command this-command)
        (progn
          ;; (push-mark (point) "NOMSG" "ACTIVATE")
          (kill-append "\n" nil)
          (forward-line 1)
          (end-of-line)
          (kill-append (buffer-substring-no-properties (line-beginning-position) (line-end-position)) nil)
          (message "Line copy appended"))
      (progn
        (kill-ring-save ξp1 ξp2)
        (if current-prefix-arg
            (message "Buffer text copied")
          (message "Text copied"))))))

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-all-or-region ()
  "Put the whole buffer content to `kill-ring', or text selection if there's one.
Respects `narrow-to-region'.
URL `http://ergoemacs.org/emacs/emacs_copy_cut_all_or_region.html'
Version 2015-08-22"
  (interactive)
  (if (use-region-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (message "Text selection copied."))
    (progn
      (kill-new (buffer-string))
      (message "Buffer content copied."))))

(defun xah-cut-all-or-region ()
  "Cut the whole buffer content to `kill-ring', or text selection if there's one.
Respects `narrow-to-region'.
URL `http://ergoemacs.org/emacs/emacs_copy_cut_all_or_region.html'
Version 2015-08-22"
  (interactive)
  (if (use-region-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
    (progn
      (kill-new (buffer-string))
      (delete-region (point-min) (point-max)))))

(defun xah-copy-all ()
  "Put the whole buffer content into the `kill-ring'.
(respects `narrow-to-region')
URL `http://ergoemacs.org/emacs/elisp_cut_copy_yank_kill-ring.html'
Version 2015-05-06"
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copied."))

(defun xah-cut-all ()
  "Cut the whole buffer content into the `kill-ring'.
Respects `narrow-to-region'."
  (interactive)
  (kill-new (buffer-string))
  (delete-region (point-min) (point-max)))


;; editing commands

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2016-01-08"
  (interactive)
  (let (
        (deactivate-mark nil)
        ξp1 ξp2)
    (if (use-region-p)
        (setq ξp1 (region-beginning)
              ξp2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]")
        (setq ξp1 (point))
        (skip-chars-forward "[:alnum:]")
        (setq ξp2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region ξp1 ξp2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region ξp1 ξp2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region ξp1 ξp2)
      (put this-command 'state 0)))))

;; test case
;; test_case some
;; test-case
;; tes▮t-case

(defun xah-toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor.
URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2015-12-22"
  (interactive)
  (let ((case-fold-search nil))
    (left-char 1)
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
    (right-char)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one or none.
Call this command again to shrink more. 3 calls will remove all whitespaces.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2015-11-04"
  (interactive)
  (let ((pos0 (point))
        ξline-has-char-p ; current line contains non-white space chars
        ξhas-space-tab-neighbor-p
        ξwhitespace-begin ξwhitespace-end
        ξspace-or-tab-begin ξspace-or-tab-end
        )
    (save-excursion
      (setq ξhas-space-tab-neighbor-p
            (if (or
                 (looking-at " \\|\t")
                 (looking-back " \\|\t" 1))
                t
              nil))
      (beginning-of-line)
      (setq ξline-has-char-p (search-forward-regexp "[[:graph:]]" (line-end-position) t))

      (goto-char pos0)
      (skip-chars-backward "\t ")
      (setq ξspace-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq ξwhitespace-begin (point))

      (goto-char pos0)
      (skip-chars-forward "\t ")
      (setq ξspace-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq ξwhitespace-end (point)))

    (if ξline-has-char-p
        (if ξhas-space-tab-neighbor-p
            (let (ξdeleted-text)
              ;; remove all whitespaces in the range
              (setq ξdeleted-text
                    (delete-and-extract-region ξspace-or-tab-begin ξspace-or-tab-end))
              ;; insert a whitespace only if we have removed something different than a simple whitespace
              (when (not (string= ξdeleted-text " "))
                (insert " ")))

          (progn
            (when (equal (char-before) 10) (delete-char -1))
            (when (equal (char-after) 10) (delete-char 1))))
      (progn (delete-blank-lines)))))

(defun xah-compact-uncompact-block ()
  "Remove or insert newline characters on the current block of text.
This is similar to a toggle for `fill-paragraph' and `unfill-paragraph'.

When there is a text selection, act on the the selection, else, act on a text block separated by blank lines.
Version 2015-06-20"
  (interactive)
  ;; This command symbol has a property “'stateIsCompact-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( ξis-compact-p
         (deactivate-mark nil)
         (ξblanks-regex "\n[ \t]*\n")
         ξp1 ξp2
         )
    (progn
      (if (use-region-p)
          (progn (setq ξp1 (region-beginning))
                 (setq ξp2 (region-end)))
        (save-excursion
          (if (re-search-backward ξblanks-regex nil "NOERROR")
              (progn (re-search-forward ξblanks-regex)
                     (setq ξp1 (point)))
            (setq ξp1 (point)))
          (if (re-search-forward ξblanks-regex nil "NOERROR")
              (progn (re-search-backward ξblanks-regex)
                     (setq ξp2 (point)))
            (setq ξp2 (point))))))
    (save-excursion
      (setq ξis-compact-p
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (progn
                (goto-char ξp1)
                (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil))))
      (if ξis-compact-p
          (fill-region ξp1 ξp2)
        (let ((fill-column most-positive-fixnum)) (fill-region ξp1 ξp2)))
      (put this-command 'stateIsCompact-p (if ξis-compact-p nil t)))))

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2015-11-28"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defun xah-unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2015-11-28"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(defun xah-cycle-hyphen-underscore-space ()
  "Cycle {underscore, space, hypen} chars of current word or text selection.
When called repeatedly, this command cycles the {“_”, “-”, “ ”} characters, in that order.

URL `http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html'
Version 2016-01-14"
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of ξcharArray.
  (let (ξp1 ξp2)
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (save-excursion
        ;; 2016-01-14 not use (bounds-of-thing-at-point 'symbol), because if at end of buffer, it returns nil. also, it's syntax table dependent
        (skip-chars-backward "-_[:alnum:]")
        (setq ξp1 (point))
        (skip-chars-forward "-_[:alnum:]")
        (setq ξp2 (point))))
    (let* ((ξinputText (buffer-substring-no-properties ξp1 ξp2))
           (ξcharArray ["_" "-" " "])
           (ξlength (length ξcharArray))
           (ξregionWasActive-p (region-active-p))
           (ξnowState
            (if (equal last-command this-command )
                (get 'xah-cycle-hyphen-underscore-space 'state)
              0 ))
           (ξchangeTo (elt ξcharArray ξnowState)))
      (save-excursion
        (save-restriction
          (narrow-to-region ξp1 ξp2)
          (goto-char (point-min))
          (while
              (search-forward-regexp
               (concat
                (elt ξcharArray (% (+ ξnowState 1) ξlength))
                "\\|"
                (elt ξcharArray (% (+ ξnowState 2) ξlength)))
               (point-max)
               'NOERROR)
            (replace-match ξchangeTo 'FIXEDCASE 'LITERAL))))
      (when (or (string= ξchangeTo " ") ξregionWasActive-p)
        (goto-char ξp2)
        (set-mark ξp1)
        (setq deactivate-mark nil))
      (put 'xah-cycle-hyphen-underscore-space 'state (% (+ ξnowState 1) ξlength)))))

(defun xah-underscore-to-space-region (φbegin φend)
  "Change  underscore char to space.
URL `http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html'
Version 2015-08-18"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region φbegin φend)
      (goto-char (point-min))
      (while
          (search-forward-regexp "_" (point-max) 'NOERROR)
        (replace-match " " 'FIXEDCASE 'LITERAL)))))

(defun xah-copy-file-path (&optional φdir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2015-12-02"
  (interactive "P")
  (let ((ξfpath
         (if (equal major-mode 'dired-mode)
             (expand-file-name default-directory)
           (if (null (buffer-file-name))
               (user-error "Current buffer is not associated with a file.")
             (buffer-file-name)))))
    (kill-new
     (if (null φdir-path-only-p)
         (progn
           (message "File path copied: 「%s」" ξfpath)
           ξfpath
           )
       (progn
         (message "Directory path copied: 「%s」" (file-name-directory ξfpath))
         (file-name-directory ξfpath))))))

(defun xah-delete-text-block ()
  "Delete the current text block and also put it to `kill-ring'.
Version 2015-12-08"
  (interactive)
  (let (ξp1 ξp2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq ξp1 (point)))
        (setq ξp1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "NOERROR")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq ξp2 (point)))
        (setq ξp2 (point))))
    (kill-region ξp1 ξp2)))

(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (let (ξp1 ξp2)
    (if (region-active-p)
        (progn (setq ξp1 (region-beginning))
               (setq ξp2 (region-end)))
      (progn (setq ξp1 (line-beginning-position))
             (setq ξp2 (line-end-position))))
    (copy-to-register ?1 ξp1 ξp2)
    (message "copied to register 1: 「%s」." (buffer-substring-no-properties ξp1 ξp2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'.
URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun xah-copy-rectangle-to-kill-ring (φbegin φend)
  "Copy region as column (rectangle region) to `kill-ring'

See also: `kill-rectangle', `copy-to-register'.
URL `http://ergoemacs.org/emacs/emacs_copy_rectangle_text_to_clipboard.html'
version 2015-11-16"
  ;; extract-rectangle suggested by YoungFrog, 2012-07-25
  (interactive "r")
  (require 'rect)
  (kill-new
   (with-temp-buffer
     (mapc (lambda (ξx) (insert ξx "\n"))
           (extract-rectangle φbegin φend))
     (delete-char -1)
     (buffer-string))))

(defun xah-upcase-sentence ()
  "Upcase sentence.
TODO 2014-09-30 command incomplete
"
  (interactive)
  (let (p1 p2)

    (if (region-active-p)
        (progn
          (setq p1 (region-beginning))
          (setq p2 (region-end)))
      (progn
        (save-excursion
          (progn
            (if (re-search-backward "\n[ \t]*\n" nil "move")
                (progn (re-search-forward "\n[ \t]*\n")
                       (setq p1 (point)))
              (setq p1 (point)))
            (if (re-search-forward "\n[ \t]*\n" nil "move")
                (progn (re-search-backward "\n[ \t]*\n")
                       (setq p2 (point)))
              (setq p2 (point)))))))

    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)

        (goto-char (point-min))
        (while (search-forward "\. \{1,2\}\\([a-z]\\)" nil t)
nil
;; (replace-match "myReplaceStr2")

)))))

(defun xah-escape-quotes (φbegin φend)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `xah-unescape-quotes'
URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2015-05-04"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
      (save-restriction
        (narrow-to-region φbegin φend)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" 'FIXEDCASE 'LITERAL)))))

(defun xah-unescape-quotes (φbegin φend)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
See also: `xah-escape-quotes'
URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2015-05-04"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region φbegin φend)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" 'FIXEDCASE 'LITERAL)))))

(defun xah-title-case-region-or-line (φbegin φend)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, φbegin φend are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2015-05-07"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           ξp1
           ξp2
           (ξskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward ξskipChars (line-beginning-position))
         (setq ξp1 (point))
         (skip-chars-forward ξskipChars (line-end-position))
         (setq ξp2 (point)))
       (list ξp1 ξp2))))
  (let* (
         (ξstrPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region φbegin φend)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (ξx)
             (goto-char (point-min))
             (while
                 (search-forward (aref ξx 0) nil t)
               (replace-match (aref ξx 1) 'FIXEDCASE 'LITERAL)))
           ξstrPairs))))))


;; insertion commands

(defun xah-insert-date (&optional φadd-time-stamp-p)
  "Insert current date and or time.

• In this format yyyy-mm-dd.
• When called with `universal-argument', insert date and time, e.g. 2012-05-28T07:06:23-07:00
• Replaces text selection.

See also `xah-current-date-time-string'.
version 2016-04-12"
  (interactive "P")
  (when (use-region-p) (delete-region (region-beginning) (region-end)))
  (insert
   (if φadd-time-stamp-p
       (xah-current-date-time-string)
     (format-time-string "%Y-%m-%d"))))

(defun xah-current-date-time-string ()
  "Returns current date-time string in full ISO 8601 format.
Example: 「2012-04-05T21:08:24-07:00」.

Note, for the time zone offset, both the formats 「hhmm」 and 「hh:mm」 are valid ISO 8601. However, Atom Webfeed spec seems to require 「hh:mm」."
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (ξx) (format "%s:%s" (substring ξx 0 3) (substring ξx 3 5))) (format-time-string "%z"))))

(defun xah-insert-bracket-pair (φleft-bracket φright-bracket)
  "Wrap or Insert a matching bracket and place cursor in between.

If there's a text selection, wrap brackets around it. Else, smartly decide wrap or insert. (basically, if there's no char after cursor, just insert bracket pair.)

φleft-bracket ＆ φright-bracket are strings.

URL `http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html'
Version 2015-04-19"
  (if (use-region-p)
      (progn
        (let (
              (ξp1 (region-beginning))
              (ξp2 (region-end)))
          (goto-char ξp2)
          (insert φright-bracket)
          (goto-char ξp1)
          (insert φleft-bracket)
          (goto-char (+ ξp2 2))))
    (progn ; no text selection
      (if
          (or
           (looking-at "[^-_[:alnum:]]")
           (eq (point) (point-max)))
          (progn
            (insert φleft-bracket φright-bracket)
            (search-backward φright-bracket ))
        (progn
          (let (ξp1 ξp2)
            ;; basically, want all alphanumeric, plus hyphen and underscore, but don't want space or punctuations. Also want chinese.
            ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
            (skip-chars-backward "-_[:alnum:]")
            (setq ξp1 (point))
            (skip-chars-forward "-_[:alnum:]")
            (setq ξp2 (point))
            (goto-char ξp2)
            (insert φright-bracket)
            (goto-char ξp1)
            (insert φleft-bracket)
            (goto-char (+ ξp2 (length φleft-bracket)))))))))

;; (insert-parentheses)

(defun xah-insert-paren () (interactive) (xah-insert-bracket-pair "(" ")") )
(defun xah-insert-square-bracket () (interactive) (xah-insert-bracket-pair "[" "]") )
(defun xah-insert-brace () (interactive) (xah-insert-bracket-pair "{" "}") )
(defun xah-insert-greater-less () (interactive) (xah-insert-bracket-pair "<" ">") )

(defun xah-insert-double-curly-quote“” () (interactive) (xah-insert-bracket-pair "“" "”") )
(defun xah-insert-curly-single-quote‘’ () (interactive) (xah-insert-bracket-pair "‘" "’") )
(defun xah-insert-single-angle-quote‹› () (interactive) (xah-insert-bracket-pair "‹" "›") )
(defun xah-insert-double-angle-quote«» () (interactive) (xah-insert-bracket-pair "«" "»") )
(defun xah-insert-ascii-double-quote () (interactive) (xah-insert-bracket-pair "\"" "\"") )
(defun xah-insert-ascii-single-quote () (interactive) (xah-insert-bracket-pair "'" "'") )
(defun xah-insert-emacs-quote () (interactive) (xah-insert-bracket-pair "`" "'") )
(defun xah-insert-corner-bracket「」 () (interactive) (xah-insert-bracket-pair "「" "」") )
(defun xah-insert-white-corner-bracket『』 () (interactive) (xah-insert-bracket-pair "『" "』") )
(defun xah-insert-angle-bracket〈〉 () (interactive) (xah-insert-bracket-pair "〈" "〉") )
(defun xah-insert-double-angle-bracket《》 () (interactive) (xah-insert-bracket-pair "《" "》") )
(defun xah-insert-white-lenticular-bracket〖〗 () (interactive) (xah-insert-bracket-pair "〖" "〗") )
(defun xah-insert-black-lenticular-bracket【】 () (interactive) (xah-insert-bracket-pair "【" "】") )
(defun xah-insert-tortoise-shell-bracket〔〕 () (interactive) (xah-insert-bracket-pair "〔" "〕") )

(defun xah-insert-string-assignment ()
  "Insert space before cursor"
  (interactive)
  (progn (insert "=\"\"")
         (left-char)))

(defun xah-insert-space-before ()
  "Insert space before cursor"
  (interactive)
  (insert " "))

(defun xah-insert-space-after ()
  "Insert space after cursor"
  (interactive)
  (insert " ")
  (left-char))

(defun xah-insert-form-feed ()
  "insert a form feed char (ASCII 12)"
  (interactive)
  (insert ""))

(defun xah-insert-column-counter (n)
  "Insert a sequence of numbers vertically.

 (this command is similar to emacs 24.x's `rectangle-number-lines'.)

For example, if your text is:

a b
c d
e f

and your cursor is after “a”, then calling this function with argument
3 will change it to become:

a1 b
c2 d
e3 f

If there are not enough existing lines after the cursor
when this function is called, it aborts at the last line.

This command is conveniently used together with `kill-rectangle' and `string-rectangle'."
  (interactive "nEnter the max integer: ")
  (let ((i 1) colpos )
    (setq colpos (- (point) (line-beginning-position)))
    (while (<= i n)
      (insert (number-to-string i))
      (forward-line) (beginning-of-line) (forward-char colpos)
      (setq i (1+ i)))))

(defun xah-insert-alphabets-az (&optional φuse-uppercase-p)
  "Insert letters a to z vertically.
If `universal-argument' is called first, use CAPITAL letters.

URL `http://ergoemacs.org/emacs/emacs_insert-alphabets.html'
Version 2015-11-06"
  (interactive "P")
  (let ((startChar (if φuse-uppercase-p 65 97 )))
    (dotimes (ξi 26)
      (insert (format "%c\n" (+ startChar ξi))))))

(defvar xah-unicode-list nil "Associative list of Unicode symbols. First element is a Unicode character, second element is a string used as key shortcut in `ido-completing-read'")
(setq xah-unicode-list
      '(
        ("_" . "underscore" )
        ("•" . ".bullet" )
        ("→" . "an")
        ("◇" . "3" )
        ("◆" . "4" )
        ("¤" . "2" )
        ("…" . "...ellipsis" )
        (" " . "nbsp" )
        ("、" . "," )
        ("⭑" . "9" )
        ("🎶" . "5" )
        ("—" . "-emdash" )
        ("＆" . "7" )
        ("↓" . "at")
        ("←" . "ah")
        ("↑" . "ac")
        ("👍" . "tu")
        ) )

(defun xah-insert-unicode ()
  "Insert a unicode"
  (interactive)
  (let (gotThis)
    (setq gotThis
          (ido-completing-read "insert:" (mapcar (lambda (x) (concat (car x) (cdr x))) xah-unicode-list)))
    (insert (car (assoc (substring gotThis 0 1) xah-unicode-list)))))


;; text selection

(defun xah-select-current-block ()
  "Select the current block of text between blank lines.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-02-07"
  (interactive)
  (let (ξp1 ξp2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq ξp1 (point)))
        (setq ξp1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq ξp2 (point)))
        (setq ξp2 (point))))
    (set-mark ξp1)))

(defun xah-select-current-line ()
  "Select current line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-02-07"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun xah-semnav-up (φarg)
"Called by `xah-extend-selection'.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-11-13.
Written by Nikolaj Schumacher, 2008-10-20. Released under GPL 2"
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> φarg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (setq φarg (1- φarg) ))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (setq φarg (1+ φarg) )))
  (up-list φarg))

(defun xah-extend-selection (φarg &optional φincremental-p)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit.

This command works mostly in lisp syntax.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-11-13.
Written by Nikolaj Schumacher, 2008-10-20. Released under GPL 2."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (or (use-region-p)
             (eq last-command this-command))))
  (if φincremental-p
      (progn
        (xah-semnav-up (- φarg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> φarg 1)
        (xah-extend-selection (1- φarg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command does not properly deal with nested brackets.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-05-16"
  (interactive)
  (let (
        (ξskipChars
         (if (boundp 'xah-brackets)
             (concat "^\"" xah-brackets)
           "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
        ξp1
        ξp2
        )
    (skip-chars-backward ξskipChars)
    (setq ξp1 (point))
    (skip-chars-forward ξskipChars)
    (setq ξp2 (point))
    (set-mark ξp1)))


;; misc

(defvar xah-switch-buffer-ignore-dired t "If t, ignore dired buffer when calling `xah-next-user-buffer' or `xah-previous-user-buffer'")
(setq xah-switch-buffer-ignore-dired t)

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
 “user buffer” is a buffer whose name does not start with “*”.
If `xah-switch-buffer-ignore-dired' is true, also skip directory buffer.
2015-01-05 URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (or
           (string-equal "*" (substring (buffer-name) 0 1))
           (if (string-equal major-mode "dired-mode")
               xah-switch-buffer-ignore-dired
             nil
             ))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
 “user buffer” is a buffer whose name does not start with “*”.
If `xah-switch-buffer-ignore-dired' is true, also skip directory buffer.
2015-01-05 URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (or
           (string-equal "*" (substring (buffer-name) 0 1))
           (if (string-equal major-mode "dired-mode")
               xah-switch-buffer-ignore-dired
             nil
             ))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
 (buffer name that starts with “*”)"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
 (buffer name that starts with “*”)"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))

(defvar xah-recently-closed-buffers nil "alist of recently closed buffers. Each element is (buffer name, file path). The max number to track is controlled by the variable `xah-recently-closed-buffers-max'.")

(defvar xah-recently-closed-buffers-max 40 "The maximum length for `xah-recently-closed-buffers'.")

(defun xah-close-current-buffer ()
  "Close the current buffer.

Similar to `kill-buffer', with the following addition:

• Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• Make sure the buffer shown after closing is a user buffer.
• If the buffer is editing a source file in an org-mode file, prompt the user to save before closing.
• If the buffer is a file, add the path to the list `xah-recently-closed-buffers'.
• If it is the minibuffer, exit the minibuffer

A emacs buffer is one whose name starts with *.
Else it is a user buffer."
  (interactive)
  (let (ξemacs-buff-p
        (ξorg-p (string-match "^*Org Src" (buffer-name))))

    (setq ξemacs-buff-p (if (string-match "^*" (buffer-name)) t nil))

    (if (string= major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit) ; if the buffer is minibuffer
      (progn
        ;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
        (when (and (buffer-modified-p)
                   (not ξemacs-buff-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        (when (and (buffer-modified-p)
                   ξorg-p)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))

        ;; save to a list of closed buffer
        (when (buffer-file-name)
          (setq xah-recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) xah-recently-closed-buffers))
          (when (> (length xah-recently-closed-buffers) xah-recently-closed-buffers-max)
            (setq xah-recently-closed-buffers (butlast xah-recently-closed-buffers 1))))

        ;; close
        (kill-buffer (current-buffer))

        ;; if emacs buffer, switch to a user buffer
        (when (string-match "^*" (buffer-name))
          (next-buffer)
          (let ((i 0))
            (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
              (setq i (1+ i)) (next-buffer))))))))

(defun xah-open-last-closed ()
  "Open the last closed file."
  (interactive)
  (if (> (length xah-recently-closed-buffers) 0)
      (find-file (cdr (pop xah-recently-closed-buffers)))
    (progn (message "No recently close buffer in this session."))))

(defun xah-open-recently-closed ()
  "Open recently closed file.
Prompt for a choice."
  (interactive)
  (find-file (ido-completing-read "open:" (mapcar (lambda (f) (cdr f)) xah-recently-closed-buffers))))

(defun xah-list-recently-closed ()
  "List recently closed file."
  (interactive)
  (let ((ξbuf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer ξbuf)
    (mapc (lambda (ξf) (insert (cdr ξf) "\n"))
          xah-recently-closed-buffers)))

(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2015-06-12"
  (interactive)
  (let ((ξbuf (generate-new-buffer "untitled")))
    (switch-to-buffer ξbuf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))

;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.



(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2016-01-28"
  (interactive)
  (let (
         (ξsuffix-map
          ;; (‹extension› . ‹shell program name›)
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("go" . "go run")
            ("js" . "node") ; node.js
            ("sh" . "bash")
            ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
            ("rkt" . "racket")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("tex" . "pdflatex")
            ("latex" . "pdflatex")
            ("java" . "javac")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            ))

         ξfname
         ξfSuffix
         ξprog-name
         ξcmd-str)

    (when (null (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))

    (setq ξfname (buffer-file-name))
    (setq ξfSuffix (file-name-extension ξfname))
    (setq ξprog-name (cdr (assoc ξfSuffix ξsuffix-map)))
    (setq ξcmd-str (concat ξprog-name " \""   ξfname "\""))

    (cond
     ((string-equal ξfSuffix "el") (load ξfname))
     ((string-equal ξfSuffix "java")
      (progn
        (shell-command ξcmd-str "*xah-run-current-file output*" )
        (shell-command
         (format "java %s" (file-name-sans-extension (file-name-nondirectory ξfname))))))
     (t (if ξprog-name
            (progn
              (message "Running…")
              (shell-command ξcmd-str "*xah-run-current-file output*" ))
          (message "No recognized program file suffix for this file."))))))

(defun xah-clean-whitespace-and-save (φbegin φend)
  "Delete trailing whitespace, and replace repeated blank lines into just 2.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.
Saves the file if it is a file.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2016-03-02"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (save-restriction
      (narrow-to-region φbegin φend)
      (progn
        (goto-char (point-min))
        (while (search-forward-regexp "[ \t]+\n" nil "noerror")
          (replace-match "\n")))
      (progn
        (goto-char (point-min))
        (while (search-forward-regexp "\n\n\n+" nil "noerror")
          (replace-match "\n\n")))
      (progn
        (goto-char (point-max))
        (while (equal (char-before) 32) 
          (delete-char -1)))))
  (when (buffer-file-name)
    (save-buffer)))

(defun xah-make-backup ()
  "Make a backup copy of current file or dired marked files.
If in dired, backup current file or marked files.
The backup file name is
 ‹name›~‹timestamp›~
example:
 file.html~20150721T014457~
in the same dir. If such a file already exist, it's overwritten.
If the current buffer is not associated with a file, nothing's done.
URL `http://ergoemacs.org/emacs/elisp_make-backup.html'
Version 2015-10-14"
  (interactive)
  (let ((ξfname (buffer-file-name)))
    (if ξfname
        (let ((ξbackup-name
               (concat ξfname "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
          (copy-file ξfname ξbackup-name t)
          (message (concat "Backup saved at: " ξbackup-name)))
      (if (string-equal major-mode "dired-mode")
          (progn
            (mapc (lambda (ξx)
                    (let ((ξbackup-name
                           (concat ξx "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
                      (copy-file ξx ξbackup-name t)))
                  (dired-get-marked-files))
            (message "marked files backed up"))
        (user-error "buffer not file nor dired")))))

(defun xah-make-backup-and-save ()
  "Backup of current file and save, or backup dired marked files.
For detail, see `xah-make-backup'.
If the current buffer is not associated with a file nor dired, nothing's done.
URL `http://ergoemacs.org/emacs/elisp_make-backup.html'
Version 2015-10-14"
  (interactive)
  (if (buffer-file-name)
      (progn
        (xah-make-backup)
        (when (buffer-modified-p)
          (save-buffer)))
    (progn
      (xah-make-backup))))

(defun xah-delete-current-file-make-backup (&optional φno-backup-p)
  "Delete current file, makes a backup~, closes the buffer.

Backup filename is “‹name›~‹date time stamp›~”. Existing file of the same name is overwritten. If the file is not associated with buffer, the backup file name starts with “xx_”.

When called with `universal-argument', don't create backup.

URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2015-05-26"
  (interactive "P")
  (let* (
         (ξfname (buffer-file-name))
         (ξbuffer-is-file-p ξfname)
         (ξbackup-suffix (concat "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
    (if ξbuffer-is-file-p
        (progn
          (save-buffer ξfname)
          (when (not φno-backup-p)
            (copy-file
             ξfname
             (concat ξfname ξbackup-suffix)
             t))
          (delete-file ξfname)
          (message "Deleted. Backup created at 「%s」." (concat ξfname ξbackup-suffix)))
      (when (not φno-backup-p)
        (widen)
        (write-region (point-min) (point-max) (concat "xx" ξbackup-suffix))
        (message "Backup created at 「%s」." (concat "xx" ξbackup-suffix))))
    (kill-buffer (current-buffer))))

(defun xah-delete-current-file-copy-to-kill-ring ()
  "Delete current buffer/file and close the buffer, push content to `kill-ring'.
URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2015-08-12"
  (interactive)
  (progn
    (kill-new (buffer-string))
    (message "Buffer content copied to kill-ring.")
    (when (buffer-file-name)
      (when (file-exists-p (buffer-file-name))
        (progn
          (delete-file (buffer-file-name))
          (message "Deleted: 「%s」." (buffer-file-name)))))
    (let ((buffer-offer-save nil))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(defun xah-delete-current-file (&optional φno-backup-p)
  "Delete current buffer/file and close the buffer.
If buffer is a file, makes a backup~, else, push file content to `kill-ring'.

The backup filename is “‹filename›~‹date time stamp›~”. Existing file of the same name is overwritten. If the file is not associated with buffer, the backup file name starts with “xx_”.

URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2015-09-02"
  (interactive "P")
  (progn
    (if (buffer-file-name)
        (xah-delete-current-file-make-backup φno-backup-p)
      (xah-delete-current-file-copy-to-kill-ring))))



(defun xah-search-current-word ()
  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)
  (let ( ξp1 ξp2 )
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq ξp1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq ξp2 (point))))
    (setq mark-active nil)
    (when (< ξp1 (point))
      (goto-char ξp1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties ξp1 ξp2))))

(defun xah-open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let (
          (process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    )))

(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-01-26"
  (interactive)
  (let* (
         (ξfile-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (ξdo-it-p (if (<= (length ξfile-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when ξdo-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (ξfpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" ξfpath t t))) ξfile-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (ξfpath) (shell-command (format "open \"%s\"" ξfpath)))  ξfile-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (ξfpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" ξfpath))) ξfile-list))))))

(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-12-10"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (message "Microsoft Windows not supported. File a bug report or pull request."))
   ((string-equal system-type "darwin")
    (message "Mac not supported. File a bug report or pull request."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "x-terminal-emulator"
                     (concat "--working-directory=" default-directory) )))))

(defun xah-next-window-or-frame ()
  "Switch to next window or frame.
If current frame has only one window, switch to next frame.
If `universal-argument' is called first, do switch frame."
  (interactive)
  (if (null current-prefix-arg)
      (if (one-window-p)
          (other-frame 1)
        (other-window 1))
    (other-frame 1)))

(defun xah-describe-major-mode ()
  "Display inline doc for current `major-mode'."
  (interactive)
  (describe-function major-mode))



(setq xah-dvorak-to-qwerty-kmap
      '(
        ("a" . "a")
        ("b" . "n")
        ("c" . "i")
        ("d" . "h")
        ("e" . "d")
        ("f" . "y")
        ("g" . "u")
        ("h" . "j")
        ("i" . "g")
        ("j" . "c")
        ("k" . "v")
        ("l" . "p")
        ("m" . "m")
        ("n" . "l")
        ("o" . "s")
        ("p" . "r")
        ("q" . "x")
        ("r" . "o")
        ("s" . ";")
        ("t" . "k")
        ("u" . "f")
        ("v" . ".")
        ("w" . ",")
        ("x" . "b")
        ("y" . "t")
        ("z" . "/")
        ("." . "e")
        ("," . "w")
        ("'" . "q")
        (";" . "z")
        ("/" . "[")
        ("[" . "-")
        ("]" . "=")
        ("=" . "]")
        ("-" . "'")))

(defun xah-dvorak-to-qwerty (charstr)
  "Convert dvorak key to qwerty. charstr is single char string."
  (interactive)
  (cdr (assoc charstr xah-dvorak-to-qwerty-kmap)))

(defun xah-qwerty-to-dvorak (charstr)
  "Convert qwerty key to dvorak. charstr is single char string."
  (interactive)
  (car (rassoc charstr xah-dvorak-to-qwerty-kmap)))

(defun xah-fly-map-keys (kmap-name key-cmd-alist)
  "similar to `define-key' but map over a alist."
  (interactive)
  (mapc
   (lambda (pair)
     (define-key kmap-name (kbd (car pair)) (cdr pair)))
   key-cmd-alist))


;; keymaps

(defvar xah-fly-swapped-1827-p nil "Boolean. If true, it means keys 1 and 8 are swapped, and 2 and 7 are swapped. See: http://xahlee.info/kbd/best_number_key_layout.html")

(defvar xah-fly-key-map nil "Keybinding for `xah-fly-keys' minor mode.")
(progn
  (setq xah-fly-key-map (make-sparse-keymap))

  ;; (define-key xah-fly-key-map (kbd "'") 'self-insert-command)

  )

;; commands in search-map and facemenu-keymap
(xah-fly-map-keys
 (define-prefix-command 'xah-highlight-keymap)
 '(("." . isearch-forward-symbol-at-point)
   ("b" . facemenu-set-bold)
   ("f" . font-lock-fontify-block)
   ("c" . center-line)
   ("d" . facemenu-set-default)
   ("h ." . highlight-symbol-at-point)
   ("h f" . hi-lock-find-patterns)
   ("h l" . highlight-lines-matching-regexp)
   ("h p" . highlight-phrase)
   ("h r" . highlight-regexp)
   ("h u" . unhighlight-regexp)
   ("h w" . hi-lock-write-interactive-patterns)
   ("i" . facemenu-set-italic)
   ("l" . facemenu-set-bold-italic)
   ("o" . facemenu-set-face)
   ("p" . center-paragraph)
   ("s" . isearch-forward-symbol)
   ("u" . facemenu-set-underline)
   ("w" . isearch-forward-word)))

(xah-fly-map-keys
 (define-prefix-command 'xah-leader-tab-keymap)
 '(
   ("TAB" . indent-for-tab-command)

   ("i" . complete-symbol)
   ("g" . indent-rigidly)
   ("r" . indent-region)
   ("s" . indent-sexp)

   ("e '" . abbrev-prefix-mark)
   ("e e" . edit-abbrevs)
   ("e p" . expand-abbrev)
   ("e r" . expand-region-abbrevs)
   ("e u" . unexpand-abbrev)
   ("e g" . add-global-abbrev)
   ("e a" . add-mode-abbrev)
   ("e v" . inverse-add-global-abbrev)
   ("e l" . inverse-add-mode-abbrev)
   ("e n" . expand-jump-to-next-slot)
   ("e p" . expand-jump-to-previous-slot)))



(xah-fly-map-keys
 (define-prefix-command 'xah-leader-c-keymap)
 '(
   ("," . xah-open-in-external-app)
   ("." . find-file)
   ("c" . bookmark-bmenu-list)
   ("h" . recentf-open-files)
   ("l" . bookmark-set)
   ("n" . xah-new-empty-buffer)
   ("o" . xah-open-in-desktop)
   ("p" . xah-open-last-closed)
   ("f" . xah-open-recently-closed)
   ("y" . xah-list-recently-closed)
   ("r" . bookmark-jump)
   ("s" . write-file)
   ("t" . ibuffer)))

(xah-fly-map-keys
 (define-prefix-command 'xah-help-keymap)
 '(
   (";" . Info-goto-emacs-command-node)
   ("a" . apropos-command)
   ("b" . describe-bindings)
   ("c" . describe-char)
   ("d" . apropos-documentation)
   ("e" . view-echo-area-messages)
   ("f" . describe-face)
   ("g" . info-lookup-symbol)
   ("h" . describe-function)
   ("i" . info)
   ("j" . man)
   ("k" . describe-input-method)
   ("K" . Info-goto-emacs-key-command-node)
   ("l" . view-lossage)
   ("m" . xah-describe-major-mode)
   ("n" . describe-key )
   ("o" . describe-language-environment)
   ("p" . finder-by-keyword)
   ("r" . apropos-variable)
   ("s" . describe-syntax)
   ("t" . describe-variable)
   ("u" . elisp-index-search)
   ("v" . apropos-value)
   ("z" . describe-coding-system)))

(xah-fly-map-keys
 (define-prefix-command 'xah-leader-i-keymap) ; commands in goto-map
 '(
   ("TAB" . move-to-column)
   ("c" . goto-char)
   ("t" . goto-line)
   ("n" . next-error)
   ("d" . previous-error
    )))

(xah-fly-map-keys
 ;; commands here are harmless (safe). They don't modify text.
 ;; they turn on minor/major mode, change display, prompt, start shell, etc.
 (define-prefix-command 'xah-harmless-keymap)
 '(
   ("SPC" . whitespace-mode)
   ("'" . frame-configuration-to-register)
   (";" . window-configuration-to-register)
   ("1" . set-input-method)
   ("2" . global-hl-line-mode)
   ("4" . linum-mode)
   ("5" . visual-line-mode)
   ("6" . calendar)
   ("7" . calc)
   ("8" . shell)
   ("9" . shell-command)
   ("0" . shell-command-on-region)
   ("a" . text-scale-adjust)
   ("b" . toggle-debug-on-error)
   ("c" . toggle-case-fold-search)
   ("d" . narrow-to-page)
   ("e" . eshell)
   ("g" . toggle-frame-fullscreen)
   ("h" . widen)
   ("i" . make-frame-command)
   ("k" . menu-bar-open)
   ("l" . toggle-word-wrap)
   ("m" . global-linum-mode)
   ("n" . narrow-to-region)
   ("p" . read-only-mode) ; toggle-read-only
   ("q n" . set-file-name-coding-system)
   ("q s" . set-next-selection-coding-system)
   ("q c" . universal-coding-system-argument)
   ("q f" . set-buffer-file-coding-system)
   ("q k" . set-keyboard-coding-system)
   ("q l" . set-language-environment)
   ("q p" . set-buffer-process-coding-system)
   ("q r" . revert-buffer-with-coding-system)
   ("q t" . set-terminal-coding-system)
   ("q x" . set-selection-coding-system)
   ("s" . flyspell-buffer)
   ("t" . narrow-to-defun)
   ("v" . variable-pitch-mode)
   ("w" . eww)
   ("x" . save-some-buffers)
   ("z" . abort-recursive-edit)))

(xah-fly-map-keys
   ;; kinda replacement related
 (define-prefix-command 'xah-edit-cmds-keymap)
 '(
   ("SPC" . rectangle-mark-mode)
   ("9" . delete-non-matching-lines)
   ("0" . delete-duplicate-lines)
   ("," . apply-macro-to-region-lines)
   ("." . kmacro-start-macro)
   ("p" . kmacro-end-macro)
   ("e" . call-last-kbd-macro)
   ("c" . replace-rectangle)
   ("d" . delete-rectangle)
   ("g" . kill-rectangle)
   ("h" . list-matching-lines)
   ("l" . clear-rectangle)
   ("n" . rectangle-number-lines)
   ("o" . open-rectangle)
   ("r" . yank-rectangle)
   ("t" . delete-matching-lines)
   ("y" . delete-whitespace-rectangle)))

(xah-fly-map-keys
 (define-prefix-command 'xah-leader-t-keymap)
 '(
   ("SPC" . xah-clean-whitespace-and-save)
   ("3" . point-to-register)
   ("4" . jump-to-register)
   ("." . sort-lines)
   ("," . sort-numeric-fields)
   ("'" . reverse-region)
   ("d" . mark-defun)
   ("h" . xah-close-current-buffer)
   ("j" . copy-to-register)
   ("k" . insert-register)
   ("l" . increment-register)
   ("m" . xah-make-backup-and-save)
   ("n" . repeat-complex-command)
   ("p" . query-replace-regexp)
   ("r" . copy-rectangle-to-register)
   ("t" . repeat)
   ("w" . xah-next-window-or-frame)
   ("z" . number-to-register)))

(xah-fly-map-keys
 (define-prefix-command 'xah-danger-keymap)
 '(
   ("DEL" . xah-delete-current-file)
   ("." . eval-buffer)
   ("e" . eval-defun)
   ("m" . eval-last-sexp)
   ("p" . eval-expression)
   ("u" . eval-region)
   ("q" . save-buffers-kill-terminal)
   ("w" . delete-frame)
   ("j" . xah-run-current-file)))

(xah-fly-map-keys
 (define-prefix-command 'xah-insertion-keymap)
 '(
   ("RET" . insert-char)
   ("SPC" . xah-insert-unicode)
;; xah-insert-date
   ("b" . xah-insert-black-lenticular-bracket【】)
   ("c" . xah-insert-ascii-single-quote)
   ("d" . xah-insert-double-curly-quote“”)
   ("f" . xah-insert-emacs-quote)
   ("g" . xah-insert-ascii-double-quote)
   ("h" . xah-insert-brace) ; {}
   ("i" . xah-insert-curly-single-quote‘’)
   ("m" . xah-insert-corner-bracket「」)
   ("n" . xah-insert-square-bracket) ; []
   ("p" . xah-insert-single-angle-quote‹›)
   ("r" . xah-insert-tortoise-shell-bracket〔〕)
   ("s" . xah-insert-string-assignment)
   ("t" . xah-insert-paren)
   ("u" . xah-insert-greater-less)
   ("w" . xah-insert-angle-bracket〈〉)
   ("W" . xah-insert-double-angle-bracket《》)
   ("y" . xah-insert-double-angle-quote«»)))

(progn
  (define-prefix-command 'xah-fly-leader-key-map)
  (define-key xah-fly-leader-key-map (kbd "SPC") 'xah-fly-insert-mode-activate)
  (define-key xah-fly-leader-key-map (kbd "TAB") xah-leader-tab-keymap)

  (define-key xah-fly-leader-key-map (kbd ".") xah-highlight-keymap)

  (define-key xah-fly-leader-key-map (kbd "'") nil)
  (define-key xah-fly-leader-key-map (kbd ",") nil)
  (define-key xah-fly-leader-key-map (kbd "-") nil)
  (define-key xah-fly-leader-key-map (kbd "/") nil)
  (define-key xah-fly-leader-key-map (kbd ";") nil)
  (define-key xah-fly-leader-key-map (kbd "=") nil)
  (define-key xah-fly-leader-key-map (kbd "[") nil)
  (define-key xah-fly-leader-key-map (kbd "\\") nil)
  (define-key xah-fly-leader-key-map (kbd "`") nil)

  (define-key xah-fly-leader-key-map (kbd "3") 'delete-other-windows)
  (define-key xah-fly-leader-key-map (kbd "4") 'split-window-right)

  (define-key xah-fly-leader-key-map (kbd "8") 'find-file-at-point)
  (define-key xah-fly-leader-key-map (kbd "9") 'ispell-word)

  (define-key xah-fly-leader-key-map (kbd "a") 'mark-whole-buffer)
  (define-key xah-fly-leader-key-map (kbd "b") 'end-of-buffer)
  (define-key xah-fly-leader-key-map (kbd "c") xah-leader-c-keymap)
  (define-key xah-fly-leader-key-map (kbd "d") 'beginning-of-buffer)
  ;; (define-key xah-fly-leader-key-map (kbd "e") nil)
  (define-key xah-fly-leader-key-map (kbd "f") 'xah-search-current-word)
  (define-key xah-fly-leader-key-map (kbd "g") 'isearch-forward)
  (define-key xah-fly-leader-key-map (kbd "h") 'xah-help-keymap)
  (define-key xah-fly-leader-key-map (kbd "i") xah-leader-i-keymap)
  (define-key xah-fly-leader-key-map (kbd "j") 'xah-cut-all-or-region)
  (define-key xah-fly-leader-key-map (kbd "k") 'yank)
  (define-key xah-fly-leader-key-map (kbd "l") 'recenter-top-bottom)
  (define-key xah-fly-leader-key-map (kbd "m") 'dired-jump)
  (define-key xah-fly-leader-key-map (kbd "n") xah-harmless-keymap)
  ;; (define-key xah-fly-leader-key-map (kbd "o") nil)
  (define-key xah-fly-leader-key-map (kbd "p") 'query-replace)
  (define-key xah-fly-leader-key-map (kbd "q") 'xah-copy-all-or-region)
  (define-key xah-fly-leader-key-map (kbd "r") xah-edit-cmds-keymap)
  (define-key xah-fly-leader-key-map (kbd "s") 'save-buffer)
  (define-key xah-fly-leader-key-map (kbd "t") xah-leader-t-keymap)
  (define-key xah-fly-leader-key-map (kbd "u") 'switch-to-buffer)
  ;; (define-key xah-fly-leader-key-map (kbd "v") nil)
  (define-key xah-fly-leader-key-map (kbd "w") xah-danger-keymap)
  ;; (define-key xah-fly-leader-key-map (kbd "x") nil)
  (define-key xah-fly-leader-key-map (kbd "y") 'xah-copy-file-path)
  (define-key xah-fly-leader-key-map (kbd "z") 'comment-dwim))


;;;; misc

;; these commands have keys in emacs, but right now i decided not to give them a key

;; C-x C-p	mark-page
;; C-x C-l	downcase-region
;; C-x C-u	upcase-region

;; C-x C-t	transpose-lines
;; C-x C-o	delete-blank-lines

;; C-x C-r	find-file-read-only
;; C-x C-v	find-alternate-file

;; C-x =	what-cursor-position, use describe-char instead
;; C-x <	scroll-left
;; C-x >	scroll-right
;; C-x [	backward-page
;; C-x ]	forward-page
;; C-x ^	enlarge-window

;; C-x {	shrink-window-horizontally
;; C-x }	enlarge-window-horizontally
;; C-x DEL	backward-kill-sentence

;; C-x C-z	suspend-frame
;; C-x +	balance-windows

;; C-x k	kill-buffer , use xah-close-current-buffer
;; C-x l	count-lines-page
;; C-x m	compose-mail


;; undecided yet

;; C-x e	kmacro-end-and-call-macro
;; C-x q	kbd-macro-query
;; C-x C-k	kmacro-keymap

;; C-x C-d	list-directory
;; C-x C-n	set-goal-column
;; C-x ESC	Prefix Command
;; C-x $	set-selective-display
;; C-x *	calc-dispatch
;; C-x -	shrink-window-if-larger-than-buffer
;; C-x .	set-fill-prefix

;; C-x 4	ctl-x-4-prefix
;; C-x 5	ctl-x-5-prefix
;; C-x 6	2C-command
;; C-x ;	comment-set-column

;; C-x `	next-error
;; C-x f	set-fill-column
;; C-x i	insert-file
;; C-x n	Prefix Command
;; C-x r	Prefix Command

;; C-x C-k C-a	kmacro-add-counter
;; C-x C-k C-c	kmacro-set-counter
;; C-x C-k C-d	kmacro-delete-ring-head
;; C-x C-k C-e	kmacro-edit-macro-repeat
;; C-x C-k C-f	kmacro-set-format
;; C-x C-k TAB	kmacro-insert-counter
;; C-x C-k C-k	kmacro-end-or-call-macro-repeat
;; C-x C-k C-l	kmacro-call-ring-2nd-repeat
;; C-x C-k RET	kmacro-edit-macro
;; C-x C-k C-n	kmacro-cycle-ring-next
;; C-x C-k C-p	kmacro-cycle-ring-previous
;; C-x C-k C-s	kmacro-start-macro
;; C-x C-k C-t	kmacro-swap-ring
;; C-x C-k C-v	kmacro-view-macro-repeat
;; C-x C-k SPC	kmacro-step-edit-macro
;; C-x C-k b	kmacro-bind-to-key
;; C-x C-k e	edit-kbd-macro
;; C-x C-k l	kmacro-edit-lossage
;; C-x C-k n	kmacro-name-last-macro
;; C-x C-k q	kbd-macro-query
;; C-x C-k r	apply-macro-to-region-lines
;; C-x C-k s	kmacro-start-macro



;; C-x 4 C-f	find-file-other-window
;; C-x 4 C-o	display-buffer
;; C-x 4 .	find-tag-other-window
;; C-x 4 0	kill-buffer-and-window
;; C-x 4 a	add-change-log-entry-other-window
;; C-x 4 b	switch-to-buffer-other-window
;; C-x 4 c	clone-indirect-buffer-other-window
;; C-x 4 d	dired-other-window
;; C-x 4 f	find-file-other-window
;; C-x 4 m	compose-mail-other-window
;; C-x 4 r	find-file-read-only-other-window

;; C-x 6 2	2C-two-columns
;; C-x 6 b	2C-associate-buffer
;; C-x 6 s	2C-split
;; C-x 6 <f2>	2C-two-columns

  ;; (define-key xah-leader-i-keymap (kbd "r") ctl-x-5-map)

;; r C-f     find-file-other-frame
;; r C-o     display-buffer-other-frame
;; r .       find-tag-other-frame
;; r 0       delete-frame
;; r 1       delete-other-frames
;; r 2       make-frame-command
;; r b       switch-to-buffer-other-frame
;; r d       dired-other-frame
;; r f       find-file-other-frame
;; r m       compose-mail-other-frame
;; r o       other-frame
;; r r       find-file-read-only-other-frame

;; (xah-fly-map-keys
;;  (define-prefix-command 'xah-leader-vc-keymap)
;;  '(
;;    ("+" . vc-update)
;;    ("=" . vc-diff)
;;    ("D" . vc-root-diff)
;;    ("L" . vc-print-root-log)
;;    ("a" . vc-update-change-log)
;;    ("b" . vc-switch-backend)
;;    ("c" . vc-rollback)
;;    ("d" . vc-dir)
;;    ("g" . vc-annotate)
;;    ("h" . vc-insert-headers)
;;    ("l" . vc-print-log)
;;    ("m" . vc-merge)
;;    ("r" . vc-retrieve-tag)
;;    ("s" . vc-create-tag)
;;    ("u" . vc-revert)
;;    ("v" . vc-next-action)
;;    ("~" . vc-revision-other-window)))

;; ;; 2013-11-04 make emacs auto show suggestions when a prefix key is pressed
;; (require 'guide-key)
;; (guide-key-mode 1)


;; setting keys

(progn
  (when xah-fly-use-control-key
    (progn
      (define-key xah-fly-key-map (kbd "<C-next>") 'xah-next-user-buffer)
      (define-key xah-fly-key-map (kbd "<C-prior>") 'xah-previous-user-buffer)
      (define-key xah-fly-key-map (kbd "<C-tab>") 'xah-next-user-buffer)
      (define-key xah-fly-key-map (kbd "<C-S-iso-lefttab>") 'xah-previous-user-buffer)
      (define-key xah-fly-key-map (kbd "C-v") 'yank)
      (define-key xah-fly-key-map (kbd "C-t") 'toggle-input-method)
      (define-key xah-fly-key-map (kbd "C-w") 'xah-close-current-buffer)
      (define-key xah-fly-key-map (kbd "C-z") 'undo)
      (define-key xah-fly-key-map (kbd "C-n") 'xah-new-empty-buffer)
      (define-key xah-fly-key-map (kbd "C-o") 'find-file)
      (define-key xah-fly-key-map (kbd "C-s") 'save-buffer)
      (define-key xah-fly-key-map (kbd "C-S-s") 'write-file)
      (define-key xah-fly-key-map (kbd "C-S-t") 'xah-open-last-closed)
      (define-key xah-fly-key-map (kbd "C-S-n") 'make-frame-command)

      (define-key xah-fly-key-map (kbd "C-,") 'flyspell-goto-next-error)

      (define-key xah-fly-key-map (kbd "C-+") 'text-scale-increase)
      (define-key xah-fly-key-map (kbd "C--") 'text-scale-decrease)
      (define-key xah-fly-key-map (kbd "C-0") (lambda () (interactive) (text-scale-set 0)))))

  (define-key xah-fly-key-map (kbd "M-RET") 'xah-cycle-hyphen-underscore-space)
  (define-key xah-fly-key-map (kbd "M-c") 'xah-toggle-letter-case )
  (define-key xah-fly-key-map (kbd "M-g") 'hippie-expand )
  (define-key xah-fly-key-map (kbd "M-h") 'xah-insert-brace )
  (define-key xah-fly-key-map (kbd "M-m") xah-insertion-keymap)
  (define-key xah-fly-key-map (kbd "M-n") 'xah-insert-square-bracket)
  (define-key xah-fly-key-map (kbd "M-t") 'xah-insert-paren)
  (define-key xah-fly-key-map (kbd "M-d") 'xah-insert-date)
  (define-key xah-fly-key-map (kbd "M-l") 'left-char)

  (define-key xah-fly-key-map (kbd "<home>") 'xah-fly-command-mode-activate)
  (define-key xah-fly-key-map (kbd "<f8>") 'xah-fly-command-mode-activate) ; as backup

  (define-key xah-fly-key-map (kbd "<menu>") xah-fly-leader-key-map)
  (define-key xah-fly-key-map (kbd "<f9>") xah-fly-leader-key-map) ; as backup

  (define-key xah-fly-key-map (kbd "C-8") 'xah-fly-command-mode-activate)

  (define-key xah-fly-key-map (kbd "<f11>") 'xah-previous-user-buffer)
  (define-key xah-fly-key-map (kbd "<f12>") 'xah-next-user-buffer)
  (define-key xah-fly-key-map (kbd "<C-f11>") 'xah-previous-emacs-buffer)
  (define-key xah-fly-key-map (kbd "<C-f12>") 'xah-next-emacs-buffer)

  ;; these are good for compatibilty. e.g. you set a mouse or other device to use this key for generic OS wide operation, and it should work in emacs too
  (define-key xah-fly-key-map (kbd "<C-prior>") 'xah-previous-user-buffer)
  (define-key xah-fly-key-map (kbd "<C-next>") 'xah-next-user-buffer)

  (progn
    ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
    (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
    (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )

    (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
    (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)

    (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
    (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer)
    ;;
    )
  ;;
  )



(defvar xah-fly-insert-state-q t "Boolean value. true means insertion mode is on.")
(setq xah-fly-insert-state-q t)

(defun xah-fly-command-mode-init ()
  "set command mode keys"
  (interactive)
  (progn

    (define-key xah-fly-key-map (kbd "'") 'xah-compact-uncompact-block)
    (define-key xah-fly-key-map (kbd ",") 'xah-shrink-whitespaces)
    (define-key xah-fly-key-map (kbd "-") nil)
    (define-key xah-fly-key-map (kbd ".") 'backward-kill-word)
    (define-key xah-fly-key-map (kbd ";") nil)
    (define-key xah-fly-key-map (kbd ":") nil)
    (define-key xah-fly-key-map (kbd "/") 'xah-backward-equal-sign)
    (define-key xah-fly-key-map (kbd "\\") nil)
    (define-key xah-fly-key-map (kbd "=") 'xah-forward-equal-sign)
    (define-key xah-fly-key-map (kbd "[") 'xah-backward-quote )
    (define-key xah-fly-key-map (kbd "]") 'xah-forward-quote-twice)
    (define-key xah-fly-key-map (kbd "`") 'other-frame)
    (define-key xah-fly-key-map (kbd "~") nil)
    (define-key xah-fly-key-map (kbd "SPC") xah-fly-leader-key-map)

    (if xah-fly-swapped-1827-p
        (progn
          (define-key xah-fly-key-map (kbd "8") nil)
          (define-key xah-fly-key-map (kbd "7") nil)
          (define-key xah-fly-key-map (kbd "2") 'xah-select-current-line)
          (define-key xah-fly-key-map (kbd "1") 'xah-extend-selection))
      (progn
        (define-key xah-fly-key-map (kbd "1") nil)
        (define-key xah-fly-key-map (kbd "2") nil)
        (define-key xah-fly-key-map (kbd "7") 'xah-select-current-line)
        (define-key xah-fly-key-map (kbd "8") 'xah-extend-selection)))

    (define-key xah-fly-key-map (kbd "3") 'delete-other-windows)
    (define-key xah-fly-key-map (kbd "4") 'split-window-below)
    (define-key xah-fly-key-map (kbd "5") 'delete-window)
    (define-key xah-fly-key-map (kbd "6") 'xah-select-current-block)
    (define-key xah-fly-key-map (kbd "9") 'xah-select-text-in-quote)
    (define-key xah-fly-key-map (kbd "0") 'xah-backward-punct)

    (define-key xah-fly-key-map (kbd "a") (if (fboundp 'smex) 'smex 'execute-extended-command ))
    (define-key xah-fly-key-map (kbd "b") 'isearch-forward)
    (define-key xah-fly-key-map (kbd "c") 'previous-line)
    (define-key xah-fly-key-map (kbd "d") 'xah-beginning-of-line-or-block)
    (define-key xah-fly-key-map (kbd "e") 'delete-backward-char)
    (define-key xah-fly-key-map (kbd "f") 'undo)
    (define-key xah-fly-key-map (kbd "g") 'backward-word)
    (define-key xah-fly-key-map (kbd "h") 'backward-char)
    (define-key xah-fly-key-map (kbd "i") 'xah-fly-insert-mode-activate)
    (define-key xah-fly-key-map (kbd "j") 'xah-cut-line-or-region)
    (define-key xah-fly-key-map (kbd "k") 'yank)
    (define-key xah-fly-key-map (kbd "l") 'xah-fly-insert-mode-activate-insert-space)
    (define-key xah-fly-key-map (kbd "m") 'xah-backward-left-bracket)
    (define-key xah-fly-key-map (kbd "n") 'forward-char)
    (define-key xah-fly-key-map (kbd "o") 'xah-fly-insert-mode-activate-newline)
    (define-key xah-fly-key-map (kbd "p") 'kill-word)
    (define-key xah-fly-key-map (kbd "q") 'xah-copy-line-or-region)
    (define-key xah-fly-key-map (kbd "r") 'forward-word)
    (define-key xah-fly-key-map (kbd "s") 'xah-end-of-line-or-block)
    (define-key xah-fly-key-map (kbd "t") 'next-line)
    (define-key xah-fly-key-map (kbd "u") 'delete-char)
    (define-key xah-fly-key-map (kbd "v") 'xah-forward-right-bracket)
    (define-key xah-fly-key-map (kbd "w") 'xah-next-window-or-frame)
    (define-key xah-fly-key-map (kbd "x") 'exchange-point-and-mark)
    (define-key xah-fly-key-map (kbd "y") 'set-mark-command)
    (define-key xah-fly-key-map (kbd "z") 'comment-dwim)

    (define-key xah-fly-key-map (kbd "C") 'pop-global-mark)
    (define-key xah-fly-key-map (kbd "E") nil)
    (define-key xah-fly-key-map (kbd "T") 'xah-jump-to-last-local-mark)
    (define-key xah-fly-key-map (kbd "U") 'kill-line)

    ;;
    ))

(defun xah-fly-insert-mode-init ()
  "Set insertion mode keys"
  (interactive)
  (progn
    (define-key xah-fly-key-map (kbd "'") nil)
    (define-key xah-fly-key-map (kbd ",") nil)
    (define-key xah-fly-key-map (kbd "-") nil)
    (define-key xah-fly-key-map (kbd ".") nil)
    (define-key xah-fly-key-map (kbd "/") nil)
    (define-key xah-fly-key-map (kbd ";") nil)
    (define-key xah-fly-key-map (kbd "=") nil)
    (define-key xah-fly-key-map (kbd "[") nil)
    (define-key xah-fly-key-map (kbd "\\") nil)
    (define-key xah-fly-key-map (kbd "]") nil)
    (define-key xah-fly-key-map (kbd "`") nil)
    (define-key xah-fly-key-map (kbd "~") nil)
    (define-key xah-fly-key-map (kbd "SPC") nil)

    (define-key xah-fly-key-map (kbd "1") nil)
    (define-key xah-fly-key-map (kbd "2") nil)
    (define-key xah-fly-key-map (kbd "3") nil)
    (define-key xah-fly-key-map (kbd "4") nil)
    (define-key xah-fly-key-map (kbd "5") nil)
    (define-key xah-fly-key-map (kbd "6") nil)
    (define-key xah-fly-key-map (kbd "7") nil)
    (define-key xah-fly-key-map (kbd "8") nil)
    (define-key xah-fly-key-map (kbd "9") nil)
    (define-key xah-fly-key-map (kbd "0") nil)

    (define-key xah-fly-key-map (kbd "a") nil)
    (define-key xah-fly-key-map (kbd "b") nil)
    (define-key xah-fly-key-map (kbd "c") nil)
    (define-key xah-fly-key-map (kbd "d") nil)
    (define-key xah-fly-key-map (kbd "e") nil)
    (define-key xah-fly-key-map (kbd "f") nil)
    (define-key xah-fly-key-map (kbd "g") nil)
    (define-key xah-fly-key-map (kbd "h") nil)
    (define-key xah-fly-key-map (kbd "i") nil)
    (define-key xah-fly-key-map (kbd "j") nil)
    (define-key xah-fly-key-map (kbd "k") nil)
    (define-key xah-fly-key-map (kbd "l") nil)
    (define-key xah-fly-key-map (kbd "m") nil)
    (define-key xah-fly-key-map (kbd "n") nil)
    (define-key xah-fly-key-map (kbd "o") nil)
    (define-key xah-fly-key-map (kbd "p") nil)
    (define-key xah-fly-key-map (kbd "q") nil)
    (define-key xah-fly-key-map (kbd "r") nil)
    (define-key xah-fly-key-map (kbd "s") nil)
    (define-key xah-fly-key-map (kbd "t") nil)
    (define-key xah-fly-key-map (kbd "u") nil)
    (define-key xah-fly-key-map (kbd "v") nil)
    (define-key xah-fly-key-map (kbd "w") nil)
    (define-key xah-fly-key-map (kbd "x") nil)
    (define-key xah-fly-key-map (kbd "y") nil)
    (define-key xah-fly-key-map (kbd "z") nil)

    (define-key xah-fly-key-map (kbd "C") nil)
    (define-key xah-fly-key-map (kbd "E") nil)
    (define-key xah-fly-key-map (kbd "T") nil)
    (define-key xah-fly-key-map (kbd "U") nil)

    ;;
))

(defun xah-fly-mode-toggle ()
  "Switch between {insertion, command} modes."
  (interactive)
  (if xah-fly-insert-state-q
      (xah-fly-command-mode-activate)
    (xah-fly-insert-mode-activate)))

;; automatic save buffer when switching to command mode
(add-hook 'xah-fly-command-mode-activate-hook 'xah-fly-save-buffer-if-file)

(defun xah-fly-save-buffer-if-file ()
  "Save current buffer if it is a file."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)))

(defun xah-fly-command-mode-activate ()
  "Activate command mode."
  (interactive)
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (setq xah-fly-insert-state-q nil )
  (xah-fly-command-mode-init)
  (run-hooks 'xah-fly-command-mode-activate-hook))

(defun xah-fly-insert-mode-activate ()
  "Activate insertion mode."
  (interactive)
  (modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
  (setq xah-fly-insert-state-q t )
  (xah-fly-insert-mode-init)
  (run-hooks 'xah-fly-insert-mode-activate-hook))

(defun xah-fly-insert-mode-activate-newline ()
  "Activate insertion mode, inserting 2 newlines below."
  (interactive)
  (xah-fly-insert-mode-activate)
  (open-line 1))

(defun xah-fly-insert-mode-activate-insert-space ()
  "Insert a space, then activate insertion mode."
  (interactive)
  (insert " ")
  (xah-fly-insert-mode-activate))



;; when in going into minibuffer, switch to insertion mode.
(add-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)
(add-hook 'minibuffer-exit-hook 'xah-fly-command-mode-activate)

;; when in shell mode, switch to insertion mode.
(add-hook 'shell-mode-hook 'xah-fly-insert-mode-activate)

;; ;; when in shell mode, switch to insertion mode.
;; (add-hook 'dired-mode-hook 'xah-fly-keys-off)



;; experimental. auto switch back to command mode after some sec of idle time
;; (setq xah-fly-timer-id (run-with-idle-timer 20 t 'xah-fly-command-mode-activate))
;; (cancel-timer xah-fly-timer-id)

(define-minor-mode xah-fly-keys
  "A modal keybinding set, like vim, but based on ergonomic principles, like Dvorak layout."
  1 "ξflykeys" xah-fly-key-map
  (xah-fly-command-mode-activate))

(defun xah-fly-keys-off ()
  "Turn off xah-fly-keys minor mode."
  (interactive)
  (xah-fly-keys 0))

(provide 'xah-fly-keys)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-fly-keys.el ends here
