xah-fly-keys is a efficient keybinding for emacs. (more efficient than vim)

It is a modal mode like vi, but key choices are based on statistics of command call frequency.

--------------------------------------------------
MANUAL INSTALL
put the file xah-fly-keys.el in ~/.emacs.d/lisp/
create the dirs if doesn't exist.

put the following in your emacs init file:

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'xah-fly-keys)
(xah-fly-keys 1)

--------------------------------------------------
HOW TO USE

M-x xah-fly-keys to toggle the mode on/off.

Important command/insert mode switch keys:

xah-fly-command-mode-activate (press 【home】 or 【Ctrl+8】 or 【F8】)
xah-fly-insert-mode-activate  (when in command mode, press letter 【i】 key)

When in command mode:
【i】 activates insertion mode
【SPACE】 is a leader key. For example, 【SPACE p】 calls query-replace.

The leader key sequence basically replace ALL emacs commands that starts with C-x key.

When using xah-fly-keys, you don't need to press Control or Meta, with the following exceptions:

C-c for major mode commands.
C-g for cancel.
C-q for quoted-insert.
C-h for getting a list of keys following a prefix/leader key.

Leader key

All emacs C-x keys have a key sequence. Most commands are 2 to 3 keys. The first key we call it leader key.

You NEVER need to press Ctrl+x

When in command mode, the 【SPACE】 is a leader key.

globally, the leader key is the 【f9】 and 【menu】 key. (on typical PC keyboard, the menu key usually at right side of space bar.)

the following stardard keys with Control are supported, when the variable xah-fly-use-control-key is t

(when xah-fly-use-control-key
    (progn
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
      (define-key xah-fly-key-map (kbd "C-,") 'flyspell-goto-next-error)
      (define-key xah-fly-key-map (kbd "C-+") 'text-scale-increase)
      (define-key xah-fly-key-map (kbd "C--") 'text-scale-decrease)
      (define-key xah-fly-key-map (kbd "C-0") (lambda () (interactive) (text-scale-set 0)))))

That't it.

On the Mac, I highly recommend using a app called Sail to set your capslock to send Home. So that it acts as xah-fly-command-mode-activate. You can set capslock or one of the cmd key to Home. See http://xahlee.info/kbd/Mac_OS_X_keymapping_keybinding_tools.html

I recommend you clone xah-fly-keys.el, and modify it, and use your modified version. Don't worry about upgrade. (I still make key tweaks every week, for the past 3 years.)

If you have a bug, post on github. If you have question, post on xah-fly-keys home page.

For detail about design and other info, see home page at
http://ergoemacs.org/misc/ergoemacs_vi_mode.html

If you like this project, Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.
