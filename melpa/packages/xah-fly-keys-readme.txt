xah-fly-keys is a efficient keybinding system for emacs. (more efficient than vim)

It is a modal mode, like vi, but key choices are based on statistics of command call frequency, and key position easy-to-press score.

xah-fly-keys does not bind any Control key, nor Meta keys (except 3, but you can turn off). use emacs as is, because no Control or Meta are used. Just leave xah-fly-keys in insertion mode.

To learn xah-fly-keys, is like learning vi for the first time. You'll need one month to adopt.

xah-fly-keys is currently optimized for Dvorak layout only. If you touch-type QWERTY or other, you will need to rebind keys. I recommend you fork it and modify the keys for your own use. See home page for detail: http://ergoemacs.org/misc/ergoemacs_vi_mode.html

--------------------------------------------------
MANUAL INSTALL
put the file xah-fly-keys.el in ~/.emacs.d/lisp/

put the following in your emacs init file:

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'xah-fly-keys)
(xah-fly-keys 1)

--------------------------------------------------
USE

Important commands and default keys

command: xah-fly-keys (【Ctrl+7】) to toggle the mode on/off.

It is necessary to toggle xah-fly-keys mode. Because, it gives you access to special modes that use letter keys, such as dired

Important command/insert mode switch keys:

xah-fly-command-mode-activate (key: 【home】, 【Ctrl+8】)
xah-fly-insert-mode-activate (when in command mode, 【SPACE】)
xah-fly-mode-toggle (toggle between command/insert modes. No key by default.)

When in command mode:
【SPACE】 activates insertion mode
【x】 is a leader key, for emacs one thousands commands

When using xah-fly-keys, you don't need to ever press Control or Meta, with the following exceptions:

C-c for major mode commands.
C-g for cancel. (i recommend you set a easy-key outside of emacs to send C-g)
C-q for quoted-insert. (because C-q is a efficient choice. Because the key after C-q needs Control held-down.)

Leader keys

All emacs C-x keys have a key sequence, starting with a leader key. Most commands are 2 to 3 keys, counting the leader key. For example, isearch is 【‹leader key› g】 (in Dvorak layout), switch-buffer is 【‹leader key› c g】.

You NEVER need to press Ctrl+x

globally, the leader key is the 【menu】 key. (on typical PC keyboard, it's usually at right side of space bar.) You should change this to a easy-key on YOUR keyboard. For example, make left Alt to send menu key signal in your OS or keyboard firmware.

When in command mode, the 【x】 is a leader key.

That is it. You should change the above mentioned critical keys to be ones easy to type on YOUR KEYBOARD.

I recommend you make a copy, and modify it, and use your modified version. Don't worry about upgrade. (I still make key tweaks every week, for the past 3 years.)

If you have a bug, post on github. If you have question, post on xah-fly-keys home page.

For detail and tutorial-like explanation, and about how to remap keys such as capslock outside of emacs, see
http://ergoemacs.org/misc/ergoemacs_vi_mode.html

If you like this project, Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.

TODO
• 2015-09-04 make it support qwerty Keyboard layout
• 2015-09-04 add option to allow support of standard open close save etc keys with Control
• 2015-09-04 add option to allow turn off any Control completely. Same for Meta. (as a way to stop habit)
