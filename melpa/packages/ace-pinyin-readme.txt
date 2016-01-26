Demos: See https://github.com/cute-jumper/ace-pinyin

                             _____________

                               ACE-PINYIN

                              Junpeng Qiu
                             _____________


Table of Contents
_________________

1 Setup
2 Usage
3 Traditional Chinese Characters Support
4 Other available commands
.. 4.1 `ace-pinyin-dwim'
.. 4.2 `ace-pinyin-jump-word'
5 Acknowledgment


Jump to Chinese characters using `ace-jump-mode' or `avy'.

UPDATE(2015-11-26): Now jumping to traditional Chinese characters is
supported by setting `ace-pinyin-simplified-chinese-only-p' to `nil'.


[[file:http://melpa.org/packages/ace-pinyin-badge.svg]]
http://melpa.org/#/ace-pinyin

[[file:http://stable.melpa.org/packages/ace-pinyin-badge.svg]]
http://stable.melpa.org/#/ace-pinyin


1 Setup
=======

  ,----
  | (add-to-list 'load-path "/path/to/ace-pinyin.el")
  | (require 'ace-pinyin)
  `----

  Or install via [melpa].


  [melpa] http://melpa.org/#/ace-pinyin


2 Usage
=======

  By default this package is using `ace-jump-mode'. When using
  `ace-jump-mode', the `ace-jump-char-mode' command can jump to Chinese
  characters. If you prefer `avy', you can make `ace-pinyin' use `avy'
  by:
  ,----
  | (setq ace-pinyin-use-avy t)
  `----

  When using `avy', `avy-goto-char', `avy-goto-char-2' and
  `avy-goto-char-in-line' are supported to jump to Chinese characters.

  Note `ace-pinyin-use-avy' variable should be set *BEFORE* you call
  `ace-pinyin-global-mode' or `turn-on-ace-pinyin-mode'.

  Example config to use `ace-pinyin' globally:
  ,----
  | ;; (setq ace-pinyin-use-avy t) ;; uncomment if you want to use `avy'
  | (ace-pinyin-global-mode +1)
  `----

  When the minor mode is enabled, then `ace-jump-char-mode' (or
  `avy-goto-char', depends on your config) will be able to jump to both
  Chinese and English characters. That is, you don't need remember an
  extra command or create extra key bindings in order to jump to Chinese
  character. Just enable the minor mode and use `ace-jump-char-mode' (or
  `avy-goto-char') to jump to Chinese characters.

  Besides, all other packages using `ace-jump-char-mode' (or
  `avy-goto-char') will also be able to jump to Chinese characters. For
  example, if you've installed [ace-jump-zap], it will also be able to
  zap to a Chinese character by the first letter of pinyin. Note
  `ace-jump-zap' is implemented by using `ace-jump-mode', so you can't
  use `avy' in this case. You can check out my fork of `ace-jump-zap'
  using `avy': [avy-zap].


  [ace-jump-zap] https://github.com/waymondo/ace-jump-zap

  [avy-zap] https://github.com/cute-jumper/avy-zap


3 Traditional Chinese Characters Support
========================================

  By default, `ace-pinyin' only supports simplified Chinese characters.
  You can make `ace-pinyin' aware of traditional Chinese characters by
  the following setting:
  ,----
  | (setq ace-pinyin-simplified-chinese-only-p)
  `----


4 Other available commands
==========================

4.1 `ace-pinyin-dwim'
~~~~~~~~~~~~~~~~~~~~~

  If called with no prefix, it can jump to both Chinese characters and
  English letters. If called with prefix, it can only jump to Chinese
  characters.


4.2 `ace-pinyin-jump-word'
~~~~~~~~~~~~~~~~~~~~~~~~~~

  Using this command, you can jump to the start of a sequence of Chinese
  characters(/i.e./ Chinese word) by typing the sequence of the first
  letters of these character's pinyins. If called without prefix, this
  command will read user's input with a default timeout 1 second(You can
  customize the timeout value). If called with prefix, then it will read
  input from the minibuffer and starts search after you press "enter".


5 Acknowledgment
================

  - The ASCII char to Chinese character table(`ace-pinyin--simplified-char-table'
    in code) is from
    [https://github.com/redguardtoo/find-by-pinyin-dired].
  - @erstern adds the table for traditional Chinese characters.
