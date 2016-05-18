# edit-at-point.el
edit(copy,cut...) things(word,symbol..) under cursor, if failed to find the thing, move backward until buffer beginning.

# sample keybinding config
```
(require 'edit-at-point)
(require 'bind-key)
(bind-keys
  ("C-S-a". edit-at-point-word-copy)
  ("C-S-b". edit-at-point-word-cut)
  ("C-S-c". edit-at-point-word-delete)
  ("C-S-d". edit-at-point-word-paste)
  ("C-S-e". edit-at-point-symbol-copy)
  ("C-S-f". edit-at-point-symbol-cut)
  ("C-S-g". edit-at-point-symbol-delete)
  ("C-S-h". edit-at-point-symbol-paste)
  ("C-S-i". edit-at-point-str-copy)
  ("C-S-i". edit-at-point-str-cut)
  ("C-S-i". edit-at-point-str-delete)
  ("C-S-i". edit-at-point-str-paste)
  ("C-S-m". edit-at-point-line-copy)
  ("C-S-n". edit-at-point-line-cut)
  ("C-S-o". edit-at-point-line-delete)
  ("C-S-p". edit-at-point-line-paste)
  ("C-S-q". edit-at-point-line-dup)
  ("C-S-r". edit-at-point-line-up)
  ("C-S-s". edit-at-point-line-down)
  ("C-S-t". edit-at-point-paren-copy)
  ("C-S-u". edit-at-point-paren-cut)
  ("C-S-v". edit-at-point-paren-delete)
  ("C-S-w". edit-at-point-paren-paste)
  ("C-S-x". edit-at-point-paren-dup)
  ("C-S-y". edit-at-point-defun-copy)
  ("C-S-z". edit-at-point-defun-cut)
  ("C-{"  . edit-at-point-defun-delete)
  ("C-:"  . edit-at-point-defun-paste)
  ("C-\"" . edit-at-point-defun-dup))
```
# license
Released under the MIT license
