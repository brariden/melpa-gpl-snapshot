# gold-mode

This is a .gold file editing package.
you can see about gold [here](https://github.com/yosssi/gold).

## Installation

From MELPA, you can install like this:

`M-x package-install gold-mode RET`

## Emacs's Configuration

If you installed from MELPA, this package may change to gold-mode
automatically when you opened .gold extension file.
So you don't need to write below codes.

Below part is for manual(git or el-get) installation user:

```lisp
(add-to-list 'load-path "path/to/gold-mode-directory")
(autoload 'gold-mode "gold-mode")
(add-to-list 'auto-mode-alist '("\\.gold$" . gold-mode))
```
