sproto-mode [![MELPA](http://melpa.org/packages/sproto-mode-badge.svg)](http://melpa.org/#/sproto-mode)
=======================================================================================================

> Emacs major mode for editing [sproto](https://github.com/cloudwu/sproto)

![](test.png)

How to Install
--------------

Install sproto-mode from [MELPA](http://melpa.org):

```
M-x package-install RET sproto-mode
```

Or put `sproto-mode.el` in Emacs load-path, and add this line to your .emacs file:

```lisp
(require 'sproto-mode)
(add-to-list 'auto-mode-alist '("\\.sproto\\'" . sproto-mode))
```
