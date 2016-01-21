sly-company
===========

A [company-mode](http://company-mode.github.io) Emacs completion
backend for [SLY](https://github.com/capitaomorte/sly), a Common Lisp
IDE.

[company-mode](http://company-mode.github.io) is a completion
framework Nikolaj Schumacher. sly-company is based on
[slime-company](https://github.com/anwyn/slime-company).

![screenshot](screenshot.jpg)

## Setup

The recommended install is via
[MELPA](http://melpa.org/#/sly-company), where all you need to add to
your `~/.emacs` or `~/.emacs.d/init/el` init file is:

```el
(add-hook 'sly-mode-hook 'sly-company-mode)
```

Optionally, use `M-x sly-company-mode` in the buffers where you want
completion to happen.

The following bindings for `company-active-map` may be useful:

```el
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)
```

If not using MELPA, precede those lines with

```el
(add-to-list 'load-path "/path/to/company-mode")
(add-to-list 'load-path "/path/to/sly-company")
(require 'sly-company)
```
