# Flycheck CStyle Checker

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/flycheck-cstyle-badge.svg)](http://melpa.org/#/flycheck-cstyle)
[![Build Status](https://travis-ci.org/alexmurray/flycheck-cstyle.svg?branch=master)](https://travis-ci.org/alexmurray/flycheck-cstyle)

Integrate [cstyle](https://github.com/alexmurray/cstyle) with
[flycheck](http://www.flycheck.org) to automatically check the
style of your C/C++ code on the fly.

## Installation

### MELPA

The preferred way to install `flycheck-cstyle` is via
[MELPA](http://melpa.org) - then you can just <kbd>M-x package-install RET
flycheck-cstyle RET</kbd>

To enable then simply add the following to your init file:

```emacs-lisp
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-cstyle)
     (flycheck-cstyle-setup)
     ;; chain after cppcheck since this is the last checker in the upstream
     ;; configuration
     (flycheck-add-next-checker 'c/c++-cppcheck '(warning . cstyle))))
```

If you do not use `cppcheck` then chain after whichever checker you do use
(ie. clang / gcc / irony etc)

```emacs-lisp
(flycheck-add-next-checker 'c/c++-clang '(warning . cstyle))
```

### Manual

If you would like to install the package manually, download or clone it and
place within Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'flycheck-cstyle)
(flycheck-cstyle-setup)
```

NOTE: This will also require the manual installation of `flycheck` if you have
not done so already.

## License

Copyright © 2015 Alex Murray

Distributed under GNU GPL, version 3.
