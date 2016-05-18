inf-php
=======

Run a php interactive shell on emacs

usage
=====

put inf-php.el to load path, and add `(require 'inf-php)` to your .emacs.


In particular environments, php interactive shell may exit just after launching.
You can avoid this behaviour with `(setq inf-php-enable-launch-workaround t)`.


key bindings
============

Following key bindings are available.

```
  (define-key php-mode-map "\M-\C-x" 'php-send-definition)
  (define-key php-mode-map "\C-x\C-e" 'php-send-last-sexp)
  (define-key php-mode-map "\C-c\C-x" 'php-send-definition)
  (define-key php-mode-map "\C-c\M-x" 'php-send-definition-and-go)
  (define-key php-mode-map "\C-c\C-r" 'php-send-region)
  (define-key php-mode-map "\C-c\M-r" 'php-send-region-and-go)
  (define-key php-mode-map "\C-c\C-z" 'php-switch-to-inf)
  (define-key php-mode-map "\C-c\C-s" 'inf-php))
```
