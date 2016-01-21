# evm.el [![Build Status](https://api.travis-ci.org/rejeep/evm.el.png?branch=master)](http://travis-ci.org/rejeep/evm.el)

Emacs Version Manager

## Installation

Add `evm` to your [Cask](https://github.com/cask/cask) file:

```lisp
(depends-on "evm")
```

## Usage

```lisp
(require 'evm)
```

## API

### evm-find `(name)`

Find file with `name` in currently selected Emacs.

```lisp
(evm-find "emacsclient") ;; => /usr/local/evm/emacs-24.3/bin/emacsclient
```

### evm-emacs `()`

Return absolute path to Emacs binary.

```lisp
(evm-emacs) ;; => /usr/local/evm/emacs-24.3/bin/emacs
```

### evm-emacsclient `()`

Return absolute path to Emacsclient binary.

```lisp
(evm-emacsclient) ;; => /usr/local/evm/emacs-24.3/bin/emacsclient
```

## Contribution

Contribution is much welcome!

Install [cask](https://github.com/cask/cask) if you haven't already,
then:

    $ cd /path/to/evm.el
    $ cask

Run all tests with:

    $ make
