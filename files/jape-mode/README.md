[![Build Status](https://secure.travis-ci.org/tanzoniteblack/jape-mode.png?branch=master)](http://travis-ci.org/tanzoniteblack/jape-mode)

# JAPE Mode

Provides Emacs font-lock and basic syntax tables for the [JAPE (Java Annotation Pattern Engine) files](https://gate.ac.uk/sale/tao/splitch8.html#x12-2150008) for the [GATE (General Architecture for Text Engineering)](https://gate.ac.uk/).

## Future work:

Work to be done yet, contributions welcome:

* Indentation
* Embedded Java code

## Installation

### MELPA (Recommended)
Available on [MELPA](https://melpa.org/). To setup MELPA do:

```el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

And then you can install either through

<kbd>M-x package-refresh-contents</kbd>

<kbd>M-x package-install [RET] jape-mode [RET]</kbd>

or keep it in your dotfiles:
```el
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))
```

### Manual

Place in a location where emacs knows to read emacs-lisp files (such as in `.emacs.d/`) and require with `(require 'jape-mode)`.

## License

Copyright © 2005 Ilya Goldin
Copyright © 2014 Ryan Smith

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.
