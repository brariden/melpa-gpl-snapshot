![Hoa](http://static.hoa-project.net/Image/Hoa_small.png)

[![MELPA](http://melpa.org/packages/hoa-pp-mode-badge.svg)](http://melpa.org/#/hoa-pp-mode)
[![MELPA Stable](http://stable.melpa.org/packages/hoa-pp-mode-badge.svg)](http://stable.melpa.org/#/hoa-pp-mode)


Hoa is a **modular**, **extensible** and **structured** set of PHP libraries.
Moreover, Hoa aims at being a bridge between industrial and research worlds.

# Contributions/Emacs/Pp

This repository contains tools for the PP grammar description language from
[`Hoa\Compiler`](http://central.hoa-project.net/Resource/Library/Compiler).

It provides a major mode for editing PP grammars with the following features:

- Syntax coloration,
- Auto-indentation,
- Imenu support.

![PP Major mode screenshot](http://central.hoa-project.net/Resource/Contributions/Emacs/Pp/screenshots/sample.png?format=raw)

## Installation

If you have setup MELPA or MELPA stable repositories, type `M-x package-install
hoa-pp-mode`.

If you want to install it by hand, you must first install
[names](https://github.com/Malabarba/names).

Then, clone this repository on your computer.

```sh
git clone git@github.com:hoaproject/Contributions-Emacs-Pp.git hoa-pp-mode
```

Finally, add this code in your Emacs configuration:

```emacs-lisp
(add-to-list 'load-path "/path/to/hoa-pp-mode")
(require 'hoa-pp-mode)
```

## Quick usage

Simply open a `.pp` file with Emacs. `hoa-pp-mode` will automatically be enabled.

## Documentation

Different documentations can be found on the website:
[http://hoa-project.net/](http://hoa-project.net/).

## License

Hoa is under the New BSD License (BSD-3-Clause). Please, see
[`LICENSE`](http://hoa-project.net/LICENSE).
