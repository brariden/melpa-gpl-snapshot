[![Build Status](https://travis-ci.org/editorconfig/editorconfig-emacs.svg?branch=master)](https://travis-ci.org/editorconfig/editorconfig-emacs)
[![MELPA](http://melpa.org/packages/editorconfig-badge.svg)](http://melpa.org/#/editorconfig)
[![MELPA Stable](https://stable.melpa.org/packages/editorconfig-badge.svg)](https://stable.melpa.org/#/editorconfig)


# EditorConfig Emacs Plugin 

This is an [EditorConfig][] plugin for [Emacs](https://www.gnu.org/software/emacs/).

## Installation

Download the [EditorConfig C Core][] and follow the instructions in the README
and INSTALL files to install it.

Or, when no Core executable is found, this plugin uses Emacs Lisp implemented
Core as a fallback.

In either case, copy `.el` files in this repository to `~/.emacs.d/lisp`
and add the following to your `~/.emacs` file:

    (add-to-list 'load-path "~/.emacs.d/lisp")
    (require 'editorconfig)
    (editorconfig-mode 1)

Alternatively, you can find the package available on
[Marmalade](http://marmalade-repo.org/packages/editorconfig) and
[MELPA](http://melpa.org/#/editorconfig).

## Supported properties

Current Emacs plugin coverage for EditorConfig's [properties][]:

* `indent_style`
* `indent_size`
* `tab_width`
* `end_of_line`
* `charset`
* `trim_trailing_whitespace`
* `insert_final_newline = true` is supported
* <del>`insert_final_newline = false`</del> is not enforced
  (as in trailing newlines actually being removed automagically),
  we just buffer-locally override any preferences that would auto-add them
  to files `.editorconfig` marks as trailing-newline-free
* `max_line_length`
* `root` (only used by EditorConfig core)

Not yet covered properties marked with <del>over-strike</del>
– pull requests implementing missing features warmly welcomed!
Typically, you will want to tie these to native functionality,
or the configuration of existing packages handling the feature.

As several packages have their own handling of, say, indention,
we might not yet cover some mode you use, but we try to add the
ones that show up on our radar. Similarly, we don't yet hook
in to all different packages for whitespace trimming to inform
them about editorconfig settings, but aim for better coverage
of things like [ws-trim](ftp://ftp.lysator.liu.se/pub/emacs/ws-trim.el).

## Testing

Make and [CMake](https://cmake.org) must be installed to run the tests.

To run the tests:

    $ make test

## Submitting Bugs and Feature Requests

Bugs, feature requests, and other issues should be submitted to the issue
tracker: https://github.com/editorconfig/editorconfig-emacs/issues

## License

EditorConfig Emacs Plugin is free software: you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program.  If not, see <https://www.gnu.org/licenses/>.

[EditorConfig]: http://editorconfig.org
[EditorConfig C Core]: https://github.com/editorconfig/editorconfig-core-c
[properties]: http://editorconfig.org/#supported-properties
