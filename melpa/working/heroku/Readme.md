# heroku.el

Enhance Heroku `run` and `pg:psql` commands with Emacs equivalents.

## Installation

Requires `heroku` command-line client to be installed; this simply
shells out and wraps it in a nicer interface. See the
[Toolbelt](https://toolbelt.herokuapp.com) for installation
instructions.

Install this via <kbd>M-x package-install-file</kbd> or from
[Marmalade](http://marmalade-repo.org):

```lisp
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
```

* <kbd>M-x eval-buffer</kbd>
* <kbd>M-x package-refresh-contents</kbd>
* <kbd>M-x package-install RET heroku RET</kbd>

## Usage

<kbd>M-x heroku-sql</kbd> opens up a fancy sql-comint buffer with
syntax highlighting, completion, etc.

<kbd>M-x heroku-run</kbd> runs an arbitrary command in a dyno for a given app.

## License

Copyright © 2012 Phil Hagelberg. Licensed under the same terms as Emacs.
