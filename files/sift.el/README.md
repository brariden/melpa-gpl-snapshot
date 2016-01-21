# sift.el

[![License GPL 2][badge-license]][LICENSE]
[![Coverage Status](https://coveralls.io/repos/nlamirault/sift.el/badge.png?branch=master)](https://coveralls.io/r/nlamirault/sift.el?branch=master)

Master :
* [![MELPA Stable](https://stable.melpa.org/packages/sift-badge.svg)](https://stable.melpa.org/#/sift)
* [![Circle CI](https://circleci.com/gh/nlamirault/sift.el/tree/master.svg?style=svg)](https://circleci.com/gh/nlamirault/sift.el/tree/master)

Develop:
* [![Melpa Status](https://melpa.org/packages/sift-badge.svg)](https://melpa.org/#/sift)
* [![Circle CI](https://circleci.com/gh/nlamirault/sift.el/tree/develop.svg?style=svg)](https://circleci.com/gh/nlamirault/sift.el/tree/develop)

``sift.el`` allows you to search using [sift][] from inside Emacs.

## Installation

The recommended way to install ``sift`` is via [MELPA][]:

    M-x package-install sift

or [Cask][]:

	(depends-on "sift")


## Usage

<kbd>M-x sift-regexp</kbd> or <kbd>M-x projectile-sift</kbd>



## Development

### Cask

``sift.el`` use [Cask][] for dependencies management. Install it and
retrieve dependencies :

    $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
    $ export PATH="$HOME/.cask/bin:$PATH"
    $ cask


### Testing

* Launch unit tests from shell

    $ make clean test

* Using [overseer][] :

Keybinding           | Description
---------------------|------------------------------------------------------------
<kbd>C-c , t</kbd>   | launch unit tests from buffer
<kbd>C-c , b</kbd>   | launch unit tests
<kbd>C-c , g</kbd>   | launch unit tests with tag (find, regexp, ...)

* Tips:

If you want to launch a single unit test, add a specify tag :

```lisp
(ert-deftest test-foobar ()
  :tags '(current)
  ```

And launch it using : <kbd>C-c , g</kbd> and specify tag : *current*


## Support / Contribute

See [here](CONTRIBUTING.md)


## Changelog

A changelog is available [here](ChangeLog.md).


## License

See [LICENSE](LICENSE).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>

[sift.el]: https://github.com/nlamirault/sift.el
[badge-license]: https://img.shields.io/badge/license-GPL_2-green.svg?style=flat
[LICENSE]: https://github.com/nlamirault/sift.el/blob/master/LICENSE

[GNU Emacs]: https://www.gnu.org/software/emacs/
[MELPA]: https://melpa.org/
[Cask]: http://cask.github.io/
[Issue tracker]: https://github.com/nlamirault/sift.el/issues

[overseer]: https://github.com/tonini/overseer.el

[sift]: https://sift-tool.org/

