opam.el
=======

[![License GPL 3][badge-license]][copying]

[OPAM][] utilities for GNU Emacs 24.

Provides `opam-init` which initialises Emacs from the current OPAM environment.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
[COPYING]: https://github.com/lunaryorn/fancy-battery.el/blob/master/COPYING
[OPAM]: http://opam.ocaml.org


Installation
------------

As usual, from [MELPA][] or [MELPA Stable][], with `M-x package-install RET
opam`.

Then add the following to your init file:

```cl
(opam-init)
```

On OS X, install [exec-path-from-shell][] first, to make sure that OPAM is on
your Emacs’ `exec-path`.

[MELPA]: http://melpa.milkbox.net
[MELPA Stable]: http://melpa-stable.milkbox.net
[exec-path-from-shell]: https://github.com/purcell/exec-path-from-shell

Usage
-----

`opam-init` calls `opam config env` and updates Emacs’ `process-environment` and
`exec-path` accordingly.

License
-------

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See [`COPYING`][copying] for details.
