frame-restore.el
================

Save and restore position and size of the Emacs frame.

**Note:** In Emacs 24.4 the built-in Desktop Save mode restores frames as well.
If you are using Emacs 24.4, you are **strongly** advised to use Desktop Save
mode instead, via:

```scheme
(desktop-save-mode)
```

Frame Restore mode will display a warning if enabled in Emacs 24.4.

Installation
------------

Install the ELPA package from [MELPA][] or [Marmalade][] with `M-x
package-install RET frame-restore`, and add the following to your `init.el`:

```scheme
(frame-restore-mode)
```

Customization
-------------

`M-x customize-group RET frame-restore`

License
-------

frame-restore.el is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

frame-restore.el is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See [COPYING][] for details.

[melpa]: http://melpa.milkbox.net
[marmalade]: http://marmalade-repo.org/
[copying]: https://github.com/lunaryorn/frame-restore.el/blob/master/COPYING
