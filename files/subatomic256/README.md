About
=====

An emacs 24 (deftheme) color theme. It's super nice! This version is a fork of [subatomic-theme.el](https://github.com/cryon/subatomic) with the colors adjusted to work in a 256 color terminal.

![subatomic256.el - kind of nice, eh?](https://raw.github.com/d11wtq/subatomic256/master/readme-files/subatomic.png "subatomic256 emacs theme")

Installation
============

MELPA
-----

Subatomic256 is available in [MELPA](http://melpa.milkbox.net/). Assuming MELPA is added to your archive list you can list the available packages by typing <code>M-x list-packages</code>, look for subatomic256-theme, mark it for installation by typing <code>i</code> and then execute (install) by typing <code>x</code>.

(Or you can install it directly with <code>M-x package-install RET subatomic256-theme</code>)

After that, enable the theme by <code>M-x load-theme RET subatomic256</code>.

Or if you want to do it in your init file, add:

```lisp
(load-theme 'subatomic256 t)
```

Manual installation
-------------------
If you prefer, you can install Subatomic manually by downloading <code>subatomic256-theme.el</code> and place it somewhere in your <code>custom-theme-load-path</code>.

You can set your <code>custom-theme-load-path</code> by adding this to your <code>.emacs.d</code> or <code>.emacs.d/init.el</code>:

```lisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
```

You should now be able to load Subatomic256 with <code>M-x load-theme RET subatomic256</code>!

Improvements
============

Feel free to report any problems or make suggestions. There are always more faces to color!

Credits
=======

Full credit is given to [John Olsson](https://github.com/cryon) for his awesome work on the Subatomic theme. This version is literally a clone with the colors changed to fit in the 256 color space.
