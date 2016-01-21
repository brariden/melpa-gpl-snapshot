flymake-vala.el
===============

An Emacs flymake handler for syntax-checking Vala source code.

Installation
=============

If you choose not to use one of the convenient packages in
[Melpa][melpa] and [Marmalade][marmalade], you'll need to add the
directory containing `flymake-vala.el` to your `load-path`, and then
`(require 'flymake-vala)`. You'll also need to install
[flymake-easy](https://github.com/purcell/flymake-easy).

Usage
=====

Add the following to your emacs init file:

    (require 'flymake-vala)
    (add-hook 'vala-mode-hook 'flymake-vala-load)


[marmalade]: http://marmalade-repo.org
[melpa]: http://melpa.org
