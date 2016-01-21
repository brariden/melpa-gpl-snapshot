flymake-elixir.el
=================

An Emacs flymake handler for syntax-checking elixir .ex source files.

Installation
------------

If you choose not to use one of the convenient packages in [Melpa][melpa]
or [Marmalade][marmalade],
you'll need to add the directory containing `flymake-elixir.el` to your
`load-path`, and then `(require 'flymake-elixir)`.

You'll also need to install:
- [elixir-mode](https://github.com/elixir-lang/emacs-elixir).
- [flymake-easy](https://github.com/purcell/flymake-easy).

Usage
-----

Add the following to your emacs init file:

    (require 'flymake-elixir)
	(add-hook 'elixir-mode-hook 'flymake-elixir-load)
    
Thanks to
---------
- Steve Purcell for flymake-easy.el and the skeleton of this markdown.
- Andreas Fuchs for the elixir major mode.

[melpa]: http://melpa.milkbox.net
[marmalade]: http://marmalade-repo.org

