gotham-theme
============

.. image:: https://raw.github.com/wasamasa/gotham-theme/master/img/gotham.png

..

    It's the colorscheme we set that defines us. (Batman)

About
-----

Gotham is a **very dark** Emacs color theme.  It's a port of the
`Gotham theme for Vim <https://github.com/whatyouhide/vim-gotham>`_
and tries adhering closely to the original.  There is support for both
GUI and terminal Emacs.  The terminal version assumes that your
terminal emulator comes with a customized 16-color palette from the
`Gotham contrib repository
<https://github.com/whatyouhide/gotham-contrib>`_, however you can
enable 256 color support by customizing
``gotham-tty-extended-palette`` in exchange for a negligible amount of
color degradation.

Screenshots
-----------

.. image:: https://raw.github.com/wasamasa/gotham-theme/master/img/scrot.png

Thanks to `Norbert Klar <https://github.com/norbertklar>`_ for the
following three!

.. image:: https://raw.github.com/wasamasa/gotham-theme/master/img/go.png

.. image:: https://raw.github.com/wasamasa/gotham-theme/master/img/go2.png

.. image:: https://raw.github.com/wasamasa/gotham-theme/master/img/go3.png

Installation
------------

To install the theme via ``package.el``, set up the `Marmalade
<https://marmalade-repo.org/>`_ or `MELPA (Stable)
<http://melpa.org/>`_ repository if you haven't already and do ``M-x
package-install RET gotham-theme RET``.

Alternatively, you can install the theme manually by downloading
``gotham-theme.el`` and putting it in a suitable location such as
``~/.emacs.d/themes/``.  Add the following to your init file:

.. code:: cl

    (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

Once the theme is installed, you can enable it with ``M-x load-theme
RET gotham RET``.  Make sure no other themes are enabled with ``M-x
disable-theme``.  To enable the theme automatically at startup, add
the following to your init file:

.. code:: cl

    (load-theme 'gotham t)

Contributing
------------

If you find bugs, have suggestions or any other problems, feel free to
report an issue on the issue tracker or hit me up on IRC, I'm always on
``#emacs``.  Patches are welcome, too, just fork, work on a separate
branch and open a pull request with it.
