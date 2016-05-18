.. |melpa| image:: http://melpa.org/packages/ac-anaconda-badge.svg
    :target: http://melpa.org/#/ac-anaconda
    :alt: Melpa

======================
Auto-complete anaconda
======================

|melpa|

Anaconda_ source for auto-complete-mode_.

.. figure:: static/ac-anaconda.png

Installation
------------

You can install this package from Melpa_::

    M-x package-install RET ac-anaconda RET

Usage
-----

Add ``ac-source-anaconda`` to allowed ``ac-sources`` list

.. code:: lisp

    (add-hook 'python-mode-hook 'ac-anaconda-setup)

.. _Anaconda: https://github.com/proofit404/anaconda-mode
.. _auto-complete-mode: https://github.com/auto-complete/auto-complete
.. _Melpa: http://melpa.milkbox.net/
