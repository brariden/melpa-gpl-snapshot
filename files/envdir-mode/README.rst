.. |melpa| image:: http://melpa.org/packages/envdir-badge.svg
    :target: http://melpa.org/#/envdir
    :alt: Melpa

===========
Envdir mode
===========

|melpa|

Let's have a look at a typical envdir:

.. code:: bash

    $ tree envs/production
    envs/production
    ├── DJANGO_SETTINGS_MODULE
    ├── MYSITE_DEBUG
    ├── MYSITE_DEPLOY_DIR
    ├── MYSITE_SECRET_KEY
    └── PYTHONSTARTUP

With ``envdir-mode`` you can easily set up internal emacs environment
from this directory.

Installation
------------

You can simply install package from Melpa_::

    M-x package-install RET envdir-mode RET

Usage
-----

Add following block to your emacs configuration

.. code:: lisp

    (envdir-mode)

Now you are available to set envdir directory::

    M-x envdir-mode-set RET /path/to/envs/production RET

You can unset current envdir with::

    M-x envdir-mode-unset

.. _envdir: https://github.com/jezdez/envdir
.. _Melpa: https://melpa.org
