
.. |travis| image:: https://travis-ci.org/proofit404/tern-django.png
    :target: https://travis-ci.org/proofit404/tern-django
    :alt: Build Status

.. |coveralls| image:: https://coveralls.io/repos/proofit404/tern-django/badge.png
    :target: https://coveralls.io/r/proofit404/tern-django
    :alt: Coverage Status

.. |requires| image:: https://requires.io/github/proofit404/tern-django/requirements.svg
    :target: https://requires.io/github/proofit404/tern-django/requirements
    :alt: Requirements Status

.. |melpa| image:: http://melpa.org/packages/tern-django-badge.svg
    :target: http://melpa.org/#/tern-django
    :alt: Melpa

.. |melpa-stable| image:: http://stable.melpa.org/packages/tern-django-badge.svg
    :target: http://stable.melpa.org/#/tern-django
    :alt: Melpa Stable

===========
Tern Django
===========

|travis| |coveralls| |requires| |melpa| |melpa-stable|

Create Tern_ projects for Django_ applications.

Use awesome Tern_ auto-complete to develop your Django_ project with
zero configuration.

Obviously all javascript code of application stored in application
static folder.  So we can write standard ``.tern-project`` file into
application root if static folder exists.  We can extend this project
looking into application templates.  We can add javascript from other
applications or download external library from internet and make it
accessible for tern.

Installation
------------

You can install Emacs package from Melpa_:
::

    M-x package-install RET tern-django RET

Or you can install python script only from Pypi_:
::

    pip install tern-django

Usage
-----

Setup your project variables and run ``tern-django`` command:
::

    M-x setenv RET DJANGO_SETTINGS_MODULE RET project.settings
    M-x setenv RET PYTHONPATH RET /home/user/path/to/project/
    M-x tern-django

In case you install it as python package activate your development
environment and run ``tern_django.py`` script.
::

    . /path/to/project/virtual_env/bin/activate
    export DJANGO_SETTINGS_MODULE=project.settings
    export PYTHONPATH=/path/to/project
    tern_django.py

Contributing
============

Attach ``tern_django.py`` output with debug option enable to any bag
report.  You can customize ``tern-django-debug`` variable within
Emacs.  Or directly run script with ``--debug`` option.

Known issues
============

If script randomly fails with following message:
::

    OperationalError: database is locked

Check that your ``sqlite`` installation was compiled with
``HAVE_USLEEP`` flag enabled.

.. _Tern: http://ternjs.net
.. _Django: https://www.djangoproject.com
.. _Melpa: http://melpa.org
.. _Pypi: https://pypi.python.org/pypi
