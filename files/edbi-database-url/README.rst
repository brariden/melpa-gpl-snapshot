.. |travis| image:: https://travis-ci.org/proofit404/edbi-database-url.png
    :target: https://travis-ci.org/proofit404/edbi-database-url
    :alt: Build Status

.. |coveralls| image:: https://coveralls.io/repos/proofit404/edbi-database-url/badge.png
    :target: https://coveralls.io/r/proofit404/edbi-database-url
    :alt: Coverage Status

.. |melpa| image:: http://melpa.org/packages/edbi-database-url-badge.svg
    :target: http://melpa.org/#/edbi-database-url
    :alt: Melpa

=================
Edbi Database Url
=================

|travis| |coveralls| |melpa|

Run Edbi_ with `database url`_.

Usage
-----

Specify database url with environment variable
::

    M-x setenv RET DATABASE_URL RET pgsql://me:secret@localhost:5678/test

Connect to you database
::

    M-x edbi-database-url

Optionally you can specify database url by marking region or type
it interactively.

.. _Edbi: https://github.com/kiwanami/emacs-edbi
.. _database url: http://12factor.net/backing-services
