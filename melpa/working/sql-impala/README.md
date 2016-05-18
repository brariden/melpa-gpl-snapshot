sql-impala.el
===============

sql-impala adds support for [impala-shell][1] to Emacs' [SQL Mode][2]

Installation
------------

`sql-impala` is available from Melpa and Melpa-Stable. Add one of the archives
to `package-archives`:

* To use Melpa:

        (require 'package)
        (add-to-list 'package-archives
                     '("melpa" . "http://melpa.org/packages/") t)

* To use Melpa-Stable:

        (require 'package)
        (add-to-list 'package-archives
                     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

Then update your package listing:

    M-x package-refresh-contents RET

And install:

    M-x package-install RET sql-impala RET

Usage
-----

`sql-impala` is used like other interactive SQL modes:

    M-x sql-impala

This will prompt for the server and database name to which to connect, and
invoke `impala-shell` to connect to Impala.

Configuration
-------------

A number of customizations are available for `sql-impala`:

* `sql-impala-program` - controls which command is invoked to start the Impala
  command interpreter.
* `sql-impala-login-parameters` - controls which login parameters are required
  to start an Impala session.
* `sql-impala-options` - additional options passed to `sql-impala-program` when
  invoked.

Copying
-------

See [COPYING](COPYING)

[1]: http://www.cloudera.com/documentation/enterprise/latest/topics/impala_impala_shell.html
[2]: https://www.emacswiki.org/emacs?SqlMode
