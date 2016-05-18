.. |melpa| image:: http://melpa.org/packages/edbi-minor-mode-badge.svg
    :target: http://melpa.org/#/edbi-minor-mode
    :alt: Melpa

===============
Edbi Minor Mode
===============

|melpa|

Use edbi_ with regular SQL files.

Usage
-----

Add following line to your configuration:

.. code:: lisp

    (add-hook 'sql-mode-hook 'edbi-minor-mode)

Connect to your database with ``edbi:open-db-viewer``.  Open regular
SQL file.  You can use following commands to perform SQL queries:

==========  =================================
Keybinding  Description
==========  =================================
C-c C-b     edbi-minor-mode-execute-buffer
C-c C-c     edbi-minor-mode-execute-paragraph
C-c C-r     edbi-minor-mode-execute-region
C-c C-s     edbi-minor-mode-execute
==========  =================================

.. _edbi: https://github.com/kiwanami/emacs-edbi
