rigid-tabs.el: Rigidify and adjust the visual alignment of TABs
===============================================================

``rigid-tabs-mode`` "rigidifies" all TABs in the current buffer, preserving
their initial width but making them non-flexible just like a block of spaces.
This allows TABs to be moved around without changing width.

This becomes helpful when viewing diffs, which introduce one or more prefix
characters that alter the target column of the displayed TABs. The resulting
misalignment may make code indentation look suspicious and overall harder to
read, even though it's perfectly aligned when the patch is applied.

The function ``rigid-tabs-rigid-align`` turns on ``rigid-tabs-mode`` and
adjusts the visual alignment of TABs to compensate for the requested amount of
prefix characters. The result is a diff that looks correctly indented, as if
applied on the source.

For convenience, to fix alignment in the various diff/magit modes, use
``rigid-tabs-diff-align``, which detects the amount of prefix for both unified
and context diffs automatically:

.. code:: elisp

  (add-hook 'diff-mode-hook 'rigid-tabs-diff-align)
  (add-hook 'magit-refresh-buffer-hook 'rigid-tabs-diff-align)

In essence, ``rigid-tabs-mode`` turns a buffer displaying TABs like this:

.. image:: http://www.thregr.org/~wavexx/rnd/20150805-rigid_tabs/patch-unaligned-2.png

into this:

.. image:: http://www.thregr.org/~wavexx/rnd/20150805-rigid_tabs/patch-aligned-2.png

without actually changing the content of the buffer.
